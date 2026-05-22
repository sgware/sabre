package edu.uky.cs.nil.sabre.comp;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.TreeSet;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Mapping;
import edu.uky.cs.nil.sabre.Signature;
import edu.uky.cs.nil.sabre.Trigger;
import edu.uky.cs.nil.sabre.Universe;
import edu.uky.cs.nil.sabre.etree.EventSet;
import edu.uky.cs.nil.sabre.logic.Arithmetic;
import edu.uky.cs.nil.sabre.logic.Atom;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Conditional;
import edu.uky.cs.nil.sabre.logic.Disjunction;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.util.ImmutableArray;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * A problem compiler is a data structure for converting {@link Fluent
 * fluents}, {@link edu.uky.cs.nil.sabre.Event events}, and {@link Expression
 * logical expressions} into {@link CompiledFluent compiled fluents}, {@link
 * CompiledEvent compiled events}, and logical expressions using compiled
 * elements. Problem compilers are generally used to create the elements of
 * a {@link CompiledProblem compiled problem}.
 * 
 * @author Stephen G. Ware
 */
public class ProblemCompiler {

	/** The universe of types and entities used by the problem being compiled */
	public final Universe universe;
	
	/** A mapping of fluents to compiled fluents */
	protected final LinkedHashMap<Fluent, CompiledFluent> fluents = new LinkedHashMap<>();
	
	/** A mapping of actions to compiled actions */
	protected final LinkedHashMap<Action, CompiledAction> actions = new LinkedHashMap<>();
	
	/** A mapping of triggers to compiled triggers */
	protected final LinkedHashMap<Trigger, CompiledTrigger> triggers = new LinkedHashMap<>();
	
	/** A mapping of objects to equivalent objects that use compiled elements */
	protected final HashMap<Object, Object> mapping = new HashMap<>();
	
	/**
	 * Constructs a new problem compiled using the given universe.
	 * 
	 * @param universe the universe the compiled problem will use
	 */
	public ProblemCompiler(Universe universe) {
		this.universe = universe;
		mapping.put(False.FALSE, False.FALSE);
		mapping.put(True.TRUE, True.TRUE);
		mapping.put(Clause.NULL, Clause.NULL);
	}
	
	/**
	 * Constructs a new problem compiler and compiles a given collection of
	 * fluents.
	 * 
	 * @param universe the universe the compiled problem will use
	 * @param fluents a collection of fluents to compile
	 */
	public ProblemCompiler(Universe universe, Iterable<Fluent> fluents) {
		this(universe);
		ArrayList<Fluent> list = new ArrayList<>();
		for(Fluent fluent : fluents)
			list.add(new Fluent(fluent.characters, fluent.signature, fluent.type, fluent.comment));
		LinkedHashMap<Fluent, Integer> map = new LinkedHashMap<>();
		for(Fluent fluent : list)
			add(fluent, map);
		int index = 0;
		for(Fluent fluent : list)
			map.put(fluent, index++);
		for(Fluent fluent : map.keySet())
			if(map.get(fluent) == -1)
				map.put(fluent, index++);
		for(Fluent fluent : map.keySet()) {
			CompiledFluent compiled = makeFluent(map.get(fluent), fluent);
			this.fluents.put(fluent, compiled);
			this.mapping.put(fluent, compiled);
		}
	}
	
	private static final void add(Fluent fluent, LinkedHashMap<Fluent, Integer> map) {
		if(fluent.characters.size() > 0)
			add(fluent.removeFirstCharacter(), map);
		map.put(fluent, -1);
	}
	
	/**
	 * Constructs a new problem compiler and compiles a given collection of
	 * fluents, actions, and triggers.
	 * 
	 * @param universe the universe the compiled problem will use
	 * @param fluents a collection of fluents to compile
	 * @param actions a collection of actions to compile
	 * @param triggers a collection of triggers to compile
	 */
	public ProblemCompiler(Universe universe, Iterable<Fluent> fluents, Iterable<Action> actions, Iterable<Trigger> triggers) {
		this(universe, fluents);
		for(Action action : actions)
			compile(action);
		for(Trigger trigger : triggers)
			compile(trigger);
	}
	
	/**
	 * Returns a {@link ImmutableSet set} of all {@link CompiledFluent compiled
	 * fluents} created so far.
	 * 
	 * @return a set of all compiled fluents
	 */
	public ImmutableSet<CompiledFluent> getFluents() {
		TreeSet<CompiledFluent> sorted = new TreeSet<>((f1, f2) -> f1.id - f2.id);
		for(CompiledFluent fluent : fluents.values())
			sorted.add(fluent);
		return new ImmutableSet<>(sorted);
	}
	
	/**
	 * Returns a {@link EventSet set} of all {@link CompiledAction
	 * compiled actions} created so far.
	 * 
	 * @return a set of all compiled actions
	 */
	public EventSet<CompiledAction> getActions() {
		return new EventSet<>(actions.values());
	}
	
	/**
	 * Returns a {@link EventSet set} of all {@link CompiledTrigger
	 * compiled triggers} created so far.
	 * 
	 * @return a set of all compiled triggers
	 */
	public EventSet<CompiledTrigger> getTriggers() {
		return new EventSet<>(triggers.values());
	}
	
	/**
	 * Compiles an object, replacing it or its parts with compiled elements.
	 * For example, fluents are replaced with {@link CompiledFluent compiled
	 * fluents}, and logical expressions that contain fluents will return an
	 * equivalent logical expression with its fluents replaced with compiled
	 * fluents. This method accepts a wide variety of objects, including
	 * compiled and non-compiled {@link Fluent fluents}, compiled and
	 * non-compiled {@link edu.uky.cs.nil.sabre.Event events}, and various
	 * {@link Expression logical expressions} that are needed to create the
	 * preconditions and effects of events {@link Expression#toPrecondition()
	 * in disjunctive normal form}.
	 * 
	 * @param <T> the type of object to be compiled
	 * @param original the object to compile
	 * @return a compiled replacement for the object
	 */
	@SuppressWarnings("unchecked")
	public <T> T compile(T original) {
		Object key = original;
		if(key instanceof Fluent && !(key.getClass().equals(Fluent.class))) {
			Fluent fluent = (Fluent) key;
			key = (T) new Fluent(fluent.characters, fluent.signature, fluent.type, fluent.comment);
		}
		else if(key instanceof Action && !(key.getClass().equals(Action.class))) {
			Action action = (Action) key;
			key = (T) new Action(action.signature, action.precondition, action.effect, action.consenting, action.observing, action.comment);
		}
		else if(key instanceof Trigger && !(key.getClass().equals(Trigger.class))) {
			Trigger trigger = (Trigger) key;
			key = (T) new Trigger(trigger.signature, trigger.precondition, trigger.effect, trigger.comment);
		}		
		Object replacement = mapping.get(key);
		if(replacement != null)
			return (T) replacement;
		else if(key instanceof ImmutableArray)
			replacement = ((ImmutableArray<?>) key).apply(o -> compile(o));
		else if(key instanceof Signature)
			replacement = makeSignature((Signature) key);
		else if(key instanceof Fluent) {
			Fluent fluent = (Fluent) key;
			fluent.mustBeGround();
			if(fluent.characters.size() > 0)
				compile(fluent.removeFirstCharacter());
			replacement = makeFluent(fluents.size(), fluent);
			fluents.put(fluent, (CompiledFluent) replacement);
		}
		else if(key instanceof Arithmetic)
			replacement = makeArithmetic((Arithmetic) key);
		else if(key instanceof Precondition)
			replacement = makePrecondition((Precondition) key);
		else if(key instanceof Effect)
			replacement = makeEffect((Effect) key);
		else if(key instanceof Clause)
			replacement = makeClause((Clause<?>) key);
		else if(key instanceof Disjunction)
			replacement = makeDisjunction((Disjunction<Clause<Precondition>>) key);
		else if(key instanceof Conditional)
			replacement = makeConditional((Conditional<Disjunction<Clause<Precondition>>>) key);
		else if(key instanceof Action) {
			Action action = (Action) key;
			action.signature.mustBeGround();
			replacement = makeAction(actions.size() + triggers.size(), action);
			actions.put(action, (CompiledAction) replacement);
		}
		else if(key instanceof Trigger) {
			Trigger trigger = (Trigger) key;
			trigger.signature.mustBeGround();
			replacement = makeTrigger(actions.size() + triggers.size(), trigger);
			triggers.put(trigger, (CompiledTrigger) replacement);
		}
		else
			replacement = original;
		mapping.put(key, replacement);
		return (T) replacement;
	}
	
	/**
	 * Converts a {@link Mapping mapping} to a {@link CompiledMapping compiled
	 * mapping}.
	 * 
	 * @param <I> the type of expression returned by the original mapping
	 * @param <O> the type of expression the compiled mapping will return
	 * @param original the original mapping
	 * @param function a function which maps expressions from the original
	 * mapping to expressions to be returned by the new mapping
	 * @return a compiled mapping
	 */
	@SuppressWarnings("unchecked")
	public final <I extends Expression, O extends Expression> CompiledMapping<O> compile(Mapping<I> original, Function<I, O> function) {
		Object replacement = mapping.get(original);
		if(replacement == null) {
			O[] compiled = (O[]) new Expression[universe.characters.size()];
			for(Character character : universe.characters)
				compiled[character.id] = compile(function.apply(original.get(character)));
			replacement = new CompiledMapping<>(compile(new ImmutableArray<>(compiled)));
			mapping.put(original, replacement);
		}
		return (CompiledMapping<O>) replacement;
	}
	
	/**
	 * Compiles a {@link Signature signature}, replacing its elements with
	 * compiled elements from this compiler.
	 * 
	 * @param signature the original signature
	 * @return the compiled signature
	 */
	protected Signature makeSignature(Signature signature) {
		return new Signature(signature.name, compile(signature.arguments));
	}
	
	/**
	 * Compiles a {@link Fluent fluent}, replacing its elements with compiled
	 * elements from this compiler.
	 * 
	 * @param id the {@link CompiledFluent#id ID number} the new compiled
	 * fluent should have
	 * @param fluent the original fluent
	 * @return the compiled fluent
	 */
	protected CompiledFluent makeFluent(int id, Fluent fluent) {
		if(fluent.characters.size() == 0)
			return new CompiledFluent(id, compile(fluent.signature), compile(fluent.type), fluent.comment);
		else
			return new CompiledFluent(id, compile((Character) fluent.characters.get(0)), (CompiledFluent) compile(fluent.removeFirstCharacter()));
	}
	
	/**
	 * Compiles an {@link Arithmetic arithmetic expression}, replacing its
	 * elements with compiled elements from this compiler.
	 * 
	 * @param arithmetic the original arithmetic expression
	 * @return the compiled arithmetic expression
	 */
	protected Arithmetic makeArithmetic(Arithmetic arithmetic) {
		return new Arithmetic(arithmetic.operator, compile(arithmetic.left), compile(arithmetic.right));
	}
	
	/**
	 * Compiled a {@link Precondition precondition}, replacing its elements
	 * with compiled elements from this compiler.
	 * 
	 * @param precondition the original precondition expression
	 * @return the compiled precondition expression
	 */
	protected Precondition makePrecondition(Precondition precondition) {
		return new Precondition(precondition.operator, compile((Fluent) precondition.left), compile(precondition.right));
	}
	
	/**
	 * Compiles an {@link Effect effect}, replacing its elements with compiled
	 * elements from this compiler.
	 * 
	 * @param effect the original effect expression
	 * @return the compiled effect expression
	 */
	protected Effect makeEffect(Effect effect) {
		return new Effect(compile(effect.condition), compile(effect.fluent), compile(effect.value));
	}
	
	/**
	 * Compiles a {@link Clause clause}, replacing its elements with compiled
	 * elements from this compiler.
	 * 
	 * @param <A> the type of {@link Atom atom} used by clause
	 * @param clause the original clause
	 * @return the compiled clause
	 */
	@SuppressWarnings("unchecked")
	protected <A extends Atom> Clause<A> makeClause(Clause<A> clause) {
		Clause<A> compiled = (Clause<A>) Clause.EMPTY;
		for(A atom : clause)
			compiled = compiled.add(compile(atom));
		return compiled;
	}
	
	/**
	 * Compiles a {@link Disjunction disjunction} in {@link
	 * Expression#toPrecondition() disjunctive normal form}, replacing its
	 * elements with compiled elements from this compiler.
	 * 
	 * @param disjunction the original disjunction
	 * @return the compiled disjunction
	 */
	protected Disjunction<Clause<Precondition>> makeDisjunction(Disjunction<Clause<Precondition>> disjunction) {
		return new Disjunction<>(compile(disjunction.arguments));
	}
	
	/**
	 * Compiles a {@link Conditional conditional} whose conditions are in
	 * {@link Expression#toPrecondition() disjunctive normal form}, replacing
	 * its elements with compiled elements from this compiler.
	 * 
	 * @param conditional the original conditional
	 * @return the compiled conditional
	 */
	protected Conditional<Disjunction<Clause<Precondition>>> makeConditional(Conditional<Disjunction<Clause<Precondition>>> conditional) {
		return new Conditional<>(compile(conditional.conditions), compile(conditional.branches));
	}
	
	/**
	 * Compiles an {@link Action action} into a {@link CompiledAction compiled
	 * action}, replacing its elements with compiled elements from this
	 * compiler.
	 * 
	 * @param id the {@link CompiledAction#id ID number} the compiled action
	 * should have
	 * @param action the original action
	 * @return the compiled action
	 */
	protected CompiledAction makeAction(int id, Action action) {
		return new CompiledAction(
			id,
			compile(action.signature),
			compile(action.precondition.toPrecondition()),
			compile(action.effect.toEffect()),
			compile(action.consenting.cast(Character.class)),
			compile(action.observing, e -> e.toPrecondition()),
			action.comment
		);
	}
	
	/**
	 * Compiles a {@link Trigger trigger} into a {@link CompiledTrigger
	 * compiled trigger}, replacing its elements with compiled elements from
	 * this compiler.
	 * 
	 * @param id the {@link CompiledTrigger#id ID  number} the compiled trigger
	 * should have
	 * @param trigger the original trigger
	 * @return the compiled trigger
	 */
	protected CompiledTrigger makeTrigger(int id, Trigger trigger) {
		return new CompiledTrigger(
			id,
			compile(trigger.signature),
			compile(trigger.precondition.toPrecondition()),
			compile(trigger.effect.toEffect()),
			trigger.comment
		);
	}
}