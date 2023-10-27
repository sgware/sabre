package edu.uky.cs.nil.sabre.comp;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.FiniteState;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Problem;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Signature;
import edu.uky.cs.nil.sabre.Trigger;
import edu.uky.cs.nil.sabre.Universe;
import edu.uky.cs.nil.sabre.etree.EventSet;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Conditional;
import edu.uky.cs.nil.sabre.logic.Disjunction;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * A compiled problem is a {@link Problem problem} which has been pre-processed
 * so that its fluents are {@link CompiledFluent compiled fluents}, so that its
 * events are {@link CompiledEvent compiled events}, and so that it defines a
 * full {@link #start a state state}. Various compilers produce different kinds
 * of compiled problems, such as the {@link Grounder grounder}, {@link
 * FluentExplicitizer fluent explicitizer}, {@link Simplifier simplifier}, and
 * {@link ActionShuffler action shuffler}.
 * 
 * @author Stephen G. Ware
 */
public class CompiledProblem extends Problem {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The compiled fluents tracked in every state of the problem */
	public final ImmutableSet<CompiledFluent> fluents;
	
	/** The set of all compiled events that can ever occur */
	public final ImmutableSet<CompiledEvent> events;
	
	/** All the compiled actions defined in this problem */
	public final EventSet<CompiledAction> actions;
	
	/** All the compiled triggers defined in this problem */
	public final EventSet<CompiledTrigger> triggers;
	
	/** A clause representing the initial state */
	public final Clause<Effect> initial;
	
	/** The initial state of the problem before planning begins */
	public final FiniteState start;
	
	/** The author's utility function */
	public final Conditional<Disjunction<Clause<Precondition>>> utility;
	
	/**
	 * A compiled mapping which returns a utility function for each character
	 */
	public final CompiledMapping<Conditional<Disjunction<Clause<Precondition>>>> utilities;

	/**
	 * Constructs a new compiled problem.
	 * 
	 * @param name the name
	 * @param universe the universe
	 * @param fluents the compiled fluents
	 * @param actions the compiled actions
	 * @param triggers the compiled triggers
	 * @param initial the compiled initial clause
	 * @param start the initial state
	 * @param utility the author's utility expression
	 * @param utilities each character's utility expressions
	 * @param comment the comment
	 */
	protected CompiledProblem(
		String name,
		Universe universe,
		ImmutableSet<CompiledFluent> fluents,
		EventSet<CompiledAction> actions,
		EventSet<CompiledTrigger> triggers,
		Clause<Effect> initial,
		FiniteState start,
		Conditional<Disjunction<Clause<Precondition>>> utility,
		CompiledMapping<Conditional<Disjunction<Clause<Precondition>>>> utilities,
		String comment
	) {
		super(
			name,
			universe,
			fluents.cast(Fluent.class),
			actions.cast(Action.class),
			triggers.cast(Trigger.class),
			initial,
			utility,
			utilities.cast(Expression.class),
			comment
		);
		this.fluents = fluents;
		this.events = super.events.cast(CompiledEvent.class);
		this.actions = actions;
		this.triggers = triggers;
		this.initial = initial;
		this.start = start;
		this.utility = utility;
		this.utilities = utilities;
	}
	
	@Override
	public String toString() {
		return "[Compiled Problem \"" + name + "\": " + fluents.size() + " fluents; " + actions.size() + " actions; " + triggers.size() + " triggers; " + (1 + universe.characters.size()) + " utilities]";
	}
	
	@Override
	public CompiledFluent getFluent(Signature signature) {
		return (CompiledFluent) super.getFluent(signature);
	}
	
	@Override
	public CompiledEvent getEvent(Signature signature) {
		return (CompiledEvent) super.getEvent(signature);
	}
	
	@Override
	public CompiledAction getAction(Signature signature) {
		return (CompiledAction) super.getAction(signature);
	}
	
	@Override
	public CompiledTrigger getTrigger(Signature signature) {
		return (CompiledTrigger) super.getTrigger(signature);
	}
	
	/**
	 * Returns a compiled fluent defined in this problem based on a given
	 * {@link Fluent fluent}, which does not need to be compiled and does not
	 * need to be from this problem. The returned fluent will have the same
	 * characters and signature as the given fluent, but all of its elements
	 * will be elements defined in this problem.
	 * 
	 * @param fluent a fluent whose characters and signature match the desired
	 * fluent
	 * @return a compiled fluent defined in this problem
	 * @throws edu.uky.cs.nil.sabre.FormatException if no compiled fluent is
	 * defined in this problem
	 */
	public CompiledFluent getFluent(Fluent fluent) {
		for(CompiledFluent compiled : fluents)
			if(compiled.characters.equals(fluent.characters) && compiled.signature.equals(fluent.signature))
				return compiled;
		throw Exceptions.notDefined("Fluent", fluent.toString());
	}
}