package edu.uky.cs.nil.sabre.comp;

import java.util.ArrayList;
import java.util.HashMap;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.FiniteState;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Trigger;
import edu.uky.cs.nil.sabre.Universe;
import edu.uky.cs.nil.sabre.etree.EventSet;
import edu.uky.cs.nil.sabre.graph.StateGraph;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Conditional;
import edu.uky.cs.nil.sabre.logic.Conjunction;
import edu.uky.cs.nil.sabre.logic.Disjunction;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ImmutableSet;
import edu.uky.cs.nil.sabre.util.Worker;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * First applies any {@link Trigger triggers} that are satisfied in a problem's
 * {@link CompiledProblem#start initial state} and then removes {@link
 * CompiledEvent compiled events} which can never occur and propositions which
 * are always true or always false from a {@link CompiledProblem compiled
 * problem}, possibly making it smaller and simpler.
 * <p>
 * After applying any triggers to the problem's initial state, this compiler
 * creates a {@link edu.uky.cs.nil.sabre.hg.HeuristicGraph heuristic graph}
 * initialized on the updated {@link CompiledProblem#start initial state} and
 * then extends it until it has leveled off. Heuristic graphs provide a
 * necessary but not sufficient way to detect which propositions must always
 * be true or always false (i.e. when they detect such a proposition they are
 * always correct, but they may not detect every such proposition).
 * A proposition which must always be true is replaced with the constant {@link
 * edu.uky.cs.nil.sabre.logic.True#TRUE true}; a proposition that can never be
 * true is replaced with the constant {@link False#FALSE false}.
 * <p>
 * Suppose <code>f</code> is a fluent of type <code>entity</code>, and after
 * extending the heuristic graph it is discovered that <code>f</code> can have
 * the values <code>a</code> and <code>b</code>, but can never have the value
 * <code>c</code>. This means that any time a proposition such as
 * <code>f=c</code> appears in a logical expression, it can be replaced with
 * {@link False#FALSE false}.
 * <p>
 * Similarly, suppose <code>f</code> has the value <code>a</code> and can never
 * have and other values. This means that any time a proposition such as
 * <code>f=a</code> appears in a logical expression, it can be replaced with
 * {@link edu.uky.cs.nil.sabre.logic.True#TRUE true}.
 * <p>
 * Similarly, if there are any events in the problem whose preconditions can
 * never be true, they are removed from the problem.
 * <p>
 * Besides possibly making the problem smaller and simpler, this compiler also
 * enables a design pattern where some fluents and triggers are defined to
 * express the initial state of a problem in a convenient way with the
 * knowledge that they will get compiled out later.
 * <p>
 * For example, consider a problem where characters can walk between locations.
 * They can only walk between certain pairs of locations that are joined by
 * roads, but roads always go in both directions. One could simply list each
 * road twice, like this:
 * <pre>
 * entity A : character;
 * entity B : location;
 * entity C : location;
 * property road(from : location, to : location) : boolean;
 * property at(character : character) : location;
 * road(B, C);
 * road(C, B);
 * action walk(character : character, from : location, to : location) {
 *     precondition: at(character) == from &amp; road(from, to);
 *     effect: at(character) = to;
 *     consenting: character;
 * };
 * </pre>
 * However this can be tedious when there are many roads. An alternative is to
 * use a trigger to ensure that all roads go both ways:
 * <pre>
 * trigger road_both_ways(from : location, to : location) {
 *     precondition: road(from, to) &amp; !road(to, from);
 *     effect: road(to, from);
 * };
 * </pre>
 * This trigger will be applied in the initial state to make roads go both
 * ways, but as long as roads never change, these triggers will never be used
 * again. This compiler will detect that case. It will apply the triggers once
 * in the initial state, remove them, and then replace all atoms referring to a
 * <code>road</code> fluent with either true or false. For example, this ground
 * action:
 * <pre>
 * action walk(A, B, C) {
 *     precondition: at(A) == B &amp; road(B, C);
 *     effect: at(A) = C;
 * };
 * </pre>
 * would be simplified to:
 * <pre>
 * action walk(A, B, C) {
 *     precondition: at(A) == B;
 *     effect: at(A) = C;
 * };
 * </pre>
 * because <code>road(B, C)</code> is always true. Similarly, consider this
 * ground action where the character would walk to a third location
 * <code>D</code> that has no road to <code>B</code>:
 * <pre>
 * action walk(A, B, D) {
 *     precondition: at(A) == B &amp; road(B, D);
 *     effect: at(A) = D;
 * };
 * </pre>
 * Because <code>road(B, D)</code> is always false, this action would get
 * simplified to:
 * <pre>
 * action walk(A, B, C) {
 *     precondition: false;
 *     effect: at(A) = C;
 * };
 * </pre>
 * and removed from the problem alltogether.
 * <p>
 * There is one important exception to the mechanism that removes impossible
 * actions. Any action which has no {@link Action#consenting consenting
 * characters} and whose precondition is originally {@link False#FALSE false}
 * (not simplified down to false) will not be removed. Even though a planner
 * could never use an action whose precondition is false when solving a problem,
 * these special actions are preserved as a way for users to impose unexpected
 * changes on a state independently from the planner.
 * 
 * @author Stephen G. Ware
 */
public class Simplifier {

	/**
	 * Applies any {@link CompiledTrigger triggers} that are satisfied in the
	 * {@link CompiledProblem#start problem's initial state}, replaces any
	 * propositions which are always true with {@link
	 * edu.uky.cs.nil.sabre.logic.True#TRUE true} and any which are always
	 * false with {@link False#FALSE false}, and then removes any {@link
	 * CompiledEvent events} which can never occur, creating a new, smaller,
	 * simpler problem.
	 * 
	 * @param problem the original compiled problem
	 * @param status a status to update while the compiler runs
	 * @return a new compiled problem that may have fewer fluent, fewer events,
	 * and simpler logical expressions
	 */
	public static CompiledProblem compile(CompiledProblem problem, Status status) {
		Simplifier simplifier = new Simplifier(problem, status);
		return new CompiledProblem(
			problem.name,
			problem.universe,
			simplifier.fluents,
			simplifier.actions,
			simplifier.triggers,
			simplifier.initial,
			simplifier.start,
			simplifier.utility,
			simplifier.utilities,
			problem.comment
		);
	}
	
	private final Universe universe;
	private final Reachable reachable;
	private final ProblemCompiler compiler;
	private final ImmutableSet<CompiledFluent> fluents;
	private final EventSet<CompiledAction> actions;
	private final EventSet<CompiledTrigger> triggers;
	private final Clause<Effect> initial;
	private final FiniteState start;
	private final Conditional<Disjunction<Clause<Precondition>>> utility;
	private final CompiledMapping<Conditional<Disjunction<Clause<Precondition>>>> utilities;
	
	@SuppressWarnings("unchecked")
	private Simplifier(CompiledProblem problem, Status status) {
		this.universe = problem.universe;
		status.setMessage("Simplifying problem \"" + problem.name + "\": Calculating possible values for fluents...");
		this.reachable = new Reachable(problem, new Worker.Status());
		status.setMessage("Simplifying problem \"" + problem.name + "\": %d/" + problem.fluents.size() + " fluents; %d/" + problem.events.size() + " events; %d/" + (1 + problem.universe.characters.size()) + " utilities", 0, 0, 0);
		ArrayList<Fluent> ordered = new ArrayList<>();
		for(int i=0; i<problem.fluents.size(); i++) {
			CompiledFluent fluent = problem.fluents.get(i);
			if(reachable.apply(fluent) instanceof Fluent)
				ordered.add(fluent);
			status.update(0, i + 1);
		}
		this.compiler = new ProblemCompiler(problem.universe, ordered);
		this.fluents = compiler.getFluents();
		for(int i=0; i<problem.events.size(); i++) {
			simplify(problem.events.get(i));
			status.update(1, i + 1);
		}
		this.actions = compiler.getActions();
		this.triggers = compiler.getTriggers();
		this.initial = simplify(problem.initial);
		FiniteState start = new StateGraph(problem.fluents, problem.universe.characters, problem.triggers, problem.start).root.afterTriggers();
		HashMap<Fluent, Fluent> mapping = new HashMap<>();
		for(Fluent fluent : problem.fluents)
			if(reachable.apply(fluent) instanceof Fluent)
				mapping.put(compiler.compile(fluent), fluent);
		this.start = new StateGraph(fluents, problem.universe.characters, triggers, new MappedState(start, f -> mapping.get(f))).root;
		this.utility = simplify(problem.utility);
		status.update(2, 1);
		Conditional<Disjunction<Clause<Precondition>>>[] utilities = new Conditional[problem.universe.characters.size()];
		for(Character character : problem.universe.characters) {
			utilities[character.id] = simplify(problem.utilities.get(character));
			status.update(2, 2 + character.id);
		}
		this.utilities = new CompiledMapping<>(utilities);
	}
	
	private CompiledEvent simplify(CompiledEvent event) {
		if(event instanceof CompiledAction)
			return simplify((CompiledAction) event);
		else
			return simplify((CompiledTrigger) event);
	}
	
	@SuppressWarnings("unchecked")
	private CompiledAction simplify(CompiledAction action) {
		boolean specialAuthorAction = action.consenting.size() == 0 && action.getPrecondition().equals(False.FALSE);
		Disjunction<Clause<Precondition>> precondition = simplify(action.precondition);
		if(precondition.equals(False.FALSE) && !specialAuthorAction)
			return null;
		Clause<Precondition> common = common(precondition);
		Clause<Effect> effect = remove(common, simplify(action.effect));
		if(effect.size() == 0)
			return null;
		Disjunction<Clause<Precondition>>[] observing = new Disjunction[universe.characters.size()];
		for(Character character : universe.characters)
			observing[character.id] = remove(common, simplify(action.observing.get(character)));
		return (CompiledAction) compiler.compile(new Action(
			action.signature,
			precondition,
			effect,
			action.consenting.cast(Parameter.class),
			new CompiledMapping<>(observing),
			action.comment
		));
	}
	
	@SuppressWarnings("unchecked")
	private static final Clause<Precondition> common(Disjunction<Clause<Precondition>> dnf) {
		Clause<Precondition> common = (Clause<Precondition>) (Clause<?>) Clause.EMPTY;
		if(dnf.size() > 0) {
			for(Precondition precondition : dnf.get(0)) {
				boolean all = true;
				for(Clause<Precondition> clause : dnf)
					if(!clause.contains(precondition))
						all = false;
				if(all)
					common = common.add(precondition);
			}
		}
		return common;
	}
	
	@SuppressWarnings("unchecked")
	private static final Disjunction<Clause<Precondition>> remove(Clause<Precondition> common, Disjunction<Clause<Precondition>> precondition) {
		Clause<Precondition>[] clauses = new Clause[precondition.size()];
		for(int i=0; i<clauses.length; i++)
			clauses[i] = precondition.get(i).remove(common);
		return new Disjunction<>(clauses).toPrecondition();
	}
	
	private static final Clause<Effect> remove(Clause<Precondition> common, Clause<Effect> clause) {
		ArrayList<Effect> effects = new ArrayList<>();
		for(Effect effect : clause) {
			Disjunction<Clause<Precondition>> condition = remove(common, effect.condition).toPrecondition();
			if(!condition.equals(False.FALSE))
				effects.add(new Effect(condition, effect.fluent, effect.value));
		}
		return new Conjunction<>(effects).toEffect();
	}
	
	private CompiledTrigger simplify(CompiledTrigger trigger) {
		Disjunction<Clause<Precondition>> precondition = simplify(trigger.precondition);
		if(precondition.equals(False.FALSE))
			return null;
		Clause<Effect> effect = simplify(trigger.effect);
		if(effect.size() == 0)
			return null;
		return (CompiledTrigger) compiler.compile(new Trigger(
			trigger.signature,
			precondition,
			effect,
			trigger.comment
		));
	}
	
	private Conditional<Disjunction<Clause<Precondition>>> simplify(Conditional<Disjunction<Clause<Precondition>>> conditional) {
		return compiler.compile(((Expression) conditional.substitute(reachable)).toValued());
	}
	
	private Disjunction<Clause<Precondition>> simplify(Disjunction<Clause<Precondition>> condition) {
		return compiler.compile(((Expression) condition.substitute(reachable)).toPrecondition());
	}
	
	private Clause<Effect> simplify(Clause<Effect> effect) {
		Clause<Effect> result = Clause.EMPTY.toEffect();
		for(Effect atom : effect)
			if(!(reachable.apply(atom.fluent) instanceof Value))
				result = result.add(atom);
		return compiler.compile(((Expression) result.substitute(reachable)).toEffect());
	}
}