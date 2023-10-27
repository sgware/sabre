package edu.uky.cs.nil.sabre.comp;

import java.util.LinkedHashSet;
import java.util.Set;
import java.util.TreeSet;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.InitialState;
import edu.uky.cs.nil.sabre.Signature;
import edu.uky.cs.nil.sabre.Trigger;
import edu.uky.cs.nil.sabre.Universe;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Conditional;
import edu.uky.cs.nil.sabre.logic.Disjunction;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * Adds additional {@link CompiledFluent compiled fluents} and {@link
 * CompiledTrigger compiled triggers} to a {@link CompiledProblem compiled
 * problem} to make it easier to check when trigger apply in a state.
 * <p>
 * When a {@link edu.uky.cs.nil.sabre.State state} represents an
 * infinitely-nested theory of mind, it can sometimes be hard to detect when
 * to apply triggers. Let <code>c</code> be a {@link Character character} and
 * let <code>x</code> and <code>y</code> be two Boolean propositions. Imagine
 * this trigger:
 * <pre>
 * Trigger t
 * Precondition: x &amp; !y
 * Effect: y
 * </pre>
 * Now suppose this is the state:
 * <pre>
 * x &amp; y &amp; believes(c, x) &amp; believes(c, !y)
 * </pre>
 * The trigger does not apply in this state because its precondition is not
 * satisfied; however it does apply in character <code>c</code>'s beliefs. The
 * state should immediately transition to:
 * <pre>
 * x &amp; y &amp; believes(c, x) &amp; believes(c, y)
 * </pre>
 * Detecting situations like this can be expansive, so this compiler offers one
 * possible solution by creating a new trigger like this:
 * <pre>
 * New Trigger t'
 * Precondition: believes(c, x &amp; !y)
 * Effect: believes(c, y)
 * </pre>
 * This way, instead of checking triggers in every character's beliefs (and
 * their beliefs about beliefs and so on), one set of triggers can be checked in
 * the current state only.
 * <p>
 * This compiler starts by finding all {@link CompiledFluent compiled fluents}
 * with {@link CompiledFluent#characters one or more characters}, for example,
 * <code>believes(c, f=v)</code> where <code>c</code> is some character,
 * <code>f</code> is some fluent, and <code>v</code> is some value. Then, for
 * any trigger that has an effect with <code>f</code> as {@link
 * edu.uky.cs.nil.sabre.logic.Assignment#fluent its fluent}, a new trigger is
 * added. If <code>p</code> is the original trigger's precondition and
 * <code>e</code> is the original trigger's effect, the new trigger's
 * precondition is <code>believes(c, p)</code> and the new trigger's effect is
 * <code>believes(c, e)</code>.
 * <p>
 * Note that creating these new triggers may create new fluents, and these new
 * fluents may require new triggers, and so on. This compiler handles those
 * situations, except where they would create an infinite number of triggers
 * and fluents, in which case this compiler will crash. The most common
 * situation that causes this crash is when a trigger makes a belief true. For
 * example, consider this trigger:
 * <pre>
 * Trigger t
 * Precondition: believes(a, x) &amp; !x
 * Effect: x
 * </pre>
 * This compiler would create this new trigger:
 * <pre>
 * Trigger t'
 * Precondition: believes(a, believes(a, x)) &amp; believes(a, !x)
 * Effect: believes(a, x)
 * </pre>
 * and then it will create this new trigger:
 * <pre>
 * Trigger t''
 * Precondition: believes(a, believes(a, believes(a, x))) &amp; believes(a, believes(a, !x))
 * Effect: believes(a, believes(a, x))
 * </pre>
 * and so on, forever, until a crash. This compiled should not be used when a
 * {@link edu.uky.cs.nil.sabre.Problem problem} contains a situation like this.
 * 
 * @author Stephen G. Ware
 */
public class FluentExplicitizer {
	
	/**
	 * Adds new {@link CompiledTrigger compiled triggers} (and possibly new
	 * {@link CompiledFluent compiled fluents}) to a {@link CompiledProblem
	 * compiled problem} to make it easier to detect when triggers should
	 * apply in a {@link edu.uky.cs.nil.sabre.State state} with beliefs.
	 * 
	 * @param problem the original compiled problem
	 * @param status a status to update while the compiler runs
	 * @return a new compiled problem identical to the original but possibly
	 * with additional triggers and fluents
	 */
	public static final CompiledProblem compile(CompiledProblem problem, Status status) {
		int iteration = 1;
		status.setMessage("Making effects explicit and adding new triggers (iteration %d): %d effects; %d triggers", iteration, 0, 0);
		CompiledProblem compiled = addEffectsAndTriggers(problem, status);
		while(problem.fluents.size() != compiled.fluents.size()) {
			problem = compiled;
			status.update(0, ++iteration);
			compiled = addEffectsAndTriggers(problem, status);
		}
		return compiled;
	}
	
	private static final CompiledProblem addEffectsAndTriggers(CompiledProblem problem, Status status) {
		Set<Fluent> sorted = new TreeSet<>();
		for(Fluent fluent : problem.fluents)
			sorted.add(new Fluent(fluent.characters, fluent.signature, fluent.type, fluent.comment));
		ProblemCompiler compiler = new ProblemCompiler(problem.universe, sorted);
		Set<Fluent> preconditionFluents = getPreconditionFluents(problem);
		int newEffects = 0;
		for(CompiledAction action : problem.actions) {
			CompiledAction compiled = (CompiledAction) compiler.compile(addEffects(action, preconditionFluents));
			newEffects += compiled.getEffect().size() - action.getEffect().size();
			status.update(1, newEffects);
		}
		for(CompiledTrigger trigger : problem.triggers) {
			addTriggers(compiler, trigger, preconditionFluents);
			status.update(2, Math.max(0, compiler.triggers.size() - problem.triggers.size()));
		}
		InitialState start = new InitialState(compiler.compile(problem.initial));
		return new CompiledProblem(
			problem.name,
			problem.universe,
			compiler.getFluents(),
			compiler.getActions(),
			compiler.getTriggers(),
			start.clause,
			start,
			compiler.compile(problem.utility),
			compiler.compile(problem.utilities, e -> e.toValued()),
			problem.comment
		);
	}
	
	private static final Set<Fluent> getPreconditionFluents(CompiledProblem problem) {
		LinkedHashSet<Fluent> fluents = new LinkedHashSet<>();
		for(CompiledEvent event : problem.events)
			collectPreconditionFluents(problem.universe, event, fluents);
		collectPreconditionFluents(problem.utility, fluents);
		for(Character character : problem.universe.characters)
			collectPreconditionFluents(problem.utilities.get(character), fluents);
		return fluents;
	}
	
	private static final void collectPreconditionFluents(Universe universe, CompiledEvent event, Set<Fluent> fluents) {
		collectPreconditionFluents(event.getPrecondition(), fluents);
		for(Effect effect : event.getEffect())
			collectPreconditionFluents(effect.condition, fluents);
		if(event instanceof Action)
			for(Character character : universe.characters)
				collectPreconditionFluents(((CompiledAction) event).observing.get(character), fluents);
	}
	
	private static final void collectPreconditionFluents(Conditional<Disjunction<Clause<Precondition>>> utility, Set<Fluent> fluents) {
		for(Disjunction<Clause<Precondition>> condition : utility.conditions)
			collectPreconditionFluents(condition, fluents);
	}
	
	private static final void collectPreconditionFluents(Disjunction<Clause<Precondition>> precondition, Set<Fluent> fluents) {
		fluents.addAll(precondition.collect(Fluent.class));
	}
	
	private static final Action addEffects(CompiledAction action, Set<Fluent> fluents) {
		Clause<Effect> effect = action.getEffect();
		for(Fluent fluent : fluents)
			effect = effect.add(action.getEffect(fluent));
		return new Action(action.signature, action.precondition, effect, action.consenting.cast(Parameter.class), action.observing.cast(Expression.class), action.comment);
	}
	
	private static final void addTriggers(ProblemCompiler compiler, CompiledTrigger trigger, Set<Fluent> fluents) {
		compiler.compile(trigger);
		for(Fluent fluent : fluents)
			addTriggers(compiler, trigger, fluent);
	}
	
	private static final void addTriggers(ProblemCompiler compiler, CompiledTrigger trigger, Fluent fluent) {
		for(Effect effect : trigger.getEffect()) {
			if(contains(fluent, effect.fluent)) {
				Trigger compiled = trigger;
				for(int i=fluent.characters.size()-effect.fluent.characters.size()-1; i>=0; i--)
					compiled = prepend((Character) fluent.characters.get(i), compiled);
				compiler.compile(compiled);
			}
		}
	}
	
	private static final boolean contains(Fluent larger, Fluent smaller) {
		if(larger.characters.size() > smaller.characters.size()) {
			while(larger.characters.size() > smaller.characters.size())
				larger = larger.removeFirstCharacter();
			return larger.equals(smaller);
		}
		return false;
	}
	
	private static final Trigger prepend(Character character, Trigger trigger) {
		Signature signature = new Signature(character.name + "_" + trigger.signature.name, trigger.signature.arguments);
		return new Trigger(signature, trigger.precondition.prepend(character).toPrecondition(), trigger.effect.prepend(character).toEffect(), trigger.comment);
	}
}