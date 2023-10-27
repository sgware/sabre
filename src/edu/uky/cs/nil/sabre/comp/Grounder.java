package edu.uky.cs.nil.sabre.comp;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Event;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.InitialState;
import edu.uky.cs.nil.sabre.Problem;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Disjunction;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.logic.Quantified;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.logic.Variable;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * Replaces all templates in a {@link Problem problem}, such as {@link Fluent
 * fluents} and {@link Event events}, with {@link Expression#isGround()}
 * instances, such as {@link CompiledFluent ground fluents} and {@link
 * CompiledEvent ground events}. {@link Quantified Quantified expressions} are
 * also {@link Quantified#expand() expanded} and replaced with equivalent
 * ground expressions.
 * <p>
 * For simplicity, elements of a {@link Problem problem} can be defined with
 * {@link Variable variables} to indicate that they are defined for every
 * possible {@link Value value} that the variable can stand for. For example,
 * consider this problem:
 * <pre>
 * entity Alice : character;
 * entity Bob : character;
 * property at(person : character) : location;
 * </pre>
 * It defines a {@link Fluent fluent} whose {@link
 * edu.uky.cs.nil.sabre.Signature#name name} is <code>"at"</code> and with one
 * parameter, a variable of type <code>character</code>. This concise definition
 * actually defines two ground fluents:
 * <ul>
 * <li><code>at(Alice)</code></li>
 * <li><code>at(Bob)</code></li>
 * </ul>
 * A grounder is a compiler that replaces each of these template definitions
 * with the many ground things they imply.
 * 
 * @author Stephen G. Ware
 */
public class Grounder {

	/**
	 * Replaces all templates in a {@link Problem problem}, such as {@link
	 * Fluent fluents} and {@link Event events}, with {@link
	 * Expression#isGround() ground} instances of those elements, such as
	 * {@link CompiledFluent compiled fluents} and {@link CompiledEvent
	 * compiled events}.
	 * 
	 * @param problem the original problem
	 * @param status a status to update while the compiler runs
	 * @return a compiled problem which contains only ground elements
	 */
	public static CompiledProblem compile(Problem problem, Status status) {
		ProblemCompiler compiler = new ProblemCompiler(problem.universe);
		status.setMessage("Grounding problem \"" + problem.name + "\": %d fluents; %d actions; %d triggers", 0, 0, 0);
		for(Fluent fluent : problem.fluents)
			ground(compiler, fluent, 0, status);
		for(Event event : problem.events)
			ground(compiler, event, 0, isSpecialAuthorAction(event), status);
		Clause<Effect> initial = compiler.compile(new InitialState(problem.initial).clause);
		return new CompiledProblem(
			problem.name,
			problem.universe,
			compiler.getFluents(),
			compiler.getActions(),
			compiler.getTriggers(),
			initial,
			new InitialState(initial),
			compiler.compile(problem.utility.toValued()),
			compiler.compile(problem.utilities, u -> u.toValued()),
			problem.comment
		);
	}
	
	private static final void ground(ProblemCompiler compiler, Fluent fluent, int index, Status status) {
		if(index < fluent.signature.arguments.size()) {
			Parameter parameter = fluent.signature.arguments.get(index);
			if(parameter instanceof Value)
				ground(compiler, fluent, index + 1, status);
			else
				for(Value value : ((Variable) parameter).type.getValues())
					ground(compiler, (Fluent) fluent.substitute(parameter, value), index + 1, status);
		}
		else {
			compiler.compile(fluent);
			status.update(0, compiler.fluents.size());
		}
	}
	
	private static final boolean isSpecialAuthorAction(Event event) {
		return event instanceof Action && ((Action) event).consenting.size() == 0 && event.getPrecondition().equals(False.FALSE);
	}
	
	private static final void ground(ProblemCompiler compiler, Event event, int index, boolean specialAuthorAction, Status status) {
		if(index < event.getSignature().arguments.size()) {
			Parameter parameter = event.getSignature().arguments.get(index);
			if(parameter instanceof Value)
				ground(compiler, event, index + 1, specialAuthorAction, status);
			else {
				for(Value value : ((Variable) parameter).type.getValues()) {
					Expression precondition = ((Expression) event.getPrecondition().substitute(parameter, value)).simplify();
					if(!precondition.equals(False.FALSE) || specialAuthorAction)
						ground(compiler, ((Event) event.substitute(parameter, value)).simplify(), index + 1, specialAuthorAction, status);
				}
			}
		}
		else {
			Disjunction<Clause<Precondition>> precondition = event.getPrecondition().toPrecondition();
			Clause<Effect> effect = event.getEffect().toEffect();
			if((!precondition.equals(False.FALSE) || specialAuthorAction) && !effect.equals(Clause.NULL)) {
				compiler.compile(event);
				status.update(1, compiler.actions.size());
				status.update(2, compiler.triggers.size());
			}
		}
	}
}