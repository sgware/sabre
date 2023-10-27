package edu.uky.cs.nil.sabre.comp;

import java.util.LinkedHashSet;

import edu.uky.cs.nil.sabre.etree.EventSet;
import edu.uky.cs.nil.sabre.logic.Conjunction;
import edu.uky.cs.nil.sabre.logic.Epistemic;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * Creates a copy of a {@link CompiledProblem compiled problem} where all of
 * {@link CompiledProblem#actions its actions} have preconditions that require
 * all {@link edu.uky.cs.nil.sabre.Action#consenting of the action's consenting
 * characters} to believe the action is possible.
 * <p>
 * Technically, an action can be taken any time its {@link
 * CompiledAction#precondition precondition} is satisfied, but generally it
 * will not make sense unless all the characters who have to take the action
 * believe it is possible. Suppose an action's precondition is <code>p</code>;
 * for each of the action's consenting characters <code>c</code>, this compiler
 * will add <code>believes(c, p)</code> to the action's precondition.
 * 
 * @author Stephen G. Ware
 */
public class ConsentExplicitizer {

	/**
	 * Adds preconditions to every action that require {@link
	 * edu.uky.cs.nil.sabre.Action#consenting the action's consenting
	 * characters} to believe the action is possible.
	 * 
	 * @param problem the original compiled problem
	 * @param status a status to update while the compiler runs
	 * @return a new compiled problem identical to the original but with
	 * consenting character belief preconditions added to each action
	 */
	public static final CompiledProblem compile(CompiledProblem problem, Status status) {
		status.setMessage("Adding character consent preconditions: %d/" + problem.actions.size() + " actions", 0);
		LinkedHashSet<CompiledAction> actions = new LinkedHashSet<>();
		for(int i=0; i<problem.actions.size(); i++) {
			CompiledAction action = problem.actions.get(i);
			Expression[] arguments = new Expression[action.consenting.size() + 1];
			arguments[0] = action.precondition;
			for(int j=0; j<action.consenting.size(); j++)
				arguments[j+1] = new Epistemic(action.consenting.get(j), action.precondition);
			actions.add(new CompiledAction(
				action.id,
				action.signature,
				new Conjunction<>(arguments).toPrecondition(),
				action.effect,
				action.consenting,
				action.observing,
				action.comment
			));
			status.update(0, i + 1);
		}
		return new CompiledProblem(
			problem.name,
			problem.universe,
			problem.fluents,
			new EventSet<>(actions),
			problem.triggers,
			problem.initial,
			problem.start,
			problem.utility,
			problem.utilities,
			problem.comment
		);
	}
}