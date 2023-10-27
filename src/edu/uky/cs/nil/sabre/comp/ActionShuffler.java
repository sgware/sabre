package edu.uky.cs.nil.sabre.comp;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Random;

import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.etree.EventSet;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * Creates a copy of a {@link CompiledProblem compiled problem} where the
 * {@link CompiledProblem#actions actions} have been randomly shuffled to be in
 * a different order.
 * <p>
 * The order of actions in a problem can affect the efficiency of a {@link
 * edu.uky.cs.nil.sabre.search.Search search}, so it may be helpful to shuffle
 * the order of actions to control for this phenomena.
 * 
 * @author Stephen G. Ware
 */
public class ActionShuffler {

	/**
	 * Randomly shuffles the order of actions in a {@link CompiledProblem
	 * compiled problem}.
	 * 
	 * @param problem the original compiled problem
	 * @param random a source of randomness used during the shuffling
	 * @param status a status to update while the compiler runs
	 * @return a new compiled problem identical to the original but with its
	 * actions in a random order
	 */
	public static CompiledProblem compile(CompiledProblem problem, Random random, Status status) {
		status.setMessage("Shuffling actions for \"" + problem.name + "\"...");
		ArrayList<CompiledAction> list = new ArrayList<>();
		for(CompiledAction action : problem.actions)
			list.add(action);
		Collections.shuffle(list, random);
		CompiledAction[] actions = Utilities.toArray(list, CompiledAction.class);
		for(int id=0; id<actions.length; id++) {
			actions[id] = new CompiledAction(
				id,
				actions[id].signature,
				actions[id].precondition,
				actions[id].effect,
				actions[id].consenting,
				actions[id].observing,
				actions[id].comment
			);
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