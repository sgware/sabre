package edu.uky.cs.nil.sabre.ptree;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.HeadPlan;
import edu.uky.cs.nil.sabre.Plan;
import edu.uky.cs.nil.sabre.Solution;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.comp.CompiledAction;
import edu.uky.cs.nil.sabre.comp.CompiledEvent;
import edu.uky.cs.nil.sabre.comp.CompiledFluent;
import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.prog.ProgressionSpace;
import edu.uky.cs.nil.sabre.util.ArrayIterable;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * A {@link ProgressionSpace progression state space} modeled as a tree, meaning
 * that the model makes no attempt to recognize when a duplicate state has been
 * generated. Not detecting duplicate states may improve the speed of a search
 * by saving the overhead requires to check for duplicates, but it may also
 * reduce the speed of a search when many duplicate states are created and must
 * then also be searched.
 * <p>
 * This state space uses the efficient {@link ProgressionTree progression tree}
 * data structure, which represents each node as a unique Java {@code long}.
 * 
 * @author Stephen G. Ware
 */
public class ProgressionTreeSpace implements ProgressionSpace<Long> {
	
	/** The tree structure that represents all states in the space */
	private final ProgressionTree tree;
	
	/**
	 * Constructs a new tree-based progression state space.
	 * 
	 * @param tree the tree of states to use
	 */
	public ProgressionTreeSpace(ProgressionTree tree) {
		this.tree = tree;
	}
	
	/**
	 * Constructs a new tree-based progression state space and the underlying
	 * tree it will use.
	 * 
	 * @param problem the problem whose states will be modeled
	 * @param status a status to update while the tree is built
	 */
	public ProgressionTreeSpace(CompiledProblem problem, Status status) {
		this(new ProgressionTree(problem, problem.triggers.buildTree(status)));
	}

	@Override
	public long size() {
		return tree.size();
	}

	@Override
	public Long initialize(State state) {
		return tree.initialize(state);
	}

	@Override
	public Solution<CompiledAction> getNextSolution() {
		long solution = tree.getNextSolution();
		if(solution == -1)
			return null;
		else
			return tree.getSolution(0, solution);
	}

	@Override
	public double compare(Long node1, Long node2) {
		return node1 - node2;
	}

	@Override
	public boolean isExplained(Long node) {
		return tree.isExplained(node);
	}

	@Override
	public boolean isExplained(Long node, Character character) {
		return tree.isExplained(node, character);
	}
	
	@Override
	public CompiledAction getAction(Long node) {
		return tree.getAction(node);
	}

	@Override
	public Value getValue(Long node, CompiledFluent fluent) {
		return tree.getValue(node, fluent);
	}
	
	@Override
	public Iterable<Long> getParents(Long node) {
		long before = tree.getJustBeforeAction(node);
		if(before == -1 || before == node)
			return new ArrayIterable<>();
		else
			return new ArrayIterable<>(before);
	}

	@Override
	public Long getChild(Long node, CompiledAction action) {
		return tree.getAfter(node, action);
	}

	@Override
	public Long getBranch(Long node, Character character) {
		long branch = tree.getBranch(node, character);
		if(branch == -1)
			return null;
		else
			return branch;
	}
	
	/**
	 * Returns all actions that have occurred between the the last {@link
	 * edu.uky.cs.nil.sabre.graph.DummyAction dummy belief update action} (or
	 * since the initial state if no dummy actions have occurred) and the given
	 * node.
	 * 
	 * @param node the ID number of the node which represents the state at the
	 * end of the desired plan
	 * @return all actions that have occurred between the last belief update and
	 * and given node
	 */
	public Plan<CompiledAction> getPlan(long node) {
		return tree.getPlan(node);
	}
	
	/**
	 * Returns all actions that have occurred since the initial state, including
	 * any {@link edu.uky.cs.nil.sabre.graph.DummyAction dummy belief update
	 * actions}.
	 * 
	 * @param node the ID number of the node whose full action history is
	 * desired
	 * @return all actions, including belief updates, that have occurred between
	 * the initial state and the given node
	 */
	public Plan<CompiledAction> getHistory(long node) {
		HeadPlan<CompiledAction> history = HeadPlan.EMPTY.cast(CompiledAction.class);
		while(tree.getBefore(node) != node) {
			CompiledEvent event = tree.getEvent(node);
			if(event instanceof CompiledAction)
				history = history.prepend((CompiledAction) event);
			node = tree.getBefore(node);
		}
		return history;
	}
}