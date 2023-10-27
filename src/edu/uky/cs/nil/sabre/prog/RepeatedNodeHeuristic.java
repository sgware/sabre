package edu.uky.cs.nil.sabre.prog;

import java.util.HashMap;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * The repeated node heuristic is a wrapper around a {@link ProgressionCost cost
 * function} that can improve the efficiency of a {@link ProgressionSearch
 * heuristic progression search} by preventing it from revisiting the same node
 * multiple times. Each time this cost function evaluates a node, it calculates
 * the sum of its {@link ProgressionNode#getTemporalOffset() temporal offset}
 * and {@link ProgressionNode#getTemporalDepth() temporal depth}. If the node
 * has never been evaluated before, or if that sum is less than the last time
 * the node was evaluated, that sum is recorded and this heuristic passes thru
 * to {@link #parent the heuristic it wraps around}. If the node has been
 * evaluated before, and if the sum is greater than or equal to the recorded
 * sum, this heuristic returns {@link Double#POSITIVE_INFINITY positive
 * infinity}. This prevents a node from being visited more than once at the
 * same temporal level. The sum of temporal offset and depth is checked because
 * a previous search of the same node might have failed due to search's
 * temporal limits, so repeating the search at a lower temporal level might
 * succeed.
 * 
 * @author Stephen G. Ware
 */
public class RepeatedNodeHeuristic implements ProgressionCost {
	
	/**
	 * A {@link ProgressionCostFactory factory} for producing {@link
	 * RepeatedNodeHeuristic repeated node heuristics}.
	 * 
	 * @author Stephen G. Ware
	 */
	public static class Factory implements ProgressionCostFactory {
		
		/** Serial version ID */
		private static final long serialVersionUID = Settings.VERSION_UID;
		
		/**
		 * The cost function factory that will be used to create the cost
		 * function to be used any time a node is not being repeated
		 */
		public final ProgressionCostFactory parent;
		
		/**
		 * Constructs a new repeated node heuristic factory.
		 * 
		 * @param parent a factory to produce the cost function that will be
		 * used any time a node is not being repeated
		 */
		public Factory(ProgressionCostFactory parent) {
			this.parent = parent;
		}
		
		@Override
		public String toString() {
			return RepeatedNodeHeuristic.toString(parent);
		}

		@Override
		public RepeatedNodeHeuristic getCost(CompiledProblem problem, Status status) {
			return new RepeatedNodeHeuristic(parent.getCost(problem, status));
		}
	}
	
	private static final String toString(Object parent) {
		return parent + " with no repeated nodes";
	}

	/** The cost function to be used any time a node is not being repeated */
	public final ProgressionCost parent;
	
	/** A hashtable to record the temporal level at which nodes were visited */
	private final HashMap<Object, Integer> nodes = new HashMap<>();
	
	/**
	 * Constructs a new repeated node heuristic.
	 * 
	 * @param parent the cost function to be used any time a node is not being
	 * repeated
	 */
	public RepeatedNodeHeuristic(ProgressionCost parent) {
		this.parent = parent;
	}
	
	@Override
	public String toString() {
		return toString(parent);
	}
	
	@Override
	public <N> void initialize(ProgressionNode<N> root) {
		nodes.clear();
	}
	
	@Override
	public <N> double evaluate(ProgressionNode<N> node) {
		int temporal = node.getTemporalOffset() + node.getTemporalDepth();
		Integer visited = nodes.get(node.getNode());
		if(visited == null || temporal < visited) {
			nodes.put(node.getNode(), temporal);
			return parent.evaluate(node);
		}
		else
			return Double.POSITIVE_INFINITY;
	}
}