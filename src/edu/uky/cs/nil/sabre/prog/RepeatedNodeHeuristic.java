package edu.uky.cs.nil.sabre.prog;

import java.util.HashMap;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * The repeated node heuristic is a wrapper around a {@link ProgressionCost cost
 * function} that can improve the efficiency of a {@link ProgressionSearch
 * heuristic progression search} by preventing it from revisiting the same node
 * multiple times. If a node has never been evaluated before, or if its {@link
 * ProgressionNode#getExplanationDepth() explanation depth} is less than the
 * last time the node was evaluated, the explanation depth is recorded and this
 * heuristic passes thru to {@link #parent the heuristic it wraps around}. If
 * the node has been evaluated before, and if the explanation depth is greater
 * than or equal to the recorded depth, this heuristic returns {@link
 * Double#POSITIVE_INFINITY positive infinity}. This prevents a node from being
 * visited more than once at the same explanation depth. The explanation depth
 * is checked because a previous search of the same node might have failed due
 * to the search's temporal limits, so repeating the search at a lower temporal
 * level might succeed where it has previously failed.
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
	
	/**
	 * A hashtable to record the explanation depth at which nodes were visited
	 */
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
		int depth = node.getExplanationDepth();
		Integer visited = nodes.get(node.getNode());
		if(visited == null || depth < visited) {
			nodes.put(node.getNode(), depth);
			return parent.evaluate(node);
		}
		else
			return Double.POSITIVE_INFINITY;
	}
}