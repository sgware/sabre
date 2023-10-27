package edu.uky.cs.nil.sabre.prog;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * The repeated root heuristic is similar to the {@link RepeatedNodeHeuristic
 * repeated node heuristic}, except that it only prevents a {@link
 * ProgressionSearch heuristic progression search} from reusing the same node
 * as a root in the search. Like the repeated root heuristic, this cost function
 * is a wrapper around some other cost function. When a root node (that is, a
 * node whose {@link ProgressionNode#getTemporalDepth() temporal depth} is 0)
 * is evaluated, its cost will be {@link Double#POSITIVE_INFINITY infinity} if
 * that node has previously been used as a root at the same temporal level (see
 * {@link RepeatedNodeHeuristic the description of the repeated node
 * heuristic).} In all other cases, this heursitic passes thru to {@link
 * RepeatedRootHeuristic#parent the cost function it wraps around}.
 * 
 * @author Stephen G. Ware
 */
public class RepeatedRootHeuristic extends RepeatedNodeHeuristic {

	/**
	 * A {@link ProgressionCostFactory factory} for producing {@link
	 * RepeatedRootHeuristic repeated root heuristics}.
	 * 
	 * @author Stephen G. Ware
	 */
	public static class Factory implements ProgressionCostFactory {
		
		/** Serial version ID */
		private static final long serialVersionUID = Settings.VERSION_UID;
		
		/**
		 * The cost function factory that will be used to create the cost
		 * function to be used any time a root is not being repeated
		 */
		public final ProgressionCostFactory parent;
		
		/**
		 * Constructs a new repeated root heuristic factory.
		 * 
		 * @param parent a factory to produce the cost function that will be
		 * used any time a root is not being repeated
		 */
		public Factory(ProgressionCostFactory parent) {
			this.parent = parent;
		}
		
		@Override
		public String toString() {
			return RepeatedRootHeuristic.toString(parent);
		}
		
		@Override
		public RepeatedRootHeuristic getCost(CompiledProblem problem, Status status) {
			return new RepeatedRootHeuristic(parent.getCost(problem, status));
		}
	}
	
	private static final String toString(Object parent) {
		return parent + " with no repeated roots";
	}

	/**
	 * Constructs a new repeated root heuristic.
	 * 
	 * @param parent the cost function to be used any time a root is not being
	 * repeated
	 */
	public RepeatedRootHeuristic(ProgressionCost parent) {
		super(parent);
	}
	
	@Override
	public String toString() {
		return toString(parent);
	}
	
	@Override
	public <N> double evaluate(ProgressionNode<N> node) {
		if(node.getTemporalDepth() == 0)
			return super.evaluate(node);
		else
			return parent.evaluate(node);
	}
}