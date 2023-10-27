package edu.uky.cs.nil.sabre.prog;

import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * The reachability heuristic is a {@link ProgressionCost cost function},
 * meant to be used as the {@link ProgressionSearch#heuristic heuristic} in a
 * {@link ProgressionSearch heuristic progression search}, which checks only the
 * {@link ProgressionNode#getRoot() roots} of a search to determine if it is
 * possible for them to achieve their goals.
 * <p>
 * This heuristic only evaluates the root node of each new branch; for every
 * other node it will return 0. When evaluating a root node, this function
 * extends a {@link edu.uky.cs.nil.sabre.hg.MaxGraph max heuristic graph} until
 * it is possible for the utility of the {@link
 * ProgressionNode#getCharacter() node's character} to be higher. If the graph
 * levels off before detecting any situation which could improve the character's
 * utility, this function returns {@link Double#POSITIVE_INFINITY positive
 * infinity}, causing the search to abandon that branch. Heuristic graphs can be
 * used to detect when certain propositions are impossible to achieve, but they
 * cannot detect all such propositions. This means that, any time this cost
 * function returns infinity, we can be sure there is no way to achieve the
 * branch's goal; however, this function will not always return infinity every
 * time it evaluates an impossible root.
 * 
 * @author Stephen G. Ware
 */
public class ReachabilityHeuristic extends GraphHeuristic.MaxGraphHeuristic {
	
	/**
	 * A {@link ProgressionCostFactory factory} for creating {@link
	 * ReachabilityHeuristic reachability heuristics}.
	 */
	public static final ProgressionCostFactory FACTORY = new ProgressionCostFactory() {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1L;

		@Override
		public String toString() {
			return "reachability";
		}
		
		@Override
		public ReachabilityHeuristic getCost(CompiledProblem problem, Status status) {
			return new ReachabilityHeuristic(problem, status);
		}
	};

	/**
	 * Constructs a new reachability heuristic.
	 * 
	 * @param problem the problem to be solved
	 * @param status a status to update while the heuristic is created
	 */
	public ReachabilityHeuristic(CompiledProblem problem, Status status) {
		super(problem, status);
	}
	
	@Override
	public String toString() {
		return FACTORY.toString();
	}
	
	@Override
	public <N> double evaluate(ProgressionNode<N> node) {
		if(node.getTemporalDepth() == 0) {
			double value = super.evaluate(node);
			if(value == Double.POSITIVE_INFINITY)
				return Double.POSITIVE_INFINITY;
		}
		return 0;
	}
}