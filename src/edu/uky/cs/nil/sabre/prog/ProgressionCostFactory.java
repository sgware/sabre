package edu.uky.cs.nil.sabre.prog;

import java.io.Serializable;

import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * A factory object that creates {@link ProgressionCost progression cost}
 * objects. Progression cost factories allow a {@link ProgressionPlanner
 * planner} to produce different cost objects for each new problem in case the
 * cost function needs to do any pre-processing based on the problem.
 * 
 * @author Stephen G. Ware
 */
public interface ProgressionCostFactory extends Serializable {
	
	/**
	 * A progression cost factory that always returns the {@link
	 * ProgressionCost#ZERO zero cost}.
	 */
	public static final ProgressionCostFactory ZERO = new ProgressionCostFactory() {
		
		private static final long serialVersionUID = 1L;
		
		@Override
		public String toString() {
			return ProgressionCost.ZERO.toString();
		}

		@Override
		public ProgressionCost getCost(CompiledProblem problem, Status status) {
			return ProgressionCost.ZERO;
		}
	};
	
	/**
	 * A progression cost factory that always returns the {@link
	 * ProgressionCost#PLAN_SIZE plan size cost}.
	 */
	public static final ProgressionCostFactory PLAN_SIZE = new ProgressionCostFactory() {
		
		private static final long serialVersionUID = 1L;
		
		@Override
		public String toString() {
			return ProgressionCost.PLAN_SIZE.toString();
		}

		@Override
		public ProgressionCost getCost(CompiledProblem problem, Status status) {
			return ProgressionCost.PLAN_SIZE;
		}
	};
	
	/**
	 * A progression cost factory that always returns the {@link
	 * ProgressionCost#TEMPORAL temporal cost}.
	 */
	public static final ProgressionCostFactory TEMPORAL = new ProgressionCostFactory() {
		
		private static final long serialVersionUID = 1L;
		
		@Override
		public String toString() {
			return ProgressionCost.TEMPORAL.toString();
		}

		@Override
		public ProgressionCost getCost(CompiledProblem problem, Status status) {
			return ProgressionCost.TEMPORAL;
		}
	};

	/**
	 * Creates a {@link ProgressionCost progression cost} object to be used
	 * while solving the given problem.
	 * 
	 * @param problem the problem that will be solved by the search that uses
	 * the cost object made by this method
	 * @param status a status to update while the cost object is created
	 * @return a progression cost object that can be used to evaluate nodes in
	 * a search to solve the given problem
	 */
	public ProgressionCost getCost(CompiledProblem problem, Status status);
}