package edu.uky.cs.nil.sabre.prog;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * A weighted cost is a wrapper around a {@link ProgressionCost cost function}
 * that multiplies a cost by a fixed {@link #weight constant}.
 * 
 * @author Stephen G. Ware
 */
public class WeightedCost implements ProgressionCost {
	
	/**
	 * A {@link ProgressionCostFactory factory} for creating {@link WeightedCost
	 * weighted cost functions}.
	 * 
	 * @author Stephen G. Ware
	 */
	public static class Factory implements ProgressionCostFactory {
		
		/** Serial version ID */
		private static final long serialVersionUID = Settings.VERSION_UID;
		
		/**
		 * The factory for the original cost function that will be multiplied by
		 * the weight constant
		 */
		public final ProgressionCostFactory cost;
		
		/** The constant to multiply the cost by */
		public final double weight;
		
		/**
		 * Constructs a new weighted cost factory.
		 * 
		 * @param cost the factory that produces the original cost function
		 * that will be weighted
		 * @param weight the weight value by which the original cost function
		 * will be multiplied
		 */
		public Factory(ProgressionCostFactory cost, double weight) {
			this.cost = cost;
			this.weight = weight;
		}
		
		@Override
		public String toString() {
			return WeightedCost.toString(cost, weight);
		}

		@Override
		public WeightedCost getCost(CompiledProblem problem, Status status) {
			return new WeightedCost(cost.getCost(problem, status), weight);
		}
	}
	
	private static final String toString(Object cost, double weight) {
		return cost + " * " + weight;
	}

	/**
	 * The original cost function whose value will be multiplied by the weight
	 * constant
	 */
	public final ProgressionCost cost;
	
	/** The constant to multiply the cost by */
	public final double weight;
	
	/**
	 * Constructs a new weighted cost function.
	 * 
	 * @param cost the original cost function that will be weighted
	 * @param weight the weight value by which the original cost function will
	 * be multiplied
	 */
	public WeightedCost(ProgressionCost cost, double weight) {
		this.cost = cost;
		this.weight = weight;
	}
	
	@Override
	public String toString() {
		return toString(cost, weight);
	}
	
	@Override
	public <N> double evaluate(ProgressionNode<N> node) {
		return weight * cost.evaluate(node);
	}
}