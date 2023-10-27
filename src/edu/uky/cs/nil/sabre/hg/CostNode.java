package edu.uky.cs.nil.sabre.hg;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Logical;

/**
 * A {@link Node heuristic graph node} that represents a Boolean proposition
 * and the {@link #getCost() cost} of that proposition being {@link
 * edu.uky.cs.nil.sabre.logic.True#TRUE true}. A cost node can be thought of as
 * a {@link CostSet cost set} whose only possible members are {@link
 * edu.uky.cs.nil.sabre.logic.True#TRUE true} and {@link
 * edu.uky.cs.nil.sabre.logic.False#FALSE false}. The {@link #getCost()} method
 * returns the cost of the set containing true. In its default state, a cost
 * node's cost is {@link Double#POSITIVE_INFINITY positive infinity}.
 * 
 * @author Stephen G. Ware
 */
public abstract class CostNode extends Node implements Comparable<CostNode> {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;

	/** The cost of the proposition represented by this node being true */
	private double cost = Double.POSITIVE_INFINITY;
	
	/**
	 * Constructs a new cost node that belongs to a given graph and represents
	 * a given Boolean logical formula.
	 * 
	 * @param graph the graph this node belongs to
	 * @param label the Boolean logical formula this node represents
	 */
	protected CostNode(HeuristicGraph graph, Logical label) {
		super(graph, label);
	}
	
	@Override
	public int compareTo(CostNode other) {
		double comparison = getCost() - other.getCost();
		if(comparison == 0)
			return 0;
		else if(comparison < 0)
			return -1;
		else
			return 1;
	}

	/**
	 * Returns the cost of this node's logical formula being {@link
	 * edu.uky.cs.nil.sabre.logic.True#TRUE true}; the cost is an estimate
	 * of the number of {@link edu.uky.cs.nil.sabre.Action actions} that need
	 * to be taken from the state the heuristic graph was {@link
	 * HeuristicGraph#initialize(edu.uky.cs.nil.sabre.State) initialized to}
	 * before this logical formula can be true.
	 * 
	 * @return an estimate of the number of actions that would need to be
	 * taken before the logical formula this node represents can be true
	 */
	public double getCost() {
		return cost;
	}
	
	/**
	 * Sets the cost of this node's logical formula being true. The node's cost
	 * will only be updated if the given cost is less than the current cost.
	 * 
	 * @param cost the new cost for this node's logical expression to be true
	 * @return true if the cost was updated, or false if this node's state did
	 * not change
	 */
	protected boolean setCost(double cost) {
		if(cost < this.cost) {
			markForReset();
			this.cost = cost;
			return true;
		}
		else
			return false;
	}
	
	@Override
	protected void reset() {
		super.reset();
		cost = Double.POSITIVE_INFINITY;
	}
}