package edu.uky.cs.nil.sabre.hg;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Logical;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * An {@link CostSet cost set} {@link Node node} whose {@link Node#label
 * logical formula} appears as a smaller part of some larger formula and which
 * needs to notify the node representing that larger formula when its gains new
 * values.
 * 
 * @author Stephen G. Ware
 */
public abstract class FormulaNode extends Node implements CostSet {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;

	/** Nodes that need to be notified when this node gains new values */
	protected final List<Node> formulas = new List<>();
	
	/**
	 * Constructs a new formula node that belongs to a given graph and
	 * represents a given logical formula.
	 * 
	 * @param graph the graph this node belongs to
	 * @param label the logical formula this node represents
	 */
	protected FormulaNode(HeuristicGraph graph, Logical label) {
		super(graph, label);
	}

	/**
	 * Sets the cost of this node's logical formula having the given {@link
	 * Value value}. The value's cost will only be updated if the given cost is
	 * less than the current cost of that value.
	 * <p>
	 * The {@link FormulaNode} class does not track values or costs; by default
	 * this method only {@link #notify(Node, Value, double) notifies} all nodes
	 * that represent formulas in which this node's formula appears as a
	 * smaller part and returns true. This method should be overridden by child
	 * classes to record values and their costs.
	 * 
	 * @param value a value that the formula this node represents can now have
	 * @param cost the cost of the formula this node represents having that
	 * value
	 * @return true if the cost of that value was updated, or false if this
	 * node's state did not change
	 */
	protected boolean setCost(Value value, double cost) {
		for(int i=0; i<formulas.size(); i++)
			formulas.get(i).notify(this, value, cost);
		return true;
	}
}