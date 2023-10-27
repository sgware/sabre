package edu.uky.cs.nil.sabre.hg;

import java.util.Iterator;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Arithmetic;
import edu.uky.cs.nil.sabre.logic.Comparison.Operator;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A {@link CostSet cost set} {@link FormulaNode node} representing an {@link
 * Arithmetic arithmetic expression}, which has an {@link Arithmetic.Operator
 * arithmetic operator} and two numeric expressions on the {@link #left left}
 * and {@link #right right} sides.
 * 
 * @author Stephen G. Ware
 */
public class ArithmeticNode extends FormulaNode {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The arithmetic expression this node represents */
	public final Arithmetic label;
	
	/** The node representing the left side of the arithmetic expression */
	public final FormulaNode left;
	
	/** The node representing the right side of the arithmetic expression */
	public final FormulaNode right;
	
	/**
	 * The set of possible values this expression can have and their associated
	 * costs
	 */
	protected final Range range;

	/**
	 * Constructs a new arithmetic node that belongs to a given graph and
	 * represents a given arithmetic expression.
	 * 
	 * @param graph the graph this node belongs to
	 * @param label the arithmetic expression this node represents
	 */
	protected ArithmeticNode(HeuristicGraph graph, Arithmetic label) {
		super(graph, label);
		this.label = label;
		this.left = (FormulaNode) graph.getNode(label.left);
		this.left.formulas.add(this);
		this.right = (FormulaNode) graph.getNode(label.right);
		this.right.formulas.add(this);
		this.range = graph.makeNumericRange();
	}
	
	@Override
	public final int size() {
		return range.size();
	}
	
	@Override
	public final Iterator<Entry> iterator() {
		return range.iterator();
	}
	
	@Override
	public final Value getValue(int index) {
		return range.getValue(index);
	}

	@Override
	public final double getCost(int index) {
		return range.getCost(index);
	}

	@Override
	public final double getCost(Operator operator, Value value) {
		return range.getCost(operator, value);
	}

	/**
	 * Sets the cost of this node's arithmetic expression having the given
	 * {@link Value value}. The value's cost will only be updated if the given
	 * cost is less than the current cost of that value.
	 * 
	 * @param value a value that the arithmetic expression can now have
	 * @param cost the cost of the arithmetic expression having that value
	 * @return true if the cost of that value was updated, or false if this
	 * node's state did not change
	 */
	@Override
	protected boolean setCost(Value value, double cost) {
		value = range.setCost(value, cost);
		if(value != null) {
			markForReset();
			super.setCost(value, cost);
			return true;
		}
		else
			return false;
	}
	
	@Override
	protected void notify(Node node, Value value, double cost) {
		if(node.equals(left))
			for(int i=0; i<right.size(); i++)
				setCost(label.operator.calculate(value, right.getValue(i)), graph.cost(this, value, cost, right.getValue(i), right.getCost(i)));
		else if(node.equals(right))
			for(int i=0; i<left.size(); i++)
				setCost(label.operator.calculate(left.getValue(i), value), graph.cost(this, left.getValue(i), left.getCost(i), value, cost));
	}
	
	@Override
	protected void reset() {
		super.reset();
		range.reset();
	}
}