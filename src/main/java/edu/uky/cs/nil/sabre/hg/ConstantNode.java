package edu.uky.cs.nil.sabre.hg;

import java.util.Iterator;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Comparison.Operator;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A {@link CostSet cost set} {@link FormulaNode node} representing a single
 * {@link Value value}, where that value always costs 0 and all other values
 * always cost {@link Double#POSITIVE_INFINITY positive infinity}.
 * 
 * @author Stephen G. Ware
 */
public class ConstantNode extends FormulaNode {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;

	/** The value this node represents */
	public final Value label;
	
	/** The set of values this node can have and their associated costs */
	private final Range range;
	
	/**
	 * Constructs a new constant node that belongs to a given graph and
	 * represents a given constant value.
	 * 
	 * @param graph the graph this node belongs to
	 * @param label the value this node represents
	 */
	protected ConstantNode(HeuristicGraph graph, Value label) {
		super(graph, label);
		this.label = label;
		this.range = graph.makeRange(label);
		this.range.setCost(label, 0);
	}
	
	@Override
	public int size() {
		return 1;
	}
	
	@Override
	public Iterator<Entry> iterator() {
		return range.iterator();
	}
	
	@Override
	public Value getValue(int index) {
		if(index == 0)
			return label;
		else
			throw Exceptions.indexOutOfBounds(index);
	}

	@Override
	public double getCost(int index) {
		if(index == 0)
			return 0;
		else
			throw Exceptions.indexOutOfBounds(index);
	}
		
	@Override
	public double getCost(Value value) {
		if(label.equals(value))
			return 0;
		else
			return Double.POSITIVE_INFINITY;
	}
	
	@Override
	public double getCost(Operator operator, Value value) {
		if(operator.test(label, value))
			return 0;
		else
			return Double.POSITIVE_INFINITY;
	}
	
	/**
	 * Does nothing and returns false. A constant represents a single value
	 * which costs 0 at all times, so its set of values cannot be modified.
	 * 
	 * @param value a value that the constant can now have
	 * @param cost the cost of the constant having that value
	 * @return false
	 */
	@Override
	protected boolean setCost(Value value, double cost) {
		return false;
	}
}