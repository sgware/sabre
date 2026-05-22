package edu.uky.cs.nil.sabre.hg;

import java.util.Iterator;

import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Comparison.Operator;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A {@link FluentNode fluent node} representing a {@link
 * edu.uky.cs.nil.sabre.logic.Expression#mustBeNumber() numeric} {@link Fluent
 * fluent}, which is any fluent that has a potentially infinite range of {@link
 * edu.uky.cs.nil.sabre.Number number values}.
 * 
 * @author Stephen G. Ware
 */
public class NumericFluentNode extends FluentNode {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;

	/**
	 * The set of possible values this fluent can have and their associated
	 * costs
	 */
	protected final Range range;
	
	/**
	 * Constructs a new fluent node that belongs to a given graph and
	 * represents a given numeric fluent.
	 * 
	 * @param graph the graph this node belongs to
	 * @param label the numeric fluent this node represents
	 * @throws edu.uky.cs.nil.sabre.FormatException if the fluent is not
	 * numeric
	 */
	protected NumericFluentNode(HeuristicGraph graph, Fluent label) {
		super(graph, label);
		label.mustBeNumber();
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
	
	@Override
	public boolean setCost(Value value, double cost) {
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
	protected void reset() {
		super.reset();
		range.reset();
	}
}