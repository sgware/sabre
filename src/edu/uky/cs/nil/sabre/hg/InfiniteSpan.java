package edu.uky.cs.nil.sabre.hg;

import edu.uky.cs.nil.sabre.Number;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Comparison;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A {@link Span span} of numeric values (and their associated costs) which
 * automatically expands to include the value {@link Number#POSITIVE_INFINITY
 * positive infinity} any time its upper bound increases and automatically
 * expands to include the value {@link Number#NEGATIVE_INFINITY negative
 * infinity} any time its lower bound decreases. An infinite span represents
 * the {@link HeuristicGraph heuristic graph} assumption that once an action
 * becomes possible (has a finite cost) it will always be possible henceforth.
 * If an action increases a numeric value, and if the action can be taken over
 * and over again, it will eventually increase a numeric range to positive
 * infinity. The same applies when decreasing a value.
 * <p>
 * In its default (empty) state, an infinite span contains no values, meaning
 * that every value has infinite cost. The first time a numeric value is added
 * to an infinite span, that value becomes the only value with a finite cost.
 * Later, if any value higher than that initial value is added, the upper bound
 * on the span becomes positive infinity; likewise, if any value lower than
 * that initial value is added, the lower bound on the span becomes negative
 * infinity.
 * <p>
 * For example, suppose the first value added to the span is 5, and it has a
 * cost of 0. When the span is asked {@link #getCost(Value) the cost} of 5, it
 * will answer 0; if asked the cost of any other value, it will answer positive
 * infinity. Now suppose the value 6 is added to the span with a cost of 1. The
 * span now contains the value 5 (which costs 0) and all numbers greater than 5
 * (which all cost 1). The value of any number less than 5 is still positive
 * infinity. Now suppose the value 4 is added to the span with a cost of 2. The
 * span now contains the value 5 (which costs 0), all numbers greater than 5
 * (which all cost 1) and all numbers less than 5 (which all cost 2).
 * 
 * @author Stephen G. Ware
 */
public class InfiniteSpan extends Span {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;

	@Override
	protected Value setCost(Value value, double cost) {
		if(getCost(Comparison.EQUAL_TO, value) > cost) {
			if(getCost(Comparison.LESS_THAN, value) <= cost)
				return super.setCost(Number.POSITIVE_INFINITY, cost);
			else if(getCost(Comparison.GREATER_THAN, value) <= cost)
				return super.setCost(Number.NEGATIVE_INFINITY, cost);
		}
		return super.setCost(value, cost);
	}
}