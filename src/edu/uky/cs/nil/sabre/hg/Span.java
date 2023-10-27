package edu.uky.cs.nil.sabre.hg;

import edu.uky.cs.nil.sabre.logic.Comparison.Operator;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Comparison;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * An implementation of {@link Range range} that supports {@link
 * edu.uky.cs.nil.sabre.logic.Typed#isNumber() numeric} {@link Value values}. A
 * span contains all numbers between its lowest and highest numeric values. For
 * example, when a span's lowest value is 2 and its highest value is 5, the
 * span contains all numbers between 2 and 5 (inclusive)--this is, all numbers
 * which are greater than or equal to 2 and less than or equal to 5.
 * 
 * @author Stephen G. Ware
 */
public class Span extends ArrayRange {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	@Override
	public double getCost(Operator operator, Value value) {
		if(operator.equals(Comparison.EQUAL_TO))
			return Math.max(getCost(Comparison.GREATER_THAN_OR_EQUAL_TO, value), getCost(Comparison.LESS_THAN_OR_EQUAL_TO, value));
		return super.getCost(operator, value);
	}
}