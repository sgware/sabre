package edu.uky.cs.nil.sabre.hg;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A range is a {@link CostSet cost set} that defines protected methods for
 * setting the cost of a value and resetting the range to its original state.
 * 
 * @author Stephen G. Ware
 */
public abstract class Range implements CostSet {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	/**
	 * Sets the costs to be associated with a given {@link Value value}. If
	 * this method did not change the state of the range (perhaps because the
	 * value already had a lower cost), it will return {@code null}, otherwise
	 * it will return the value that was effectively added to the range, which
	 * may differ from the given value in some cases (see {@link
	 * InfiniteSpan}).
	 * 
	 * @param value the value whose cost will be set
	 * @param cost the cost to be associated with the value
	 * @return the value that was effectively added to the set if the state of
	 * this set was changed by this method, null otherwise
	 */
	protected abstract Value setCost(Value value, double cost);
	
	/**
	 * Returns this cost set to its default state, which is an empty set where
	 * every value has {@link Double#POSITIVE_INFINITY infinite cost}.
	 */
	protected abstract void reset();
}