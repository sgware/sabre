package edu.uky.cs.nil.sabre.hg;

import java.io.Serializable;
import java.util.Arrays;

import edu.uky.cs.nil.sabre.logic.Comparison.Operator;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * An implementation of {@link Range a range} that uses an array to store its
 * values and their costs. The values in this range are always kept in order
 * from lowest to highest cost.
 * 
 * @author Stephen G. Ware
 */
public class ArrayRange extends Range {
	
	/**
	 * A mutable implementation of {@link CostSet.Entry}.
	 * 
	 * @author Stephen G. Ware
	 */
	private final class MutableEntry implements Serializable {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1L;
		
		/** A value in the cost set */
		public Value value;
		
		/** The cost of the value in the set */
		public double cost;
	}
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The array in which entries are stored */
	private MutableEntry[] entries = new MutableEntry[0];
	
	/** The number of entries currently in the array */
	private int size = 0;
	
	@Override
	public int size() {
		return size;
	}

	@Override
	public Value getValue(int index) {
		return entries[index].value;
	}

	@Override
	public double getCost(int index) {
		return entries[index].cost;
	}

	@Override
	public double getCost(Operator operator, Value value) {
		for(int i=0; i<size(); i++)
			if(operator.test(getValue(i), value))
				return getCost(i);
		return Double.POSITIVE_INFINITY;
	}

	@Override
	protected Value setCost(Value value, double cost) {
		// Return null if this value already has this cost.
		if(cost >= getCost(value))
			return null;
		// If an entry for this value already exists, move it to the end.
		for(int i=0; i<size-1; i++)
			if(entries[i].value.equals(value))
				swap(i);
		// If the entry at the end is not this value, set it to this value and increase the size.
		if(size == 0 || !entries[size - 1].value.equals(value)) {
			if(entries.length == size) {
				entries = Arrays.copyOf(entries, size + 1);
				entries[size] = new MutableEntry();
			}
			entries[size].value = value;
			size++;
		}
		// Set this value's cost to the cost.
		entries[size - 1].cost = cost;
		// Move the entry toward the start until its cost is in sorted order.
		for(int i = size - 2; i >= 0 && entries[i].cost > entries[i + 1].cost; i--)
			swap(i);
		return value;
	}
	
	private final void swap(int index) {
		MutableEntry temp = entries[index];
		entries[index] = entries[index + 1];
		entries[index + 1] = temp;
	}

	@Override
	protected void reset() {
		size = 0;
	}
}
