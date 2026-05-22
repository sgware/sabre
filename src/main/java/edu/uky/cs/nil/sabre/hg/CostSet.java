package edu.uky.cs.nil.sabre.hg;

import java.io.Serializable;
import java.util.Iterator;

import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.logic.Comparison;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ArrayIterator;
import edu.uky.cs.nil.sabre.util.Countable;

/**
 * A cost set is a collection of {@link Value values} where each is associated
 * with a numeric cost.
 * 
 * @author Stephen G. Ware
 */
public interface CostSet extends Iterable<CostSet.Entry>, Serializable, Countable {
	
	/**
	 * An individual {@link Value value}/cost pair in a {@link CostSet cost
	 * set}. Cost set entries are {@link Comparable#compareTo(Object) naturally
	 * ordered} by their costs.
	 * 
	 * @author Stephen G. Ware
	 */
	public static class Entry implements Comparable<Entry> {

		/** The value in the cost set */
		public final Value value;
		
		/** The cost of the value in the cost set */
		public final double cost;
		
		/**
		 * Constructs a new cost set entry.
		 * 
		 * @param value a value from the cost set
		 * @param cost the cost of the value
		 */
		protected Entry(Value value, double cost) {
			this.value = value;
			this.cost = cost;
		}
		
		@Override
		public boolean equals(Object other) {
			if(getClass().equals(other.getClass())) {
				Entry otherCost = (Entry) other;
				return Utilities.equals(value, otherCost.value) && cost == otherCost.cost;
			}
			return false;
		}
		
		@Override
		public int hashCode() {
			return Utilities.hashCode(getClass(), value, cost);
		}
		
		@Override
		public String toString() {
			return Utilities.DEFAULT_PRINTER.toString(this);
		}

		@Override
		public int compareTo(Entry other) {
			double comparison = cost - other.cost;
			if(comparison == 0)
				return 0;
			else if(comparison < 0)
				return -1;
			else
				return 1;
		}
	}
	
	@Override
	public default Iterator<Entry> iterator() {
		Entry[] entries = new Entry[size()];
		for(int i=0; i<entries.length; i++)
			entries[i] = new Entry(getValue(i), getCost(i));
		return new ArrayIterator<>(entries);
	}
	
	/**
	 * Returns the {@link Value value} at a given index in the set. The cost of
	 * value can be accessed via {@link #getCost(int)}.
	 * 
	 * @param index the index of the desired value
	 * @return the value at that index
	 */
	public Value getValue(int index);
	
	/**
	 * Returns the cost of the value at a given index in the set. The value
	 * itself can be accessed via {@link #getValue(int)}.
	 * 
	 * @param index the index of the value whose cost is desired
	 * @return the cost of the value at that index
	 */
	public double getCost(int index);

	/**
	 * Returns the cost associated with the given value, which is an estimate
	 * of the number of {@link edu.uky.cs.nil.sabre.Action actions} that need
	 * to be taken from the state a heuristic graph was {@link
	 * HeuristicGraph#initialize(edu.uky.cs.nil.sabre.State) initialized to}
	 * before the {@link edu.uky.cs.nil.sabre.logic.Expression logical
	 * expression} this set represents can have the given value. A cost of 0
	 * means the logical expression has this value from the start. A cost of
	 * {@link Double#POSITIVE_INFINITY positive infinity} means the expression
	 * cannot have this value.
	 * 
	 * @param value a value the logical expression this set represents might
	 * have
	 * @return an estimate of the number of actions that would need to be
	 * taken before this set can contain that value
	 */
	public default double getCost(Value value) {
		return getCost(Comparison.EQUAL_TO, value);
	}
	
	/**
	 * Returns the cost of some value in this set satisfying the given {@link
	 * Comparison.Operator comparison operator} when it is on the left and a
	 * given value is on the right. For example, if this method is called with
	 * the {@link Comparison#GREATER_THAN greater than operator} and the value
	 * 0, it means "what is the cost of this set having a value above 0?"
	 * 
	 * @param operator the comparison operator to satisfy
	 * @param value the value on the right hand side of the comparison
	 * @return the cost of some value in this set that satisfies the comparison
	 * when it is on the left hand side of the comparison
	 */
	public double getCost(Comparison.Operator operator, Value value);
}