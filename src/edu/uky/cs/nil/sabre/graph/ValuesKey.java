package edu.uky.cs.nil.sabre.graph;

import java.io.Serializable;
import java.util.Arrays;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A values key is a utility object that serves as the key to the {@link
 * java.util.HashMap hashtable} which {@link StateGraph#values interns unique
 * arrays of values} for the {@link StateGraph#fluents fluents tracked by a
 * state graph}. These keys ensure that, for each possible unique combination
 * of values that can be stored in an array, only one copy exists and is shared
 * by all nodes which have those same values for the graph's fluents.
 * 
 * @author Stephen G. Ware
 */
final class ValuesKey implements Serializable {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * The values for each of the {@link StateGraph#fluents fluents tracked by
	 * the graph}, where the i'th fluent's value is at index i
	 */
	public final Value[] values;
	
	ValuesKey(Value[] values) {
		this.values = values;
	}
	
	@Override
	public boolean equals(Object other) {
		return other instanceof ValuesKey && Arrays.equals(values, ((ValuesKey) other).values);
	}
	
	@Override
	public int hashCode() {
		return Arrays.hashCode(values);
	}
}