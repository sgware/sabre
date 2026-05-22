package edu.uky.cs.nil.sabre;

import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A state is any function which can map {@link Fluent fluents} to {@link Value
 * values}.
 * 
 * @author Stephen G. Ware
 */
@FunctionalInterface
public interface State {

	/**
	 * Returns the value assigned to a fluent in the current state.
	 * 
	 * @param fluent the fluent
	 * @return the value
	 */
	public Value getValue(Fluent fluent);
}