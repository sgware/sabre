package edu.uky.cs.nil.sabre;

import edu.uky.cs.nil.sabre.util.Countable;

/**
 * A plan is a sequence of {@link Action actions}.
 * 
 * @param <A> the type of {@link Action action} this plan is made of
 * @author Stephen G. Ware
 */
public interface Plan<A extends Action> extends Iterable<A>, Countable {

	/**
	 * Returns the action at a given index in this plan.
	 * 
	 * @param index the index of the desired action
	 * @return the action
	 * @throws IndexOutOfBoundsException if no such action exists
	 */
	public A get(int index);
}