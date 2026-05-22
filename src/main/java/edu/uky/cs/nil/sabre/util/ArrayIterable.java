package edu.uky.cs.nil.sabre.util;

import java.util.Iterator;

/**
 * An {@link Iterable iterable} wrapper for an array of objects.
 * 
 * @param <T> the component type of the array
 * @author Stephen G. Ware
 */
public class ArrayIterable<T> implements Iterable<T> {

	private final T[] array;
	
	/**
	 * Constructs a new array iterable.
	 * 
	 * @param array the array to make iterable
	 */
	@SafeVarargs
	public ArrayIterable(T...array) {
		this.array = array;
	}
	
	@Override
	public Iterator<T> iterator() {
		return new ArrayIterator<T>(array);
	}
}
