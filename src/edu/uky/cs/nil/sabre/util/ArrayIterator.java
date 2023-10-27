package edu.uky.cs.nil.sabre.util;

import java.util.Iterator;

import edu.uky.cs.nil.sabre.Exceptions;

/**
 * An {@link Iterator iterator} which iterates through each element in an
 * array.
 * 
 * @param <T> the component type of the array
 * @author Stephen G. Ware
 */
public class ArrayIterator<T> implements Iterator<T> {

	private final Object[] array;
	private final int last;
	private int index;
	
	/**
	 * Constructs a new array iterator that will iterate over a subset of
	 * elements in an array.
	 * 
	 * @param array the array to iterate over
	 * @param first index of the first element to return while iterating
	 * @param last index of the last element to return while iterating
	 */
	public ArrayIterator(T[] array, int first, int last) {
		this.array = array;
		this.index = Math.max(first, 0);
		this.last = Math.min(last, array.length - 1);
	}
	
	/**
	 * Constructs a new array iterator that will iterate over all elements in
	 * an array.
	 * 
	 * @param array the array to iterator over
	 */
	@SuppressWarnings("unchecked")
	public ArrayIterator(T...array) {
		this(array, 0, array.length - 1);
	}
	
	@Override
	public boolean hasNext() {
		return index <= last;
	}

	@Override
	@SuppressWarnings("unchecked")
	public T next() {
		if(!hasNext())
			throw Exceptions.iteratorOutOfElements();
		return (T) array[index++];
	}
}