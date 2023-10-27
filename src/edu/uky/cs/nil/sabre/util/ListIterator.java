package edu.uky.cs.nil.sabre.util;

import java.util.Iterator;

import edu.uky.cs.nil.sabre.Exceptions;

/**
 * An {@link Iterator iterator} for iterating through an {@link
 * ImmutableList}.
 * 
 * @param <T> the type of element in the list 
 * @author Stephen G. Ware
 */
public class ListIterator<T> implements Iterator<T> {
	
	private ImmutableList<T> list;
	
	/**
	 * Constructs a new iterator for a given immutable list.
	 * 
	 * @param list the list
	 */
	public ListIterator(ImmutableList<T> list) {
		this.list = list;
	}

	@Override
	public boolean hasNext() {
		return list != ImmutableList.EMPTY;
	}

	@Override
	public T next() {
		if(!hasNext())
			throw Exceptions.iteratorOutOfElements();
		T element = list.first;
		list = list.rest;
		return element;
	}
}