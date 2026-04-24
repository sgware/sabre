package edu.uky.cs.nil.sabre.graph;

import java.util.Iterator;

import edu.uky.cs.nil.sabre.Exceptions;

/**
 * An {@link java.util.Iterator iterator} for a collection of {@link Edge
 * edges}.
 * 
 * @param <E> the type of edge returned by this iterator
 * @author Stephen G. Ware
 */
public final class EdgeIterator<E extends Edge> implements Iterator<E> {

	private final Edge.Group group;
	private Edge current;
	
	/**
	 * Constructs a new edge iterator.
	 * 
	 * @param first the first edge in the list
	 * @param group the group representing the type of edges to be iterated over
	 */
	EdgeIterator(Edge.Group group, E first) {
		this.group = group;
		this.current = first;
	}
	
	@Override
	public boolean hasNext() {
		return current != null;
	}

	@Override
	@SuppressWarnings("unchecked")
	public E next() {
		if(!hasNext())
			throw Exceptions.iteratorOutOfElements();
		Edge next = current;
		current = group.getNext(current);
		return (E) next;
	}
}