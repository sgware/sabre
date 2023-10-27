package edu.uky.cs.nil.sabre.graph;

import java.util.Iterator;

import edu.uky.cs.nil.sabre.Exceptions;

/**
 * An {@link java.util.Iterator iterator} for a collection of {@link Edge
 * edges}.
 * 
 * @param <E> the type of edge returned by this iterator
 * @author Stephen
 */
public class EdgeIterator<E extends Edge> implements Iterator<E> {

	private E edge;
	private final Edge.Group group;
	
	/**
	 * Constructs a new edge iterator.
	 * 
	 * @param first the first edge in the list
	 * @param group the group representing the type of edges to be iterated
	 * over
	 */
	EdgeIterator(E first, Edge.Group group) {
		this.edge = first;
		this.group = group;
	}
	
	@Override
	public boolean hasNext() {
		return edge != null;
	}

	@Override
	@SuppressWarnings("unchecked")
	public E next() {
		if(!hasNext())
			throw Exceptions.iteratorOutOfElements();
		E edge = this.edge;
		this.edge = (E) group.getNext(this.edge);
		return edge;
	}
}