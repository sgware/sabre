package edu.uky.cs.nil.sabre.graph;

import java.util.Iterator;

import edu.uky.cs.nil.sabre.util.Countable;

/**
 * A {@link Countable countable}, {@link Iterable iterable} collection of
 * {@link Edge edges}.
 * 
 * @param <E> the type of edge in this collection
 * @author Stephen G. Ware
 */
public class EdgeIterable<E extends Edge> implements Iterable<E>, Countable {
	
	private final E first;
	private final Edge.Group group;
	
	/**
	 * Constructs a new iterable collection of edges.
	 * 
	 * @param first the first edge in the list
	 * @param group the group representing the type of edges to be iterated
	 * over
	 */
	EdgeIterable(E first, Edge.Group group) {
		this.first = first;
		this.group = group;
	}
	
	@Override
	public Iterator<E> iterator() {
		return new EdgeIterator<E>(first, group);
	}

	@Override
	public int size() {
		return size(first, group);
	}
	
	private static final int size(Edge edge, Edge.Group group) {
		if(edge == null)
			return 0;
		else
			return 1 + size(group.getNext(edge), group);
	}
}