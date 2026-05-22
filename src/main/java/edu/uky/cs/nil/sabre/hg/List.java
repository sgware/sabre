package edu.uky.cs.nil.sabre.hg;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Iterator;
import java.util.function.Consumer;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.util.Countable;

/**
 * A list for holding elements of a {@link HeuristicGraph} heuristic graph.
 * This list is implemented similarly to an {@link java.util.ArrayList array
 * list}, but the method to add to it is package-private to prevent outside
 * modification and the method to remove all elements runs in constant time.
 * These lists are primary used when {@link Node heuristic graph nodes} need
 * to maintain lists of other nodes in the graph.
 * 
 * @param <T> the type of element in the list
 * @author Stephen G. Ware
 */
public class List<T> implements Iterable<T>, Serializable, Countable {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** An array to story the elements of the list */
	private T[] array;
	
	/** The number of elements currently in the list */
	private int size;

	/**
	 * Constructs a new heuristic graph list.
	 */
	@SuppressWarnings("unchecked")
	public List() {
		this.array = (T[]) new Object[0];
	}
	
	@Override
	public int size() {
		return size;
	}
	
	/**
	 * An {@link Iterator iterator} for a heuristic graph {@link List list}.
	 * 
	 * @author Stephen G. Ware
	 */
	private final class ListIterator implements Iterator<T> {

		private int index = 0;

		@Override
		public boolean hasNext() {
			return index < size();
		}

		@Override
		public T next() {
			if(hasNext())
				return get(index++);
			else
				throw Exceptions.iteratorOutOfElements();
		}
	}

	@Override
	public Iterator<T> iterator() {
		return new ListIterator();
	}
	
	@Override
	public void forEach(Consumer<? super T> consumer) {
		for(int i=0; i<size(); i++)
			consumer.accept(get(i));
	}

	/**
	 * Returns the list element at the given index.
	 * 
	 * @param index the index of the desired element
	 * @return the element at this index
	 * @throws IndexOutOfBoundsException if the index does not exist
	 */
	public T get(int index) {
		return array[index];
	}
	
	/**
	 * Adds an element to the list. This method is package-private so that
	 * only members of the {@link HeuristicGraph heuristic graph} package can
	 * add to these lists.
	 * 
	 * @param element
	 */
	final void add(T element) {
		if(size == array.length)
			setCapacity(array.length + 1);
		array[size++] = element;
	}
	
	/**
	 * Ensures that the array which stores the list elements has at least a
	 * given number of indices.
	 * 
	 * @param capacity the minimum number of elements that the array must have
	 */
	final void setCapacity(int capacity) {
		if(capacity > array.length)
			array = Arrays.copyOf(array, capacity);
	}
	
	/**
	 * Removes all elements from this list. Elements are not actually removed
	 * from the underlying array, which means this method runs in constant time
	 * but may cause a memory leak if references to objects remain that would
	 * otherwise be garbage-collected.
	 */
	final void clear() {
		size = 0;
	}
}