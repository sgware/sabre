package edu.uky.cs.nil.sabre.util;

import java.io.Serializable;
import java.util.Iterator;
import java.util.function.Consumer;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Utilities;

/**
 * A singly-linked list whose structure cannot be modified.
 * 
 * @param <T> the type of element in the list
 * @author Stephen G. Ware
 */
public class ImmutableList<T> implements Iterable<T>, Serializable, Countable {
	
	/** An empty list */
	public static final ImmutableList<Object> EMPTY = new ImmutableList<>(null, null);
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The first element of the list */
	public final T first;
	
	/** All but the first element of the list */
	public final ImmutableList<T> rest;
	
	private ImmutableList(T first, ImmutableList<T> rest) {
		this.first = first;
		this.rest = rest;
	}
	
	@Override
	public boolean equals(Object other) {
		return getClass().equals(other.getClass()) && equals(this, (ImmutableList<?>) other);
	}
	
	private static final boolean equals(ImmutableList<?> l1, ImmutableList<?> l2) {
		if(l1 == EMPTY)
			return l2 == EMPTY;
		else if(l2 == EMPTY)
			return false;
		else
			return Utilities.equals(l1.first, l2.first) && equals(l1.rest, l2.rest);
	}
	
	@Override
	public int hashCode() {
		int hashCode = getClass().hashCode();
		ImmutableList<T> list = this;
		while(list != EMPTY) {
			hashCode = Utilities.hashCode(hashCode, first);
			list = list.rest;
		}
		return hashCode;
	}
	
	@Override
	public String toString() {
		String string = "[";
		ImmutableList<T> list = this;
		while(list != EMPTY) {
			if(list != this)
				string += ", ";
			string += list.first;
			list = list.rest;
		}
		return string + "]";
	}
	
	@Override
	public int size() {
		if(this == EMPTY)
			return 0;
		else
			return 1 + rest.size();
	}
	
	@Override
	public Iterator<T> iterator() {
		return new ListIterator<>(this);
	}
	
	@Override
	public void forEach(Consumer<? super T> consumer) {
		ImmutableList<T> list = this;
		while(list != EMPTY) {
			consumer.accept(list.first);
			list = list.rest;
		}
	}
	
	/**
	 * Returns the element at the given index in the list.
	 * 
	 * @param index the index
	 * @return the element at that index
	 * @throws IndexOutOfBoundsException in the index does not exist
	 */
	public T get(int index) {
		ImmutableList<T> list = this;
		for(int i=0; i<index && list!=EMPTY; i++)
			list = list.rest;
		if(list == EMPTY)
			throw Exceptions.indexOutOfBounds(index);
		else
			return list.first;
	}
	
	/**
	 * Casts the type of elements in this list to a different type.
	 * 
	 * @param <N> the new type
	 * @param type the class object for the new type
	 * @return an immutable list of the new type
	 * @throws ClassCastException if any element in the list cannot be cast
	 */
	@SuppressWarnings("unchecked")
	public <N> ImmutableList<N> cast(Class<N> type) {
		ImmutableList<T> list = this;
		while(list != EMPTY) {
			type.cast(list.first);
			list = list.rest;
		}
		return (ImmutableList<N>) this;
	}
	
	/**
	 * Tests whether a given element appears in the list.
	 * 
	 * @param object the element to search for
	 * @return true if the element is in the list, false otherwise
	 */
	public boolean contains(Object object) {
		ImmutableList<T> list = this;
		while(list != EMPTY) {
			if(Utilities.equals(list.first, object))
				return true;
			list = list.rest;
		}
		return false;
	}
	
	/**
	 * Returns a new immutable list with the given object added to the
	 * beginning.
	 * 
	 * @param object the object to add
	 * @return a copy of this immutable list but with the new object added
	 */
	public ImmutableList<T> add(T object) {
		return new ImmutableList<>(object, this);
	}
	
	/**
	 * Returns an immutable list with all objects that are {@link
	 * Object#equals(Object) equal to} a given object removed. If the list
	 * contains no such objects, this list is returned.
	 * 
	 * @param object the object to remove
	 * @return a list which contains no elements equal to the query
	 */
	public ImmutableList<T> remove(Object object) {
		return remove(this, object);
	}
	
	private static final <T> ImmutableList<T> remove(ImmutableList<T> list, Object object) {
		if(list == EMPTY)
			return list;
		else if(Utilities.equals(list.first, object))
			return list.rest;
		else {
			ImmutableList<T> rest = remove(list.rest, object);
			if(rest == list.rest)
				return list;
			else
				return rest.add(list.first);
		}
	}
}