package edu.uky.cs.nil.sabre.util;

import java.util.LinkedHashSet;
import java.util.Set;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Utilities;

/**
 * An {@link ImmutableArray} which contains no duplicates.
 * 
 * @param <T> the component type of the array
 * @author Stephen G. Ware
 */
public class ImmutableSet<T> extends ImmutableArray<T> {

	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * A package private constructor used by classes which extend this class
	 * and need to set the array of objects without exposing the constructor.
	 * 
	 * @param array the array
	 * @param ignored not used
	 */
	ImmutableSet(Object[] array, Void ignored) {
		super(array, null);
	}
	
	/**
	 * Constructs a new immutable set by reusing the underlying array of an
	 * existing immutable set.
	 * 
	 * @param toClone an existing immutable set whose underlying array will be
	 * reused
	 */
	protected ImmutableSet(ImmutableSet<T> toClone) {
		this(toClone.array, null);
	}
	
	/**
	 * Constructs a new immutable set from a given Java array, removing any
	 * duplicates found in the array.
	 * 
	 * @param array the array
	 */
	@SuppressWarnings("unchecked")
	public ImmutableSet(T...array) {
		super(removeDuplicates(array), null);
	}
	
	/**
	 * Constructs a new immutable set from an {@link Iterable}.
	 * 
	 * @param iterable the iterable to be converted into an array
	 */
	public ImmutableSet(Iterable<? extends T> iterable) {
		super(toArray(iterable), null);
	}
	
	private static final Object[] toArray(Iterable<?> iterable) {
		if(iterable instanceof ImmutableSet<?>)
			return ((ImmutableSet<?>) iterable).array;
		else if(iterable instanceof Set<?>)
			return Utilities.toArray(iterable, Object.class);
		else
			return removeDuplicates(Utilities.toArray(iterable, Object.class));
	}
	
	private static final Object[] removeDuplicates(Object[] array) {
		LinkedHashSet<Object> set = new LinkedHashSet<>();
		for(Object element : array)
			set.add(element);
		return set.toArray(new Object[set.size()]);
	}
	
	@Override
	public <N> ImmutableSet<N> cast(Class<N> type) {
		return (ImmutableSet<N>) super.cast(type);
	}
	
	/**
	 * Returns a new immutable set with the given object added to the end,
	 * unless the set already contains this object, in which case this set is
	 * returned.
	 * 
	 * @param object the object to add
	 * @return an immutable set that contains the object
	 */
	@Override
	public ImmutableSet<T> add(T object) {
		if(contains(object))
			return this;
		else
			return new ImmutableSet<>(super.add(object).array, null);
	}
	
	@Override
	public ImmutableSet<T> remove(Object object) {
		ImmutableArray<T> array = super.remove(object);
		if(array == this)
			return this;
		else
			return new ImmutableSet<>(array.array, null);
	}
	
	/**
	 * Returns an immutable set where a given function has been applied to 
	 * every element in the set and duplicates removed. If the function does
	 * not change any of the elements, this set is returned.
	 * 
	 * @param function the function to apply to all elements
	 * @return a set where all elements are the output of the function minus
	 * duplicates
	 */
	@Override
	public ImmutableSet<T> apply(Function<Object, Object> function) {
		ImmutableArray<T> result = super.apply(function);
		if(result == this)
			return this;
		else
			return new ImmutableSet<>(removeDuplicates(result.array), null);
	}
	
	/**
	 * Returns a new set containing all elements in this set and all elements
	 * in a given second set.
	 * 
	 * @param other the second set
	 * @return the union of this set and the second set
	 */
	public ImmutableSet<T> union(ImmutableSet<T> other) {
		LinkedHashSet<T> set = new LinkedHashSet<>();
		for(T element : this)
			set.add(element);
		for(T element : other)
			set.add(element);
		if(set.size() == this.size())
			return this;
		else if(set.size() == other.size())
			return other;
		else
			return new ImmutableSet<>(set);
	}
	
	/**
	 * Returns a new set containing only elements which appear in both this set
	 * and in a given second set.
	 * 
	 * @param other the second set
	 * @return the intersection of this set and the second set
	 */
	public ImmutableSet<T> intersection(ImmutableSet<T> other) {
		LinkedHashSet<T> set = new LinkedHashSet<>();
		for(T element : this)
			if(other.contains(element))
				set.add(element);
		if(set.size() == this.size())
			return this;
		else
			return new ImmutableSet<>(set);
	}
}