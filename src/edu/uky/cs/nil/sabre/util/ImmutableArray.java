package edu.uky.cs.nil.sabre.util;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Iterator;
import java.util.function.Consumer;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Utilities;

/**
 * A wrapper around a Java {@code array} that makes it read-only and provides
 * some additional helpful methods.
 * 
 * @param <T> the component type of the array
 * @author Stephen G. Ware
 */
public class ImmutableArray<T> implements Iterable<T>, Serializable, Countable {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The mutable array which this class protects from modification */
	final Object[] array;
	
	/**
	 * A package private constructor used by classes which extend this class
	 * and need to set the array of objects without exposing the constructor.
	 * 
	 * @param array the array
	 * @param ignored not used
	 */
	ImmutableArray(Object[] array, Void ignored) {
		this.array = array;
	}
	
	/**
	 * Constructs a new immutable array from a given Java array.
	 * 
	 * @param array the array
	 */
	@SafeVarargs
	public ImmutableArray(T...array) {
		this(array, null);
	}
	
	/**
	 * Constructs a new immutable array from an {@link Iterable}.
	 * 
	 * @param iterable the collection which will be converted into an array
	 */
	public ImmutableArray(Iterable<? extends T> iterable) {
		this(Utilities.toArray(iterable, Object.class), null);
	}
	
	@Override
	public boolean equals(Object other) {
		return getClass().equals(other.getClass()) && Arrays.equals(array, ((ImmutableArray<?>) other).array);
	}
	
	@Override
	public int hashCode() {
		int hashCode = getClass().hashCode();
		for(Object element : array)
			hashCode = Utilities.hashCode(hashCode, element);
		return hashCode;
	}
	
	@Override
	public String toString() {
		return Arrays.toString(array);
	}

	@Override
	@SuppressWarnings("unchecked")
	public Iterator<T> iterator() {
		return new ArrayIterator<T>((T[]) array);
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public void forEach(Consumer<? super T> consumer) {
		for(Object element : array)
			consumer.accept((T) element);
	}
	
	@Override
	public int size() {
		return array.length;
	}
	
	/**
	 * Returns the element at the given index in the array.
	 * 
	 * @param index the index
	 * @return the element at that index
	 * @throws IndexOutOfBoundsException in the index does not exist
	 */
	@SuppressWarnings("unchecked")
	public T get(int index) {
		return (T) array[index];
	}
	
	/**
	 * Casts the type of elements returned by this array to a different type.
	 * 
	 * @param <N> the new type
	 * @param type the class object for the new type
	 * @return an immutable array of the new type
	 * @throws ClassCastException if any element in the array cannot be cast
	 */
	@SuppressWarnings("unchecked")
	public <N> ImmutableArray<N> cast(Class<N> type) {
		for(Object object : array)
			type.cast(object);
		return (ImmutableArray<N>) this;
	}
	
	/**
	 * Tests whether a given element appears in the array.
	 * 
	 * @param object the element to search for
	 * @return true if the element is in the array, false otherwise
	 */
	public boolean contains(Object object) {
		return indexOf(object) != -1;
	}
	
	/**
	 * Returns the index of the first element in the array that is {@link
	 * Object#equals(Object) equal to} to given object.
	 * 
	 * @param object the object to search for
	 * @return the index of the first object equal to the query, or -1 if none
	 * were found
	 */
	public int indexOf(Object object) {
		for(int i=0; i<array.length; i++)
			if(object.equals(array[i]))
				return i;
		return -1;
	}
	
	/**
	 * Returns a new immutable array with the given object added to the end.
	 * 
	 * @param object the object to add
	 * @return a copy of this immutable array but with the new object added
	 */
	public ImmutableArray<T> add(T object) {
		Object[] array = new Object[this.array.length + 1];
		System.arraycopy(this.array, 0, array, 0, this.array.length);
		array[array.length - 1] = object;
		return new ImmutableArray<>(array, null);
	}
	
	/**
	 * Returns an immutable array with all objects that are {@link
	 * Object#equals(Object) equal to} a given object removed. If the array
	 * contains no such objects, this array is returned.
	 * 
	 * @param object the object to remove
	 * @return an array which contains no elements equal to the query
	 */
	public ImmutableArray<T> remove(Object object) {
		Object[] array = remove(this.array, 0, object, 0);
		if(array == this.array)
			return this;
		else
			return new ImmutableArray<>(array, null);
	}
	
	private static final Object[] remove(Object[] array, int index, Object object, int size) {
		if(index == array.length) {
			if(size == index)
				return array;
			else
				return new Object[size];
		}
		else if(Utilities.equals(array[index], object))
			return remove(array, index + 1, object, size);
		else {
			Object[] result = remove(array, index + 1, object, size + 1);
			result[size] = array[index];
			return result;
		}
	}
	
	/**
	 * Returns an immutable array where a given function has been applied to
	 * every element in the array. If the function does not change any of the
	 * elements, this array is returned.
	 * 
	 * @param function the function to apply to all elements
	 * @return an array where all elements are the output of the function
	 */
	public ImmutableArray<T> apply(Function<Object, Object> function) {
		for(int i=0; i<array.length; i++) {
			Object element = function.apply(array[i]);
			if(element != array[i]) {
				Object[] applied = new Object[array.length];
				for(int j=0; j<i; j++)
					applied[j] = array[j];
				applied[i] = element;
				for(int j=i+1; j<applied.length; j++)
					applied[j] = function.apply(array[j]);
				return new ImmutableArray<>(applied, null);
			}
		}
		return this;
	}
}