package edu.uky.cs.nil.sabre.util;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Consumer;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Utilities;

/**
 * A unique map uses {@link Unique} objects as keys and maps each key to a
 * value. This map assumes the keys are all members of the same set of unique
 * values, meaning their {@link Object#hashCode() hash codes} are unique
 * sequential integers starting at 0.
 *
 * @param <K> the type of the key objects
 * @param <V> the type of the value objects
 * @author Stephen G. Ware
 */
public class UniqueMap<K extends Unique, V> implements Iterable<V>, Serializable, Countable {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** An array in which to store the values */
	private Object[] mapping;
	
	/** The number of values stored in this map */
	private int size = 0;
	
	/**
	 * Constructs a new unique map which starts with the capacity to store up
	 * to a given number of keys. Unique maps will expand as needed, but if the
	 * number of keys a map will hold is known in advance this can same some
	 * array allocation.
	 * 
	 * @param capacity the number of keys to hold
	 */
	public UniqueMap(int capacity) {
		this.mapping = new Object[capacity];
	}
	
	/**
	 * Constructs a new unique map.
	 */
	public UniqueMap() {
		this(0);
	}
	
	/**
	 * Constructs a unique map which use a given map for its initial key/value
	 * mapping.
	 * 
	 * @param map a map of keys to values
	 */
	public UniqueMap(Map<K, V> map) {
		this(capacity(map));
		for(Entry<K, V> entry : map.entrySet())
			put(entry.getKey(), entry.getValue());
	}
	
	private static final <K extends Unique, V> int capacity(Map<K, V> map) {
		int size = 0;
		for(Entry<K, V> entry : map.entrySet())
			size = Math.max(size, entry.getKey().hashCode());
		return size;
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			Object[] otherMapping = ((UniqueMap<?, ?>) other).mapping;
			for(int i=0; i<Math.max(mapping.length, otherMapping.length); i++)
				if(!equals(i, mapping, otherMapping))
					return false;
			return true;
		}
		return false;
	}
	
	private static final boolean equals(int index, Object[] array1, Object[] array2) {
		Object object1 = index < array1.length ? array1[index] : null;
		Object object2 = index < array2.length ? array2[index] : null;
		return Utilities.equals(object1, object2);
	}
	
	@Override
	public int hashCode() {
		int hashCode = 1;
		for(Object object : mapping)
			if(object != null)
				hashCode = hashCode * 31 + object.hashCode();
		return hashCode;
	}
	
	@Override
	public String toString() {
		String string = "[";
		boolean first = true;
		for(int i=0; i<mapping.length; i++) {
			if(mapping[i] != null) {
				if(first)
					first = false;
				else
					string += ", ";
				string += i + " -> " + mapping[i];
			}
		}
		return string + "]";
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public Iterator<V> iterator() {
		Object[] values = new Object[size()];
		int index = 0;
		for(Object value : mapping)
			if(value != null)
				values[index++] = value;
		return new ArrayIterator<>((V[]) values);
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public void forEach(Consumer<? super V> consumer) {
		for(Object value : mapping)
			if(value != null)
				consumer.accept((V) value);
	}
	
	@Override
	public int size() {
		return size;
	}
	
	/**
	 * Returns the value mapped to a given {@link Unique unique key}.
	 * 
	 * @param key the unique key
	 * @return the value mapped to that key, or null if the key does not exist
	 * in the map or if it has been explicitly mapped to null
	 */
	@SuppressWarnings("unchecked")
	public V get(K key) {
		int id = key.hashCode();
		if(id >= 0 && id < mapping.length)
			return (V) mapping[id];
		else
			return null;
	}
	
	/**
	 * Maps a given {@link Unique unique key} to a given value. The {@link
	 * Object#hashCode() key object's hash code} method should return a
	 * positive integer that is unique for all other keys in the same set.
	 * If the key given returns the same hash code as some other key object,
	 * the original mapping will be overwritten. The map will automatically
	 * expand to accommodate the mapping.
	 * 
	 * @param key the unique key
	 * @param value the value
	 * @throws IllegalArgumentException if the key's hash code is less than 0
	 */
	public void put(K key, V value) {
		int id = key.hashCode();
		if(id < 0)
			throw Exceptions.uniqueHashCodesNotNegative(key);
		if(id >= mapping.length)
			mapping = Arrays.copyOf(mapping, id + 1);
		if(mapping[id] == null && value != null)
			size++;
		else if(mapping[id] != null && value == null)
			size--;
		mapping[id] = value;
	}
}