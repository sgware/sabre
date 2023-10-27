package edu.uky.cs.nil.sabre;

import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.concurrent.TimeUnit;

import edu.uky.cs.nil.sabre.io.DefaultPrinter;
import edu.uky.cs.nil.sabre.logic.Logical;
import edu.uky.cs.nil.sabre.util.ImmutableArray;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * A collection of miscellaneous utility methods.
 * 
 * @author Stephen G. Ware
 */
public class Utilities {
	
	/**
	 * A default {@link edu.uky.cs.nil.sabre.io.Printer printer} generally used
	 * for converting objects to strings
	 */
	public static final DefaultPrinter DEFAULT_PRINTER = new DefaultPrinter();
	
	/**
	 * Checks whether two objects are {@link Object#equals(Object) equal to}
	 * one another without causing a {@link NullPointerException} if one or
	 * both of them is null.
	 * 
	 * @param o1 the first object
	 * @param o2 the second object
	 * @return true if they are equal or both null, false otherwise
	 */
	public static final boolean equals(Object o1, Object o2) {
		if(o1 == null)
			return o2 == null;
		else if(o2 == null)
			return false;
		else
			return o1.equals(o2);
	}
	
	/**
	 * Returns an object's {@link Object#hashCode() hash code} or 0 if the
	 * object is null.
	 * 
	 * @param object the object or null
	 * @return the object's hash code or 0 if the object was null
	 */
	public static final int hashCode(Object object) {
		if(object == null)
			return 0;
		else
			return object.hashCode();
	}
	
	/**
	 * Combines two {@link #hashCode(Object) hash code} values.
	 * 
	 * @param hc1 the first hash code
	 * @param hc2 the second hash code
	 * @return the combined hash code
	 */
	public static final int hashCode(int hc1, int hc2) {
		return hc1 * 31 + hc2;
	}
	
	/**
	 * Combines a {@link #hashCode(Object) hash code} value with the {@link
	 * #hashCode(Object) hash code} of an object.
	 * 
	 * @param hc the hash code value
	 * @param o1 the object
	 * @return the combined hash code
	 */
	public static final int hashCode(int hc, Object o1) {
		return hashCode(hc, hashCode(o1));
	}

	/**
	 * Combines the {@link #hashCode(Object) hash code} values of two objects.
	 * 
	 * @param o1 the first object
	 * @param o2 the second object
	 * @return the combined hash code
	 */
	public static final int hashCode(Object o1, Object o2) {
		return hashCode(hashCode(o1), hashCode(o2));
	}
	
	/**
	 * Combines a {@link #hashCode(Object) hash code} value with the {@link
	 * #hashCode(Object) hash code} of two objects.
	 * 
	 * @param hc the hash code value
	 * @param o1 the first object
	 * @param o2 the second object
	 * @return the combined hash code
	 */
	public static final int hashCode(int hc, Object o1, Object o2) {
		return hashCode(hashCode(hc, o1), hashCode(o2));
	}
	
	/**
	 * Combines the {@link #hashCode(Object) hash code} values of three
	 * objects.
	 * 
	 * @param o1 the first object
	 * @param o2 the second object
	 * @param o3 the third object
	 * @return the combined hash code
	 */
	public static final int hashCode(Object o1, Object o2, Object o3) {
		return hashCode(hashCode(o1, o2), hashCode(o3));
	}
	
	/**
	 * Combines a {@link #hashCode(Object) hash code} value with the {@link
	 * #hashCode(Object) hash code} of three objects.
	 * 
	 * @param hc the hash code value
	 * @param o1 the first object
	 * @param o2 the second object
	 * @param o3 the third object
	 * @return the combined hash code
	 */
	public static final int hashCode(int hc, Object o1, Object o2, Object o3) {
		return hashCode(hashCode(hc, o1, o2), hashCode(o3));
	}
	
	/**
	 * Combines the {@link #hashCode(Object) hash code} values of four objects.
	 * 
	 * @param o1 the first object
	 * @param o2 the second object
	 * @param o3 the third object
	 * @param o4 the fourth object
	 * @return the combined hash code
	 */
	public static final int hashCode(Object o1, Object o2, Object o3, Object o4) {
		return hashCode(hashCode(o1, o2, o3), hashCode(o4));
	}
	
	/**
	 * Combines a {@link #hashCode(Object) hash code} value with the {@link
	 * #hashCode(Object) hash code} of four objects.
	 * 
	 * @param hc the hash code value
	 * @param o1 the first object
	 * @param o2 the second object
	 * @param o3 the third object
	 * @param o4 the fourth object
	 * @return the combined hash code
	 */
	public static final int hashCode(int hc, Object o1, Object o2, Object o3, Object o4) {
		return hashCode(hashCode(hc, o1, o2, o3), hashCode(o4));
	}
	
	/**
	 * Combines the {@link #hashCode(Object) hash code} values of five objects.
	 * 
	 * @param o1 the first object
	 * @param o2 the second object
	 * @param o3 the third object
	 * @param o4 the fourth object
	 * @param o5 the fifth object
	 * @return the combined hash code
	 */
	public static final int hashCode(Object o1, Object o2, Object o3, Object o4, Object o5) {
		return hashCode(hashCode(o1, o2, o3, o4), hashCode(o5));
	}
	
	/**
	 * Combines a {@link #hashCode(Object) hash code} value with the {@link
	 * #hashCode(Object) hash code} of five objects.
	 * 
	 * @param hc the hash code value
	 * @param o1 the first object
	 * @param o2 the second object
	 * @param o3 the third object
	 * @param o4 the fourth object
	 * @param o5 the fifth object
	 * @return the combined hash code
	 */
	public static final int hashCode(int hc, Object o1, Object o2, Object o3, Object o4, Object o5) {
		return hashCode(hashCode(hc, o1, o2, o3, o4), hashCode(o5));
	}
	
	/**
	 * Combines the {@link #hashCode(Object) hash code} values of six objects.
	 * 
	 * @param o1 the first object
	 * @param o2 the second object
	 * @param o3 the third object
	 * @param o4 the fourth object
	 * @param o5 the fifth object
	 * @param o6 the sixth object
	 * @return the combined hash code
	 */
	public static final int hashCode(Object o1, Object o2, Object o3, Object o4, Object o5, Object o6) {
		return hashCode(hashCode(o1, o2, o3, o4, o5), hashCode(o6));
	}
	
	/**
	 * Rounds a {@code double} up to the nearest whole number.
	 * 
	 * @param value the value to round up
	 * @return the nearest whole number that is greater than or equal to the
	 * value
	 */
	public static final double roundUp(double value) {
		return new BigDecimal(value).setScale(0, RoundingMode.UP).doubleValue();
	}
	
	/**
	 * Compares a pair of {@link Comparable comparable} objects.
	 * 
	 * @param <C1> the type of the objects
	 * @param o1 the first object
	 * @param o2 the second object
	 * @return a negative integer, zero, or a positive integer as the first
	 * object is less than, equal to, or greater than the second object
	 */
	public static final <C1 extends Comparable<C1>> int compare(C1 o1, C1 o2) {
		return o1.compareTo(o2);
	}
	
	/**
	 * Compares two pairs of {@link Comparable comparable} objects. The first
	 * pair (that is, the first two arguments) is compared. If the result is
	 * not 0, it is returned, otherwise the second pair (that is, the third and
	 * fourth arguments) is compared, and so on.
	 * 
	 * @param <C1> the type of the first pair of objects
	 * @param <C2> the type of the second pair of objects
	 * @param o1 the first object in the first pair
	 * @param o2 the second object in the first pair
	 * @param o3 the first object in the second pair
	 * @param o4 the second object in the second pair
	 * @return a negative integer, zero, or a positive integer as the earlier
	 * pairs are less than, equal to, or greater than the later pairs
	 */
	public static final <C1 extends Comparable<C1>, C2 extends Comparable<C2>> int compare(C1 o1, C1 o2, C2 o3, C2 o4) {
		int comparison = compare(o1, o2);
		if(comparison == 0)
			comparison = compare(o3, o4);
		return comparison;
	}
	
	/**
	 * Compares three pairs of {@link Comparable comparable} objects. The first
	 * pair (that is, the first two arguments) is compared. If the result is not
	 * 0, it is returned, otherwise the second pair (that is, the third and
	 * fourth arguments) is compared, and so on.
	 * 
	 * @param <C1> the type of the first pair of objects
	 * @param <C2> the type of the second pair of objects
	 * @param <C3> the type of the third pair of objects
	 * @param o1 the first object in the first pair
	 * @param o2 the second object in the first pair
	 * @param o3 the first object in the second pair
	 * @param o4 the second object in the second pair
	 * @param o5 the first object in the third pair
	 * @param o6 the second object in the third pair
	 * @return a negative integer, zero, or a positive integer as the earlier
	 * pairs are less than, equal to, or greater than the later pairs
	 */
	public static final <C1 extends Comparable<C1>, C2 extends Comparable<C2>, C3 extends Comparable<C3>> int compare(C1 o1, C1 o2, C2 o3, C2 o4, C3 o5, C3 o6) {
		int comparison = compare(o1, o2, o3, o4);
		if(comparison == 0)
			comparison = compare(o5, o6);
		return comparison;
	}
	
	/**
	 * Compares four pairs of {@link Comparable comparable} objects. The first
	 * pair (that is, the first two arguments) is compared. If the result is
	 * not 0, it is returned, otherwise the second pair (that is, the third and
	 * fourth arguments) is compared, and so on.
	 * 
	 * @param <C1> the type of the first pair of objects
	 * @param <C2> the type of the second pair of objects
	 * @param <C3> the type of the third pair of objects
	 * @param <C4> the type of the fourth pair of objects
	 * @param o1 the first object in the first pair
	 * @param o2 the second object in the first pair
	 * @param o3 the first object in the second pair
	 * @param o4 the second object in the second pair
	 * @param o5 the first object in the third pair
	 * @param o6 the second object in the third pair
	 * @param o7 the first object in the fourth pair
	 * @param o8 the second object in the fourth pair
	 * @return a negative integer, zero, or a positive integer as the earlier
	 * pairs are less than, equal to, or greater than the later pairs
	 */
	public static final <C1 extends Comparable<C1>, C2 extends Comparable<C2>, C3 extends Comparable<C3>, C4 extends Comparable<C4>> int compare(C1 o1, C1 o2, C2 o3, C2 o4, C3 o5, C3 o6, C4 o7, C4 o8) {
		int comparison = compare(o1, o2, o3, o4, o5, o6);
		if(comparison == 0)
			comparison = compare(o7, o8);
		return comparison;
	}
	
	/**
	 * Compares two {@link ImmutableArray arrays} of {@link Logical logical}
	 * objects. The first elements of both arrays are compared. If the result
	 * is not 0, it is returned, otherwise the second elements of both arrays
	 * are compared, and so on.
	 * 
	 * @param array1 the first array
	 * @param array2 the second array
	 * @return a negative integer, zero, or a positive integer as the earlier
	 * elements are less than, equal to, or greater than the later elements
	 */
	public static final int compare(ImmutableArray<? extends Logical> array1, ImmutableArray<? extends Logical> array2) {
		int comparison = 0;
		int size = Math.min(array1.size(), array2.size());
		for(int i=0; i<size && comparison==0; i++)
			comparison = array1.get(i).compareTo(array2.get(i));
		if(comparison == 0) {
			if(array1.size() > array2.size())
				return 1;
			else if(array2.size() > array1.size())
				return -1;
		}
		return comparison;
	}

	/**
	 * Iterates through all elements of an {@link Iterable iterable} and places
	 * them into an array of a given type.
	 * 
	 * @param <T> the component type of the array
	 * @param iterable the iterable
	 * @param type the class of the component type of the array
	 * @return an array containing all the elements in the iterable
	 */
	public static final <T> T[] toArray(Iterable<? extends T> iterable, Class<T> type) {
		return toArray(iterable.iterator(), 0, type);
	}
	
	@SuppressWarnings("unchecked")
	private static final <T> T[] toArray(Iterator<? extends T> iterator, int index, Class<T> type) {
		if(iterator.hasNext()) {
			T element = iterator.next();
			T[] array = toArray(iterator, index + 1, type);
			array[index] = element;
			return array;
		}
		else
			return (T[]) Array.newInstance(type, index);
	}
	
	/**
	 * Iterates through all elements of an {@link Iterable iterable} and places
	 * into a {@link ImmutableSet set} only those items which are of a given
	 * type.
	 * 
	 * @param <T> the type of elements to collect
	 * @param type the class object of the type
	 * @param iterable the iterable to iterate through
	 * @return a set of elements from the iterable of the given type
	 */
	@SuppressWarnings("unchecked")
	public static final <T> ImmutableSet<T> collect(Class<T> type, Iterable<?> iterable) {
		LinkedHashSet<T> set = new LinkedHashSet<>();
		for(Object element : iterable)
			if(type.isAssignableFrom(element.getClass()))
				set.add((T) element);
		return new ImmutableSet<>(set);
	}
	
	/**
	 * Wrap a string in parentheses, unless it is already so wrapped.
	 * 
	 * @param string the second
	 * @return the string, wrapped in parentheses
	 */
	public static final String wrap(String string) {
		if(string.startsWith("(") && string.endsWith(")"))
			return string;
		else
			return "(" + string + ")";
	}
	
	/**
	 * Converts a number of milliseconds into a short string representation
	 * that gives the numbers of days, hours, minutes, seconds, and
	 * milliseconds. For example, {@code "2d1h3m45s20ms"} represents two days,
	 * 1 hour, 3 minutes, fourty-five seconds, and twenty milliseconds.
	 * 
	 * @param ms the number of milliseconds
	 * @return a string representation of that amount of time
	 */
	public static final String time(long ms) {
		long days = TimeUnit.MILLISECONDS.toDays(ms);
		ms -= TimeUnit.DAYS.toMillis(days);
		long hours = TimeUnit.MILLISECONDS.toHours(ms);
		ms -= TimeUnit.HOURS.toMillis(hours);
		long minutes = TimeUnit.MILLISECONDS.toMinutes(ms);
		ms -= TimeUnit.MINUTES.toMillis(minutes);
		long seconds = TimeUnit.MILLISECONDS.toSeconds(ms);
		ms -= TimeUnit.SECONDS.toMillis(seconds);
		String string = "";
		if(days > 0)
			string += days + "d";
		if(hours > 0)
			string += hours + "h";
		if(minutes > 0)
			string += minutes + "m";
		if(seconds > 0)
			string += seconds + "s";
		if(ms > 0)
			string += ms + "ms";
		if(string.isEmpty())
			return "0ms";
		return string;
	}
}