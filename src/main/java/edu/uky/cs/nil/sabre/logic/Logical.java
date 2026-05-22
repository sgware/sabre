package edu.uky.cs.nil.sabre.logic;

import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;

import edu.uky.cs.nil.sabre.Exceptions;

/**
 * A logical formula is an atomic or complex logical proposition or the pieces
 * which make up a logical proposition.
 * 
 * @author Stephen G. Ware
 */
public interface Logical extends Comparable<Logical>, Serializable {

	@Override
	public default int compareTo(Logical other) {
		if(this instanceof Value && !(other instanceof Value))
			return -1;
		else if(!(this instanceof Value) && other instanceof Value)
			return 1;
		else
			return getClass().getName().compareTo(other.getClass().getName());
	}
	
	/**
	 * Tests whether this logical formula is ground, which means that it
	 * contains no {@link Variable variables}.
	 * 
	 * @return true if this formula contains no variables, false otherwise
	 */
	public boolean isGround();
	
	/**
	 * If this logical formula is ground, this method does nothing; if it is
	 * not ground, {@link edu.uky.cs.nil.sabre.FormatException an exception} is
	 * thrown.
	 * 
	 * @throws edu.uky.cs.nil.sabre.FormatException if this logical formula is
	 * not ground
	 */
	public default void mustBeGround() {
		if(!isGround())
			throw Exceptions.mustBeGround(this);
	}
	
	/**
	 * Tests whether some query object occurs anywhere in this logical formula
	 * or any of the formulas which make up this formula.
	 * 
	 * @param query the object which might occur in this formula
	 * @return true if the object is this formula, occurs in this formula, or
	 * occurs in any of this formula's parts, false otherwise
	 */
	public default boolean occurs(Object query) {
		return find(Object.class, object -> object.equals(query)) != null;
	}
	
	/**
	 * Returns the first object which is of a given type and which satisfies a
	 * given predicate that can be found in this logical formula (or any of the
	 * logical formulas which make it up). If this formula meets those
	 * criteria, it may be returned.
	 * 
	 * @param <T> the type of object to return
	 * @param type the class object for the type
	 * @param predicate the predicate which the object of the given type must
	 * satisfy in order to be returned
	 * @return the first object found which is of the given type and satisfies
	 * the predicate
	 */
	public default <T> T find(Class<T> type, Predicate<? super T> predicate) {
		Set<T> result = collect(type, predicate);
		if(result.size() > 0)
			return result.iterator().next();
		else
			return null;
	}
	
	/**
	 * Returns the first object which is of a given type that can be found in
	 * this logical formula (or any of the logical formulas which make it up).
	 * If this formula is of that type, it may be returned.
	 * 
	 * @param <T> the type of object to return
	 * @param type the class object for the type
	 * @return the first object found which is of the given type
	 */
	public default <T> T find(Class<T> type) {
		return find(type, object -> true);
	}
	
	/**
	 * Returns the first object which satisfied the given predicate that can be
	 * found in this logical formula (or any of the logical formulas which make
	 * it up). If this formula satisfies the predicate, it may be returned.
	 * 
	 * @param predicate the predicate the object must satisfy
	 * @return the first object found which satisfies the predicate
	 */
	public default Object find(Predicate<Object> predicate) {
		return find(Object.class, predicate);
	}
	
	/**
	 * Returns a {@link Set set} of all objects that can be found in this
	 * logical formula (or any of the logical formulas which make it up) that
	 * are of a given type and satisfy a given predicate. If this formula
	 * meets those criteria, it will be included.
	 * 
	 * @param <T> the type of object to return
	 * @param type the class object for the type
	 * @param predicate the predicate the objects must satisfy
	 * @return a set of all objects found of the given type which satisfied the
	 * given predicate
	 */
	@SuppressWarnings("unchecked")
	public default <T> Set<T> collect(Class<T> type, Predicate<? super T> predicate) {
		LinkedHashSet<T> set = new LinkedHashSet<>();
		substitute(object -> {
			if(type.isAssignableFrom(object.getClass())) {
				T candidate = (T) object;
				if(predicate.test(candidate))
					set.add(candidate);
			}
			return object;
		});
		return set;
	}
	
	/**
	 * Returns a {@link Set set} of all objects that can be found in this
	 * logical formula (or any of the logical formulas which make it up) that
	 * are of a given type. If this formula meets those criteria, it will be
	 * included.
	 * 
	 * @param <T> the type of object to return
	 * @param type the class object for the type
	 * @return a set of all objects found of the given type
	 */
	public default <T> Set<T> collect(Class<T> type) {
		return collect(type, object -> true);
	}
	
	/**
	 * Returns a {@link Set set} of all objects that can be found in this
	 * logical formula (or any of the logical formulas which make it up) that
	 * satisfy a given predicate. If this formula meets those criteria, it will
	 * be included.
	 * 
	 * @param predicate the predicate the objects must satisfy
	 * @return a set of all objects found which satisfy the predicate
	 */
	public default Set<Object> collect(Predicate<Object> predicate) {
		return collect(Object.class, predicate);
	}

	/**
	 * Applies a given function to every part of this logical formula and then
	 * creates a new logical formula of this same type using the results
	 * returned from the function. If, for every part of this formula, the
	 * function always returns its input as output with no changes, this
	 * method should return this object (not just any object that is {@link
	 * Object#equals(Object) equal to} this object).
	 * 
	 * @param function the function to apply to all parts of this formula
	 * @return a logical formula of the same type as this formula made from the
	 * returned objects, or this object if no parts were changed by the
	 * function
	 */
	public Logical apply(Function<Object, Object> function);
	
	/**
	 * Recursively {@link #apply(Function) applies} a given function to every
	 * part of this logical formula (and to every part of those parts, etc.)
	 * and then creates a new logical formula of this same type using the
	 * results returned by the function. As a final step, the newly created
	 * formula is then also given as input to the function and the result is
	 * returned from this method. If, for every part of this formula, the
	 * function always returns its input as output with no changes, and this
	 * object itself is not replaced by the function, this method should return
	 * this object (not just any object that is {@link Object#equals(Object)
	 * equal to} this object).
	 * 
	 * @param substitution the function to be applied recursively to all parts
	 * of this formula, and all parts of their parts, etc., and then to the
	 * new object which is created from the results
	 * @return the result of applying the function to a formula of the same
	 * type as this formula that was made from the recursively modified parts
	 * of this formula
	 */
	public default Object substitute(Function<Object, Object> substitution) {
		return substitution.apply(apply(part -> {
			if(part instanceof Logical)
				return ((Logical) part).substitute(substitution);
			else
				return substitution.apply(part);
		}));
	}
	
	/**
	 * Recursively replaces one object in this logical formula (and in every
	 * part of this formula, and the parts of their parts, etc.) and then
	 * creates a new logical formula of this same type using the new parts.
	 * If the object being replaced does not appear anywhere in this formula,
	 * and this object itself is not the thing being replaced, this method
	 * should return this object (not just any object that is {@link
	 * Object#equals(Object) equal to} this object).
	 * 
	 * @param original the object being replaced
	 * @param replacement the object to replace the original with
	 * @return a logical formula of the same type as this formula made from the
	 * parts of this formula but with the given object replaced, or if this
	 * formula is the thing being replaced, the replacement, otherwise this
	 * object
	 */
	public default Object substitute(Object original, Object replacement) {
		return substitute(object -> object.equals(original) ? replacement : object);
	}
	
	/**
	 * Returns a copy of this formula such that none of its {@link Variable
	 * variables} are {@link Variable#equals(Object) the same as} any variables
	 * in the given formula. This method can be used when comparing two
	 * formulas to ensure they do no appear the same because they happen to
	 * contain two different variables with the same name and type.
	 * 
	 * @param other the formula with whom no variables should overlap
	 * @return this formula, but with any variables that occur in the given
	 * formula renamed
	 */
	public default Logical distinguish(Logical other) {
		Substitution rename = new HashSubstitution();
		for(Variable original : collect(Variable.class)) {
			int index = 1;
			Variable replacement = original;
			while(other.occurs(replacement))
				replacement = new Variable(original.name + "-" + (index++), original.type);
			if(replacement != original)
				rename.replace(original, replacement);
		}
		return (Logical) substitute(rename);
	}
}