package edu.uky.cs.nil.sabre.logic;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Utilities;

/**
 * A substitution is a {@link Function function} which maps one set of objects
 * to other objects that will replace them. Substitutions are often used to
 * replace the {@link Variable variables} or {@link Fluent fluents} in a {@link
 * Logical logical formula} with {@link Value values}.
 * 
 * @author Stephen G. Ware
 */
public interface Substitution extends Function<Object, Object> {
	
	/**
	 * An empty substitution which maps every object to itself.
	 */
	public static final Substitution EMPTY = new Substitution() {

		@Override
		public Object apply(Object original) {
			return original;
		}

		@Override
		public HashSubstitution replace(Object original, Object replacement) {
			return new HashSubstitution(original, replacement);
		}
	};
	
	/**
	 * Checks whether a replacement object can be substituted for some original
	 * object in a given substitution. Substitutions are allowed or disallowed
	 * in the following cases:
	 * <ul>
	 * <li>If the substitution already maps the original to that replacement,
	 * the substitution is allowed.</li>
	 * <li>If the substitution already maps the original to a different
	 * replacement, the substitution is not allowed.</li>
	 * <li>If the original object is a {@link Variable variable} and the
	 * replacement is a {@link Typed typed formula} of {@link Variable#type the
	 * variable's type}, the substitution is allowed.</li>
	 * <li>If the original object is a {@link Fluent fluent} and the
	 * replacement is a {@link Typed typed formula} of {@link Fluent#type the
	 * fluent's type}, the substitution is allowed.</li>
	 * <li>Otherwise, the substitution is not allowed.</li>
	 * </ul>
	 * Note that this method does not modify the substitution; it only checks
	 * whether a hypothetical substitution would be allowed.
	 * 
	 * @param original the original object that would be replaced
	 * @param replacement the object that the original would be replaced with
	 * @param substitution an existing substitution to which the proposed
	 * replacement would be added
	 * @return true if the substitution is allowed, false otherwise
	 */
	public static boolean canSubstitute(Object original, Object replacement, Substitution substitution) {
		Object current = substitution.apply(original);
		if(replacement.equals(current))
			return true;
		else if(!original.equals(current) && !current.equals(replacement))
			return false;
		else if(!(replacement instanceof Typed))
			return false;
		else if(original instanceof Variable)
			return ((Typed) replacement).is(((Variable) original).type);
		else if(original instanceof Fluent && replacement instanceof Value)
			return ((Typed) replacement).is(((Fluent) original).type);
		else
			return false;
	}
	
	/**
	 * Checks whether {@link #canSubstitute(Object, Object, Substitution) a
	 * proposed substitution is allowed} in an {@link #EMPTY empty
	 * substitution}.
	 * 
	 * @param original the original object that would be replaced
	 * @param replacement the object that the original would be replaced with
	 * @return true if the substitution is allowed, false otherwise
	 */
	public static boolean canSubstitute(Object original, Object replacement) {
		return canSubstitute(original, replacement, EMPTY);
	}
	
	/**
	 * Checks whether {@link #canSubstitute(Object, Object, Substitution) a
	 * series of proposed substitutions is allowed} given an existing
	 * substitution. The first original object would be replaced by the first
	 * replacement object, the second original by the second replacement, etc.
	 * This method does not modify the substitution.
	 * 
	 * @param originals an array of original objects that would be replaced
	 * @param replacements an array of objects that the originals would be
	 * replaced with, which should be the same size as the originals array
	 * @param substitution an existing substitution to which the proposed
	 * replacements would be added
	 * @return true if the substitutions are allowed, false otherwise
	 * @throws IllegalArgumentException if the number of original objects does
	 * not match the number of replacement objects
	 */
	public static boolean canSubstitute(Object[] originals, Object[] replacements, Substitution substitution) {
		if(originals.length != replacements.length)
			throw Exceptions.substitutionCount(originals.length, replacements.length);
		HashSubstitution s = new HashSubstitution(substitution);
		for(int i=0; i<originals.length; i++) {
			if(canSubstitute(originals[i], replacements[i], s))
				s.replace(originals[i], replacements[i]);
			else
				return false;
		}
		return true;
	}
	
	/**
	 * Checks whether {@link #canSubstitute(Object[], Object[], Substitution) a
	 * series of proposed substitutions is allowed} in an {@link #EMPTY empty
	 * substitution}.
	 * 
	 * @param originals an array of original objects that would be replaced
	 * @param replacements an array of objects that the originals would be
	 * replaced with, which should be the same size as the originals array
	 * @return true if the substitutions are allowed, false otherwise
	 * @throws IllegalArgumentException if the number of original objects does
	 * not match the number of replacement objects
	 */
	public static boolean canSubstitute(Object[] originals, Object[] replacements) {
		return canSubstitute(originals, replacements, EMPTY);
	}
	
	/**
	 * Checks whether {@link #canSubstitute(Object, Object, Substitution) a
	 * series of proposed substitutions is allowed} given an existing
	 * substitution. The first original object would be replaced by the first
	 * replacement object, the second original by the second replacement, etc.
	 * This method does not modify the substitution.
	 * 
	 * @param originals an {@link Iterable iterable} of original objects that
	 * would be replaced
	 * @param replacements an {@link Iterable iterable} of objects that the
	 * originals would be replaced with, which should have the same number of
	 * elements as the originals iterable
	 * @param substitution an existing substitution to which the proposed
	 * replacements would be added
	 * @return true if the substitutions are allowed, false otherwise
	 * @throws IllegalArgumentException if the number of original objects does
	 * not match the number of replacement objects
	 */
	public static boolean canSubstitute(Iterable<?> originals, Iterable<?> replacements, Substitution substitution) {
		return canSubstitute(Utilities.toArray(originals, Object.class), Utilities.toArray(replacements, Object.class), substitution);
	}
	
	/**
	 * Checks whether {@link #canSubstitute(Iterable, Iterable, Substitution) a
	 * series of proposed substitutions is allowed} in an {@link #EMPTY empty
	 * substitution}.
	 * 
	 * @param originals an {@link Iterable iterable} of original objects that
	 * would be replaced
	 * @param replacements an {@link Iterable iterable} of objects that the
	 * originals would be replaced with, which should have the same number of
	 * elements as the originals iterable
	 * @return true if the substitutions are allowed, false otherwise
	 * @throws IllegalArgumentException if the number of original objects does
	 * not match the number of replacement objects
	 */
	public static boolean canSubstitute(Iterable<?> originals, Iterable<?> replacements) {
		return canSubstitute(originals, replacements, EMPTY);
	}
	
	/**
	 * Returns a substitution in which maps a given original object to a given
	 * replacement. Depending on how the substitution is implemented, this
	 * method may modify this substitution and return itself or it may return
	 * a new substitution with the added mapping. This method does not
	 * necessarily {@link #canSubstitute(Iterable, Iterable, Substitution)
	 * check whether the substitution is allowed}.
	 * 
	 * @param original the original object to be replaced
	 * @param replacement the object to replace the original with
	 * @return a substitution which maps the original object to its replacement
	 */
	public Substitution replace(Object original, Object replacement);
}