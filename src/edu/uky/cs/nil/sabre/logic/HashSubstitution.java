package edu.uky.cs.nil.sabre.logic;

import java.io.Serializable;
import java.util.HashMap;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Utilities;

/**
 * An implementation of the {@link Substitution substitution interface} backed
 * by a {@link HashMap hash map}.
 * 
 * @author Stephen G. Ware
 */
public class HashSubstitution implements Serializable, Substitution {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The substitution to which this substitution is an extension */
	private final Substitution previous;
	
	/** A mapping of original objects to replacements */
	private final HashMap<Object, Object> mapping = new HashMap<>();
	
	/**
	 * Constructs a new hash substitution that builds on a given existing
	 * substitution. The mappings in the existing substitution will also exist
	 * in this substitution. The existing substitution will not be modified
	 * when this substitution is modified.
	 * 
	 * @param previous an existing substitution whose mappings will also be
	 * included in this substitution
	 */
	public HashSubstitution(Substitution previous) {
		this.previous = previous;
	}
	
	/**
	 * Constructs a new empty hash substitution.
	 */
	public HashSubstitution() {
		this(EMPTY);
	}
	
	/**
	 * Constructs a new hash substitution initialized with the given mappings
	 * and which will build on a given existing substitution. The mappings in
	 * the existing substitution will also exist in this substitution. The
	 * existing substitution will not be modified when this substitution is
	 * modified.
	 * 
	 * @param originals an array of objects to be replaced
	 * @param replacements an array of objects to replace the original objects
	 * with
	 * @param previous an existing substitution whose mappings will also be
	 * included in this substitution
	 * @throws IllegalArgumentException if the number of replacement objects
	 * does not match the number of original objects
	 */
	public HashSubstitution(Object[] originals, Object[] replacements, Substitution previous) {
		this(previous);
		if(originals.length != replacements.length)
			throw Exceptions.substitutionCount(originals.length, replacements.length);
		for(int i=0; i<originals.length; i++)
			replace(originals[i], replacements[i]);
	}
	
	/**
	 * Constructs a new hash substitution initialized with the given mappings.
	 * 
	 * @param originals an array of objects to be replaced
	 * @param replacements an array of objects to replace the original objects
	 * with
	 * @throws IllegalArgumentException if the number of replacement objects
	 * does not match the number of original objects
	 */
	public HashSubstitution(Object[] originals, Object[] replacements) {
		this(originals, replacements, EMPTY);
	}
	
	/**
	 * Constructs a new hash substitution initialized with the given mappings
	 * and which will build on a given existing substitution. The mappings in
	 * the existing substitution will also exist in this substitution. The
	 * existing substitution will not be modified when this substitution is
	 * modified.
	 * 
	 * @param originals an {@link Iterable iterable} of objects to be replaced
	 * @param replacements an {@link Iterable iterable} of objects to replace
	 * the original objects with
	 * @param previous an existing substitution whose mappings will also be
	 * included in this substitution
	 * @throws IllegalArgumentException if the number of replacement objects
	 * does not match the number of original objects
	 */
	public HashSubstitution(Iterable<?> originals, Iterable<?> replacements, Substitution previous) {
		this(Utilities.toArray(originals, Object.class), Utilities.toArray(replacements, Object.class), previous);
	}
	
	/**
	 * Constructs a new hash substitution initialized with the given mappings.
	 * 
	 * @param originals an {@link Iterable iterable} of objects to be replaced
	 * @param replacements an {@link Iterable iterable} of objects to replace
	 * the original objects with
	 * @throws IllegalArgumentException if the number of replacement objects
	 * does not match the number of original objects
	 */
	public HashSubstitution(Iterable<?> originals, Iterable<?> replacements) {
		this(originals, replacements, EMPTY);
	}
	
	/**
	 * Constructs a new hash substitution initialized with a given mapping
	 * and which will build on a given existing substitution. The mappings in
	 * the existing substitution will also exist in this substitution. The
	 * existing substitution will not be modified when this substitution is
	 * modified.
	 * 
	 * @param original an object to be replaced
	 * @param replacement an object to replace the original object with
	 * @param previous an existing substitution whose mappings will also be
	 * included in this substitution
	 */
	public HashSubstitution(Object original, Object replacement, Substitution previous) {
		this(new Object[] { original }, new Object[] { replacement }, previous);
	}
	
	/**
	 * Constructs a new hash substitution initialized with a given mapping.
	 * 
	 * @param original an object to be replaced
	 * @param replacement an object to replace the original object with
	 */
	public HashSubstitution(Object original, Object replacement) {
		this(original, replacement, EMPTY);
	}

	@Override
	public Object apply(Object original) {
		Object replacement = previous.apply(original);
		while(mapping.containsKey(replacement)) {
			Object candidate = mapping.get(replacement);
			if(candidate == replacement)
				break;
			else
				replacement = candidate;
		}
		return replacement;
	}

	/**
	 * Maps an original object to a replacement object in this substitution's
	 * hash map and then returns this substitution. This method does not {@link
	 * #canSubstitute(Iterable, Iterable, Substitution) check whether the
	 * substitution is allowed}.
	 * 
	 * @param original the original object to be replaced
	 * @param replacement the object to replace the original with
	 * @return this substitution, after it has been modified to include the
	 * mapping
	 */
	@Override
	public HashSubstitution replace(Object original, Object replacement) {
		mapping.put(original, replacement);
		return this;
	}
}