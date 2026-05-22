package edu.uky.cs.nil.sabre;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.logic.Logical;
import edu.uky.cs.nil.sabre.logic.Typed;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ImmutableSet;
import edu.uky.cs.nil.sabre.util.Unique;

/**
 * Types provide a way to group values, to express relationships between
 * values, and to define which values are legal for variables and properties.
 * Multiple inheritance is allowed, meaning one type may have multiple
 * supertypes.
 * <p>
 * Types are unique symbols defined in a {@link Universe universe}, which means
 * type objects are only the same if they are from the same universe. Types
 * with the same ID number and name are different types if they come from
 * different universes. This means types can be compared using {@code ==}. The
 * ID number of a type corresponds with its index in {@link Universe#types}.
 * <p>
 * Types should not be constructed directly. They should be defined by a
 * {@link UniverseBuilder}.
 * 
 * @author Stephen G. Ware
 */
public class Type implements Unique, Typed {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The universe in which this type is defined */
	public final Universe universe;
	
	/** The unique sequential ID number of this type in its universe */
	public final int id;
	
	/** The unique name of this type in this universe */
	public final String name;
	
	/** This type's direct parent supertypes */
	public final ImmutableSet<Type> parents;
	
	/** The comment associated with this type from the problem definition */
	public final String comment;
	
	/**
	 * Constructs a type in a given universe with the given identifiers.
	 * 
	 * @param universe the universe in which this type will be defined
	 * @param id the ID
	 * @param name the name
	 * @param parents the set of parent supertypes
	 * @param comment the comment
	 */
	Type(Universe universe, int id, String name, ImmutableSet<Type> parents, String comment) {
		this.universe = universe;
		this.id = id;
		this.name = name;
		this.parents = parents;
		this.comment = comment;
	}
	
	/**
	 * Used by {@link UniverseConstructor} to create a type which is a copy of
	 * an existing type, with modifications specified by the universe
	 * constructor.
	 * 
	 * @param original the type to be copied
	 * @param constructor the universe constructor
	 */
	Type(Type original, UniverseConstructor constructor) {
		constructor.put(original, this);
		this.universe = (Universe) constructor.apply(original.universe);
		this.id = original.id;
		this.name = original.name;
		this.parents = original.parents.apply(constructor);
		this.comment = original.comment;
	}
	
	@Override
	public int hashCode() {
		return id;
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(getClass()))
			return id - ((Type) other).id;
		else
			return Typed.super.compareTo(other);
	}
	
	@Override
	public boolean isGround() {
		return true;
	}

	@Override
	public Type apply(Function<Object, Object> function) {
		return this;
	}
	
	@Override
	public boolean isBoolean() {
		return is(universe.types.get(Settings.BOOLEAN_TYPE_ID));
	}
	
	@Override
	public boolean isNumber() {
		return is(universe.types.get(Settings.NUMBER_TYPE_ID));
	}
	
	@Override
	public boolean isEntity() {
		return is(universe.types.get(Settings.ENTITY_TYPE_ID));
	}
	
	@Override
	public boolean isCharacter() {
		return is(universe.types.get(Settings.CHARACTER_TYPE_ID));
	}

	@Override
	public boolean is(Type type) {
		return universe.is(this, type);
	}
	
	/**
	 * Returns the default value that a {@link Fluent fluent} of this type
	 * will have when it has not been explicitly assigned a value.
	 * 
	 * @return the default value
	 */
	public Value getDefaultValue() {
		return universe.getDefaultValue(this);
	}
	
	/**
	 * Returns a {@link ImmutableSet set} of all values that are of this type
	 * in the type's universe. See {@link Universe#getValues(Type)}.
	 * 
	 * @return the set of all values of this type in the universe
	 */
	public ImmutableSet<Value> getValues() {
		return universe.getValues(this);
	}
	
	/**
	 * Returns a {@link ImmutableSet set} of all values that are of both this
	 * type and of the given type in the type's universe. See {@link
	 * Universe#getIntersection(Type, Type)}.
	 * 
	 * @param other the second type
	 * @return the set of all values of both types in the universe
	 */
	public ImmutableSet<Value> getIntersection(Type other) {
		return universe.getIntersection(this, other);
	}
	
	/**
	 * Tests whether there are any values of both this type and the given type
	 * defined in the type's universe. See
	 * {@link Universe#intersects(Type, Type)}.
	 * 
	 * @param other the second type
	 * @return true if at least one value of both types exists, false otherwise
	 */
	public boolean intersects(Type other) {
		return universe.intersects(this, other);
	}
}