package edu.uky.cs.nil.sabre;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.logic.Logical;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ImmutableSet;
import edu.uky.cs.nil.sabre.util.Unique;

/**
 * An entity represents a discrete object in a {@link Problem problem}.
 * <p>
 * Entities are unique symbols defined in a {@link Universe universe}, which
 * means entity objects are only the same if they are from the same universe.
 * Entities with the same ID number and name are different entities if they
 * come from different universes. This means entities can be compared using
 * {@code ==}. The ID number of an entity corresponds with its index in {@link
 * Universe#entities}.
 * <p>
 * Entities should not be constructed directly. They should be defined by a
 * {@link UniverseBuilder}.
 * 
 * @author Stephen G. Ware
 */
public class Entity implements Unique, Value {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The universe in which this entity is defined */
	public final Universe universe;
	
	/** The unique sequential ID number of this entity in its universe */
	public final int id;
	
	/** The unique name of this entity in this universe */
	public final String name;
	
	/** The direct parent types of this entity */
	public final ImmutableSet<Type> types;
	
	/** The comment associated with this entity from the problem definition */
	public final String comment;
	
	/**
	 * Constructs an entity in a given universe with the given identifiers.
	 * 
	 * @param universe the universe in which this entity will be defined
	 * @param id the ID number
	 * @param name the name
	 * @param types the set of types for this entity
	 * @param comment the comment
	 */
	Entity(Universe universe, int id, String name, ImmutableSet<Type> types, String comment) {
		this.universe = universe;
		this.id = id;
		this.name = name;
		this.types = types;
		this.comment = comment;
	}
	
	/**
	 * Used by {@link UniverseConstructor} to create an entity which is a copy
	 * of an existing entity, with modifications specified by the universe
	 * constructor.
	 * 
	 * @param original the entity to be copied
	 * @param constructor the universe constructor
	 */
	Entity(Entity original, UniverseConstructor constructor) {
		constructor.put(original, this);
		this.universe = (Universe) constructor.apply(original.universe);
		this.id = original.id;
		this.name = original.name;
		this.types = original.types.apply(constructor);
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
		if(other instanceof Entity)
			return id - ((Entity) other).id;
		else
			return Value.super.compareTo(other);
	}
	
	@Override
	public Entity apply(Function<Object, Object> function) {
		return this;
	}
	
	@Override
	public boolean isBoolean() {
		return false;
	}
	
	@Override
	public boolean isNumber() {
		return false;
	}
	
	@Override
	public boolean isEntity() {
		return true;
	}
	
	@Override
	public boolean isCharacter() {
		return is(universe.types.get(Settings.CHARACTER_TYPE_ID));
	}

	@Override
	public boolean is(Type type) {
		return universe.is(this, type);
	}
	
	@Override
	public Entity simplify() {
		return this;
	}
	
	@Override
	public Entity evaluate(State state) {
		return this;
	}
	
	@Override
	public Entity prepend(Parameter character) {
		return this;
	}
}