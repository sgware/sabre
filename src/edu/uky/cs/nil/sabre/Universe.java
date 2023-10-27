package edu.uky.cs.nil.sabre;

import java.io.Serializable;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Unknown;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ImmutableSet;
import edu.uky.cs.nil.sabre.util.UniqueMap;

/**
 * A universe is the collection of all the things in a {@link Problem planning
 * problem}, specifically all the {@link Entity entities} and the {@link Type
 * types} of those entities. A universe provides a way to look up types and
 * entities by name and to efficiently query type relationships, like whether
 * one type is a subtype of another, whether an entity is of a type, and which
 * entities are of a type.
 * <p>
 * A universe is an immutable object; types and entities cannot be added or
 * removed. To modify a universe, use a {@link UniverseBuilder} to make a copy
 * of a universe that reflects the desired changes.
 * 
 * @author Stephen G. Ware
 */
public class Universe implements Function<Object, Object>, Serializable {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * The types defined in this universe (index corresponds to {@link Type#id})
	 */
	public final ImmutableSet<Type> types;
	
	/**
	 * The entities defined in this universe (index corresponds to {@link
	 * Entity#id}; characters are always listed first)
	 */
	public final ImmutableSet<Entity> entities;
	
	/**
	 * The characters defined in this universe (index corresponds to {@link
	 * Entity#id})
	 */
	public final ImmutableSet<Character> characters;
	
	/** Maps type names to type objects */
	private final HashMap<String, Type> typesByName;
	
	/** Maps entity names to entity objects */
	private final HashMap<String, Entity> entitiesByName;
	
	/** Lookup table for type relationships */
	private final boolean[][] typeRelationships;
	
	/** Lookup table for entity/type relationships */
	private final boolean[][] entityRelationships;
	
	/**
	 * Constructs a default "empty" universe. The default universe has four
	 * types defined: {@code boolean}, {@code number}, {@code entity}, and
	 * {@code character}. It contains no entities, though some values, such as
	 * {@link True#TRUE true} and {@link False#FALSE false} and {@link Number
	 * numbers} are always defined and not associated with any specific
	 * universe.
	 */
	public Universe() {
		Type booleanType = new Type(this, Settings.BOOLEAN_TYPE_ID, Settings.BOOLEAN_TYPE_NAME, new ImmutableSet<>(), Settings.BOOLEAN_TYPE_COMMENT);
		Type numberType = new Type(this, Settings.NUMBER_TYPE_ID, Settings.NUMBER_TYPE_NAME, new ImmutableSet<>(), Settings.NUMBER_TYPE_COMMENT);
		Type entityType = new Type(this, Settings.ENTITY_TYPE_ID, Settings.ENTITY_TYPE_NAME, new ImmutableSet<>(), Settings.ENTITY_TYPE_COMMENT);
		Type characterType = new Type(this, Settings.CHARACTER_TYPE_ID, Settings.CHARACTER_TYPE_NAME, new ImmutableSet<>(entityType), Settings.CHARACTER_TYPE_COMMENT);
		this.types = new ImmutableSet<>(booleanType, numberType, entityType, characterType);
		this.entities = new ImmutableSet<>();
		this.characters = new ImmutableSet<>();
		this.typesByName = new HashMap<>(types.size());
		this.entitiesByName = new HashMap<>(entities.size());
		this.typeRelationships = new boolean[types.size()][types.size()];
		this.entityRelationships = new boolean[entities.size()][types.size()];
		this.valuesByType = new UniqueMap<>(types.size());
		this.intersections = new UniqueMap<>(types.size());
		initialize(this);
	}
	
	/**
	 * Constructs a universe according the a {@link UniverseConstructor}.
	 * 
	 * @param original the original universe being copied
	 * @param constructor the universe constructor that specifies changes to the
	 * original
	 */
	Universe(Universe original, UniverseConstructor constructor) {
		constructor.put(original, this);
		this.types = constructor.collect(Type.class);
		this.entities = constructor.collect(Entity.class);
		this.characters = constructor.collect(Character.class);
		this.typesByName = new HashMap<>(types.size());
		this.entitiesByName = new HashMap<>(entities.size());
		this.typeRelationships = new boolean[types.size()][types.size()];
		this.entityRelationships = new boolean[entities.size()][types.size()];
		this.valuesByType = new UniqueMap<>(types.size());
		this.intersections = new UniqueMap<>(types.size());
		initialize(this);
	}
	
	private static final void initialize(Universe universe) {
		for(Type type : universe.types)
			universe.typesByName.put(type.name, type);
		for(Entity entity : universe.entities)
			universe.entitiesByName.put(entity.name, entity);
		for(Type child : universe.types)
			for(Type parent : universe.types)
				universe.typeRelationships[child.id][parent.id] = _is(child, parent);
		for(Entity entity : universe.entities)
			for(Type type : universe.types)
				universe.entityRelationships[entity.id][type.id] = _is(entity, type);
	}
	
	private static final boolean _is(Type child, Type ancestor) {
		if(child == ancestor)
			return true;
		for(Type parent : child.parents)
			if(_is(parent, ancestor))
				return true;
		return false;
	}
	
	private static final boolean _is(Entity entity, Type type) {
		for(Type child : entity.types)
			if(child.is(type))
				return true;
		return false;
	}
	
	@Override
	public String toString() {
		return "[Universe: " + types.size() + " types; " + entities.size() + " entities]";
	}

	@Override
	public Object apply(Object original) {
		if(original instanceof Type)
			return getType(((Type) original).name);
		else if(original instanceof Entity)
			return getEntity(((Entity) original).name);
		else
			return original;
	}
	
	/**
	 * Returns the type with a given name.
	 * 
	 * @param name the name of the type
	 * @return the type
	 * @throws FormatException if there is no type with this name
	 */
	public Type getType(String name) {
		Type type = typesByName.get(name);
		if(type == null)
			throw Exceptions.notDefined(Type.class.getSimpleName(), name);
		else
			return type;
	}
	
	/**
	 * Returns the entity with a given name.
	 * 
	 * @param name the name of the entity
	 * @return the entity
	 * @throws FormatException if there is no entity with this name
	 */
	public Entity getEntity(String name) {
		Entity entity = entitiesByName.get(name);
		if(entity == null)
			throw Exceptions.notDefined(Entity.class.getSimpleName(), name);
		else
			return entity;
	}
	
	/**
	 * Returns the character with a given name.
	 * 
	 * @param name the name of the character
	 * @return the character
	 * @throws FormatException if there is no character with this name
	 */
	public Character getCharacter(String name) {
		Entity entity = entitiesByName.get(name);
		if(entity == null || !(entity instanceof Character))
			throw Exceptions.notDefined(Character.class.getSimpleName(), name);
		else
			return (Character) entity;
	}
	
	/**
	 * Tests whether one type is a subtype of another. This method runs in
	 * constant time.
	 * 
	 * @param child the type which might be a subtype of the parent
	 * @param parent the type which might be a supertype of the child
	 * @return true if child is the same type as or a subtype of the parent,
	 * false otherwise
	 */
	public boolean is(Type child, Type parent) {
		return typeRelationships[child.id][parent.id];
	}
	
	/**
	 * Tests whether an entity is of a given type. This method runs in constant
	 * time.
	 * 
	 * @param entity the entity which might be of the given type
	 * @param type the type
	 * @return true if the entity is of that type, false otherwise
	 */
	public boolean is(Entity entity, Type type) {
		return entityRelationships[entity.id][type.id];
	}
	
	/**
	 * Returns the default value that a {@link Fluent fluent} of the given
	 * type will have when it has not been explicitly assigned a value. By
	 * default, boolean fluents are false, numbers are zero, and entities
	 * are {@link Unknown#UNKNOWN unknown}.
	 * 
	 * @param type the type whose default value is desired
	 * @return the default value
	 */
	public Value getDefaultValue(Type type) {
		if(type.id == Settings.BOOLEAN_TYPE_ID)
			return False.FALSE;
		else if(type.id == Settings.NUMBER_TYPE_ID)
			return Number.ZERO;
		else
			return Unknown.UNKNOWN;
	}

	/** A cache of all values of a certain type */
	private final UniqueMap<Type, ImmutableSet<Value>> valuesByType;
	
	/**
	 * Returns a {@link ImmutableSet set} of all values that are of the given
	 * type. If the type given is {@code boolean}, the list will always be
	 * \{{@link False#FALSE false}, {@link True#TRUE true}\}. If the type given
	 * is a subtype of {@code entity}, all relevant entities will be returned.
	 * If the type given is {@code number}, an exception will be thrown, since
	 * there are infinitely many values of that type.
	 * 
	 * @param type the type
	 * @return a set of values of that type
	 * @throws FormatException if the type given is {@code number}
	 */
	public ImmutableSet<Value> getValues(Type type) {
		ImmutableSet<Value> values = valuesByType.get(type);
		if(values == null) {
			if(type.id == Settings.BOOLEAN_TYPE_ID)
				values = new ImmutableSet<>(False.FALSE, True.TRUE);
			else if(type.id == Settings.NUMBER_TYPE_ID)
				throw Exceptions.infiniteValues();
			else {
				LinkedHashSet<Value> set = new LinkedHashSet<>();
				for(Entity entity : entities)
					if(entity.is(type))
						set.add(entity);
				values = new ImmutableSet<>(set);
			}
			valuesByType.put(type, values);
		}
		return values;
	}
	
	/** A cache of type intersections */
	private final UniqueMap<Type, UniqueMap<Type, ImmutableSet<Value>>> intersections;
	
	/**
	 * Returns a {@link ImmutableSet set} of values which are of both given
	 * types.
	 * 
	 * @param first the first type
	 * @param second the second type
	 * @return a set of values both both types
	 * @throws FormatException if both types given are {@code number}
	 */
	public ImmutableSet<Value> getIntersection(Type first, Type second) {
		if(first.isNumber() && second.isNumber())
			throw Exceptions.infiniteValues();
		else if(first.id < second.id)
			return getIntersection(second, first);
		else {
			UniqueMap<Type, ImmutableSet<Value>> intersections = this.intersections.get(first);
			if(intersections == null) {
				intersections = new UniqueMap<>();
				this.intersections.put(first, intersections);
			}
			ImmutableSet<Value> values = intersections.get(second);
			if(values == null) {
				values = getValues(first).intersection(getValues(second));
				intersections.put(second, values);
			}
			return values;
		}
	}
	
	/**
	 * Tests whether any values exist which are of both types.
	 * 
	 * @param first the first type
	 * @param second the second type
	 * @return true if at least one value exists which is of both types, false
	 * otherwise
	 */
	public boolean intersects(Type first, Type second) {
		if(first.isNumber())
			return second.isNumber();
		else if(second.isNumber())
			return false;
		else
			return getIntersection(first, second).size() > 0;
	}
}