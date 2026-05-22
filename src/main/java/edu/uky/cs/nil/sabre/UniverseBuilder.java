package edu.uky.cs.nil.sabre;

import java.util.TreeSet;

import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * A universe builder assists in defining the {@link Type types} and {@link
 * Entity entities} of a {@link Universe universe} by enforcing requirements
 * on their definitions.
 * 
 * @author Stephen G. Ware
 */
public class UniverseBuilder {
	
	private Universe universe;
	
	/**
	 * Constructs a new universe builder that will begin with all the types and
	 * entities in the given universe and can be used to define a new universe
	 * that is an extension or modification of that universe.
	 * 
	 * @param universe the pre-defined universe
	 */
	public UniverseBuilder(Universe universe) {
		this.universe = universe;
	}
	
	/**
	 * Constructs a universe builder for a new, empty universe, which will
	 * contains only the pre-defined types that all universes contain and no
	 * entities.
	 */
	public UniverseBuilder() {
		this(new Universe());
	}
	
	/**
	 * Returns the currently defined universe.
	 * 
	 * @return the universe
	 */
	public Universe getUniverse() {
		return universe;
	}
	
	/**
	 * Defines a new type with the given name, which will have {@code entity}
	 * as its parent type, unless a type with this name already exists, in
	 * which case that type will be returned.
	 * 
	 * @param name the name of the new type
	 * @return the new (or existing) type
	 */
	public Type defineType(String name) {
		Type type = null;
		try { type = universe.getType(name); }
		catch(FormatException ex) {/* do nothing */}
		if(type == null) {
			define(new Type(universe, universe.types.size(), name, new ImmutableSet<>(universe.types.get(Settings.ENTITY_TYPE_ID)), Settings.DEFAULT_TYPE_COMMENT));
			type = universe.getType(name);
		}
		return type;
	}
	
	/**
	 * Makes one type a subtype of the other, creating the types if they do not
	 * already exist.
	 * 
	 * @param child the name of the subtype (child) type
	 * @param parent the name of the supertype (parent) type
	 * @return the modified child type
	 * @throws FormatException if either type is {@code boolean} or {@code
	 * number}, since these types cannot be subtype or supertypes of other
	 * types
	 * @throws FormatException if this could create a cycle in the type
	 * ontology
	 */
	public Type addTypeRelationship(String child, String parent) {
		Type _child = universe.getType(child);
		Type _parent = universe.getType(parent);
		if(_parent.id == Settings.BOOLEAN_TYPE_ID || _parent.id == Settings.NUMBER_TYPE_ID)
			throw Exceptions.typeMayNotBeExtended(_parent);
		else if(_child.id == Settings.BOOLEAN_TYPE_ID || _child.id == Settings.NUMBER_TYPE_ID)
			throw Exceptions.typeMayNotExtend(_child);
		else if(_child.is(_parent))
			return _child;
		else if(_parent.is(_child))
			throw Exceptions.typeCycle(_child, _parent);
		replace(_child, new Type(universe, _child.id, _child.name, add(_child.parents, _parent), _child.comment));
		sortEntities();
		return universe.getType(child);
	}
	
	/**
	 * Sets the comment associated with the given type.
	 * 
	 * @param type the name of the type
	 * @param comment the comment
	 * @return the modified type
	 */
	public Type setTypeComment(String type, String comment) {
		Type _type = universe.getType(type);
		replace(_type, new Type(_type.universe, _type.id, _type.name, _type.parents, comment));
		return universe.getType(type);
	}
	
	/**
	 * Defines a new type with the given name, which will have {@code entity}
	 * as its only type, unless an entity with this name already exists, in
	 * which case that entity will be returned.
	 * 
	 * @param name the name of the new entity
	 * @return the new (or existing) entity
	 */
	public Entity defineEntity(String name) {
		Entity entity = null;
		try { entity = universe.getEntity(name); }
		catch(FormatException ex) {/* do nothing */}
		if(entity == null) {
			define(new Entity(universe, universe.entities.size(), name, new ImmutableSet<>(universe.types.get(Settings.ENTITY_TYPE_ID)), Settings.DEFAULT_ENTITY_COMMENT));
			entity = universe.getEntity(name);
		}
		return entity;
	}
	
	/**
	 * Defines an entity to be of the given type.
	 * 
	 * @param entity the name of the entity
	 * @param type the name of the type
	 * @return the modified entity
	 * @throws FormatException if the type is {@code boolean} or {@code number}
	 */
	public Entity addEntityType(String entity, String type) {
		Entity _entity = universe.getEntity(entity);
		Type _type = universe.getType(type);
		if(_entity.is(_type))
			return _entity;
		else if(_type.isBoolean() || _type.isNumber())
			throw Exceptions.entityMayNotBeOfType(_entity, _type);
		replace(_entity, new Entity(universe, _entity.id, _entity.name, add(_entity.types, _type), _entity.comment));
		sortEntities();
		return universe.getEntity(entity);
	}
	
	/**
	 * Sets the comment associated with the given entity.
	 * 
	 * @param entity the name of the entity
	 * @param comment the comment
	 * @return the modified entity
	 */
	public Entity setEntityComment(String entity, String comment) {
		Entity _entity = universe.getEntity(entity);
		replace(_entity, new Entity(_entity.universe, _entity.id, _entity.name, _entity.types, comment));
		return universe.getEntity(entity);
	}
	
	private final void define(Object object) {
		UniverseConstructor constructor = new UniverseConstructor(universe);
		constructor.put(object, object);
		universe = new Universe(universe, constructor);
	}
	
	private final void replace(Object original, Object replacement) {
		UniverseConstructor constructor = new UniverseConstructor(universe);
		constructor.put(original, replacement);
		universe = new Universe(universe, constructor);
	}
	
	private static final ImmutableSet<Type> add(ImmutableSet<Type> types, Type type) {
		TreeSet<Type> list = new TreeSet<>();
		for(Type t : types)
			if(!type.is(t))
				list.add(t);
		list.add(type);
		return new ImmutableSet<Type>(list);
	}
	
	private final void sortEntities() {
		if(areEntitiesSorted(universe))
			return;
		UniverseConstructor constructor = new UniverseConstructor(universe);
		int id = 0;
		for(Entity entity : universe.entities)
			if(entity.isCharacter())
				constructor.put(entity, new Character(universe, id++, entity.name, entity.types, entity.comment));
		for(Entity entity : universe.entities)
			if(!entity.isCharacter())
				constructor.put(entity, new Entity(universe, id++, entity.name, entity.types, entity.comment));
		universe = new Universe(universe, constructor);
	}
	
	private static final boolean areEntitiesSorted(Universe universe) {
		for(int i=0; i<universe.entities.size()-1; i++)
			if(!universe.entities.get(i).isCharacter() && universe.entities.get(i + 1).isCharacter())
				return false;
		return true;
	}
}