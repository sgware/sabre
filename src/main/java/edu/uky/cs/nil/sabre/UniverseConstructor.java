package edu.uky.cs.nil.sabre;

import java.util.HashMap;
import java.util.TreeSet;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * A helper object used by a {@link UniverseBuilder universe builder} to ensure
 * all the elements of a {@link Universe universe} reference each other
 * correctly. Because the types and entities defined in a universe all have
 * final references to {@link Type#universe their universe}, and that universe
 * has final references to all its types and entities, a universe and all its
 * objects must be reconstructed every time a new elements is added. A universe
 * constructor helps with that process.
 * 
 * @author Stephen G. Ware
 */
final class UniverseConstructor implements Function<Object, Object> {
	
	private HashMap<Object, Object> mapping = new HashMap<>();
	
	/**
	 * Constructs a new universe constructor that initially contains
	 * definitions of all the types and entities in a given universe.
	 * 
	 * @param universe the universe whose definitions will be copied
	 */
	UniverseConstructor(Universe universe) {
		for(Type type : universe.types)
			mapping.put(type, type);
		for(Entity entity : universe.entities)
			mapping.put(entity, entity);
	}

	@Override
	public Object apply(Object original) {
		Object replacement;
		if(mapping.containsKey(original))
			replacement = mapping.get(original);
		else
			replacement = original;
		if(replacement instanceof Type) {
			Type type = (Type) replacement;
			if(type.universe != apply(type.universe)) {
				replacement = new Type(type, this);
				mapping.put(original, replacement);
			}
		}
		else if(replacement instanceof Entity) {
			Entity entity = (Entity) replacement;
			if(entity.universe != apply(entity.universe)) {
				if(isCharacter(entity))
					replacement = new Character(entity, this);
				else
					replacement = new Entity(entity, this);
				mapping.put(original, replacement);
			}
		}
		return replacement;
	}
	
	private static final boolean isCharacter(Entity entity) {
		for(Type type : entity.types)
			if(isCharacter(type))
				return true;
		return false;
	}
	
	private static final boolean isCharacter(Type type) {
		if(type.id == Settings.CHARACTER_TYPE_ID)
			return true;
		for(Type parent : type.parents)
			if(isCharacter(parent))
				return true;
		return false;
	}
	
	/**
	 * Maps an object from an old universe to an object in the new universe
	 * this constructor is building.
	 * 
	 * @param original the old object from the old universe
	 * @param replacement the equivalent new object in the new universe
	 */
	final void put(Object original, Object replacement) {
		mapping.put(original, replacement);
	}
	
	/**
	 * Gathers all elements in the new universe this constructor is building,
	 * sorts them according to their {@link Comparable#compareTo(Object)
	 * natural order} and puts them into a {@link ImmutableSet set}.
	 * 
	 * @param <T> the type of object to collect
	 * @param type the Java class of the type of object to collect
	 * @return a set of objects of that type from the universe this constructor
	 * is building
	 */
	@SuppressWarnings("unchecked")
	final <T> ImmutableSet<T> collect(Class<T> type) {
		TreeSet<T> set = new TreeSet<>();
		for(Object original : mapping.keySet().toArray()) {
			Object replacement = apply(original);
			if(type.isAssignableFrom(replacement.getClass()))
				set.add((T) replacement);
		}
		return new ImmutableSet<>(set);
	}
}