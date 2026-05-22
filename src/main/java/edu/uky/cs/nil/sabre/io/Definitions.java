package edu.uky.cs.nil.sabre.io;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.Utilities;

/**
 * A set of objects which have been defined and which may be referenced while
 * {@link Builder building} an object from a {@link ParseTree parse tree}.
 * Objects can be referenced by name, by type, or both.
 * 
 * @author Stephen G. Ware
 */
public final class Definitions implements Cloneable {

	private final class Definition {
		
		public final Object object;
		public final String name;
		public final Definition next;
		
		private Definition(Object object, String name, Definition next) {
			this.object = object;
			this.name = name;
			this.next = next;
		}
	}
	
	private Definition definitions;
	
	private Definitions(Definition definitions) {
		this.definitions = definitions;
	}
	
	/**
	 * Constructs an empty set of defined objects.
	 */
	public Definitions() {
		this(null);
	}
	
	@Override
	public Definitions clone() {
		return new Definitions(definitions);
	}
	
	/**
	 * Returns the most recently defined object with a given name and of a
	 * given type if one exists.
	 * 
	 * @param <T> the type of the desired object
	 * @param name the name of the desired object
	 * @param type the Java class of the desired object
	 * @return the most recently defined object with that name and type, or
	 * null if no such object is defined
	 */
	public <T> T get(String name, Class<T> type) {
		Definition definitions = this.definitions;
		while(definitions != null) {
			if((name == null || Utilities.equals(name, definitions.name)) && type.isAssignableFrom(definitions.object.getClass()))
				return type.cast(definitions.object);
			definitions = definitions.next;
		}
		return null;
	}
	
	/**
	 * Returns the most recently defined object with a given name if one
	 * exists.
	 * 
	 * @param name the name of the desired object
	 * @return the most recently defined object with that name, or null if no
	 * such object is defined
	 */
	public Object get(String name) {
		return get(name, Object.class);
	}
	
	/**
	 * Returns the most recently defined object of a given type if one exists.
	 * 
	 * @param <T> the type of the desired object
	 * @param type the Java class of the desired object
	 * @return the most recently defined object of this type, or null if no
	 * such object is defined
	 */
	public <T> T get(Class<T> type) {
		return get(null, type);
	}
	
	/**
	 * Returns the most recently defined object with a given name and of a
	 * given type or throws an exception if one does not exist.
	 * 
	 * @param <T> the type of the desired object
	 * @param name the name of the desired object
	 * @param type the Java class of the desired object
	 * @return the most recently defined object with that name and type
	 * @throws edu.uky.cs.nil.sabre.FormatException if no such object is
	 * defined
	 */
	public <T> T require(String name, Class<T> type) {
		T result = get(name, type);
		if(result == null)
			throw Exceptions.notDefined(type.getSimpleName(), name);
		else
			return result;
	}
	
	/**
	 * Returns the most recently defined object with a given name or throws an
	 * exception if one does not exist.
	 * 
	 * @param name the name of the desired object
	 * @return the most recently defined object with that name
	 * @throws edu.uky.cs.nil.sabre.FormatException if no such object is
	 * defined
	 */
	public Object require(String name) {
		return require(name, Object.class);
	}
	
	/**
	 * Returns the most recently defined object of a given type or throws an
	 * exception if one does not exist.
	 * 
	 * @param <T> the type of the desired object
	 * @param type the Java class of the desired object
	 * @return the most recently defined object of this type
	 * @throws edu.uky.cs.nil.sabre.FormatException if no such object is
	 * defined
	 */
	public <T> T require(Class<T> type) {
		return require(null, type);
	}
	
	/**
	 * Defines a new object with a given name.
	 * 
	 * @param name the name of the object
	 * @param object the object
	 */
	public void add(String name, Object object) {
		definitions = new Definition(object, name, definitions);
	}
	
	/**
	 * Defines a new object, using the object's {@link
	 * java.lang.Object#toString() toString()} method for its name.
	 * 
	 * @param object the object
	 */
	public void add(Object object) {
		add(object.toString(), object);
	}
}