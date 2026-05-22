package edu.uky.cs.nil.sabre.logic;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Type;

/**
 * A typed formula is a {@link Logical logical formula} that has a {@link
 * Type type}.
 * 
 * @author Stephen G. Ware
 */
public interface Typed extends Logical {
	
	/**
	 * Tests whether this formula is of type {@code boolean}.
	 * 
	 * @return true if the formula is Boolean, false otherwise
	 */
	public boolean isBoolean();
	
	/**
	 * If this formula is of type {@code boolean}, this method does nothing;
	 * if this formula is not of type {@code boolean}, {@link
	 * edu.uky.cs.nil.sabre.FormatException an exception} is thrown.
	 * 
	 * @throws edu.uky.cs.nil.sabre.FormatException if this formula is not
	 * Boolean
	 */
	public default void mustBeBoolean() {
		if(!isBoolean())
			throw Exceptions.mustBe(this, Settings.BOOLEAN_TYPE_NAME);
	}
	
	/**
	 * If this formula is of type {@code boolean}, this method throws {@link
	 * edu.uky.cs.nil.sabre.FormatException an exception}; if this formula is
	 * not of type {@code boolean}, this method does nothing.
	 * 
	 * @throws edu.uky.cs.nil.sabre.FormatException if this formula is Boolean
	 */
	public default void mustNotBeBoolean() {
		if(isBoolean())
			throw Exceptions.mustNotBe(this, Settings.BOOLEAN_TYPE_NAME);
	}
	
	/**
	 * Tests whether this formula is of type {@code number}.
	 * 
	 * @return true if the formula is numeric, false otherwise
	 */
	public boolean isNumber();
	
	/**
	 * If this formula is of type {@code number}, this method does nothing;
	 * if this formula is not of type {@code number}, {@link
	 * edu.uky.cs.nil.sabre.FormatException an exception} is thrown.
	 * 
	 * @throws edu.uky.cs.nil.sabre.FormatException if this formula is not
	 * numeric
	 */
	public default void mustBeNumber() {
		if(!isNumber())
			throw Exceptions.mustBe(this, Settings.NUMBER_TYPE_NAME);
	}
	
	/**
	 * If this formula is of type {@code number}, this method throws {@link
	 * edu.uky.cs.nil.sabre.FormatException an exception}; if this formula is
	 * not of type {@code number}, this method does nothing.
	 * 
	 * @throws edu.uky.cs.nil.sabre.FormatException if this formula is numeric
	 */
	public default void mustNotBeNumber() {
		if(isNumber())
			throw Exceptions.mustNotBe(this, Settings.NUMBER_TYPE_NAME);
	}
	
	/**
	 * Tests whether this formula is of type {@code entity}.
	 * 
	 * @return true if the formula is of type entity, false otherwise
	 */
	public boolean isEntity();
	
	/**
	 * If this formula is of type {@code entity}, this method does nothing;
	 * if this formula is not of type {@code entity}, {@link
	 * edu.uky.cs.nil.sabre.FormatException an exception} is thrown.
	 * 
	 * @throws edu.uky.cs.nil.sabre.FormatException if this formula is not of
	 * type {@code entity}
	 */
	public default void mustBeEntity() {
		if(!isEntity())
			throw Exceptions.mustBe(this, Settings.ENTITY_TYPE_NAME);
	}
	
	/**
	 * If this formula is of type {@code entity}, this method throws {@link
	 * edu.uky.cs.nil.sabre.FormatException an exception}; if this formula is
	 * not of type {@code entity}, this method does nothing.
	 * 
	 * @throws edu.uky.cs.nil.sabre.FormatException if this formula is of type
	 * {@code entity}
	 */
	public default void mustNotBeEntity() {
		if(isEntity())
			throw Exceptions.mustNotBe(this, Settings.ENTITY_TYPE_NAME);
	}
	
	/**
	 * Tests whether this formula is of type {@code character}.
	 * 
	 * @return true if the formula is of type character, false otherwise
	 */
	public boolean isCharacter();
	
	/**
	 * If this formula is of type {@code character}, this method does nothing;
	 * if this formula is not of type {@code character}, {@link
	 * edu.uky.cs.nil.sabre.FormatException an exception} is thrown.
	 * 
	 * @throws edu.uky.cs.nil.sabre.FormatException if this formula is not of
	 * type {@code character}
	 */
	public default void mustBeCharacter() {
		if(!isCharacter())
			throw Exceptions.mustBe(this, Settings.CHARACTER_TYPE_NAME);
	}
	
	/**
	 * If this formula is of type {@code character}, this method throws {@link
	 * edu.uky.cs.nil.sabre.FormatException an exception}; if this formula is
	 * not of type {@code character}, this method does nothing.
	 * 
	 * @throws edu.uky.cs.nil.sabre.FormatException if this formula is of type
	 * {@code character}
	 */
	public default void mustNotBeCharacter() {
		if(isCharacter())
			throw Exceptions.mustNotBe(this, Settings.CHARACTER_TYPE_NAME);
	}

	/**
	 * Tests whether this formula is of a given {@link Type type}.
	 * 
	 * @param type the type being tested
	 * @return true if the formula is of this type or one of its subtypes, false
	 * otherwise
	 */
	public boolean is(Type type);
	
	/**
	 * If this formula is of the given type, this method does nothing; if this
	 * formula is not of the given type, {@link
	 * edu.uky.cs.nil.sabre.FormatException an exception} is thrown.
	 * 
	 * @param type the type being tested
	 * @throws edu.uky.cs.nil.sabre.FormatException if this formula is not of
	 * the given type
	 */
	public default void mustBe(Type type) {
		if(!is(type))
			throw Exceptions.mustBe(this, type.name);
	}
	
	/**
	 * If this formula is of the given type, this method throws {@link
	 * edu.uky.cs.nil.sabre.FormatException an exception}; if this formula is
	 * not of the given type, this method does nothing.
	 * 
	 * @param type the type being tested
	 * @throws edu.uky.cs.nil.sabre.FormatException if this formula is of the
	 * given type
	 */
	public default void mustNotBe(Type type) {
		if(is(type))
			throw Exceptions.mustNotBe(this, type.name);
	}
}