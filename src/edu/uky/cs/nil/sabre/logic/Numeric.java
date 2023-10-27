package edu.uky.cs.nil.sabre.logic;

import edu.uky.cs.nil.sabre.Type;

/**
 * A numeric expression is a {@link Expression logical expression} of type
 * {@code number}. This interface implements some functionality that all
 * numeric expressions share, but numeric expressions do not need to implement
 * this interface.
 * 
 * @author Stephen G. Ware
 */
public interface Numeric extends Expression {
	
	@Override
	public default boolean isBoolean() {
		return false;
	}
	
	@Override
	public default boolean isNumber() {
		return true;
	}
	
	@Override
	public default boolean isEntity() {
		return false;
	}
	
	@Override
	public default boolean isCharacter() {
		return false;
	}
	
	@Override
	public default boolean is(Type type) {
		return type.isNumber();
	}
}