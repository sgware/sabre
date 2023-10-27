package edu.uky.cs.nil.sabre.logic;

import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Type;

/**
 * A proposition is a {@link Expression logical expression} of type {@code
 * boolean}. This interface implements some functionality that all Boolean
 * expressions share, but Boolean expressions do not need to implement this
 * interface.
 * 
 * @author Stephen G. Ware
 */
public interface Proposition extends Expression {
	
	@Override
	public default boolean isBoolean() {
		return true;
	}
	
	@Override
	public default boolean isNumber() {
		return false;
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
		return type.isBoolean();
	}
	
	@Override
	public default Conditional<Disjunction<Clause<Precondition>>> toValued() {
		return new Conditional<>(toPrecondition(), True.TRUE, False.FALSE);
	}
	
	/**
	 * Tests whether this proposition {@link Expression#evaluate(State)
	 * evaluates} to {@link True true} in the given {@link State state}.
	 * 
	 * @param state the state in which the proposition will be evaluated
	 * @return true if the proposition evaluates to {@link True true}, false
	 * otherwise
	 */
	public default boolean test(State state) {
		return evaluate(state).equals(True.TRUE);
	}
}