package edu.uky.cs.nil.sabre.logic;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.State;

/**
 * A value is a {@link Expression logical expression} which evaluates to
 * itself. Values can be assigned to {@link edu.uky.cs.nil.sabre.Fluent
 * fluents} in {@link State states}.
 * 
 * @author Stephen G. Ware
 */
public interface Value extends Parameter {
	
	@Override
	public default boolean isGround() {
		return true;
	}

	@Override
	public default Value apply(Function<Object, Object> function) {
		return this;
	}
	
	@Override
	public default Value simplify() {
		return this;
	}
	
	@Override
	public default boolean isValued() {
		return true;
	}
	
	@Override
	public default Value evaluate(State state) {
		return this;
	}
	
	@Override
	public default Value prepend(Parameter character) {
		return this;
	}
	
	@Override
	public default Conditional<Disjunction<Clause<Precondition>>> toValued() {
		return new Conditional<>(this);
	}
}