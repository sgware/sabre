package edu.uky.cs.nil.sabre.logic;

import java.util.function.Function;

/**
 * A parameter is a {@link Value value} or a placeholder for a value, such as a
 * {@link Variable variable}. Parameters are used to define {@link
 * edu.uky.cs.nil.sabre.Fluent fluents}, {@link edu.uky.cs.nil.sabre.Event
 * events}, and other parameterized things in a {@link
 * edu.uky.cs.nil.sabre.Problem planning problem}.
 * 
 * @author Stephen G. Ware
 */
public interface Parameter extends Expression {

	@Override
	public Parameter apply(Function<Object, Object> function);
	
	@Override
	public default Parameter simplify() {
		return this;
	}
	
	@Override
	public default boolean isValued() {
		return true;
	}
	
	@Override
	public default Parameter prepend(Parameter character) {
		return this;
	}
	
	@Override
	public default Conditional<Disjunction<Clause<Precondition>>> toValued() {
		return new Conditional<>(this);
	}
}