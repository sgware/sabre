package edu.uky.cs.nil.sabre.logic;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Type;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * A type constraint is a {@link Proposition proposition} that is true if and
 * only if a given {@link #argument argument} is of a given {@link
 * edu.uky.cs.nil.sabre.Type type}.
 * 
 * @author Stephen G. Ware
 */
public class TypeConstraint implements Proposition {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** A logical expression which should be of a certain type */
	public final Expression argument;
	
	/** The type the argument should be */
	public final Type type;
	
	/**
	 * Constructs a new type constraint.
	 * 
	 * @param argument a logical expression which should be of the given type
	 * @param type the type the argument should be
	 */
	public TypeConstraint(Expression argument, Type type) {
		this.argument = argument;
		this.type = type;
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			TypeConstraint otherConstraint = (TypeConstraint) other;
			return argument.equals(otherConstraint.argument) && type.equals(otherConstraint.type);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), argument, type);
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(other.getClass())) {
			TypeConstraint otherConstraint = (TypeConstraint) other;
			return Utilities.compare(argument, otherConstraint.argument, type, otherConstraint.type);
		}
		else
			return Proposition.super.compareTo(other);
	}
	
	@Override
	public boolean isGround() {
		return argument.isGround();
	}

	@Override
	public TypeConstraint apply(Function<Object, Object> function) {
		Expression argument = (Expression) function.apply(this.argument);
		Type type = (Type) function.apply(this.type);
		if(argument != this.argument || type != this.type)
			return new TypeConstraint(argument, type);
		else
			return this;
	}
	
	@Override
	public Expression simplify() {
		TypeConstraint simplified = (TypeConstraint) Proposition.super.simplify();
		if(simplified.argument instanceof Value) {
			if(argument.is(type))
				return True.TRUE;
			else
				return False.FALSE;
		}
		else if(simplified.type.isBoolean()) {
			if(simplified.argument.isBoolean())
				return True.TRUE;
			else
				return False.FALSE;
		}
		else if(simplified.type.isNumber()) {
			if(simplified.argument.isNumber())
				return True.TRUE;
			else
				return False.FALSE;
		}
		else if(simplified.type.isEntity() && !argument.isEntity())
			return False.FALSE;
		else
			return simplified;
	}
	
	/**
	 * First {@link Expression#evaluate(State) evaluates} the {@link #argument
	 * argument} and then returns {@link True true} if the argument is of the
	 * specified {@link #type type}, or {@link False false} otherwise.
	 * 
	 * @param state the state in which the argument will be evaluated
	 * @return true if the argument is of the specified type, false otherwise
	 */
	@Override
	public Value evaluate(State state) {
		if(argument.evaluate(state).is(type))
			return True.TRUE;
		else
			return False.FALSE;
	}
	
	@Override
	public TypeConstraint prepend(Parameter character) {
		return (TypeConstraint) Proposition.super.prepend(character);
	}
	
	@Override
	public Expression negate() {
		return expand().negate();
	}
	
	@Override
	public Disjunction<Clause<Precondition>> toPrecondition() {
		return expand().toPrecondition();
	}
	
	/**
	 * Returns a logical expression that is logically equivalent to this type
	 * constraint but which does not include this type constraint. The
	 * expression returned is a disjunction of {@link Comparison#EQUAL_TO
	 * equal to comparisons} that compares this expression's {@link #argument
	 * argument} to {@link Type#getValues() every possible value of this type
	 * constraint's type}.
	 * 
	 * @return a proposition logically equivalent to this type constraint which
	 * does not contain this type constraint
	 */
	public Expression expand() {
		ImmutableSet<Value> values = type.getValues();
		Expression[] arguments = new Expression[values.size()];
		for(int i=0; i<arguments.length; i++)
			arguments[i] = new Comparison(Comparison.EQUAL_TO, argument, values.get(i));
		return new Disjunction<>(arguments);
	}
}