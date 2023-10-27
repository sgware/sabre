package edu.uky.cs.nil.sabre.logic;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Utilities;

/**
 * A negation is {@link Proposition proposition} whose value is the opposite of
 * its single {@code boolean} {@link #argument argument}.
 * 
 * @author Stephen G. Ware
 */
public class Negation implements Proposition {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The argument whose value this negation will be the opposite of */
	public final Expression argument;
	
	/**
	 * Constructs a negation.
	 * 
	 * @param argument the argument whose value this negation will be the
	 * opposite of
	 * @throws edu.uky.cs.nil.sabre.FormatException if the argument is not of
	 * type {@code boolean}
	 */
	public Negation(Expression argument) {
		argument.mustBeBoolean();
		this.argument = argument;
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass()))
			return argument.equals(((Negation) other).argument);
		return false;
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), argument);
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(other.getClass()))
			return Utilities.compare(argument, ((Negation) other).argument);
		else
			return Proposition.super.compareTo(other);
	}

	@Override
	public boolean isGround() {
		return argument.isGround();
	}

	@Override
	public Negation apply(Function<Object, Object> function) {
		Expression argument = (Expression) function.apply(this.argument);
		if(argument != this.argument)
			return new Negation(argument);
		else
			return this;
	}
	
	/**
	 * This method first simplifies the negation's {@link #argument argument}.
	 * If it simplifies to {@link True#TRUE true}, this method returns {@link
	 * False#FALSE false}. If it simplifies to {@link False#FALSE false}, this
	 * method returns {@link True#TRUE true}. If the argument simplifies to a
	 * negation, that negation's argument is further simplified and returned
	 * (in other words, double negations are removed). This method does not
	 * call {@link Expression#negate()} to avoid potential infinite loops.
	 * 
	 * @return a simplified expression, or this expression if nothing could
	 * be simplified
	 */
	@Override
	public Expression simplify() {
		Negation simplified = (Negation) Proposition.super.simplify();
		if(simplified.argument.equals(True.TRUE))
			return False.FALSE;
		else if(simplified.argument.equals(False.FALSE))
			return True.TRUE;
		else if(simplified.argument instanceof Negation)
			return ((Negation) argument).argument;
		else
			return simplified;
	}
	
	/**
	 * Evaluates the negation's {@link #argument argument} and returns the
	 * opposite of its {@code boolean} value.
	 * 
	 * @return true if the argument was false, or false if the negation was
	 * true
	 */
	@Override
	public Value evaluate(State state) {
		if(argument.evaluate(state).equals(True.TRUE))
			return False.FALSE;
		else
			return True.TRUE;
	}
	
	@Override
	public Negation prepend(Parameter character) {
		return (Negation) Proposition.super.prepend(character);
	}
	
	@Override
	public Expression negate() {
		return argument;
	}
	
	@Override
	public Conditional<Disjunction<Clause<Precondition>>> toValued() {
		return argument.negate().toValued();
	}
	
	@Override
	public Disjunction<Clause<Precondition>> toPrecondition() {
		return argument.negate().toPrecondition();
	}

	@Override
	public Clause<Effect> toEffect() {
		return argument.negate().toEffect();
	}
}