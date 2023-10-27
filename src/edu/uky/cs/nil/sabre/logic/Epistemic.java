package edu.uky.cs.nil.sabre.logic;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Type;
import edu.uky.cs.nil.sabre.Utilities;

/**
 * An epistemic expression is a {@link Expression logical expression} about
 * what an {@link edu.uky.cs.nil.sabre.Character character} believes the value
 * of a logical expression to be.
 * 
 * @author Stephen G. Ware
 */
public class Epistemic implements Expression {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The character which holds the belief */
	public final Parameter character;
	
	/** The logical expression about which the character holds a belief */
	public final Expression argument;
	
	/**
	 * Constructs a new epistemic expression.
	 * 
	 * @param character the character which holds the belief
	 * @param argument the logical expression about which the character holds a
	 * belief
	 * @throws edu.uky.cs.nil.sabre.FormatException if the character parameter
	 * is not of type {@code character}
	 */
	public Epistemic(Parameter character, Expression argument) {
		character.mustBeCharacter();
		this.character = character;
		this.argument = argument;
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			Epistemic otherEpistemic = (Epistemic) other;
			return character.equals(otherEpistemic.character) && argument.equals(otherEpistemic.argument);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), character, argument);
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(other.getClass())) {
			Epistemic otherEpistemic = (Epistemic) other;
			return Utilities.compare(character, otherEpistemic.character, argument, otherEpistemic.argument);
		}
		else
			return Expression.super.compareTo(other);
	}
	
	@Override
	public boolean isGround() {
		return character.isGround() && argument.isGround();
	}

	@Override
	public Epistemic apply(Function<Object, Object> function) {
		Parameter character = (Parameter) function.apply(this.character);
		Expression argument = (Expression) function.apply(this.argument);
		if(character != this.character || argument != this.argument)
			return new Epistemic(character, argument);
		else
			return this;
	}

	@Override
	public boolean isBoolean() {
		return argument.isBoolean();
	}

	@Override
	public boolean isNumber() {
		return argument.isNumber();
	}

	@Override
	public boolean isEntity() {
		return argument.isEntity();
	}

	@Override
	public boolean isCharacter() {
		return argument.isCharacter();
	}

	@Override
	public boolean is(Type type) {
		return argument.is(type);
	}
	
	@Override
	public Epistemic simplify() {
		return (Epistemic) Expression.super.simplify();
	}

	/**
	 * Return the value this expression's {@link #character character} believes
	 * that this expression's {@link #argument argument} has in the given state.
	 * 
	 * @param state the state in which this expression will be evaluated
	 * @return the character's believed value of this expression's argument
	 */
	@Override
	public Value evaluate(State state) {
		return argument.prepend(character).evaluate(state);
	}
	
	@Override
	public Expression prepend(Parameter character) {
		return new Epistemic(character, argument.prepend(this.character));
	}
	
	@Override
	public Epistemic negate() {
		return new Epistemic(character, argument.negate());
	}
	
	@Override
	public Conditional<Disjunction<Clause<Precondition>>> toValued() {
		return argument.prepend(character).toValued();
	}
	
	@Override
	public Disjunction<Clause<Precondition>> toPrecondition() {
		return argument.prepend(character).toPrecondition();
	}

	@Override
	public Clause<Effect> toEffect() {
		return argument.toEffect().prepend(character);
	}
}