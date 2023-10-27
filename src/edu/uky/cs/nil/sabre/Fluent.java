package edu.uky.cs.nil.sabre;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Comparison;
import edu.uky.cs.nil.sabre.logic.Conditional;
import edu.uky.cs.nil.sabre.logic.Disjunction;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.Logical;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Unknown;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ImmutableArray;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * A fluent is a property of a {@link State state} which can be assigned a
 * value and whose value may change as a result of an {@link Event event}. A
 * fluent is identified by its {@link #signature signature}. The values that
 * can legally be assigned to a fluent are determined by the fluent's {@link
 * #type type}. An character's belief about a fluent is represented as a
 * different fluent. When a fluent defines one or more {@link #characters
 * characters}, it means the fluent represents what the first character believes
 * the second character believes (etc.) about the fluent.
 * <p>
 * For example, say there are two characters defined, {@code A} and {@code B}.
 * The fluent {@code alive(A)} represents whether or not A is alive. Its type is
 * {@code boolean}, meaning it can be {@code true} or {@code false}. If the
 * the fluent's list of {@link #characters characters} is {@code {A, B}}, the
 * fluent represents whether character A believes that character B believes that
 * A is alive.
 * 
 * @author Stephen G. Ware
 */
public class Fluent implements Expression, Signed {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * An ordered list of {@link Character characters} or {@link Parameter
	 * parameters} of type {@code character} specifying whose beliefs this
	 * fluent represents
	 */
	public final ImmutableArray<Parameter> characters;
	
	/** The name and arguments that identify the fluent */
	public final Signature signature;
	
	/** Identifies which values may be assigned to this fluent */
	public final Type type;
	
	/** The comment associated with the fluent from the problem definition */
	public final String comment;
	
	/**
	 * Constructs a new fluent with the given identifiers.
	 * 
	 * @param characters the characters whose beliefs this fluent represents
	 * @param signature the name and arguments that identify the fluent
	 * @param type the type of values the fluent can have
	 * @param comment the comment
	 */
	public Fluent(ImmutableArray<Parameter> characters, Signature signature, Type type, String comment) {
		this.characters = characters;
		this.signature = signature;
		this.type = type;
		this.comment = comment;
	}
	
	/**
	 * Constructs a fluent with no {@link #characters characters}.
	 * 
	 * @param signature the name and arguments that identify the fluent
	 * @param type the type of values the fluent can have
	 * @param comment the comment
	 */
	public Fluent(Signature signature, Type type, String comment) {
		this(new ImmutableArray<>(), signature, type, comment);
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			Fluent otherFluent = (Fluent) other;
			return characters.equals(otherFluent.characters) && signature.equals(otherFluent.signature) && type.equals(otherFluent.type);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), characters, signature, type);
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(other.getClass())) {
			Fluent otherFluent = (Fluent) other;
			int comparison = Utilities.compare(characters, otherFluent.characters);
			if(comparison == 0)
				comparison = Utilities.compare(signature, otherFluent.signature, type, otherFluent.type);
			return comparison;
		}
		else
			return Signed.super.compareTo(other);
	}
	
	@Override
	public boolean isGround() {
		for(int i=0; i<characters.size(); i++)
			if(!characters.get(i).isGround())
				return false;
		return signature.isGround();
	}

	@Override
	public Fluent apply(Function<Object, Object> function) {
		ImmutableArray<Parameter> characters = this.characters.apply(function);
		Signature signature = (Signature) function.apply(this.signature);
		Type type = (Type) function.apply(this.type);
		if(characters != this.characters || signature != this.signature || type != this.type)
			return new Fluent(characters, signature, type, comment);
		else
			return this;
	}
	
	@Override
	public boolean isBoolean() {
		return type.isBoolean();
	}
	
	@Override
	public boolean isNumber() {
		return type.isNumber();
	}
	
	@Override
	public boolean isEntity() {
		return type.isEntity();
	}
	
	@Override
	public boolean isCharacter() {
		return type.isCharacter();
	}

	@Override
	public boolean is(Type type) {
		return this.type.is(type);
	}
	
	@Override
	public Fluent simplify() {
		return this;
	}
	
	@Override
	public boolean isValued() {
		return isNumber();
	}

	@Override
	public Value evaluate(State state) {
		mustBeGround();
		return state.getValue(this);
	}
	
	@Override
	public Fluent prepend(Parameter character) {
		Parameter[] characters = new Parameter[this.characters.size() + 1];
		characters[0] = character;
		for(int i=0; i<this.characters.size(); i++)
			characters[i + 1] = this.characters.get(i);
		return new Fluent(new ImmutableArray<>(characters), signature, type, comment);
	}
	
	@Override
	public Expression negate() {
		if(isBoolean())
			return new Comparison(Comparison.EQUAL_TO, this, False.FALSE);
		else
			return Expression.super.negate();
	}
	
	@Override
	public Conditional<Disjunction<Clause<Precondition>>> toValued() {
		if(isBoolean())
			return new Conditional<>(this, True.TRUE, False.FALSE).toValued();
		else if(isNumber())
			return new Conditional<>(this);
		else {
			ImmutableSet<Value> values = type.getValues();
			Expression[] conditions = new Expression[values.size()];
			Expression[] branches = new Expression[values.size() + 1];
			for(int i=0; i<conditions.length; i++) {
				conditions[i] = new Comparison(Comparison.EQUAL_TO, this, values.get(i));
				branches[i] = values.get(i);
			}
			branches[branches.length - 1] = Unknown.UNKNOWN;
			return new Conditional<>(conditions, branches).toValued();
		}
	}
	
	@Override
	public Disjunction<Clause<Precondition>> toPrecondition() {
		if(isBoolean())
			return new Comparison(Comparison.EQUAL_TO, this, True.TRUE).toPrecondition();
		else
			return Expression.super.toPrecondition();
	}

	@Override
	public Clause<Effect> toEffect() {
		if(isBoolean())
			return new Effect(this, True.TRUE).toEffect();
		else
			return Expression.super.toEffect();
	}

	@Override
	public Signature getSignature() {
		return signature;
	}
	
	/**
	 * Tests whether the given fluent is the same as this fluent, except that
	 * its list of {@link #characters characters} is the same as or a shorter
	 * version of this fluent's list of characters. For example, the fluent
	 * {@code believes(A, believes(B, alive(A)))} contains itself, and it
	 * contains {@code believes(B, alive(A))}, and it contains {@code alive(A)}.
	 * 
	 * @param fluent the fluent to test
	 * @return true if the given fluent is the same as this fluent but with the
	 * same or shortened list of characters, false otherwise
	 */
	public boolean contains(Fluent fluent) {
		if(characters.size() == fluent.characters.size())
			return equals(fluent);
		else if(characters.size() > fluent.characters.size())
			return removeFirstCharacter().contains(fluent);
		else
			return false;
	}
	
	/**
	 * Returns a new fluent where the first (leftmost) character has been
	 * removed from {@link #characters}. If there are no characters, this method
	 * returns the same fluent.
	 * 
	 * @return a fluent with the first characters removed
	 */
	public Fluent removeFirstCharacter() {
		if(characters.size() == 0)
			return this;
		else {
			Parameter[] characters = new Parameter[this.characters.size() - 1];
			for(int i=1; i<this.characters.size(); i++)
				characters[i - 1] = this.characters.get(i);
			return new Fluent(new ImmutableArray<>(characters), signature, type, comment);
		}
	}
	
	/**
	 * Returns a new fluent where the last (rightmost) character has been
	 * removed from {@link #characters}. If there are no characters, this method
	 * returns the same fluent. 
	 * 
	 * @return a fluent with the last character removed
	 */
	public Fluent removeLastCharacter() {
		if(characters.size() == 0)
			return this;
		else if(characters.size() == 1)
			return removeFirstCharacter();
		else
			return removeFirstCharacter().removeLastCharacter().prepend(characters.get(0));
	}
}