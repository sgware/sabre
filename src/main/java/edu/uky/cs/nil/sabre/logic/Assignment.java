package edu.uky.cs.nil.sabre.logic;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.util.ImmutableArray;

/**
 * An assignment specifies that the {@link #fluent fluent} on its left side
 * should now have the value of the {@link Expression logical expression} on
 * its right side. Assignments are typically used to specify the
 * {@link edu.uky.cs.nil.sabre.Problem#initial initial state of a planning
 * problem} and the {@link edu.uky.cs.nil.sabre.Event#getEffect() effects of an
 * event}. Assignments are technically {@link Proposition propositions} but
 * generally are not used as such because they specify change not a meaningful
 * relationship between values.
 * 
 * @author Stephen G. Ware
 */
public class Assignment implements Proposition {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The fluent whose value will be set */
	public final Fluent fluent;
	
	/** The logical expression whose value will be assigned to the fluent */
	public final Expression value;
	
	/**
	 * Constructs a new assignment with the given {@link
	 * edu.uky.cs.nil.sabre.Fluent fluent} and {@link Expression value
	 * expression}.
	 * 
	 * @param fluent the fluent whose value will be set
	 * @param value the expression whose value will be assigned to the fluent
	 * and whose type must match {@link Fluent#type fluent's type}
	 * @throws edu.uky.cs.nil.sabre.FormatException if the value expression
	 * does not match the type of the fluent
	 */
	public Assignment(Fluent fluent, Expression value) {
		this.fluent = fluent;
		if(!value.is(fluent.type))
			throw Exceptions.cannotAssign(fluent, value);
		this.value = value;
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			Assignment otherAssignment = (Assignment) other;
			return fluent.equals(otherAssignment.fluent) && value.equals(otherAssignment.value);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), fluent, value);
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(other.getClass())) {
			Assignment otherAssignment = (Assignment) other;
			return Utilities.compare(fluent, otherAssignment.fluent, value, otherAssignment.value);
		}
		else
			return Proposition.super.compareTo(other);
	}

	@Override
	public boolean isGround() {
		return fluent.isGround() && value.isGround();
	}

	@Override
	public Expression apply(Function<Object, Object> function) {
		Fluent fluent;
		Object candidate = function.apply(this.fluent);
		if(candidate instanceof Fluent)
			fluent = (Fluent) candidate;
		else
			fluent = this.fluent.apply(function);
		Expression value = (Expression) function.apply(this.value);
		if(fluent != this.fluent || value != this.value)
			return new Assignment(fluent, value);
		else
			return this;
	}
	
	@Override
	public Assignment simplify() {
		return (Assignment) Proposition.super.simplify();
	}

	/**
	 * Returns {@link True true} if this assignment would change the given
	 * state. In other words, this method returns {@link True true} if the
	 * assignment's {@link #fluent fluent} has a different value in the given
	 * state than the assignment's {@link #value value expression}.
	 * 
	 * @param state the state in which this expression will be evaluated
	 * @return true if the assignment would change the state, false if the
	 * assignment would not change the state
	 */
	@Override
	public Value evaluate(State state) {
		if(fluent.evaluate(state).equals(value.evaluate(state)))
			return False.FALSE;
		else
			return True.TRUE;
	}
	
	/**
	 * In general, assignments cannot be negated because the result would not
	 * clearly specify what value the fluent would have and would thus be
	 * nondeterministic; however, if the assignment's {@link #fluent fluent} is
	 * Boolean, this method will return an assignment with the same fluent on
	 * the left side and the assignment's {@link #value value} {@link
	 * Expression#negate() negated} on the right.
	 * 
	 * @return an assignment to a negated value
	 * @throws edu.uky.cs.nil.sabre.FormatException if the fluent is not
	 * Boolean
	 */
	@Override
	public Assignment negate() {
		if(fluent.isBoolean())
			return new Assignment(fluent, new Negation(value));
		else
			return (Assignment) Proposition.super.negate();
	}
	
	@Override
	public Assignment prepend(Parameter character) {
		return new Assignment(fluent.prepend(character), new Epistemic(character, value));
	}
	
	/**
	 * A {@link Expression#isValued() valued expression} is suitable to appear
	 * on the right side of a {@link Precondition precondition} or an {@link
	 * Effect effect}; thus assignments cannot be converted into valued
	 * expressions and this method throws a {@link
	 * edu.uky.cs.nil.sabre.FormatException}.
	 */
	@Override
	public Conditional<Disjunction<Clause<Precondition>>> toValued() {
		throw Exceptions.cannotConvertToValued(this);
	}

	@Override
	public Clause<Effect> toEffect() {
		Conditional<Disjunction<Clause<Precondition>>> value = this.value.toValued();
		if(value.branches.size() == 1)
			return Clause.EMPTY.toEffect().add(new Effect(fluent, value.branches.get(0)));
		else {
			Expression[] branches = new Expression[value.branches.size()];
			for(int i=0; i<branches.length; i++)
				branches[i] = new Assignment(fluent, value.branches.get(i));
			return new Conditional<>(value.conditions, new ImmutableArray<>(branches)).toEffect();
		}
	}
}
