package edu.uky.cs.nil.sabre.logic;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Utilities;

/**
 * An effect is an {@link Atom atomic} {@link Assignment assignment} that must
 * have a {@link Fluent fluent} on the left side, a {@link
 * Expression#isValued() valued expression} on the right side, and a {@link
 * #condition condition} in {@link Expression#toPrecondition() in disjunctive
 * normal form} under which the assignment would occur.
 * 
 * @author Stephen G. Ware
 */
public class Effect extends Assignment implements Atom {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * The condition under which this assignment would occur (or {@link True
	 * true} if the assignment would always occur)
	 */
	public final Disjunction<Clause<Precondition>> condition;
	
	/**
	 * Constructs a new effect without checking whether the right side is
	 * {@link Expression#isValued() is valued}.
	 * 
	 * @param condition the condition in disjunctive normal form under which
	 * the assignment would occur
	 * @param fluent the fluent whose value will be assigned
	 * @param value a {@link Expression#isValued() valued expression} whose
	 * value will be assigned to the fluent
	 */
	protected Effect(Disjunction<Clause<Precondition>> condition, Fluent fluent, Expression value) {
		super(fluent, value);
		this.condition = condition;
	}
	
	/**
	 * Constructs a new effect from a given condition, fluent, and {@link
	 * Expression#isValued() valued expression}.
	 * 
	 * @param condition the condition under which the assignment would occur,
	 * which will be {@link Expression#toPrecondition() to disjunctive normal
	 * form}
	 * @param fluent the fluent whose value will be assigned
	 * @param value a {@link Expression#isValued() valued expression} whose
	 * value will be assigned to the fluent
	 * @throws edu.uky.cs.nil.sabre.FormatException if the condition is {@link
	 * False false} when {@link Expression#toPrecondition() converted to
	 * disjunctive normal form}
	 * @throws edu.uky.cs.nil.sabre.FormatException if the expression is not
	 * valued
	 */
	public Effect(Expression condition, Fluent fluent, Expression value) {
		this(toPrecondition(condition), fluent, value);
		value.mustBeValued();
	}
	
	private static final Disjunction<Clause<Precondition>> toPrecondition(Expression expression) {
		Disjunction<Clause<Precondition>> dnf = expression.toPrecondition();
		if(dnf.equals(False.FALSE))
			throw Exceptions.effectConditionCannotBeFalse();
		else
			return dnf;
	}
	
	/**
	 * Constructs a new effect from a given fluent and {@link 
	 * Expression#isValued() valued expression} whose condition will be {@link
	 * True true}.
	 * 
	 * @param fluent the fluent whose value will be assigned
	 * @param value a {@link Expression#isValued() valued expression} whose
	 * value will be assigned to the fluent
	 * @throws edu.uky.cs.nil.sabre.FormatException if the expression is not
	 * valued
	 */
	public Effect(Fluent fluent, Expression value) {
		this((Expression) True.TRUE, fluent, value);
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other) && condition.equals(((Effect) other).condition);
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), condition, fluent, value);
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(other.getClass())) {
			Effect otherEffect = (Effect) other;
			return Utilities.compare(fluent, otherEffect.fluent, value, otherEffect.value, condition, otherEffect.condition);
		}
		else
			return super.compareTo(other);
	}
	
	@Override
	public boolean isGround() {
		return super.isGround() && condition.isGround();
	}
	
	@Override
	public Expression apply(Function<Object, Object> function) {
		Expression condition = (Expression) function.apply(this.condition);
		Assignment assignment = (Assignment) super.apply(function);
		if(condition != this.condition || assignment != this)
			return new Conditional<>(condition, new Assignment(assignment.fluent, assignment.value), True.TRUE);
		else
			return this;
	}
	
	@Override
	public Effect simplify() {
		return this;
	}
	
	@Override
	public Value evaluate(State state) {
		if(condition.test(state))
			return super.evaluate(state);
		else
			return False.FALSE;
	}
	
	@Override
	public Effect prepend(Parameter character) {
		return new Effect(new Epistemic(character, condition), fluent.prepend(character), value.prepend(character).toValued().branches.get(0));
	}
	
	/**
	 * An effect is negated similarly to an {@link Assignment assignment}, but
	 * similarly to a {@link Conditional conditional}, the {@link #condition
	 * condition} remains unchanged (i.e. is not negated) when an effect is
	 * negated.
	 * 
	 * @return an expression with the opposite value of this expression
	 * @throws edu.uky.cs.nil.sabre.FormatException if this expression cannot
	 * be negated, usually because its fluent is not of type {@code boolean}
	 */
	@Override
	public Effect negate() {
		Assignment negated = super.negate();
		return new Effect(condition, negated.fluent, negated.value);
	}
	
	@Override
	public Clause<Effect> toEffect() {
		return new Clause<>(this);
	}
	
	@Override
	public Effect combine(Atom other) {
		if(other instanceof Effect) {
			Effect otherEffect = (Effect) other;
			if(fluent.equals(otherEffect.fluent) && value.equals(otherEffect.value))
				return new Effect(new Disjunction<>(condition, otherEffect.condition).toPrecondition(), fluent, value);
		}
		return null;
	}

	@Override
	public boolean negates(Atom other) {
		if(other instanceof Effect) {
			Effect otherEffect = (Effect) other;
			return fluent.equals(otherEffect.fluent) &&
				!value.equals(otherEffect.value) &&
				((condition.equals(True.TRUE) && otherEffect.condition.equals(True.TRUE)) ||
				new Conjunction<>(condition, otherEffect.condition).toPrecondition().equals(True.TRUE));
		}
		return false;
	}
}