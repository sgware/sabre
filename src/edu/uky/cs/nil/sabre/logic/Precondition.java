package edu.uky.cs.nil.sabre.logic;

import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;

/**
 * A precondition is an {@link Atom atomic} {@link Comparison comparison}
 * which must have one of two formats: a {@link Variable variable} on the left
 * and a {@link Parameter parameter} on the right, or a {@link
 * Fluent fluent} on the left and a {@link Expression#isValued() valued
 * expression} on the right.
 * 
 * @author Stephen G. Ware
 */
public class Precondition extends Comparison implements Atom {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * Constructs a new precondition without checking its left and right sides.
	 * 
	 * @param operator the relationship between the sides
	 * @param left the left side
	 * @param right the right side
	 */
	protected Precondition(Operator operator, Expression left, Expression right) {
		super(operator, left, right);
	}
	
	/**
	 * Constructs a new precondition where the left side is a {@link Variable
	 * variable} and the right side is a {@link Parameter parameter} (generally
	 * another {@link Variable variable} or a {@link Value value}).
	 * 
	 * @param operator the relationship between the sides
	 * @param left the variable on the left side
	 * @param right the parameter on the right side
	 */
	public Precondition(Operator operator, Variable left, Parameter right) {
		this(operator, (Expression) left, (Expression) right);
	}
	
	/**
	 * Constructs a new precondition where the left side is a {@link
	 * Fluent fluent} and the right side is a {@link Expression#isValued()
	 * valued expression}.
	 * 
	 * @param operator the relationship between the sides
	 * @param fluent the fluent on the left side
	 * @param value the valued expression on the right side
	 * @throws edu.uky.cs.nil.sabre.FormatException if the right side is not
	 * valued
	 */
	public Precondition(Operator operator, Fluent fluent, Expression value) {
		this(operator, (Expression) fluent, (Expression) value);
		value.mustBeValued();
	}
	
	@Override
	public Precondition simplify() {
		return this;
	}
	
	@Override
	public Precondition prepend(Parameter character) {
		return new Precondition(operator, left.prepend(character), right.prepend(character));
	}
	
	@Override
	public Precondition negate() {
		if(right.equals(False.FALSE))
			return new Precondition(operator, left, True.TRUE);
		else if(right.equals(True.TRUE))
			return new Precondition(operator, left, False.FALSE);
		else
			return new Precondition(operator.negate(), left, right);
	}
	
	@Override
	public Disjunction<Clause<Precondition>> toPrecondition() {
		return new Disjunction<>(new Clause<>(this));
	}
	
	@Override
	public Precondition combine(Atom other) {
		Precondition result = null;
		if(other instanceof Precondition) {
			Precondition otherPrecondition = (Precondition) other;
			if(left.equals(otherPrecondition.left)) {
				result = operator.combine(this, otherPrecondition);
				if(result == null && !operator.equals(otherPrecondition.operator))
					result = otherPrecondition.operator.combine(otherPrecondition, this);
			}
		}
		return result;
	}

	@Override
	public boolean negates(Atom other) {
		boolean result = false;
		if(other instanceof Precondition) {
			Precondition otherPrecondition = (Precondition) other;
			if(left.equals(otherPrecondition.left))
				result = operator.negates(this, otherPrecondition) || otherPrecondition.operator.negates(otherPrecondition, this);
		}
		return result;
	}
}