package edu.uky.cs.nil.sabre.logic;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.util.ImmutableArray;

/**
 * A logical expression is a {@link Logical logical formula} which can be
 * {@link #evaluate(State) evaluated} in a {@link edu.uky.cs.nil.sabre.State
 * state} to return a {@link Value value}.
 * 
 * @author Stephen G. Ware
 */
public interface Expression extends Typed, Simplifiable {
	
	@Override
	public Expression apply(Function<Object, Object> function);
	
	@Override
	public default Expression simplify() {
		return (Expression) Simplifiable.super.simplify();
	}
	
	/**
	 * Tests whether or not this expression is a valued expression. A valued
	 * expression is a {@link Value value} or an expression which describes a
	 * value and cannot be simplified any further without knowing the current
	 * {@link State state}. Valued expressions are the only kind of expression
	 * which can appear on the right side of an {@link Effect effect's}
	 * assignment. Valued expressions include:
	 * <ul>
	 * <li>a {@link Value value}</li>
	 * <li>a {@link Variable variable}</li>
	 * <li>a numeric {@link edu.uky.cs.nil.sabre.Fluent fluent}</li>
	 * <li>an {@link Arithmetic arithmetic expression} composed only of other
	 * valued expressions</li>
	 * </ul>
	 * This method returns false by default and should be overridden by
	 * expressions which are or can be valued expressions.
	 * 
	 * @return true if the expression describes a value and cannot be further
	 * simplified without knowing the current state, false otherwise
	 */
	public default boolean isValued() {
		return false;
	}
	
	/**
	 * If this expression is {@link #isValued() valued}, this method does
	 * nothing; if it is not valued, an {@link
	 * edu.uky.cs.nil.sabre.FormatException exception} is thrown.
	 * 
	 * @throws edu.uky.cs.nil.sabre.FormatException if the expression is not
	 * valued
	 */
	public default void mustBeValued() {
		if(!isValued())
			throw Exceptions.mustByValued(this);
	}
	
	/**
	 * Returns the {@link Value value} of this expression in a given {@link
	 * State state}.
	 * 
	 * @param state the state in which this expression will be evaluated
	 * @return the value of this expression
	 */
	public Value evaluate(State state);
	
	/**
	 * Returns an expression which represents a character's beliefs about this
	 * expression.
	 * 
	 * @param character the character whose beliefs will be represented
	 * @return an expression representing the character's beliefs about this
	 * expression
	 */
	public default Expression prepend(Parameter character) {
		return apply(object -> object instanceof Expression ? new Epistemic(character, (Expression) object) : object);
	}
	
	/**
	 * If this expression is of type {@code boolean}, this method returns an
	 * expression that will always have the opposite value; if this expression
	 * is of any other type, or cannot be negated, this method thrown an
	 * exception.
	 * 
	 * @return an expression with the opposite value of this expression
	 * @throws edu.uky.cs.nil.sabre.FormatException if this expression cannot
	 * be negated, usually because it is not of type {@code boolean}
	 */
	public default Expression negate() {
		throw Exceptions.cannotNegate(this);
	}
	
	/**
	 * Returns a {@link Conditional conditional} expression whose {@link
	 * Conditional#conditions conditions} are in {@link #toPrecondition()
	 * disjunctive normal form} and whose {@link Conditional#branches branches}
	 * are {@link #isValued() valued expressions}. This method is used to
	 * simplify expressions by converting anything which is not valued into
	 * valued expressions and moving any {@link Conditional conditionals}
	 * outside of the expression. If the expression is already valued, the
	 * conditional returned should have no conditions and a single branch
	 * that is this expression.
	 * 
	 * @return a conditional with conditions in disjunctive normal form and
	 * branches that are valued expressions
	 */
	@SuppressWarnings("unchecked")
	public default Conditional<Disjunction<Clause<Precondition>>> toValued() {
		Conditional<Disjunction<Clause<Precondition>>>[] conditional = new Conditional[1];
		Expression valued = apply(object -> {
			if(object instanceof Expression) {
				Conditional<Disjunction<Clause<Precondition>>> c = ((Expression) object).toValued();
				if(c.branches.size() == 1)
					object = c.branches.get(0);
				else {
					object = c;
					if(conditional[0] == null)
						conditional[0] = c;
				}
			}
			return object;
		});
		if(conditional[0] == null)
			return new Conditional<>(valued);
		else {
			Expression[] branches = new Expression[conditional[0].branches.size()];
			for(int i=0; i<branches.length; i++)
				branches[i] = (Expression) valued.substitute(conditional[0], conditional[0].branches.get(i));
			return new Conditional<>(conditional[0].conditions, new ImmutableArray<>(branches)).toValued();
		}
	}
	
	/**
	 * Converts an expression whose type is {@code boolean} to disjunctive
	 * normal form (DNF) suitable to appear in the precondition of an {@link
	 * edu.uky.cs.nil.sabre.Event event}. A DNF expression is a {@link
	 * Disjunction disjunction} of {@link Clause conjunctive clauses} whose
	 * {@link Atom atoms} are {@link Precondition preconditions}.
	 * 
	 * @return a disjunction of conjunctive clauses whose atoms are
	 * preconditions
	 * @throws edu.uky.cs.nil.sabre.FormatException if the expression is not
	 * {@code boolean} or cannot be converted to DNF
	 */
	public default Disjunction<Clause<Precondition>> toPrecondition() {
		throw Exceptions.cannotConvertToPrecondition(this);
	}
	
	/**
	 * Converts an expression whose type is {@code boolean} to disjunctive
	 * normal form (DNF) suitable to appear in the deterministic effect of an
	 * {@link edu.uky.cs.nil.sabre.Event event}. Because effects must be
	 * deterministic, this process must result in a {@link Disjunction
	 * disjunction} of no more than one {@link Clause conjunctive clause} whose
	 * {@link Atom atoms} are {@link Effect effects}. Because there will only
	 * be at most one clause, that clause is returned instead of a disjunction.
	 * If no clauses would be returned (in other words, if {@link False false}
	 * would be returned), the {@link Clause#NULL null clause} is returned.
	 * 
	 * @return a conjunctive clause whose atoms are effects
	 * @throws edu.uky.cs.nil.sabre.FormatException if the expression is not
	 * {@code boolean}, is not deterministic, or cannot be converted to DNF
	 */
	public default Clause<Effect> toEffect() {
		throw Exceptions.cannotConvertToEffect(this);
	}
}