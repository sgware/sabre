package edu.uky.cs.nil.sabre;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.Simplifiable;

/**
 * An event changes the state of the world; there are two main types of events,
 * {@link Action actions} and {@link Trigger triggers}. An event defines a
 * {@link Signed#getSignature() signature}, a {@link #getPrecondition()
 * precondition}, and an {@link #getEffect() effect}.
 * 
 * @author Stephen G. Ware
 */
public interface Event extends Simplifiable, Signed {
	
	@Override
	public default boolean isGround() {
		return getSignature().isGround() && getPrecondition().isGround() && getEffect().isGround();
	}
	
	@Override
	public Event apply(Function<Object, Object> function);
	
	@Override
	public default Event simplify() {
		return (Event) Simplifiable.super.simplify();
	}

	/**
	 * Returns a logical expression which must evaluate to {@link
	 * edu.uky.cs.nil.sabre.logic.True#TRUE true} before the event can occur.
	 * 
	 * @return a logical expression
	 */
	public Expression getPrecondition();
	
	/**
	 * Returns a logical expression which becomes true as a result of the
	 * event occurring.
	 * 
	 * @return a logical expression
	 */
	public Expression getEffect();
	
	/**
	 * Returns the comment associated with the action from the problem
	 * definition.
	 * 
	 * @return the comment
	 */
	public String getComment();
}