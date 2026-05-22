package edu.uky.cs.nil.sabre;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.Logical;

/**
 * A trigger is an {@link Event event} that must occur when its precondition is
 * satisfied. Triggers are commonly used for inference and updating beliefs,
 * such as a when a character should observe a property that would would not
 * otherwise observe as a result of an {@link Action action}. Because triggers
 * are events, each defines a {@link #signature signature}, {@link
 * #precondition precondition}, an {@link #effect effect}.
 * 
 * @author Stephen G. Ware
 */
public class Trigger implements Event {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** Identifies the trigger's name and arguments */
	public final Signature signature;
	
	/** A logical expression that must be true before the trigger can occur */
	public final Expression precondition;
	
	/** A logical expression that will be true after the trigger occurs */
	public final Expression effect;
	
	/** The comment associated with the trigger from the problem definition */
	public final String comment;
	
	/**
	 * Constructs a new trigger.
	 * 
	 * @param signature the trigger's signature
	 * @param precondition the precondition
	 * @param effect the effect
	 * @param comment the comment
	 */
	public Trigger(Signature signature, Expression precondition, Expression effect, String comment) {
		this.signature = signature;
		precondition.toPrecondition();
		this.precondition = precondition;
		if(effect.toEffect().equals(Clause.NULL))
			throw Exceptions.illegalEffect(effect);
		this.effect = effect;
		this.comment = comment;
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			Trigger otherTrigger = (Trigger) other;
			return signature.equals(otherTrigger.signature) && precondition.equals(otherTrigger.precondition) && effect.equals(otherTrigger.effect);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), signature, precondition, effect);
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(other.getClass())) {
			Trigger otherTrigger = (Trigger) other;
			return Utilities.compare(signature, otherTrigger.signature, precondition, otherTrigger.precondition, effect, otherTrigger.effect);
		}
		else
			return Event.super.compareTo(other);
	}
	
	@Override
	public Trigger apply(Function<Object, Object> function) {
		Signature signature = (Signature) function.apply(this.signature);
		Expression precondition = (Expression) function.apply(this.precondition);
		Expression effect = (Expression) function.apply(this.effect);
		if(signature != this.signature || precondition != this.precondition || effect != this.effect)
			return new Trigger(signature, precondition, effect, comment);
		else
			return this;
	}
	
	@Override
	public Trigger simplify() {
		return (Trigger) Event.super.simplify();
	}

	@Override
	public Signature getSignature() {
		return signature;
	}
	
	@Override
	public Expression getPrecondition() {
		return precondition;
	}
	
	@Override
	public Expression getEffect() {
		return effect;
	}
	
	@Override
	public String getComment() {
		return comment;
	}
}