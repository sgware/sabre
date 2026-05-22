package edu.uky.cs.nil.sabre;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.Logical;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * An action is an {@link Event event} that a planner can choose to take in
 * service of a goal. Because actions are events, each defines a {@link
 * #signature signature}, a {@link #precondition precondition}, and an {@link
 * #effect effect}. Actions also define a set of zero to many {@link
 * #consenting consenting characters} who must have a reason to take the action.
 * Actions also define an {@link #observing observation function} which
 * specifies under what conditions a character would see the action occur.
 * {@link Plan Plans} are made of actions.
 * <p>
 * When an action has zero consenting characters, it represents an accident or a
 * force of nature. The planner does not need to justify the action---that is,
 * the action only needs to contribute to the {@link Problem#utility author's
 * utility}. Characters cannot expect accidents, so these actions cannot be used
 * in character plans.
 * <p>
 * When an action has more than one consenting character, every character needs
 * a reason to take the action, but the reasons can be different.
 * 
 * @author Stephen G. Ware
 */
public class Action implements Event {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** Identifies the action's name and arguments */
	public final Signature signature;
	
	/** A logical expression that must be true before the action can occur */
	public final Expression precondition;
	
	/** A logical expression that will be true after the action occurs */
	public final Expression effect;
	
	/** A set of zero to many characters who need a reason to take the action */
	public final ImmutableSet<Parameter> consenting;
	
	/**
	 * A function which, for any character, returns a logical expression which
	 * is used to decide whether that character observes the action
	 */
	public final Mapping<Expression> observing;
	
	/** The comment associated with the action from the problem definition */
	public final String comment;

	/**
	 * Constructs a new action.
	 * 
	 * @param signature the action's signature
	 * @param precondition the precondition
	 * @param effect the effect
	 * @param consenting the set of consenting characters
	 * @param observing the character observation function
	 * @param comment the comment
	 */
	public Action(Signature signature, Expression precondition, Expression effect, ImmutableSet<Parameter> consenting, Mapping<Expression> observing, String comment) {
		this.signature = signature;
		precondition.toPrecondition();
		this.precondition = precondition;
		if(effect.toEffect().equals(Clause.NULL))
			throw Exceptions.illegalEffect(effect);
		this.effect = effect;
		for(Parameter character : consenting)
			character.mustBeCharacter();
		this.consenting = consenting;
		this.observing = observing;
		this.comment = comment;
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			Action otherAction = (Action) other;
			return (
				signature.equals(otherAction.signature) &&
				precondition.equals(otherAction.precondition) &&
				effect.equals(otherAction.effect) &&
				consenting.equals(otherAction.consenting) &&
				observing.equals(otherAction.observing)
			);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), signature, precondition, effect, consenting, observing);
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(other.getClass())) {
			Action otherAction = (Action) other;
			int comparison = Utilities.compare(signature, otherAction.signature, precondition, otherAction.precondition, effect, otherAction.effect);
			if(comparison == 0)
				comparison = Utilities.compare(consenting, otherAction.consenting);
			if(comparison == 0)
				comparison = Utilities.compare(observing, otherAction.observing);
			return comparison;
		}
		else
			return Event.super.compareTo(other);
	}
	
	@Override
	public boolean isGround() {
		if(Event.super.isGround())
			for(int i=0; i<consenting.size(); i++)
				if(!consenting.get(i).isGround())
					return false;
		return observing.isGround();
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public Action apply(Function<Object, Object> function) {
		Signature signature = (Signature) function.apply(this.signature);
		Expression precondition = (Expression) function.apply(this.precondition);
		Expression effect = (Expression) function.apply(this.effect);
		ImmutableSet<Parameter> consenting = this.consenting.apply(function);
		Mapping<Expression> observing = (Mapping<Expression>) function.apply(this.observing);
		if(signature != this.signature || precondition != this.precondition || effect != this.effect || consenting != this.consenting || observing != this.observing)
			return new Action(signature, precondition, effect, consenting, observing, comment);
		else
			return this;
	}
	
	@Override
	public Action simplify() {
		return (Action) Event.super.simplify();
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