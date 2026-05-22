package edu.uky.cs.nil.sabre.comp;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Signature;
import edu.uky.cs.nil.sabre.Trigger;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Disjunction;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.Precondition;

/**
 * A compiled trigger is a {@link edu.uky.cs.nil.sabre.logic.Logical#isGround()
 * ground} {@link Trigger trigger} which defines a {@link
 * edu.uky.cs.nil.sabre.util.Unique unique} {@link #id ID number} that
 * corresponds to the trigger's index in its {@link CompiledProblem#events
 * compiled problem's set of events}, whose {@link #precondition precondition}
 * is in {@link edu.uky.cs.nil.sabre.logic.Expression#toPrecondition()
 * disjunctive normal form}, and whose {@link #effect effect} is {@link
 * edu.uky.cs.nil.sabre.logic.Expression#toEffect() a clause}.
 * 
 * @author Stephen G. Ware
 */
public class CompiledTrigger extends Trigger implements CompiledEvent {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * A unique ID number that corresponds to this trigger's index in {@link
	 * CompiledProblem#events its compiled problem's set of events}
	 */
	public final int id;
	
	/**
	 * The trigger's {@link edu.uky.cs.nil.sabre.Event#getPrecondition()
	 * precondition} in {@link
	 * edu.uky.cs.nil.sabre.logic.Expression#toPrecondition() in disjunctive
	 * normal form}
	 */
	public final Disjunction<Clause<Precondition>> precondition;
	
	/**
	 * The trigger's {@link edu.uky.cs.nil.sabre.Event#getEffect() effect} as a
	 * {@link edu.uky.cs.nil.sabre.logic.Expression#toEffect() clause}
	 */
	public final Clause<Effect> effect;

	/**
	 * Creates a new compiled trigger.
	 * 
	 * @param id the trigger's unique ID number
	 * @param signature the signature
	 * @param precondition the precondition, in disjunctive normal form
	 * @param effect the effect, as a clause
	 * @param comment the comment
	 */
	public CompiledTrigger(int id, Signature signature, Disjunction<Clause<Precondition>> precondition, Clause<Effect> effect, String comment) {
		super(signature, precondition, effect, comment);
		this.id = id;
		this.precondition = precondition;
		this.effect = effect;
	}
	
	@Override
	public boolean equals(Object other) {
		return this == other;
	}
	
	@Override
	public int hashCode() {
		return id;
	}
	
	@Override
	public boolean isGround() {
		return true;
	}
	
	@Override
	public CompiledTrigger simplify() {
		return this;
	}
	
	@Override
	public Disjunction<Clause<Precondition>> getPrecondition() {
		return precondition;
	}
	
	@Override
	public Clause<Effect> getEffect() {
		return effect;
	}
	
	@Override
	public int getID() {
		return id;
	}
}