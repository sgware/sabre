package edu.uky.cs.nil.sabre.comp;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Signature;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Conjunction;
import edu.uky.cs.nil.sabre.logic.Disjunction;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * A compiled action is a {@link edu.uky.cs.nil.sabre.logic.Logical#isGround()
 * ground} {@link Action action} which defines a {@link
 * edu.uky.cs.nil.sabre.util.Unique unique} {@link #id ID number} that
 * corresponds to the action's index in its {@link CompiledProblem#events
 * compiled problem's set of events}, whose {@link #precondition precondition}
 * and {@link #observing observation function} are in {@link
 * Expression#toPrecondition() disjunctive normal form}, and whose {@link
 * #effect effect} is {@link Expression#toEffect() a clause}.
 * 
 * @author Stephen G. Ware
 */
public class CompiledAction extends Action implements CompiledEvent {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * A unique ID number that corresponds to this action's index in {@link
	 * CompiledProblem#events its compiled problem's set of events}
	 */
	public final int id;
	
	/**
	 * The action's {@link edu.uky.cs.nil.sabre.Event#getPrecondition()
	 * precondition} in {@link Expression#toPrecondition() in disjunctive normal
	 * form}
	 */
	public final Disjunction<Clause<Precondition>> precondition;
	
	/**
	 * The action's {@link edu.uky.cs.nil.sabre.Event#getEffect() effect} as a
	 * {@link Expression#toEffect() clause}
	 */
	public final Clause<Effect> effect;
	
	/** The action's ground {@link Action#consenting} consenting characters */
	public final ImmutableSet<Character> consenting;
	
	/**
	 * The action's {@link Action#observing observing characters function} in
	 * {@link Expression#toPrecondition() in disjunctive normal form}
	 */
	public final CompiledMapping<Disjunction<Clause<Precondition>>> observing;

	/**
	 * Constructs a new compiled action.
	 * 
	 * @param id the action's unique ID number
	 * @param signature the signature
	 * @param precondition the precondition, in disjunctive normal form
	 * @param effect the effect, as a clause
	 * @param consenting the consenting characters
	 * @param observing the observing characters function, in disjunctive normal
	 * form
	 * @param comment the comment
	 */
	public CompiledAction(int id, Signature signature, Disjunction<Clause<Precondition>> precondition, Clause<Effect> effect, ImmutableSet<Character> consenting, CompiledMapping<Disjunction<Clause<Precondition>>> observing, String comment) {
		super(signature, precondition, effect, consenting.cast(Parameter.class), observing.cast(Expression.class), comment);
		this.id = id;
		this.precondition = precondition;
		this.effect = effect;
		this.consenting = consenting;
		this.observing = observing;
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
	public CompiledAction simplify() {
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
	
	@Override
	public Clause<Effect> getEffect(Fluent fluent) {
		fluent.mustBeGround();
		Clause<Effect> clause = CompiledEvent.super.getEffect(fluent);
		if(fluent.characters.size() > 0) {
			Character character = (Character) fluent.characters.get(0);
			for(Effect effect : getEffect(fluent.removeFirstCharacter())) {
				effect = effect.prepend(character);
				Expression condition = new Conjunction<>(observing.get(character), effect.condition).simplify();
				if(!condition.equals(False.FALSE))
					clause = clause.add(new Effect(condition, fluent, effect.value));
			}
		}
		return clause;
	}
}