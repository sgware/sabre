package edu.uky.cs.nil.sabre.graph;

import java.util.HashMap;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Event;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Mapping;
import edu.uky.cs.nil.sabre.Signature;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.io.DefaultParser;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Comparison;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Unknown;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * Dummy actions are special {@link Action actions} which are sometimes created
 * to update a state when a traditional action occurs. Dummy actions are not
 * authored as part of a {@link edu.uky.cs.nil.sabre.Problem planning problem}
 * but are implied by certain state update operations.
 * 
 * @author Stephen G. Ware
 */
public class DummyAction {

	private static final ImmutableSet<Parameter> NO_CONSENTING = new ImmutableSet<>();
	private static final Mapping<Expression> NO_OBSERVING = new Mapping.Predicate().cast(Expression.class);
	
	/**
	 * A surprise action is a dummy action used to update an {@link Character
	 * character's} beliefs when the character {@link Action#observing observes}
	 * an {@link Action action} which they don't believe is possible. When an
	 * action occurs, and some character observes it, but that {@link 
	 * Action#precondition action's precondition} is not satisfied in that
	 * character's beliefs, the character should first update their beliefs and
	 * then apply the {@link Action#effect action's effect} to their beliefs.
	 * A surprise dummy action is used to perform that initial belief update
	 * which occurs before taking the real action in the character's beliefs.
	 * <p>
	 * The precondition of a dummy surprise action is always {@link
	 * False#FALSE false} to signal that the character was not able to foresee
	 * this outcome. The effects of a dummy surprise action are changes which
	 * need to be made to the character's beliefs before they observe the real
	 * action occur. A dummy surprise action has no {@link Action#consenting
	 * consenting} or {@link Action#observing observing} characters.
	 * <p>
	 * If the character already believes the real action is possible (in other
	 * words, no changes need to be made to their beliefs), this method returns
	 * null.
	 * <p>
	 * Because the real action's precondition could be {@link
	 * edu.uky.cs.nil.sabre.logic.Disjunction a disjunction}, there may be
	 * several different ways the character's beliefs could be changed before
	 * the real action occurs; however, modeling this would introduce epistemic
	 * uncertainty, which is not supported. The effects of a dummy surprise
	 * action are calculated like this: The method checks every {@link
	 * Clause clause} in the real action's precondition. If a clause is
	 * satisfied in the state where the real action is happening, but not
	 * satisfied in the character's beliefs, then for every {@link Precondition
	 * precondition} in the clause which is not satisfied in the character's
	 * beliefs, an effect is added to the dummy surprise action that modifies
	 * the {@link Comparison#left fluent on the left side of the precondition}.
	 * The new value of the fluent is based on the kind of precondition:
	 * <ul>
	 * <li>If the precondition's operator is {@link Comparison#EQUAL_TO the
	 * equal to operator}, the fluent will be set to the value that the {@link
	 * Comparison#right right side of the precondition} {@link
	 * Expression#evaluate(State) evaluates to} in the state where the real
	 * action is occurring.</li>
	 * <li>If the precondition has any other operator, the fluent will be set
	 * to {@link Unknown unknown}.</li>
	 * </ul>
	 * Note that, due to the second case above, a dummy surprise action does
	 * not always modify an character's beliefs to be a state where the real
	 * action can occur. For example, consider a {@code buy} action which has
	 * the precondition that the buyer's wealth is greater than or equal to the
	 * price of the thing being bought. A second character, who believes the
	 * buyer has no money, observes the buyer buy something. A dummy surprise
	 * action is needed to update that bystandar's beliefs, because they
	 * observed an action they thought could not happen. The dummy surprise
	 * action will not update the bystander's beliefs by causing them to know
	 * exactly how much money the buyer has; rather, it will set the bystander's
	 * beliefs about the buyer's wealth to unknown, because the bystander is now
	 * uncertain about how much money the buyer has.
	 * 
	 * @param state the state in which the event is occurring; the given
	 * event's precondition should be satisfied in this state
	 * @param event the event that is occurring and being observed
	 * @param beliefs the beliefs of an character who observes the event but may
	 * not believe its precondition is satisfied
	 * @return a dummy surprise action to update the observing character's
	 * beliefs before they observe the action, or null if the action's
	 * precondition is already satisfied in the character's belief state
	 */
	public static Action surprise(State state, Event event, State beliefs) {
		if(event.getPrecondition().evaluate(beliefs).equals(True.TRUE))
			return null;
		HashMap<Fluent, Value> values = new HashMap<>();
		for(Clause<Precondition> clause : event.getPrecondition().toPrecondition()) {
			if(clause.test(state)) {
				for(Precondition precondition : clause) {
					if(!precondition.test(beliefs)) {
						Fluent fluent = (Fluent) precondition.left;
						if(precondition.operator.equals(Comparison.EQUAL_TO))
							values.put(fluent, moreSpecific(precondition.right.evaluate(state), values.get(fluent)));
						else
							values.put(fluent, moreSpecific(Unknown.UNKNOWN, values.get(fluent)));
					}
				}
			}
		}
		if(values.size() == 0)
			return null;
		Clause<Effect> effect = Clause.EMPTY.toEffect();
		for(Fluent fluent : values.keySet())
			effect = effect.add(new Effect(fluent, values.get(fluent)));
		return new Action(
			signature(effect),
			False.FALSE,
			effect,
			NO_CONSENTING,
			NO_OBSERVING,
			""
		);
	}
	
	private static final Value moreSpecific(Value v1, Value v2) {
		if(v2 == null || v2.equals(Unknown.UNKNOWN))
			return v1;
		else
			return v2;
	}
	
	private static final Signature signature(Clause<Effect> effect) {
		String name = "";
		for(int i=0; i<effect.size(); i++) {
			if(i > 0)
				name += "_";
			name += effect.get(i).fluent.signature.name;
			for(Parameter argument : effect.get(i).fluent.signature.arguments)
				name += "_" + argument;
			name += "_" + effect.get(i).value;
		}
		return new Signature(name);
	}
	
	/**
	 * A belief update action is a dummy action used to modify the beliefs of
	 * a character when {@link Action#effect an action's effect} directly
	 * modifies the character's beliefs. We say that an action directly modifies
	 * a character's beliefs when it has an effect of the form "character {@code
	 * c} believes fluent {@code f} has value {@code v}." This is opposed an
	 * action indirectly modifying a character's beliefs when a character {@link
	 * Action#observing observes} an action. For every effect in the given
	 * event of the form "character {@code c} believes fluent {@code f} has
	 * value {@code v}," this method creates a dummy action which has character
	 * {@code c} as the only observing character and an effect of the form
	 * "fluent {@code f} has value {@code v}." This dummy belief update action
	 * can then be taken in the state that reflects character {@code c}'s
	 * beliefs to capture that idea that {@code f=v} and {@code c} believes
	 * {@code f=v} and {@code c} believes {@code c} believes {@code f=v}, and so
	 * on.
	 * <p>
	 * If the action has no effects which directly modify the character's
	 * beliefs, this method returns null.
	 * <p>
	 * Consider a {@code tell} action where character {@code a} tells character
	 * {@code b} that fluent {@code f} has value {@code v}. The effect of the
	 * action is that {@code b} believes {@code f=v}. When this method is called
	 * with {@code b} as the character, {@code tell} as the event, and the state
	 * before {@code tell} as the state, it will return a dummy belief update
	 * action which has {@code b} as its only observing character and {@code
	 * f=v} as its only effect. We can go to the state that reflects character
	 * {@code b}'s beliefs and take this action. In the resulting state, {@code
	 * f=v}, but also {@code b} believes {@code f=v}, and {@code b} believes
	 * {@code b} believes {@code f=v}, etc.
	 * 
	 * @param character the character for whom the dummy belief update action
	 * will be created, and the character which will be the only observing
	 * character of that dummy action
	 * @param event the event which may have effects that directly modify the
	 * character's beliefs
	 * @param state the state in which the event is occurring
	 * @return a dummy belief update action whose effects reflect the direct
	 * belief updates of the event for the character, or null if the event has
	 * no direct belief updates for that character
	 */
	public static Action update(Character character, Event event, State state) {
		Clause<Effect> effect = event.getEffect().toEffect();
		Clause<Effect> update = Clause.EMPTY.toEffect();
		for(int i=0; i<effect.size(); i++) {
			Effect e = effect.get(i);
			if(e.fluent.characters.size() > 0 && e.fluent.characters.get(0).equals(character) && e.condition.test(state))
				update = update.add(new Effect(e.fluent.removeFirstCharacter(), e.value.evaluate(state)));
		}
		if(update.equals(Clause.EMPTY))
			return null;
		return new Action(
			signature(character, update),
			False.FALSE,
			update,
			NO_CONSENTING,
			new Mapping.Predicate(character).cast(Expression.class),
			""
		);
	}
	
	private static final Signature signature(Character observing, Clause<Effect> effect) {
		return new Signature(DefaultParser.EPISTEMIC_KEYWORD + "_" + observing + signature(effect).name);
	}
}