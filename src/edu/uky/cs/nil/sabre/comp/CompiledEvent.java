package edu.uky.cs.nil.sabre.comp;

import edu.uky.cs.nil.sabre.Event;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Disjunction;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.util.Unique;

/**
 * A compiled event is a {@link edu.uky.cs.nil.sabre.logic.Logical#isGround()
 * ground} {@link Event event} which defines a {@link Unique unique} {@link
 * #getID() ID number} that corresponds to the event's index in its {@link
 * CompiledProblem#events compiled problem's set of events}, whose {@link
 * #getPrecondition() precondition} is in {@link
 * edu.uky.cs.nil.sabre.logic.Expression#toPrecondition() disjunctive normal
 * form}, and whose {@link #getEffect() effect} is {@link
 * edu.uky.cs.nil.sabre.logic.Expression#toEffect() a clause}.
 * 
 * @author Stephen G. Ware
 */
public interface CompiledEvent extends Event, Unique {

	@Override
	public default boolean isGround() {
		return true;
	}
	
	@Override
	public default CompiledEvent simplify() {
		return this;
	}
	
	@Override
	public Disjunction<Clause<Precondition>> getPrecondition();
	
	@Override
	public Clause<Effect> getEffect();
	
	/**
	 * Returns the event's unique ID number, which should correspond to this
	 * event's index in its {@link CompiledProblem#events compiled problem's
	 * set of events}.
	 * 
	 * @return the event's unique ID number
	 */
	public int getID();
	
	/**
	 * Returns a {@link Clause clause} composed of every {@link Effect effect}
	 * in {@link #getEffect() this event's effect} that has the given fluent
	 * {@link edu.uky.cs.nil.sabre.logic.Assignment#fluent as its fluent}. The
	 * returned clause may be empty if this event does not affect the given
	 * fluent, or it may have several atoms if this event has several
	 * conditional effects which might affect the given fluent.
	 * 
	 * @param fluent the fluent which this event may affect
	 * @return a clause of all ways this event could affect the given fluent
	 */
	public default Clause<Effect> getEffect(Fluent fluent) {
		Clause<Effect> clause = Clause.EMPTY.toEffect();
		for(Effect effect : getEffect())
			if(effect.fluent.equals(fluent))
				clause = clause.add(effect);
		return clause;
	}
}