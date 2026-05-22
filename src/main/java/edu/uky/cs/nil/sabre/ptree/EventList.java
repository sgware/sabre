package edu.uky.cs.nil.sabre.ptree;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Signature;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.comp.CompiledAction;
import edu.uky.cs.nil.sabre.comp.CompiledEvent;
import edu.uky.cs.nil.sabre.comp.CompiledFluent;
import edu.uky.cs.nil.sabre.comp.CompiledMapping;
import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Disjunction;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * A list of {@link CompiledEvent compiled events} (including dummy belief
 * update actions) and their effect for use in a {@link ProgressionTree
 * progression tree}.
 * 
 * @author Stephen G. Ware
 */
final class EventList implements Serializable {

	/**
	 * A key for storing a belief update dummy action in a {@link HashMap
	 * hashtable}.
	 * 
	 * @author Stephen G. Ware
	 */
	private final class UpdateKey implements Serializable {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1L;
		
		/** The character whose beliefs will be updated */
		public Character character;
		
		/** The fluent whose value will be set */
		public CompiledFluent fluent;
		
		/** The value to set the fluent to */
		public Value value;
		
		/**
		 * Constructs a new dummy belief update action key.
		 * 
		 * @param character the character whose beliefs will be updated
		 * @param fluent the fluent whose value will be set
		 * @param value the value to set the fluent to
		 */
		public UpdateKey(Character character, CompiledFluent fluent, Value value) {
			this.character = character;
			this.fluent = fluent;
			this.value = value;
		}
		
		@Override
		public boolean equals(Object other) {
			if(other instanceof UpdateKey) {
				UpdateKey otherKey = (UpdateKey) other;
				return character.equals(otherKey.character) && fluent.equals(otherKey.fluent) && value.equals(otherKey.value);
			}
			return false;
		}
		
		@Override
		public int hashCode() {
			return Utilities.hashCode(character, fluent, value);
		}
	}

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** A list of all events by ID, including dummy belief update events */
	private final ArrayList<CompiledEvent> events;
	
	/** A map of dummy belief update actions */
	private final HashMap<UpdateKey, CompiledEvent> updates;
	
	/** A key used to look up dummy belief update actions in {@link #updates} */
	private final UpdateKey key = new UpdateKey(null, null, null);
	
	/**
	 * A table of each event's effect for each fluent; the first array is
	 * indexed by {@link CompiledEvent#getID() the event ID}, and the second
	 * array is indexed by {@link CompiledFluent#id the fluent ID}
	 */
	private final Clause<Effect>[][] effects;
	
	/**
	 * Constructs a new event list.
	 * 
	 * @param problem the compiled problem whose events will be indexed
	 */
	@SuppressWarnings("unchecked")
	public EventList(CompiledProblem problem) {
		this.events = new ArrayList<>((int) (((double) problem.events.size()) * 1.2));
		for(CompiledEvent event : problem.events)
			this.events.add(event);
		this.updates = new HashMap<>(this.events.size() - problem.events.size());
		this.effects = new Clause[problem.events.size()][problem.fluents.size()];
		for(CompiledEvent event : problem.events)
			for(CompiledFluent fluent : problem.fluents)
				this.effects[event.getID()][fluent.id] = effect(event, fluent);
	}
	
	private static final Clause<Effect> effect(CompiledEvent event, CompiledFluent fluent) {
		Clause<Effect> effect = event.getEffect(fluent);
		return effect.equals(Clause.EMPTY) ? null : effect;
	}
	
	/**
	 * Returns the compiled event with the given {@link CompiledEvent#getID()
	 * ID number}, including any dummy belief update actions that have been
	 * {@link #getUpdate(Character, CompiledFluent, Value) created} by this list.
	 * 
	 * @param id the ID number of the event
	 * @return the event with that ID number
	 */
	public CompiledEvent getEvent(int id) {
		return events.get(id);
	}
	
	/**
	 * Returns a compiled action defined like a {@link
	 * edu.uky.cs.nil.sabre.graph.DummyAction#update(Character, edu.uky.cs.nil.sabre.Event, edu.uky.cs.nil.sabre.State)
	 * dummy belief update action}. The action always has {@link False#FALSE
	 * false} as its precondition, a single effect setting a fluent to a value,
	 * no consenting characters, and a single observing character whose beliefs
	 * are being updated by the action. If no such action has been created by
	 * this list yet, this method will create one.
	 * 
	 * @param character the character whose beliefs will be updated
	 * @param fluent the fluent whose value will change
	 * @param value the value the fluent will have
	 * @return a compiled dummy belief update action observed by the character
	 * which sets the fluent to that value
	 */
	public CompiledEvent getUpdate(Character character, CompiledFluent fluent, Value value) {
		key.character = character;
		key.fluent = fluent;
		key.value = value;
		CompiledEvent update = updates.get(key);
		if(update == null) {
			update = update(character, fluent, value);
			events.add(update);
			updates.put(new UpdateKey(character, fluent, value), update);
		}
		return update;
	}
	
	private static final ImmutableSet<Character> NO_CONSENTING_CHARACTERS = new ImmutableSet<>();
	
	@SuppressWarnings("unchecked")
	private final CompiledAction update(Character character, CompiledFluent fluent, Value value) {
		String name = "dummy_" + character.name + "_believes_" + fluent.signature.name;
		for(Parameter argument : fluent.signature.arguments)
			name += "_" + argument;
		name += "_" + value;
		Disjunction<Clause<Precondition>> precondition = False.FALSE.toPrecondition();
		Clause<Effect> effect = Clause.EMPTY.toEffect().add(new Effect(fluent, value));
		Disjunction<Clause<Precondition>>[] observing = new Disjunction[character.universe.characters.size()];
		for(int i=0; i<observing.length; i++)
			observing[i] = False.FALSE.toPrecondition();
		observing[character.id] = True.TRUE.toPrecondition();
		return new CompiledAction(events.size(), new Signature(name), precondition, effect, NO_CONSENTING_CHARACTERS, new CompiledMapping<>(observing), "");
	}
	
	/**
	 * Returns a clause of effects that a given event has on a given fluent.
	 * Because effects can be conditional, an event may have more than one
	 * effect that sets the same fluent.
	 * 
	 * @param event the event whose effects are desired
	 * @param fluent the fluent which the effect may set
	 * @return a clause of the event's effects that modify that fluent, or null
	 * the event has no effects that modify that fluent
	 */
	public Clause<Effect> getEffect(CompiledEvent event, CompiledFluent fluent) {
		if(event.getID() < effects.length)
			return effects[event.getID()][fluent.id];
		else if(event.getEffect().get(0).fluent.equals(fluent))
			return event.getEffect();
		else
			return null;
	}
}