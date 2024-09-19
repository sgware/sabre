package edu.uky.cs.nil.sabre.graph;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Event;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Trigger;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * An event node is a {@link TemporaryNode temporary node} that is created to be
 * the child of a {@link TemporalEdge temporal edge}. The values of its fluents
 * and epistemic edges are created on demand when {@link #getValue(Fluent)} and
 * {@link #getBeliefs(Character)} methods are called respectively to enable
 * recursive computations without creating infinite loops.
 * 
 * @author Stephen G. Ware
 */
class EventNode extends TemporaryNode {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The node representing the state in which the event is happening */
	public final StateNode parent;
	
	/** The event that leads to the state represented by this node */
	public final Event event;
	
	/** The effect of the event */
	private final Clause<Effect> effect;
	
	/**
	 * Constructs a new event node from a given parent node and event.
	 * 
	 * @param parent the node representing the state in which the event happens
	 * @param event the event that leads to the state represente dby this node
	 */
	EventNode(StateNode parent, Event event) {
		super(parent.graph, event == null ? parent.values : parent.values.clone());
		this.parent = parent;
		this.event = event;
		if(event == null)
			this.effect = null;
		else {
			// Initially, all values are the same as in the parent, but here we
			// set the values that this event might change to null; they will be
			// calculated on demand by getValue(Fluent).
			this.effect = event.getEffect().toEffect();
			for(Effect effect : this.effect) {
				Integer index = graph.index.apply(effect.fluent);
				if(index != null)
					values[index] = null;
			}
		}
	}
	
	@Override
	public Value getValue(Fluent fluent) {
		if(fluent.characters.size() == 0) {
			Integer index = graph.index.apply(fluent);
			if(index == null)
				return super.getValue(fluent);
			// If the value of this fluent has not yet been calculated, calculate it.
			if(values[index] == null) {
				// Check if the event sets this fluent's value.
				for(Effect effect : this.effect)
					if(effect.fluent.equals(fluent) && effect.condition.test(parent))
						values[index] = effect.value.evaluate(parent);
				// If the event did not set this fluent, get the value from the parent.
				if(values[index] == null)
					values[index] = parent.getValue(fluent);
			}
			return values[index];
		}
		else
			return super.getValue(fluent);
	}
	
	@Override
	public StateNode getBeliefs(Character character) {
		if(character == null)
			return this;
		// Check if an epistemic edge already exists.
		EpistemicEdge epistemic = getEpistemicChild(character);
		StateNode child = epistemic == null ? null : epistemic.child;
		// If an epistemic edge does not already exist, create it.
		if(child == null) {
			// First, assume the character has whatever beliefs they had in the parent.
			child = parent.getBeliefs(character);
			// If this node's event is null, follow triggers in the beliefs.
			if(event == null) {
				// If the parent epistemic edge was a loop, this should also be a loop.
				if(child == parent)
					child = this;
				// Else, follow triggers up to the first null edge.
				else
					child = followTriggers(child);
			}
			// If this node's event is not null...
			else {
				// If the character observes the event...
				if(observes(character)) {
					// If the event is a surprise, update the character's beliefs first.
					Action surprise = DummyAction.surprise(parent, event, child);
					if(surprise != null)
						child = child.getAfter(surprise);
					// Take the action in the character's beliefs.
					child = child.getAfter(event);
				}
				// If the event directly modifies the character's beliefs,
				// create and apply a dummy belief update action.
				Action update = DummyAction.update(character, event, parent);
				if(update != null)
					child = child.getAfter(update);
			}
			// Create the new epistemic edge.
			new EpistemicEdge(this, character, child);
		}
		return child;
	}
	
	/**
	 * For a given node, recursively apply all triggers, and when no more
	 * triggers apply, taken exactly one null action. This method is very
	 * similar to {@link #getAfterTriggers()}, except that it stops after
	 * exactly one null action.
	 * 
	 * @param node the node to which triggers should be applied
	 * @return the node after applying all triggers and exactly one null action
	 */
	private static final StateNode followTriggers(StateNode node) {
		for(TemporalEdge temporal : node.getTemporalChildren()) {
			if(temporal.label instanceof Trigger)
				return followTriggers(temporal.child);
			else if(temporal.label == null)
				return temporal.child;
		}
		Trigger trigger = node.graph.triggers.getAny(node);
		if(trigger == null)
			return node.getAfter(null);
		else
			return followTriggers(node.getAfter(trigger));
	}
	
	/**
	 * A character observes this node's event if the event is an action and the
	 * character meets the condition for observing it, or if the event is a
	 * trigger or the null action and the parent's epistemic edge for that
	 * character is a loop.
	 * 
	 * @param character a character
	 * @return true if the character observes this node's event, false otherwise
	 */
	private final boolean observes(Character character) {
		if(parent.getBeliefs(character) == this)
			return false;
		else if(event instanceof Action)
			return ((Action) event).observing.get(character).evaluate(parent).equals(True.TRUE);
		else
			return parent.getBeliefs(character) == parent;
	}
	
	@Override
	void clean() {
		// Make sure that all fluent values have been calculated
		// before interning the values array.
		for(Fluent fluent : graph.fluents)
			getValue(fluent);
		super.clean();
	}
}