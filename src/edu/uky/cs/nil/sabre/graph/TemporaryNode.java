package edu.uky.cs.nil.sabre.graph;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Event;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Trigger;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A temporary node is a placeholder {@link StateNode state node} created while
 * adding new nodes and edges to a {@link StateGraph state graph} which may be
 * a duplicate of a permanent node but whose duplicate status cannot be
 * determined until the process of adding nodes and edges is complete.
 * Temporary nodes are replaced by permanent nodes using the {@link #resolve()}
 * method.
 * 
 * @author Stephen G. Ware
 */
final class TemporaryNode extends StateNode {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The state before the event which lead to this state */
	final StateNode parent;
	
	/** The event which, when taken in the parent state, led to this state */
	final Event event;
	
	/** The effect of the event */
	private final Clause<Effect> effect;
	
	/** The permanent node this temporary node will be replaced with */
	private StateNode replacement = null;
	
	TemporaryNode(StateNode parent, Event event) {
		super(parent.graph, -1, event == null ? parent.values : new Value[parent.graph.fluents.size()]);
		this.parent = parent;
		this.event = event;
		if(event == null)
			this.effect = null;
		else
			this.effect = event.getEffect().toEffect();
	}
	
	@Override
	public Value getValue(Fluent fluent) {
		Value value = super.getValue(fluent);
		if(value == null) {
			value = parent.getValue(fluent);
			if(event != null) {
				for(int i=0; i<effect.size(); i++) {
					Effect e = effect.get(i);
					if(e.fluent.equals(fluent) && e.condition.evaluate(parent).equals(True.TRUE))
						value = e.value.evaluate(parent);
				}
			}
			values[graph.index.get(fluent)] = value;
		}
		return value;
	}
	
	@Override
	public StateNode getBeliefs(Character character) {
		EpistemicEdge edge = getEpistemicChild(character);
		if(edge == null) {
			StateNode child = parent.getBeliefs(character);
			if(event == null) {
				if(child == parent)
					child = this;
				else {
					if(advance(child) != child)
						child = advance(child);
					else
						child = child.afterTriggers();
				}
			}
			else {
				if(observes(character, event, parent)) {
					Action surprise = DummyAction.surprise(parent, event, child);
					if(surprise != null)
						child = child.after(surprise);
					child = child.after(event);
				}
				Action update = DummyAction.update(character, event, parent);
				if(update != null)
					child = child.after(update);
			}
			edge = getEpistemicChild(character);
			if(edge == null)
				edge = new EpistemicEdge(this, character, child);
		}
		return edge.child;
	}
	
	private static final StateNode advance(StateNode node) {
		Edge edge = node.temporalOut;
		while(edge != null) {
			if((edge.label instanceof Trigger || edge.label == null) && edge.child != node)
				return advance(edge.child);
			edge = edge.nextOut;
		}
		return node;
	}
	
	private static final boolean observes(Character character, Event event, StateNode state) {
		if(event instanceof Action)
			return ((Action) event).observing.get(character).evaluate(state).equals(True.TRUE);
		else
			return state.getBeliefs(character) == state;
	}
	
	@Override
	StateNode after(Event event) {
		if(Utilities.equals(this.event, event))
			return this;
		else
			return super.after(event);
	}
	
	@Override
	StateNode resolve() {
		if(replacement == null) {
			for(int i=0; i<graph.fluents.size(); i++)
				if(values[i] == null)
					getValue(graph.fluents.get(i));
			for(int i=0; i<graph.characters.size(); i++)
				getBeliefs(graph.characters.get(i));
			values = graph.resolve(values);
			replacement = graph.resolve(this);
			resolve(replacement.epistemicOut);
		}
		return replacement;
	}
	
	private static final void resolve(Edge edge) {
		if(edge != null) {
			resolve(edge.nextOut);
			edge.resolve();
		}
	}
}