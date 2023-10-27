package edu.uky.cs.nil.sabre.graph;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Event;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.False;

/**
 * A temporal edge is an {@link Edge edge} whose parent is the world state
 * before an {@link Event event} occurred and whose child is the world state
 * after the event occurred. The edge's {@link #label} is the event.
 * <p>
 * When a temporal edge's label is null, this indicates that a {@link
 * StateGraph#triggers trigger} has occurred in some other node which can be
 * reached from this node via {@link EpistemicEdge epistemic edges}. A
 * temporal edge with a null label still leads to a new child state, even
 * though none of the parent node's values will be changed.
 * 
 * @author Stephen G. Ware
 */
public class TemporalEdge extends Edge {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * The event which caused the state to change from the parent state to the
	 * child state
	 */
	public final Event label;

	TemporalEdge(StateNode parent, Event label, StateNode child) {
		super(parent, label, child);
		this.label = label;
		nextOut = parent.temporalOut;
		parent.temporalOut = this;
		nextIn = child.temporalIn;
		child.temporalIn = this;
	}

	@Override
	TemporalEdge resolve() {
		StateNode parent = this.parent.resolve();
		StateNode child = this.child.resolve();
		TemporalEdge existing = parent.getTemporalChild(label);
		if(existing != null && existing.child == child)
			return existing;
		this.parent.removeTemporalChild(this);
		this.child.removeTemporalParent(this);
		return new TemporalEdge(parent, label, child);
	}
	
	/**
	 * Tests whether this edge represents an {@link Action action} with no
	 * consenting characters (in other words, an action that only the author
	 * needs a reason to take).
	 * 
	 * @return true if {@link #label this edge's label} is an action with zero
	 * consenting characters
	 */
	public boolean isAuthorAction() {
		return label instanceof Action && ((Action) label).consenting.size() == 0;
	}
	
	/**
	 * Tests whether this edge represents a {@link
	 * edu.uky.cs.nil.sabre.graph.DummyAction dummy action}, which includes the
	 * null action and any action whose precondition is {@link False#FALSE
	 * false}.
	 * 
	 * @return true if {@link #label this edge's label} is null or an event
	 * whose precondition is false
	 */
	public boolean isDummyAction() {
		return label == null || label.getPrecondition().equals(False.FALSE);
	}
	
	/**
	 * When {@link #label this edge's label} is an {@link Action action}, this
	 * method returns the {@link TemporalEdge temporal edge} corresponding to
	 * this same action but taken from the {@link StateNode state} that some
	 * given {@link Action#consenting consenting character} believed they were
	 * in. A branch only exists when (1) this action's label is a non-dummy
	 * action and (2) the given character is one of the action's consenting
	 * characters or the given character is null, representing the author. When
	 * this edge's label is a non-dummy action and the given character is null
	 * (representing the author), this method returns this edge itself.
	 * <p>
	 * A branch is useful when reasoning about whether an action is explained.
	 * To be explained, an action must be explained for each of its consenting
	 * characters. For an action to be explained for a consenting character,
	 * that character must believe the action can lead to an increase in its
	 * utility. This can be checked by finding the action's branch for that
	 * character and testing whether the branch's {@link Edge#child child} state
	 * has a higher utility for the character or can lead to a state where
	 * utility is higher.
	 * 
	 * @param character a character, or null for the author
	 * @return returns the corresponding temporal edge in this graph whose
	 * {@link Edge#parent} is the character's {@link
	 * StateNode#getBeliefs(Character) beliefs} and whose {@link #label} is the
	 * same action, or null if a branch is not defined for this edge
	 */
	public TemporalEdge getBranch(Character character) {
		if(label instanceof Action && !isDummyAction() && (character == null || ((Action) label).consenting.contains(character)))
			return parent.getBeliefs(character).getTemporalChild(label);
		else
			return null;
	}
}