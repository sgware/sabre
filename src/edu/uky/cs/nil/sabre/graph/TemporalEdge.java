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
 * temporal edge with a null label may lead to a new child state, even though
 * none of the parent node's values will have changed. When a temporal edge
 * with a null label leads back to the node itself, it indicates that no
 * triggers apply in this state or any of the believes states that are
 * accessible from this node.
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
		// Initially, nodes only track out edges. In edges are only tracked once
		// both the parent and child are known to be permanent nodes.
		parent.addOutEdge(this);
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
	 * in. A branch only exists when this action's label is a non-dummy action.
	 * When the given character is null (representing the author), this method
	 * returns this edge itself.
	 * <p>
	 * A branch is useful when reasoning about whether an action is explained.
	 * To be explained, an action must be explained for each of its consenting
	 * characters. For an action to be explained for a consenting character,
	 * that character must believe the action can lead to an increase in its
	 * utility. This can be checked by finding the action's branch for that
	 * character and testing whether the branch's {@link Edge#child child} state
	 * ({@link StateNode#getAfterTriggers() after triggers}) has a higher
	 * utility for the character or can lead to a state where utility is higher.
	 * 
	 * @param character a character, or null for the author
	 * @return returns the corresponding temporal edge in this graph whose
	 * {@link Edge#parent} is the character's {@link
	 * StateNode#getBeliefs(Character) beliefs} and whose {@link #label} is the
	 * same action, or null if a branch is not defined or does not yet exist
	 * for this edge
	 */
	public TemporalEdge getBranch(Character character) {
		if(label instanceof Action && !isDummyAction())
			return parent.getBeliefs(character).getTemporalChild(label);
		else
			return null;
	}
	
	@Override
	void clean() {
		// If the child node has been replaced, delete this edge and create a
		// new one pointing to the replacement child.
		if(child.intern() != child) {
			parent.removeOutEdge(this);
			new TemporalEdge(parent, label, child.intern());
		}
		// If the parent node has been replaced, copy this edge in the
		// replacement parent (unless it already has an edge for this label).
		else if(parent.intern() != parent) {
			StateNode parent = this.parent.intern();
			if(parent.getTemporalChild(label) == null)
				new TemporalEdge(parent, label, child);
		}
		// If this edge goes from a permanent node to a permanent node, register
		// the edge in the child node's list of in edges.
		else
			child.addInEdge(this);
	}
}