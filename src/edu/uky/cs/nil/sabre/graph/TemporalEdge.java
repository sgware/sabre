package edu.uky.cs.nil.sabre.graph;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Event;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Solution;
import edu.uky.cs.nil.sabre.logic.Comparison;
import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.Value;

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
	
	/**
	 * Determines whether a {@link Solution solution} is valid for this edge. A
	 * valid solution:
	 * <ul>
	 * <li>is a non-empty plan</li>
	 * <li>whose first action matches the label of this edge</li>
	 * <li>that is possible to execute in the {@link #parent parent state}</li>
	 * <li>that raises the utility of {@link Solution#getCharacter() its
	 * character}</li>
	 * <li>that is composed of actions which are explained for all the other
	 * consenting characters who need to take them, with the possible exception
	 * of the first action (i.e. this egde's action)</li>
	 * <li>is minimal, meaning none of the actions can be left out while still
	 * achieving the same or better utility</li>
	 * </ul>
	 * Note that the first action in the solution (i.e. this edge's action) does
	 * not need to be explained for all consenting characters; it only needs to
	 * be explained for the solution's character. This is the primary difference
	 * between validity at an edge vs. {@link StateNode#isValid(Solution)
	 * validity in a state} (which requires that the first action be explained
	 * for all its consenting characters). Validity at an edge means that a
	 * solution can explain an action for its character, but may not be a fully
	 * valid solution.
	 * <p>
	 * This method may add nodes and edges to the graph.
	 * 
	 * @param solution the solution to test for this edge
	 * @return true if the solution is valid for this edge, false otherwise
	 */
	public boolean isValid(Solution<? extends Action> solution) {
		// The plan must not be null.
		if(solution == null)
			return false;
		// The plan must start with this action.
		if(!solution.get(0).equals(label))
			return false;
		// The plan must be possible.
		StateNode after = parent.getAfter(solution);
		if(after == null)
			return false;
		// The plan must improve utility.
		if(!Comparison.GREATER_THAN.test(after.getUtility(solution.getCharacter()), parent.getUtility(solution.getCharacter())))
			return false;
		// If the plan is one action, it is valid.
		if(solution.size() == 1)
			return true;
		// If the plan has more than one action, the rest of the solution must be valid.
		if(!child.getAfterTriggers().isValid(solution.next()))
			return false;
		// The plan must be minimal.
		return isMinimal(solution);
	}
	
	private final boolean isMinimal(Solution<? extends Action> solution) {
		Value goal = parent.getAfter(solution).getUtility(solution.getCharacter());
		return !StateNode.findSubsequence(parent, solution.next(), goal, true);
	}
}