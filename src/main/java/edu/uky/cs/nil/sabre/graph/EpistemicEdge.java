package edu.uky.cs.nil.sabre.graph;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Settings;

/**
 * An epistemic edge is an {@link Edge edge} whose parent is some world state
 * and whose child represents the beliefs of the edge's {@link Character
 * character}. In other words, when the world is in the parent state, the edge's
 * character believes the world is in the child state. The edge's {@link #label
 * label} is the character.
 * 
 * @author Stephen G. Ware
 */
public class EpistemicEdge extends Edge {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * The character who believes the world is in the child state when the state
	 * is actually the parent state
	 */
	public final Character label;

	EpistemicEdge(StateNode parent, Character label, StateNode child) {
		super(parent, label, child);
		this.label = label;
		// Initially, nodes only track out edges. In edges are only tracked once
		// both the parent and child are known to be permanent nodes.
		parent.addOutEdge(this);
	}
	
	@Override
	void clean() {
		// If the child node has been replaced, delete this edge and create a
		// new one pointing to the replacement child.
		if(child.intern() != child) {
			parent.removeOutEdge(this);
			new EpistemicEdge(parent, label, child.intern());
		}
		// If this edge goes from a permanent node to a permanent node, register
		// the edge in the child node's list of in edges.
		else if(parent.intern() == parent)
			child.addInEdge(this);
	}
}