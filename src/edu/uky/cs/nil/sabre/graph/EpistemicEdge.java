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
		nextOut = parent.epistemicOut;
		parent.epistemicOut = this;
		nextIn = child.epistemicIn;
		child.epistemicIn = this;
	}

	@Override
	EpistemicEdge resolve() {
		StateNode parent = this.parent.resolve();	
		StateNode child = this.child.resolve();
		if(parent != this.parent || child != this.child) {
			this.parent.removeEpistemicChild(this);
			this.child.removeEpistemicParent(this);
			EpistemicEdge existing = parent.getEpistemicChild(label);
			if(existing != null && existing.child == child)
				return existing;
			else if(existing != null) {
				existing.parent.removeEpistemicChild(existing);
				existing.child.removeEpistemicParent(existing);
			}
			return new EpistemicEdge(parent, label, child);
		}
		else
			return this;
	}
}