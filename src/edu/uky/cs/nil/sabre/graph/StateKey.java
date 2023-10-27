package edu.uky.cs.nil.sabre.graph;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Utilities;

/**
 * A state key is a utility object that serves as the key to the {@link
 * java.util.HashMap hashtable} which {@link StateGraph#states interns unique
 * state nodes in a state graph}. A state key is used to detect (via the {@link
 * Object#equals(Object)} and {@link Object#hashCode()} methods) when two
 * {@link StateNode state nodes} are equivalent so that only one copy will be
 * kept. Two state nodes are equivalent when they have the same {@link
 * StateNode#values values} for {@link StateGraph#fluents the fluents tracked
 * by the state graph} and when, for each {@link StateGraph#characters character
 * tracked by the state graph}, {@link StateNode#getEpistemicChild(Character)
 * the epistemic edges extending from both nodes} also lead to equivalent nodes.
 * In other words, two states are the same if the actual state is the same,
 * and they have the same beliefs for all characters, and the same beliefs about
 * beliefs for all characters, etc.
 * 
 * @author Stephen G. Ware
 */
final class StateKey implements Serializable {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** A state node */
	public final StateNode node;
	
	/** The {@link Object#hashCode() hash code} of the node */
	private final int hashCode;
	
	StateKey(StateNode node) {
		this.node = node;
		this.hashCode = hashCode(node);
	}
	
	private static final int hashCode(StateNode node) {
		int hashCode = Arrays.hashCode(node.values);
		for(int i=0; i<node.graph.characters.size(); i++)
			hashCode = Utilities.hashCode(hashCode, Arrays.hashCode(node.getBeliefs(node.graph.characters.get(i)).values));
		return hashCode;
	}
	
	@Override
	public boolean equals(Object other) {
		if(other instanceof StateKey) {
			node.graph.pairs.clear();
			return equals(node, ((StateKey) other).node, node.graph.pairs);
		}
		return false;
	}
	
	private static final boolean equals(StateNode n1, StateNode n2, ArrayList<StateNode> pairs) {
		if(n1 == n2)
			return true;
		else if(n1.values != n2.values)
			return false;
		for(int i=0; i<pairs.size(); i+=2)
			if((pairs.get(i) == n1 && pairs.get(i+1) == n2) || (pairs.get(i) == n2 && pairs.get(i+1) == n1))
				return true;
		pairs.add(n1);
		pairs.add(n2);
		for(int i=0; i<n1.graph.characters.size(); i++) {
			Character character = n1.graph.characters.get(i);
			if(!equals(n1.getBeliefs(character), n2.getBeliefs(character), pairs))
				return false;
		}
		return true;
	}
	
	@Override
	public int hashCode() {
		return hashCode;
	}
}