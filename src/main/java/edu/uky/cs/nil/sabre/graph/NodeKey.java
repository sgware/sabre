package edu.uky.cs.nil.sabre.graph;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Utilities;

/**
 * A node key is a wrapper around a {@link StateNode state node} such that the
 * {@link #equals(Object)} method will return true if and only if two node keys
 * represent equivalent nodes, and {@link #hashCode()} will return the same
 * value if calculated for equivalent nodes.
 */
final class NodeKey {

	/**
	 * The epistemic depth to which {@link #hashCode()} will go when hashing
	 * beliefs about beliefs, etc.
	 */
	public static final int HASH_CODE_DEPTH = 2;
	
	/** The state node with which this key is associated */
	public final StateNode node;
	
	/** The hash code for this node */
	public final int hashCode;
	
	/**
	 * Constructs a new node key for a given node.
	 * 
	 * @param node the node which which this key is associated
	 */
	public NodeKey(StateNode node) {
		this.node = node;
		this.hashCode = hashCode(node, HASH_CODE_DEPTH);
	}

	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			node.graph.pairs.clear();
			return equals(node, ((NodeKey) other).node, node.graph.pairs);
		}
		return false;
	}
	
	private static final boolean equals(StateNode n1, StateNode n2, PairList pairs) {
		if(n1 == n2)
			return true;
		else if(pairs.contains(n1, n2))
			return true;
		else if(n1.values != n2.values)
			return false;
		else {
			pairs.add(n1, n2);
			for(Character character : n1.graph.characters)
				if(!equals(n1.getBeliefs(character), n2.getBeliefs(character), pairs))
					return false;
			return true;
		}
	}
	
	@Override
	public int hashCode() {
		return hashCode;
	}
	
	private static final int hashCode(StateNode node, int depth) {
		int hashCode = node.values.hashCode();
		if(depth > 0)
			for(Character character : node.graph.characters)
				hashCode = Utilities.hashCode(hashCode, hashCode(node.getBeliefs(character), depth - 1));
		return hashCode;
	}
}