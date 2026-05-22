package edu.uky.cs.nil.sabre.graph;

import java.util.ArrayList;

/**
 * A list of unordered pairs of state nodes used when {@link
 * NodeKey#equals(Object) detecting} if two nodes represent the same state
 * during {@link StateGraph#clean() graph cleaning}.
 */
final class PairList {

	private final ArrayList<StateNode> list = new ArrayList<>();
	
	/**
	 * Returns true if and only if the list contains an unordered pair of nodes.
	 * 
	 * @param n1 a node
	 * @param n2 another node
	 * @return true if the list contains this pair of nodes, false otherwise
	 */
	public boolean contains(StateNode n1, StateNode n2) {
		for(int i=0; i<list.size(); i+=2)
			if((list.get(i).equals(n1) && list.get(i + 1).equals(n2)) || (list.get(i).equals(n2) && list.get(i + 1).equals(n1)))
				return true;
		return false;
	}
	
	/**
	 * Adds an unordered pair of nodes to the list.
	 * 
	 * @param n1 a node
	 * @param n2 another node
	 */
	public void add(StateNode n1, StateNode n2) {
		list.add(n1);
		list.add(n2);
	}
	
	/**
	 * Reset the list to empty.
	 */
	public void clear() {
		list.clear();
	}
}