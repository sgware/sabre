package edu.uky.cs.nil.sabre.graph;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A temporary node is a placeholder {@link StateNode state node} created while
 * adding new nodes and edges to a {@link StateGraph state graph} which may be
 * a duplicate of a permanent node but whose duplicate status cannot be
 * determined until the process of adding nodes and edges is complete.
 * Temporary nodes are replaced by permanent nodes during {@link
 * StateGraph#clean() graph cleaning}.
 * 
 * @author Stephen G. Ware
 */
abstract class TemporaryNode extends StateNode {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** A unique ID number to assign to temporary nodes for debugging */
	private static long nextID = -1;
	
	/** The permanent node this node gets replaced with */
	private StateNode replacement = null;

	/**
	 * Constructs a new temporary node and registers it for later cleaning.
	 * 
	 * @param graph the graph to which this node belongs
	 * @param values the values of the node's fluents
	 */
	TemporaryNode(StateGraph graph, Value[] values) {
		super(graph, nextID--, values);
		graph.newNodes.add(this);
	}
	
	/**
	 * This methods runs during {@link StateGraph#clean() graph cleaning} to
	 * prepare the node to detect whether it is a duplicate.
	 */
	void clean() {
		// The values array must be interned for all nodes before
		// NodeKey can be used to detect duplicate nodes.
		values = graph.intern(values);
	}
	
	@Override
	StateNode intern() {
		if(replacement == null)
			replacement = graph.intern(this);
		return replacement;
	}
}