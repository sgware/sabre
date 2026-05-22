package edu.uky.cs.nil.sabre.hg;

import java.io.Serializable;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.logic.Logical;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * The parent class of all {@link HeuristicGraph heuristic graph} nodes.
 * 
 * @author Stephen G. Ware
 */
public abstract class Node implements Serializable {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;

	/** The heuristic graph this node belong to */
	public HeuristicGraph graph;
	
	/** The logical formula this node represents in the graph */
	public Logical label;
	
	/**
	 * The next node in the graph's linked list of nodes that need to be reset
	 */
	Node nextToReset = null;
	
	/**
	 * Constructs a new node that belongs to a given graph and represents a
	 * given logical formula.
	 * 
	 * @param graph the graph this node belongs to
	 * @param label the logical formula this node represents
	 */
	protected Node(HeuristicGraph graph, Logical label) {
		this.graph = graph;
		this.label = label;
		graph.nodes.put(label, this);
	}
	
	@Override
	public int hashCode() {
		return label.hashCode();
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	/**
	 * This method is called when some other node in the graph which is
	 * important to this node takes on a new value. This method has a
	 * variety of uses depending on the type of node, but it is usually called
	 * when this node might need to be updated based on an update to another
	 * related node.
	 * 
	 * @param node the node which took on a new value
	 * @param value the new value taken on by the node
	 * @param cost the cost of that node taking on that value
	 */
	protected void notify(Node node, Value value, double cost) {
		// This method does nothing by default.
	}
	
	/**
	 * Adds this node to the the graph's list of nodes that need to be {@link
	 * #reset() reset} when {@link HeuristicGraph#reset() the graph's reset}
	 * method is called. This method should be called any time a node changes
	 * its state from its default state.
	 * 
	 * @return true if this is the first time this node has been marked as
	 * needing to be reset, or false otherwise
	 */
	protected boolean markForReset() {
		if(nextToReset == null) {
			nextToReset = graph.firstToReset;
			graph.firstToReset = this;
			return true;
		}
		else
			return false;
	}
	
	/**
	 * Returns this node to its default state.
	 */
	protected void reset() {
		nextToReset = null;
	}
}