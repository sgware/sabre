package edu.uky.cs.nil.sabre.graph;

import java.io.Serializable;

import edu.uky.cs.nil.sabre.Settings;

/**
 * An directed edge in a {@link StateGraph state graph} extends from a {@link
 * #parent parent node} to a {@link #child child node} and has a {@link
 * Edge#label label} explaining the meaning of the transition. There are two
 * main types of edges in state graphs: {@link TemporalEdge temporal edges}
 * and {@link EpistemicEdge epistemic edges}.
 * 
 * @author Stephen G. Ware
 */
public abstract class Edge implements Serializable {
	
	/**
	 * The edge group interface provides a convenient way to iterator through
	 * and modify a singly linked list of one type of edge.
	 * 
	 * @author Stephen G. Ware
	 */
	static interface Group {
		
		/**
		 * Returns the next edge in the singly linked list.
		 * 
		 * @param edge the current edge
		 * @return the next edge in the list
		 */
		public Edge getNext(Edge edge);
		
		/**
		 * Sets the next edge in the singly linked list.
		 * 
		 * @param edge the current edge
		 * @param next the edge that will be set as the next edge in the list
		 */
		public void setNext(Edge edge, Edge next);
	}
	
	/**
	 * An edge group for iterating through and modifying a singly linked list
	 * of the edges that have one node as their parent.
	 */
	static final Group OUT = new Group() {

		@Override
		public Edge getNext(Edge edge) {
			return edge.nextOut;
		}

		@Override
		public void setNext(Edge edge, Edge next) {
			edge.nextOut = next;
		}
	};
	
	/**
	 * An edge group for iterating through and modifying a singly linked list
	 * of the edges that have one node as their child.
	 */
	static final Group IN = new Group() {

		@Override
		public Edge getNext(Edge edge) {
			return edge.nextIn;
		}

		@Override
		public void setNext(Edge edge, Edge next) {
			edge.nextIn = next;
		}
	};

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The state node out from which this edge extends */
	public final StateNode parent;
	
	/** A object explaining the relationship of the parent to the child */
	public final Object label;
	
	/** A state node into which this edge goes */
	public final StateNode child;
	
	/** The next outgoing edge in a singly linked list */
	Edge nextOut = null;
	
	/** The next incoming edge in a singly linked list */
	Edge nextIn = null;
	
	Edge(StateNode parent, Object label, StateNode child) {
		this.parent = parent;
		this.label = label;
		this.child = child;
	}
	
	@Override
	public String toString() {
		return parent.id + " -(" + label + ")-> " + child.id;
	}
	
	/**
	 * Returns a permanent edge equivalent to this edge whose parent and child
	 * are both permanent state nodes (that is, the parent and child nodes have
	 * been {@link StateNode#resolve() resolved}).
	 * 
	 * @return a permanent edge equivalent to this edge
	 */
	abstract Edge resolve();
}