package edu.uky.cs.nil.sabre.prog;

import java.util.Iterator;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.comp.CompiledAction;
import edu.uky.cs.nil.sabre.comp.CompiledFluent;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ArrayIterable;

/**
 * An implementation of {@link ProgressionNode a progression node} that also
 * records the {@link #getCost() cost} and {@link #getHeuristic() heuristic}
 * values of the node in a {@link ProgressionSearch progression search}.
 * 
 * @param <N> the type of object used to represent a node in {@link #space the
 * search space}
 * @author Stephen
 */
class SearchNode<N> implements ProgressionNode<N> {

	/** The search node that is the root for this node */
	public final SearchRoot<N> root;
	
	/** The progression space node this search node refers to */
	public final N node;
	
	/** The node's temporal depth */
	public final int temporal;
	
	/** The node's {@link ProgressionSearch#cost cost} value */
	private double cost;
	
	/** The node's {@link ProgressionSearch#heuristic heuristic} value */
	private double heuristic;
	
	/**
	 * This constructor should be used only by {@link SearchRoot search root
	 * nodes}.
	 * 
	 * @param start the search space root node
	 */
	SearchNode(N start) {
		this.root = (SearchRoot<N>) this;
		this.node = start;
		this.temporal = 0;
	}
	
	/**
	 * Constructs a new search node.
	 * 
	 * @param root this node's root
	 * @param node the search space node this search node will refer to
	 * @param temporal the temporal depth of this search node
	 */
	private SearchNode(SearchRoot<N> root, N node, int temporal) {
		this.root = root;
		this.node = node;
		this.temporal = temporal;
	}
	
	/**
	 * Constructs a new search node from a given parent node and the action
	 * that will be taken to reach this node's state.
	 * 
	 * @param parent the parent node of the node being constructed, representing
	 * the state in which the action will be taken
	 * @param action the action to be taken
	 */
	private SearchNode(SearchNode<N> parent, CompiledAction action) {
		this(
			parent.root,
			parent.getSpace().getChild(parent.getNode(), action),
			parent.getTemporalDepth() + 1
		);
	}
	
	@Override
	public String toString() {
		String string = "[Search Node: " + node;
		string += "; trunk=" + getTrunk();
		string += "; character=" + getCharacter();
		string += "; root=" + getRoot();
		string += "; offset=" + getTemporalOffset();
		string += "; action=" + getAction();
		string += "; temporal=" + getTemporalDepth();
		string += "; epistemic=" + getEpistemicDepth();
		string += "; cost=" + getCost();
		string += "; heuristic=" + getHeuristic();
		return string + "]";
	}
	
	@Override
	public Value getValue(Fluent fluent) {
		return getSpace().getValue(getNode(), (CompiledFluent) fluent);
	}

	@Override
	public ProgressionSearch getSearch() {
		return root.search;
	}

	@Override
	public Character getCharacter() {
		return root.character;
	}

	@Override
	public N getTrunk() {
		return root.trunk;
	}

	@Override
	public N getRoot() {
		return root.node;
	}

	@Override
	public N getNode() {
		return node;
	}

	@Override
	public int getTemporalOffset() {
		return root.offset;
	}

	@Override
	public int getTemporalDepth() {
		return temporal;
	}

	@Override
	public int getEpistemicDepth() {
		return root.epistemic;
	}
	
	/**
	 * Returns the cost value of this node, which should be {@link
	 * #setCost(double) set} to {@link ProgressionSearch#cost the value of the
	 * search's cost function} for this node. If {@link #setCost(double)} has
	 * not yet been called, this method returns 0.
	 * 
	 * @return the node's cost value, or 0 if this node's cost has not yet been
	 * set
	 */
	public double getCost() {
		return cost;
	}
	
	/**
	 * Sets the {@link #getCost() cost value} for this node.
	 * 
	 * @param value the node's new cost value
	 */
	void setCost(double value) {
		cost = value;
	}
	
	/**
	 * Returns the heuristic value of this node, which should be {@link
	 * #setHeuristic(double) set} to {@link ProgressionSearch#heuristic the
	 * value of the search's heuristic function} for this node. If {@link
	 * #setHeuristic(double)} has not yet been called, this method returns 0.
	 * 
	 * @return the node's heuristic value, or 0 if this node's heuristic value
	 * has not yet been set
	 */
	public double getHeuristic() {
		return heuristic;
	}
	
	/**
	 * Sets the {@link #getHeuristic() heuristic value} for this node.
	 * 
	 * @param value the node's new heuristic value
	 */
	void setHeuristic(double value) {
		heuristic = value;
	}
	
	/**
	 * Returns the {@link ProgressionSpace#getParents(Object) parents} of this
	 * search node's {@link #getNode() search space node} as search nodes.
	 * 
	 * @return the node's parents
	 */
	public Iterable<SearchNode<N>> getParents() {
		if(getTemporalDepth() > 0)
			return new ArrayIterable<>(parents(this, getSpace().getParents(getNode()).iterator(), 0));
		else
			return new ArrayIterable<>();
	}
	
	@SuppressWarnings("unchecked")
	private final SearchNode<N>[] parents(SearchNode<N> child, Iterator<N> parents, int count) {
		if(parents.hasNext()) {
			N parent = parents.next();
			SearchNode<N>[] array = parents(child, parents, count + 1);
			array[count] = new SearchNode<>(child.root, parent, child.getTemporalDepth() - 1);
			return array;
		}
		else
			return new SearchNode[count];
	}
	
	/**
	 * Returns a {@link ProgressionSpace#getChild(Object, CompiledAction) child}
	 * of this search node's {@link #getNode() search space node} as a
	 * search node.
	 * 
	 * @param action the action to be taken in the state represented by this
	 * node
	 * @return a search node representing the state after taking the action
	 */
	public SearchNode<N> getChild(CompiledAction action) {
		return new SearchNode<>(this, action);
	}
	
	/**
	 * Returns a {@link ProgressionSpace#getBranch(Object, Character) branch} of
	 * this this search node's {@link #getNode() search space node} as a search
	 * node.
	 * 
	 * @param character the character the branch is meant to explain this node's
	 * action for
	 * @return a search node representing the branch created to explain this
	 * node for the given character, or null if the branch does not exist
	 */
	public SearchRoot<N> getBranch(Character character) {
		N branch = getSpace().getBranch(getNode(), character);
		if(branch == null)
			return null;
		else
			return new SearchRoot<>(this, character, branch);
	}
}