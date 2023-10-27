package edu.uky.cs.nil.sabre.prog;

import edu.uky.cs.nil.sabre.Character;

/**
 * A special {@link SearchNode search node} that represents the root of a
 * search to improve the utility of a specific character. Since all search nodes
 * have the same trunk, character, temporal offset, and epistemic depth as their
 * root nodes, that information is stored in the root node rather than copied
 * into each search node.
 * 
 * @param <N>
 * @author Stephen G. Ware
 */
class SearchRoot<N> extends SearchNode<N> {

	/** The search that generated this node */
	public final ProgressionSearch search;
	
	/**
	 * The character whose utility the root and all its descendants are trying
	 * to improve, or null if this node is the author root
	 */
	public final Character character;
	
	/**
	 * The trunk node for which this root is a branch (or null if this is an
	 * author node)
	 */
	public final N trunk;
	
	/**
	 * The {@link #getTemporalOffset() temporal offset} of this root node and
	 * all of its descendants
	 */
	public final int offset;
	
	/**
	 * The {@link #getEpistemicDepth() epistemic depth} of this root node and
	 * all of its descendants
	 */
	public final int epistemic;
	
	/**
	 * Constructs a new author search root node.
	 * 
	 * @param search the search that generated this node
	 * @param start the search space node representing the problem's initial
	 * state
	 */
	public SearchRoot(ProgressionSearch search, N start) {
		super(start);
		this.search = search;
		this.character = null;
		this.trunk = null;
		this.offset = 0;
		this.epistemic = 0;
	}
	
	/**
	 * Constructs a new search root node for a branch.
	 * 
	 * @param trunk the search space node for which this root is a branch
	 * @param character the character whose utility this branch is trying to
	 * improve
	 * @param branch the search space node that will be the root state
	 */
	SearchRoot(SearchNode<N> trunk, Character character, N branch) {
		super(branch);
		this.search = trunk.getSearch();
		this.character = character;
		this.trunk = trunk.getNode();
		if(trunk.getCharacter() == null)
			this.offset = 1;
		else
			this.offset = trunk.getTemporalOffset() + trunk.getTemporalDepth();
		this.epistemic = trunk.getEpistemicDepth() + 1;
	}
}