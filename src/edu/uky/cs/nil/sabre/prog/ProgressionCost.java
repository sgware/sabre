package edu.uky.cs.nil.sabre.prog;

/**
 * A function used to measure the cost of a {@link ProgressionNode search node}
 * during a {@link ProgressionSearch progression search}. The progression node
 * includes a {@link ProgressionNode#getNode() reference to the node} in the
 * {@link ProgressionSpace search space} as well as contextual information
 * specific to the search, such as its {@link ProgressionNode#getTemporalDepth()
 * temporal} and {@link ProgressionNode#getEpistemicDepth() epistemic depth}.
 * A cost function can be used both as the {@link ProgressionSearch#cost cost}
 * and {@link ProgressionSearch#heuristic heuristic} in a progression search,
 * though not all cost functions will make sense in both roles.
 * 
 * @author Stephen G. Ware
 */
@FunctionalInterface
public interface ProgressionCost {

	/**
	 * A cost function that always returns 0.
	 */
	public static final ProgressionCost ZERO = new ProgressionCost() {
		
		@Override
		public String toString() {
			return "0";
		}
		
		@Override
		public <N> double evaluate(ProgressionNode<N> node) {
			return 0;
		}
	};
	
	/**
	 * A cost function that returns the {@link
	 * ProgressionNode#getTemporalDepth() temporal depth} of the node it is
	 * evaluation, which is equal to the number of actions that have been taken
	 * since the root state of the search.
	 */
	public static final ProgressionCost PLAN_SIZE = new ProgressionCost() {

		@Override
		public String toString() {
			return "size";
		}
		
		@Override
		public <N> double evaluate(ProgressionNode<N> node) {
			return node.getTemporalDepth();
		}
	};
	
	/**
	 * A cost function that returns the sum of a node's {@link
	 * ProgressionNode#getTemporalOffset() temporal offset} and {@link
	 * ProgressionNode#getTemporalDepth() temporal depth}, which represent the
	 * number of actions that have been taken since the root state as well as
	 * the starting temporal depth of the root state.
	 */
	public static final ProgressionCost TEMPORAL = new ProgressionCost() {

		@Override
		public String toString() {
			return "temporal";
		}
		
		@Override
		public <N> double evaluate(ProgressionNode<N> node) {
			return node.getTemporalOffset() + node.getTemporalDepth();
		}
	};
	
	/**
	 * This method is called once each time a {@link
	 * ProgressionSearch#setStart(edu.uky.cs.nil.sabre.State) progression
	 * heuristic search is initialized} with a new start state; it is used to
	 * initialize or reset this cost function before it is used for a search.
	 * 
	 * @param <N> the type of object used to represent a node in the search's
	 * {@link ProgressionSpace search space}
	 * @param root the author's root (epistemic depth 0, temporal depth 0) that
	 * is the starting point of a progression search
	 */
	public default <N> void initialize(ProgressionNode<N> root) {}
	
	/**
	 * Returns the cost of a given node based on its context in the {@link
	 * ProgressionSearch progression search} that generated it.
	 * 
	 * @param <N> the type of object used to represent a node in the search's
	 * {@link ProgressionSpace search space}
	 * @param node the search node whose cost should be evaluated
	 * @return the cost of the node
	 */
	public <N> double evaluate(ProgressionNode<N> node);
}