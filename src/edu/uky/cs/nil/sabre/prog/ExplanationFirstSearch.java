package edu.uky.cs.nil.sabre.prog;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.comp.CompiledAction;
import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.etree.EventTree;
import edu.uky.cs.nil.sabre.search.Progress;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * Explanation-first search is a type of {@link ProgressionSearch progression
 * search} that requires every action in a plan to be {@link
 * ProgressionSpace#isExplained(Object) explained} before it will consider
 * adding additional actions to the plan. Specifically, every action in a plan
 * must be explained for every {@link CompiledAction#consenting consenting
 * character} except {@link ProgressionNode#getCharacter() the character
 * associated with the plan} before more actions can be added to a plan.
 * <p>
 * When an action is added to a plan expanded, branches are created for each of
 * the action's consenting characters to explain why they would take that
 * action. The search {@link #compare(SearchNode, SearchNode) prioritizes}
 * branches with deeper {@link ProgressionNode#getEpistemicDepth() epistemic
 * depth}, meaning that a node's branches will be visited before the node
 * itself. By the time the trunk is visited, if it has not been explained for
 * all characters except {@link ProgressionNode#getCharacter() the node's
 * character}, then some attempt to explain the action for one of its characters
 * must have failed, and the plan can be removed from consideration.
 * 
 * @author Stephen G. Ware
 */
public class ExplanationFirstSearch extends ProgressionSearch {
	
	/** The temporal depth of the deepest author node visited so far */
	private int depth = 0;

	/**
	 * Constructs a new explanation-first progression search.
	 * 
	 * @param problem the compiled problem to be solved
	 * @param cost a function to measure the cost of a plan so far
	 * @param heuristic a function to estimate the cost of making a plan into a
	 * solution
	 * @param actions an event tree of all actions that can be taken during
	 * search
	 * @param space the model of states that will be searched
	 * @param searchLimit the max number of nodes that can be visited
	 * @param spaceLimit the max number of nodes that can be generated
	 * @param timeLimit the max number of milliseconds a search can take
	 * @param authorTemporalLimit the max number of actions in the main plan
	 * @param characterTemporalLimit the max number of actions a character can
	 * imagine when trying to explain its actions
	 * @param epistemicLimit the max depth in a character's theory of mind to
	 * search
	 * @param explanationPruning whether the search should stop exploring a
	 * branch once its root has been explained
	 */
	public ExplanationFirstSearch(
		CompiledProblem problem,
		ProgressionCost cost,
		ProgressionCost heuristic,
		EventTree<CompiledAction> actions,
		ProgressionSpace<?> space,
		long searchLimit,
		long spaceLimit,
		long timeLimit,
		int authorTemporalLimit,
		int characterTemporalLimit,
		int epistemicLimit,
		boolean explanationPruning
	) {
		super(
			problem,
			cost,
			heuristic,
			actions,
			space,
			searchLimit,
			spaceLimit,
			timeLimit,
			authorTemporalLimit,
			characterTemporalLimit,
			epistemicLimit,
			explanationPruning
		);
	}

	@Override
	public String toString() {
		return "[" + toString("Explanation-First Heurstic Progression Search") + "]";
	}
	
	@Override
	public void setStart(State state) {
		super.setStart(state);
		depth = 0;
	}
	
	@Override
	protected void setStatus(Status status, Progress<CompiledAction> progress) {
		status.setMessage("Explanation-first search for \"" + problem.name + "\": depth %d; %d visited; %d generated; %d in queue", 0, 0, 0, 0);
	}
	
	@Override
	protected void updateStatus(Status status, Progress<CompiledAction> progress) {
		status.update(0, depth);
		status.update(1, progress.getVisited());
		status.update(2, progress.getGenerated());
		status.update(3, queue.size());
	}
	
	/**
	 * {@inheritDoc}
	 * <p>
	 * During explanation-first search, any node which has not been explained
	 * for all of its {@link CompiledAction#consenting consenting characters}
	 * except for {@link ProgressionNode#getCharacter() the node's character}
	 * will be pruned. When a node is expanded, all of its branches are also
	 * added to the queue, and explanation-first search prioritizes nodes with
	 * higher epistemic depth. This means that all of a node's branches will be
	 * searched before the trunk, so before a node is visited, if any of its
	 * branches are not explained, it is because the search failed to find an
	 * explanation for the character of that branch and thus the action can
	 * never be explained.
	 */
	@Override
	protected <N> boolean prune(SearchNode<N> node) {
		if(node.getTemporalDepth() > 0) {
			CompiledAction action = node.getAction();
			for(int i=0; i<action.consenting.size(); i++) {
				Character other = action.consenting.get(i);
				if(!Utilities.equals(other, node.getCharacter()) && !node.isExplained(other))
					return true;
			}
		}
		return super.prune(node);
	}
	
	@Override
	protected <N> boolean visit(SearchNode<N> node) {
		if(node.getEpistemicDepth() == 0)
			depth = Math.max(depth, node.getTemporalDepth());
		expand(node);
		return true;
	}
	
	/**
	 * {@inheritDoc}
	 * <p>
	 * During explanation-first search, nodes with higher epistemic depth are
	 * visited first to ensure a node's branches are always searched before
	 * the node (the trunk) is searched.
	 * 
	 * @param <N> the type of object used to represent a node in {@link #space
	 * the search space}
	 */
	@Override
	protected <N> double compare(SearchNode<N> node1, SearchNode<N> node2) {
		double comparison = node2.getEpistemicDepth() - node1.getEpistemicDepth();
		if(comparison != 0)
			return comparison;
		return super.compare(node1, node2);
	}
}