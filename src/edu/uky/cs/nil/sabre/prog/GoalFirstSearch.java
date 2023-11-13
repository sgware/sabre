package edu.uky.cs.nil.sabre.prog;

import java.util.HashMap;

import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.comp.CompiledAction;
import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.etree.EventTree;
import edu.uky.cs.nil.sabre.search.Progress;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * Goal-first search is a type of {@link ProgressionSearch progression search}
 * that requires a plan to achieve its goal before attempting to explain its
 * actions. Specifically, a plan must be {@link
 * ProgressionSpace#isExplained(Object) explained} for {@link
 * ProgressionNode#getCharacter() its character} before the search tries to
 * explain it for any other {@link CompiledAction#consenting consenting
 * characters} who act in the plan. For example, since the search always starts
 * by trying to improve the author's utility, it must first find a plan which
 * raises the author utility before it will attempt to explain any of the
 * actions taken for the characters who took them.
 * <p>
 * Like all progression searches, this search works by {@link #visit(SearchNode)
 * visiting} nodes one at a time, but this search may visit the same node more
 * than once. The procedure for visiting a node works like this:
 * <ul>
 * <li>First, calculate the sum of the node's {@link
 * ProgressionNode#getTemporalOffset() temporal offset} and its {@link
 * ProgressionNode#getTemporalDepth() temporal depth}. Call this value the
 * node's temporal distance.</li>
 * <li>If the node has not yet been visited at or below that temporal distance,
 * {@link SearchNode#getChild(CompiledAction) children} will be generated for
 * each relevant action, but the branches needed to explain those children will
 * not be generated. This allows the search to move toward the goal without
 * yet explaining any actions.</li>
 * <li>If the {@link SearchNode#getAction() node's action} is explained for
 * {@link SearchNode#getCharacter() the node's character} (that is, it can lead
 * to an improved utility for the character of interest), but not for all of
 * its consenting characters, then the branches needed to explain that action
 * are added to the queue, and the node itself is added back onto the queue to
 * be visited again later. Goal-first search {@link
 * #compare(SearchNode, SearchNode) prioritizes actions with higher epistemic
 * depth}, so the branches will be searched before the node is revisited.</li>
 * <li>If the {@link SearchNode#getAction() node's action} is explained for all
 * its consenting characters and can lead to an improved utility for {@link
 * SearchNode#getCharacter() the node's character}, all of its {@link
 * SearchNode#getParents() parents} are pushed back onto the queue to be queue
 * to be visited again. Otherwise, the node is pruned, because the search to
 * explain one of its branches must have failed.</li>
 * </ul>
 * The reason a node's temporal distance is checked is because if a node is
 * revisited later at a lower temporal distance, it should be expanded and/or
 * explained again, because it is possible that previous searches only failed
 * because the search ran up against its temporal limits.
 * <p>
 * Nodes only count toward {@link #getVisited() the number of visited nodes}
 * the first time they are visited; revisits at the same or higher temporal
 * distance are not counted.
 * 
 * @author Stephen G. Ware
 */
public class GoalFirstSearch extends ProgressionSearch {

	/** Tracks which nodes have already been expanded */
	private final HashMap<Object, Integer> expanded = new HashMap<>();
	
	/** Tracks which nodes have already been explained */
	private final HashMap<Object, Integer> explained = new HashMap<>();
	
	/** The temporal depth of the deepest author node visited so far */
	private int deepest = 0;
	
	/**
	 * The temporal depth of the shallowest explained author node that improves
	 * the author's utility
	 */
	private int shallowest = Integer.MAX_VALUE;
	
	/**
	 * Constructs a new goal-first progression search.
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
	 * @param characterTemporalLimit the max number of actions an agent can
	 * imagine when trying to explain its actions
	 * @param epistemicLimit the max depth in a character's theory of mind to
	 * search
	 * @param explanationPruning whether the search should stop exploring a
	 * branch once its root has been explained
	 */
	public GoalFirstSearch(
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
		return "[" + toString("Goal-First Heurstic Progression Search") + "]";
	}
	
	@Override
	public void setStart(State state) {
		super.setStart(state);
		expanded.clear();
		explained.clear();
		deepest = 0;
		shallowest = Integer.MAX_VALUE;
	}
	
	/**
	 * {@inheritDoc}
	 * <p>
	 * During goal-first search, this method will expand a node's children
	 * (without generating the branches needed to explain them) the first time
	 * a node is visited. When a node is visited that can lead to an improved
	 * utility for {@link SearchNode#getCharacter() its character}, the branches
	 * needed to explain {@link SearchNode#getAction() the node's action} are
	 * then generated and the node is put back onto the queue to be revisited
	 * later. When a node is visited that can improve utility for its character
	 * and which is explained for all characters, its parent nodes are all put
	 * back onto the queue so that they can be revisited and explained in
	 * future iterations.
	 */
	@Override
	protected <N> boolean visit(SearchNode<N> node) {
		boolean result = expandOnce(node);
		if(node.isExplained() && node.isExplained(node.getCharacter())) {
			for(SearchNode<N> parent : node.getParents())
				push(parent);
			if(node.getCharacter() == null)
				shallowest = Math.min(shallowest, node.getTemporalDepth() - 1);
		}
		else if(node.isExplained(node.getCharacter()))
			explainOnce(node);
		if(node.getCharacter() == null)
			deepest = Math.max(deepest, node.getTemporalDepth());
		return result;
	}
	
	private final boolean expandOnce(SearchNode<?> node) {
		int temporal = node.getTemporalOffset() + node.getTemporalDepth();
		Integer expanded = this.expanded.get(node.getNode());
		if(expanded == null || temporal < expanded) {
			this.expanded.put(node.getNode(), temporal);
			expand(node);
			return true;
		}
		return false;
	}
	
	private final void explainOnce(SearchNode<?> node) {
		int temporal = node.getTemporalOffset() + node.getTemporalDepth();
		Integer explained = this.explained.get(node.getNode());
		if(explained == null || temporal < explained) {
			this.explained.put(node.getNode(), temporal);
			if(explain(node))
				push(node);
		}
	}
	
	@Override
	protected void setStatus(Status status, Progress<CompiledAction> progress) {
		status.setMessage("Goal-first progression search for \"" + problem.name + "\": deepest %s; shallowest %s; %d visited; %d generated; %d in queue", deepest, getShallowest(), 0, 0, 0);
	}
	
	@Override
	protected void updateStatus(Status status, Progress<CompiledAction> progress) {
		status.update(0, deepest);
		status.update(1, getShallowest());
		status.update(2, progress.getVisited());
		status.update(3, progress.getGenerated());
		status.update(4, queue.size());
	}
	
	private final Object getShallowest() {
		if(shallowest == Integer.MAX_VALUE)
			return "none";
		else
			return shallowest;
	}
	
	/**
	 * {@inheritDoc}
	 * <p>
	 * During goal-first search, this method only expands {@link
	 * SearchNode#getChild(CompiledAction) the node's children}, but does not
	 * generate the {@link SearchNode#getBranch(edu.uky.cs.nil.sabre.Character)
	 * branches} needed to explains those children.
	 */
	@Override
	protected <N> boolean expand(SearchNode<N> parent, CompiledAction action) {
		return push(parent.getChild(action));
	}
	
	/**
	 * {@inheritDoc}
	 * <p>
	 * During goal-first search, nodes with higher epistemic depth are visited
	 * first to ensure a node's branches are always searched before a node (the
	 * trunk) is revisited. This means that, when a node is revisited, if it is
	 * still not explained for some character, the search must have failed to
	 * explain the action for that character and the node can be pruned.
	 */
	@Override
	protected <N> double compare(SearchNode<N> node1, SearchNode<N> node2) {
		double comparison = node2.getEpistemicDepth() - node1.getEpistemicDepth();
		if(comparison != 0)
			return comparison;
		return super.compare(node1, node2);
	}
}