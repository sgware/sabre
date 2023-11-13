package edu.uky.cs.nil.sabre.prog;

import java.util.Comparator;
import java.util.PriorityQueue;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Number;
import edu.uky.cs.nil.sabre.Solution;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.comp.CompiledAction;
import edu.uky.cs.nil.sabre.comp.CompiledFluent;
import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.etree.EventTree;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.search.Planner;
import edu.uky.cs.nil.sabre.search.Progress;
import edu.uky.cs.nil.sabre.search.Search;
import edu.uky.cs.nil.sabre.util.ImmutableSet;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * A progression {@link Search search} is a forward search through the space of
 * {@link State states} that starts as a specified {@link #setStart(State)
 * initial state} and {@link #get(Status) runs} until its finds a {@link
 * Solution solution plan} which increases the {@link
 * edu.uky.cs.nil.sabre.Problem#utility author's utility} to or above a
 * specified {@link #setGoal(Number) goal value} and for which every {@link
 * CompiledAction action} in the plan makes sense based on the beliefs and
 * intentions of the {@link CompiledAction#consenting characters who take that
 * action}.
 * <p>
 * A progression search iteratively {@link #getGenerated() generates} and {@link
 * #getVisited() visits} {@link ProgressionNode search nodes}, which are states
 * defined by the sequence of actions (a plan) taken to reach that state. Each
 * time an action is added to a plan, that action must be {@link
 * ProgressionNode#isExplained() explained} for each of its consenting
 * characters. A {@link ProgressionSpace#getBranch(Object, Character) branch} is
 * created for each consenting character by calculating the state that character
 * believes to be that case after taking the action. This state forms the {@link
 * ProgressionNode#getRoot() root} of a smaller search that takes place within
 * the larger search to improve {@link ProgressionNode#getCharacter() that
 * character's} {@link ProgressionNode#getUtility(Character) utility}. 
 * <p>
 * In other words, a progression search can be thought of as many smaller
 * searches, each with its own root, that tries to improve the utility for one
 * character. The primary search starts in the problem's initial state and tries
 * to improve the author's utility. Each time an action needs to be explained
 * for a character, a new search starts, rooted in the state that character
 * believes to be the case after taking the action and aiming to improve that
 * character's utility. When characters anticipate the actions of other
 * characters, new searches start, and so on.
 * <p>
 * A progression search is primarily defined by four things:
 * <ul>
 * <li>The {@link #space search space} used to model states.</li>
 * <li>The {@link ProgressionPlanner.Method search method}, which constrains the
 * order in which states are visited.</li>
 * <li>A {@link #cost cost function} which calculates the work done to
 * construct a plan so far.
 * <li>A {@link #heuristic heuristic function} which estimates the remaining
 * work still neded to make a plan into a solution.</li>
 * </ul>
 * By default, a progression search visits states in a {@link
 * ProgressionPlanner.Method#BEST_FIRST best first} order, which means the next
 * node to be visited is the one with the lowest sum of its {@link
 * SearchNode#getCost() cost} and {@link SearchNode#getHeuristic() heuristic}.
 * See {@link #compare(SearchNode, SearchNode)} for more details on the order in
 * which states are visited.
 * 
 * @author Stephen G. Ware
 */
public class ProgressionSearch extends Search<CompiledAction> {

	/** The problem this search is solving */
	public final CompiledProblem problem;
	
	/** A function to measure the cost of a plan so far */
	public final ProgressionCost cost;
	
	/** A function to estimate the cost to complete a plan */
	public final ProgressionCost heuristic;
	
	/**
	 * Whether or not this search will use explanation pruning, which means that
	 * a node will not be visited if its {@link ProgressionNode#getRoot() root}
	 * is {@link ProgressionNode#isExplained() already explained}. In other
	 * words, explanation pruning means that once one explanation for an action
	 * has been found, that branch of the search stops. Explanation pruning can
	 * improve the speed of the overall search, but may sometimes be undesirable
	 * if the search is meant to find more than one explanation for some
	 * actions.
	 */
	public final boolean explanationPruning;
	
	/**
	 * An event tree of actions for efficiently detecting which actions apply
	 * in a state
	 */
	protected final EventTree<CompiledAction> actions;
	
	/** A underlying model of states used in the search */
	protected final ProgressionSpace<?> space;
	
	/**
	 * A min priority queue of {@link SearchNode search nodes} that defines the
	 * order in which states are visited
	 */
	protected final PriorityQueue<SearchNode<?>> queue;
	
	/** The initial state that a search should start in */
	private State start;
	
	/** The initial state as a {@link SearchRoot search root node} */
	private SearchRoot<?> root;
	
	/**
	 * The value of the {@link edu.uky.cs.nil.sabre.Problem#utility author's
	 * utility} that must be reached or exceeded for a plan to be a solution
	 */
	private Number goal;
	
	/**
	 * The total number of nodes visited by the search since the last time it
	 * was {@link #setStart(State) reset}
	 */
	private long visited = 0;
	
	/**
	 * Constructs a new progression search.
	 * 
	 * @param <N> the type of object used to represent a node in {@link #space
	 * the search space}
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
	@SuppressWarnings("unchecked")
	public <N> ProgressionSearch(
		CompiledProblem problem,
		ProgressionCost cost,
		ProgressionCost heuristic,
		EventTree<CompiledAction> actions,
		ProgressionSpace<N> space,
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
			searchLimit,
			spaceLimit,
			timeLimit,
			authorTemporalLimit,
			characterTemporalLimit,
			epistemicLimit
		);
		this.problem = problem;
		this.cost = cost;
		this.heuristic = heuristic;
		this.actions = actions;
		this.space = space;
		Comparator<SearchNode<N>> comparator = new Comparator<>() {
			@Override
			public int compare(SearchNode<N> node1, SearchNode<N> node2) {
				double comparison = ProgressionSearch.this.compare(node1, node2);
				if(comparison < 0)
					return -1;
				else if(comparison > 0)
					return 1;
				else
					return 0;
			}
		};
		PriorityQueue<SearchNode<N>> queue = new PriorityQueue<>(comparator);
		this.queue = (PriorityQueue<SearchNode<?>>) (PriorityQueue<?>) queue;
		this.explanationPruning = explanationPruning;
	}
	
	@Override
	public String toString() {
		return "[" + toString("Heuristic Progression Search") + "]";
	}
	
	@Override
	protected String toString(String name) {
		String string = super.toString(name);
		string += "; c=\"" + cost + "\"";
		string += "; h=\"" + heuristic + "\"";
		string += "; ep=" + explanationPruning;
		return string;
	}

	@Override
	public State getStart() {
		return start;
	}

	@Override
	public void setStart(State state) {
		queue.clear();
		start = state;
		root = new SearchRoot<>(this, space.initialize(start));
		cost.initialize(root);
		heuristic.initialize(root);
		push(root);
		visited = 0;
	}

	@Override
	public Number getGoal() {
		return goal;
	}

	@Override
	public void setGoal(Number goal) {
		this.goal = goal;
	}

	@Override
	public long getVisited() {
		return visited;
	}

	@Override
	public long getGenerated() {
		return space.size();
	}

	@Override
	protected void run(Progress<CompiledAction> progress, Status status) {
		setStatus(status, progress);
		while(!progress.isDone()) {
			Solution<CompiledAction> solution = space.getNextSolution();
			if(solution != null) {
				Value utility = utility(solution);
				if(utility instanceof Number)
					progress.setSolution(solution, (Number) utility);
			}
			else if(!queue.isEmpty()) {
				SearchNode<?> node = queue.poll();
				if(!prune(node) && visit(node))
					visited++;
				updateStatus(status, progress);
			}
			else
				break;
		}
		updateStatus(status, progress);
	}
	
	@SuppressWarnings("unchecked")
	private final <N> Value utility(Solution<CompiledAction> solution) {
		ProgressionSpace<N> space = (ProgressionSpace<N>) this.space;
		N state = (N) root.getNode();
		for(CompiledAction action : solution)
			state = space.getChild(state, action);
		final N end = state;
		return problem.utility.evaluate(f -> space.getValue(end, (CompiledFluent) f));
	}
	
	/**
	 * Sets the message and parameters of the {@link Status status} that will
	 * be updated during the {@link #get(Status) search}. This method will be
	 * called once at the beginning of a new search, and the status will be
	 * periodically updated via {@link #updateStatus(Status, Progress)}.
	 * 
	 * @param status the status object whose message and parameters will be set
	 * @param progress the progress object for the search that is just starting
	 */
	protected void setStatus(Status status, Progress<CompiledAction> progress) {
		status.setMessage("Heuristic progression search for \"" + problem.name + "\": %d visited; %d generated; %d in queue", 0, 0, 0);
	}
	
	/**
	 * Updates the status during a {@link #get(Status) search}. This method will
	 * be called after visiting each node during the search, but only after
	 * {@link #setStatus(Status, Progress)} has been called once for this
	 * search.
	 * 
	 * @param status the status object to update
	 * @param progress the progress object for the current search
	 */
	protected void updateStatus(Status status, Progress<CompiledAction> progress) {
		status.update(0, progress.getVisited());
		status.update(1, progress.getGenerated());
		status.update(2, queue.size());
	}
	
	/**
	 * Determines whether a {@link SearchNode search node} should be pruned
	 * (that is, not visited). This method is called before {@link
	 * #visit(SearchNode)}, and it it returns true, the node will not be
	 * visited.
	 * <p>
	 * By default, this method returns true only when {@link
	 * #explanationPruning explanation pruning} is on, the node's {@link
	 * SearchNode#getEpistemicDepth() epistemic depth} is greater than 0, and 
	 * the given node's {@link SearchNode#getRoot() root} is {@link
	 * SearchNode#isExplained() explained}.
	 * 
	 * @param <N> the type of object used to represent a node in {@link #space
	 * the search space}
	 * @param node the search node that is about to be visited if it is not
	 * pruned
	 * @return true if the search node should be pruned (that is, not visited),
	 * or false if the node should be visited
	 */
	protected <N> boolean prune(SearchNode<N> node) {
		return explanationPruning && node.getEpistemicDepth() > 0 && node.root.isExplained();
	}
	
	/**
	 * Performs one iteration of the search on the given {@link SearchNode
	 * search node}. One iteration usually means removing one node from the 
	 * {@link #queue queue}, {@link #visit(ProgressionTreeSearchBranch)
	 * visiting} that node, and possibly {@link #push(SearchNode) adding more
	 * nodes to the queue}. If this method returns true, {@link #getVisited()
	 * the number of visited nodes} will be incremented by one, otherwise it
	 * will not be incremented.
	 * <p>
	 * By default, this method simply calls {@link #expand(SearchNode)} with the
	 * same node and returns true.
	 * 
	 * @param <N> the type of object used to represent a node in {@link #space
	 * the search space}
	 * @param node the search node to be visited
	 * @return true if the number of visited nodes should be incremented, false
	 * otherwise
	 */
	protected <N> boolean visit(SearchNode<N> node) {
		expand(node);
		return true;
	}
	
	/**
	 * Finds all relevant {@link CompiledAction actions} that can be taken in a
	 * given search node's state and calls {@link
	 * #expand(SearchNode, CompiledAction)} for the node and each action. There
	 * are several criteria for a relevant action:
	 * <ul>
	 * <li>A relevant action's {@link CompiledAction#getPrecondition()
	 * precondition} must {@link
	 * edu.uky.cs.nil.sabre.logic.Expression#evaluate(State) evaluate} to {@link
	 * edu.uky.cs.nil.sabre.logic.True#TRUE true} in the state represented by
	 * the given search node.</li>
	 * <li>Actions which have no {@link CompiledAction#consenting consenting
	 * characters} can only be taken when the node's {@link
	 * SearchNode#getEpistemicDepth() epistemic depth} is 0 (in other words,
	 * actions taken by no character can only be chosen by the author).</li>
	 * <li>If the node's {@link SearchNode#getTemporalDepth() temporal depth} is
	 * greater than or equal to the search's temporal limit (which is {@link
	 * #authorTemporalLimit the author temporal limit} when {@link
	 * SearchNode#getEpistemicDepth() epistemic depth} is 0, or {@link
	 * #characterTemporalLimit the character temporal limit} when epistemic
	 * depth is greater than 0), no actions are relevant, because taking any
	 * action would expand the search space past its temporal limit.</li>
	 * </ul>
	 * 
	 * @param <N> the type of object used to represent a node in {@link #space
	 * the search space}
	 * @param node the search node whose relevant actions will be considered
	 */
	protected <N> void expand(SearchNode<N> node) {
		if(checkLimits(node))
			expand(node, actions);
	}
	
	private final boolean checkLimits(SearchNode<?> node) {
		int temporal = node.getTemporalOffset() + node.getTemporalDepth();
		int limit = node.getEpistemicDepth() == 0 ? authorTemporalLimit : characterTemporalLimit;
		return temporal < limit || limit == Planner.UNLIMITED_DEPTH;
	}
	
	private final void expand(SearchNode<?> node, EventTree<CompiledAction> actions) {
		if(actions == null)
			return;
		for(int i=0; i<actions.events.size(); i++) {
			CompiledAction action = actions.events.get(i);
			if(checkLimits(node, action))
				expand(node, action);
		}
		expand(node, actions.getBranch(actions.expression.evaluate(node)));
		expand(node, actions.getBranch(null));
	}
	
	private final boolean checkLimits(SearchNode<?> parent, CompiledAction action) {
		if(action.consenting.size() == 0)
			return parent.getEpistemicDepth() == 0;
		else
			return true;
	}
	
	/**
	 * This method is called for each {@link CompiledAction action} that is
	 * relevant to the state represented by a given {@link SearchNode search
	 * node}. See {@link #expand(SearchNode)} for a description of when actions
	 * are relevant.
	 * <p>
	 * By default, this method will {@link SearchNode#getChild(CompiledAction)
	 * generate a child} for the given node using the given action and call
	 * {@link #explain(SearchNode)}, which generates {@link
	 * SearchNode#getBranch(Character) a branch} for each of the action's
	 * {@link CompiledAction#consenting consenting characters}. The child and
	 * all branches will only be added to the {@link #queue queue} if all of
	 * them can be {@link #push(SearchNode) added to the queue}. In other words,
	 * if {@link #push(SearchNode)} returns false for the child or any branch,
	 * neither the child nor any of its branches will be added to the queue.
	 * 
	 * @param <N> the type of object used to represent a node in {@link #space
	 * the search space}
	 * @param parent the search node to which the action is relevant
	 * @param action the relevant action
	 * @return true if the child generated for that action was successfully
	 * added to the search queue, or false if nothing was added to the queue
	 */
	protected <N> boolean expand(SearchNode<N> parent, CompiledAction action) {
		SearchNode<N> child = parent.getChild(action);
		if(!push(child))
			return false;
		else if(explain(child))
			return true;
		else {
			queue.remove(child);
			return false;
		}
	}
	
	/**
	 * Generates {@link SearchNode#getBranch(Character) a branch} for each
	 * {@link CompiledAction#consenting consenting character} of the action that
	 * led to the given {@link SearchNode search node} and {@link
	 * #push(SearchNode) adds them} to the {@link #queue search queue}. The
	 * branches will only be added if all of them can be added; in other words,
	 * if {@link #push(SearchNode)} fails for any branch, none of the branches
	 * will be added.
	 * 
	 * @param <N> the type of object used to represent a node in {@link #space
	 * the search space}
	 * @param trunk a search node representing the state after taking the
	 * action that will be explained
	 * @return true if all branches were successfully added to the search queue,
	 * or false if any branch does not exist or could not be added
	 */
	protected <N> boolean explain(SearchNode<N> trunk) {
		CompiledAction action = trunk.getAction();
		if(action == null)
			return false;
		else
			return explain(trunk, action.consenting, 0);
	}
	
	private final <N> boolean explain(SearchNode<N> trunk, ImmutableSet<Character> characters, int index) {
		if(index == characters.size())
			return true;
		else if(Utilities.equals(trunk.getCharacter(), characters.get(index)))
			return explain(trunk, characters, index + 1);
		else {
			SearchRoot<N> branch = trunk.getBranch(characters.get(index));
			if(branch == null)
				return false;
			else if(!explain(trunk, characters, index + 1))
				return false;
			else {
				push(branch);
				return true;
			}
		}
	}
	
	/**
	 * Adds a {@link SearchNode search node} to the {@link #queue search queue}.
	 * This method returns true if the node was added successfully or false if
	 * the node was not added.
	 * <p>
	 * By default, this method returns false if {@link
	 * SearchNode#getEpistemicDepth() the node's epistemic depth} exceeds {@link
	 * #epistemicLimit the search's epistemic limit}. It also calculates the
	 * {@link #cost cost} and {@link #heuristic heuristic} values for the search
	 * node and {@link SearchNode#setCost(double) sets them}. If either cost is
	 * {@link Double#POSITIVE_INFINITY positive infinity}, this method returns
	 * false. Otherwise, this method adds the node to the queue and returns
	 * true.
	 * 
	 * @param <N> the type of object used to represent a node in {@link #space
	 * the search space}
	 * @param node the node to be added to the queue
	 * @return true if the node was added to the queue; false if the node was
	 * not added
	 */
	protected <N> boolean push(SearchNode<N> node) {
		if(epistemicLimit != Planner.UNLIMITED_DEPTH && node.getEpistemicDepth() > epistemicLimit)
			return false;
		node.setCost(cost.evaluate(node));
		if(node.getCost() == Double.POSITIVE_INFINITY)
			return false;
		node.setHeuristic(heuristic.evaluate(node));
		if(node.getHeuristic() == Double.POSITIVE_INFINITY)
			return false;
		queue.offer(node);
		return true;
	}
	
	/**
	 * Defines the priority in which {@link SearchNode search nodes} should be
	 * visited. This method follows the contract of {@link
	 * Comparator#compare(Object, Object)}, except that it returns a {@code
	 * double} instead of an {@code int}. In other words, this method should
	 * return a negative number if the first node should be visited before the
	 * second node, a positive number if the second node should be visited
	 * before the first, or 0 if the order does not matter.
	 * <p>
	 * By default, this method prioritizes nodes in this order:
	 * <ul>
	 * <li>The node with the lowest {@link SearchNode#getCost() cost} plus
	 * {@link SearchNode#getHeuristic() heuristic} value is first.</li>
	 * <li>In case of a tie, the node with the lowest heuristic value is first.
	 * </li>
	 * <li>In case of a further tie, the node with the lowest {@link
	 * SearchNode#getEpistemicDepth() epistemic depth} is first.</li>
	 * <li>In case of a further tie, the {@link SearchNode#getRoot() root for
	 * both nodes} {@link ProgressionSpace#compare(Object, Object) are
	 * compared}.</li>
	 * <li>In case of a further tie, the {@link SearchNode#getNode() node
	 * objects of both nodes} {@link ProgressionSpace#compare(Object, Object)
	 * are compared}.</li>
	 * </ul>
	 * 
	 * @param <N> the type of object used to represent a node in {@link #space
	 * the search space}
	 * @param node1 the first node to be compared
	 * @param node2 the second node to be compared
	 * @return a negative double, zero, or a positive double as the first
	 * node is higher priority, the same priority, or lower priority than the
	 * second node
	 */
	@SuppressWarnings("unchecked")
	protected <N> double compare(SearchNode<N> node1, SearchNode<N> node2) {
		double comparison = (node1.getCost() + node1.getHeuristic()) - (node2.getCost() + node2.getHeuristic());
		if(comparison != 0)
			return comparison;
		comparison = node1.getHeuristic() - node2.getHeuristic();
		if(comparison != 0)
			return comparison;
		comparison = node1.getEpistemicDepth() - node2.getEpistemicDepth();
		if(comparison != 0)
			return comparison;
		ProgressionSpace<N> space = (ProgressionSpace<N>) this.space;
		comparison = space.compare(node1.getRoot(), node2.getRoot());
		if(comparison != 0)
			return comparison;
		return space.compare(node1.getNode(), node2.getNode());
	}
}