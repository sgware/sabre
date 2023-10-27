package edu.uky.cs.nil.sabre.search;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Number;
import edu.uky.cs.nil.sabre.Solution;

/**
 * A progress object provides a convenient way to track whether a {@link Search
 * search} has finished, its result, how many nodes it has visited and
 * generated, and how much time has elapsed. A new progress object should be
 * created for each call to {@link
 * Search#get(edu.uky.cs.nil.sabre.util.Worker.Status)}. While the search is
 * running, it should periodically call {@link #isDone()} to check whether the
 * search has finished. Every time the search finds a plan which improves the
 * {@link edu.uky.cs.nil.sabre.Problem#utility author's utility} over the
 * {@link Search#getStart() search's initial state}, it should report that
 * potential solution to this progress object via
 * {@link #setSolution(Solution, Number)}. This progress object will only report
 * that the search {@link #isDone() is done} if the solution meets or exceeds
 * {@link Search#getGoal() the search's goal}, otherwise, it will track the
 * best solution found so far. If the search finishes without finding a
 * solution (perhaps because it exceeded a node or time limit), the best
 * solution found will be reported, even if it does not meet or exceed the
 * goal.
 * 
 * @param <A> the type of {@link Action action} used in plans produced by the
 * search this progress represents
 * @author Stephen G. Ware
 */
public class Progress<A extends Action> {

	private final Search<A> search;
	private final long visitedBefore;
	private final long maxVisited;
	private final long generatedBefore;
	private final long maxGenerated;
	private final long timeBefore;
	private final long maxTime;
	private Solution<A> solution = null;
	private Number utility = null;
	private String message = null;
	
	/**
	 * Constructs a new progress object for a given search.
	 * 
	 * @param search the search whose progress is being tracked
	 */
	public Progress(Search<A> search) {
		this.search = search;
		this.visitedBefore = search.getVisited();
		this.maxVisited = search.searchLimit == Planner.UNLIMITED_NODES ? Long.MAX_VALUE : visitedBefore + search.searchLimit;
		this.generatedBefore = search.getGenerated();
		this.maxGenerated = search.spaceLimit == Planner.UNLIMITED_NODES ? Integer.MAX_VALUE : generatedBefore + search.spaceLimit;
		this.timeBefore = System.currentTimeMillis();
		this.maxTime = search.timeLimit == Planner.UNLIMITED_TIME ? Long.MAX_VALUE : timeBefore + search.timeLimit;
	}
	
	/**
	 * Checks whether a search is finished, either because a solution was found
	 * or because the search exceeded some limit, such as {@link
	 * Planner#setSearchLimit(long) the number of nodes visited} or {@link
	 * Planner#setTimeLimit(long) amount of time allowed}.
	 * 
	 * @return true if the search is finished, false otherwise
	 */
	public boolean isDone() {
		getMessage();
		return message != null;
	}
	
	/**
	 * Returns the best solution reported to {@link
	 * #setSolution(Solution, Number)} so far or null if no solutions have been
	 * reported. If {@link #isDone()} is still reporting false, the solution
	 * returned by this method will be null or will not meet or exceed {@link
	 * Search#getGoal() the search's goal} for {@link
	 * edu.uky.cs.nil.sabre.Problem#utility author utility}.
	 * 
	 * @return the best solution reported so far, or null if none have been
	 * reported
	 */
	public Solution<A> getSolution() {
		return solution;
	}
	
	/**
	 * This method is called from {@link Search this progress's search} each
	 * time a plan which improves {@link edu.uky.cs.nil.sabre.Problem#utility
	 * author's utility} above the {@link Search#getStart() search's initial
	 * state} is found. The utility reported should be the author's utility
	 * in the state immediately after the solution plan has been executed in
	 * the search's initial state. All plans which improve author utility
	 * should be reported, even if they do not improve author utility up
	 * to or above {@link Search#getGoal() the goal}. If the plan meets or
	 * exceeds the goal, {@link #isDone()} will begin reporting true after
	 * that solution is reported and {@link #getMessage()} will indicate
	 * success.
	 * 
	 * @param solution any potential solution that improves the author's
	 * utility, even if it does not meet or exceed the goal
	 * @param utility the author utility achieved by the solution
	 */
	public void setSolution(Solution<A> solution, Number utility) {
		if(this.utility == null || utility.isGreaterThan(this.utility)) {
			this.solution = solution;
			this.utility = utility;
			if(utility.isGreaterThanOrEqualTo(search.getGoal()))
				message = "Success!";
		}
	}
	
	/**
	 * Returns the {@link edu.uky.cs.nil.sabre.Problem#utility author utility}
	 * achieved by the best solution reported to {@link
	 * #setSolution(Solution, Number)} so far or null if no solutions have been
	 * reported. If {@link #isDone()} is still reporting false, the utility
	 * returned by this method will be null or will not meet or exceed {@link
	 * Search#getGoal() the search's goal}.
	 * 
	 * @return the author utility achieved by the best solution reported so
	 * far, or null if none have been reported
	 */
	public Number getUtility() {
		return utility;
	}
	
	/**
	 * Returns a brief message that explains the final result of the {@link
	 * Search search}. If the search ended because the {@link
	 * Planner#getSearchLimit() search limit}, {@link Planner#getSpaceLimit()
	 * space limit}, or {@link Planner#getTimeLimit() time limit} was exceeded,
	 * the message will say so. If a solution that achieves the {@link
	 * Search#getGoal()} was {@link #setSolution(Solution, Number) reported},
	 * the message will indicate success. If a solution was reported that
	 * improves {@link edu.uky.cs.nil.sabre.Problem#utility author utility} but
	 * not enough to meet or exceed the goal, the message will indicate that a
	 * suboptimal solution was found. Otherwise, the message will indicate that
	 * no solution exists.
	 * 
	 * @return a brief message explaining the results of the search
	 */
	public String getMessage() {
		if(message == null) {
			if(search.getVisited() >= maxVisited)
				message = "Search limit reached.";
			else if(search.getGenerated() >= maxGenerated)
				message = "Space limit reached.";
			else if(System.currentTimeMillis() >= maxTime)
				message = "Time limit reached.";
			else if(solution != null)
				return "Suboptimal solution found.";
			else
				return "No solution exists.";
		}
		return message;
	}
	
	/**
	 * Returns the number of {@link Planner#getSearchLimit() nodes visited}
	 * during {@link Search#get(edu.uky.cs.nil.sabre.util.Worker.Status) this
	 * search}. In contrast, {@link Search#getVisited()} reports the total
	 * number of nodes visited during all searches since the last time the
	 * {@link Search#setStart(edu.uky.cs.nil.sabre.State) search was reset}.
	 * 
	 * @return the number of nodes visited during the search which uses this
	 * problem object
	 */
	public long getVisited() {
		return search.getVisited() - visitedBefore;
	}
	
	/**
	 * Returns the number of {@link Planner#getSpaceLimit() nodes generated}
	 * during {@link Search#get(edu.uky.cs.nil.sabre.util.Worker.Status) this
	 * search}. In contrast, {@link Search#getGenerated()} reports the total
	 * number of nodes generated during all searches since the last time the
	 * {@link Search#setStart(edu.uky.cs.nil.sabre.State) search was reset}.
	 * 
	 * @return the number of nodes generated during the search which uses this
	 * problem object
	 */
	public long getGenerated() {
		return search.getGenerated() - generatedBefore;
	}
	
	/**
	 * Returns the number of milliseconds elapsed during {@link
	 * Search#get(edu.uky.cs.nil.sabre.util.Worker.Status) this search}.
	 * 
	 * @return the number of milliseconds elapsed
	 */
	public long getTime() {
		return System.currentTimeMillis() - timeBefore;
	}
}