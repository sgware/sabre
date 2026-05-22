package edu.uky.cs.nil.sabre.search;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Number;
import edu.uky.cs.nil.sabre.Plan;
import edu.uky.cs.nil.sabre.Problem;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.comp.CompiledAction;
import edu.uky.cs.nil.sabre.util.Worker.Getter;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * A search represents the process of finding one or more {@link Plan plans}
 * that are solutions to a {@link Problem planning problem}. A search is one
 * specific configuration of one approach for solving one problem. Searches
 * are {@link Planner#getSearch(Problem, Status) created by planners}.
 * <p>
 * Though the problem and settings cannot be changed, it is possible to
 * {@link #setStart(State) change the initial state} from which this search
 * will start and {@link #setGoal(Number) the goal value} that a plan must
 * achieve. Any solution {@link #get(Status) returned by this search} must be a
 * {@link Plan plan} which can be executed from the {@link #getStart() current
 * initial state} and which will increase the {@link Problem#utility author's
 * utility} up to or above the {@link #getGoal() the current goal}.
 * <p>
 * A search should maintain its state after returning a solution because it may
 * be asked to resume and find additional solutions. A search can be reset
 * (meaning it can discard its internal state and reset counters for things
 * like {@link #getVisited() the number of nodes visited}) when the {@link
 * #setStart(State)} method is called.
 * 
 * @param <A> the type of {@link Action action} used in the plans this search
 * produces
 * @author Stephen G. Ware
 */
public abstract class Search<A extends Action> implements Getter<Result<A>> {

	/** The problem this search is finding solutions for */
	public final Problem problem;
	
	/**
	 * The maximum number of nodes which may be visited while {@link
	 * #get(Status) finding one solution}. This setting does not limit the
	 * {@link #getVisited() total number of nodes visited}, only the number
	 * of new nodes that can be visited during a single search. The constant
	 * {@link Planner#UNLIMITED_NODES} represents no limit.
	 */
	public final long searchLimit;
	
	/**
	 * The maximum number of nodes which may be generated while {@link
	 * #get(Status) finding one solution}. This setting does not limit the
	 * {@link #getGenerated() total number of nodes generated}, only the number
	 * of new nodes that can be generated during a single search. The constant
	 * {@link Planner#UNLIMITED_NODES} represents no limit.
	 */
	public final long spaceLimit;
	
	/**
	 * The maximum number of milliseconds that may elapse while {@link
	 * #get(Status) finding one solution}. The constant {@link
	 * Planner#UNLIMITED_TIME} represents no limit.
	 */
	public final long timeLimit;
	
	/**
	 * The maximum number of {@link CompiledAction actions} a {@link Plan
	 * solution plan} produced by this search may use to improve the {@link
	 * Problem#utility author's utility}. This can be thought of as the limit on
	 * the length of the main plan. The constant {@link Planner#UNLIMITED_DEPTH}
	 * represents no limit.
	 */
	public final int authorTemporalLimit;
	
	/**
	 * The maximum number of {@link edu.uky.cs.nil.sabre.Action actions} a
	 * {@link Plan solution plan} produced by this search may use to justify
	 * why an {@link Character character} would take an action (the limit
	 * includes the action itself). This can be thought of as the limit on the
	 * length of a character's plan when characters imagine how an action can
	 * improve their utility. The constant{@link Planner#UNLIMITED_DEPTH}
	 * represents no limit.
	 */
	public final int characterTemporalLimit;
	
	/**
	 * The maximum depth that each {@link Character character}'s theory of mind
	 * (what one character believes another character believes, etc.) can be
	 * searched. Theory of mind is always infinite, and this setting does not
	 * change how it behaves; it only limits which states will be {@link
	 * #getVisited() visited} by the search. A value of 0 means the search will
	 * never visit states that represent what a character believes the state to
	 * be. This does not mean character beliefs are not modeled (they are). It
	 * also does not necessarily mean the search will fail to find solutions
	 * that contain character actions (in some cases, it still can). As long as
	 * the states needed to explain character actions are {@link #getGenerated()
	 * generated}, actions will be explained regardless of whether they are
	 * visited by the search. However, a value of 0 would prevent the solution
	 * from finding actions that can only be explained by plans that will not
	 * actually work. These kinds of mistaken explanations can only be found if
	 * the search visits the states that represent the hypothetical plan. A
	 * value of 1 means the search will visit states that represent what a
	 * character believes. A value of 2 means the search will visit states that
	 * represents what one character believes another believes, and so on. The
	 * constant {@link Planner#UNLIMITED_DEPTH} represents no limit.
	 */
	public final int epistemicLimit;
	
	/**
	 * Constructs a new search with a given problem and settings.
	 * 
	 * @param problem the planning problem this search will find solutions for
	 * @param searchLimit the maximum number of nodes that may be visited while
	 * finding one solution
	 * @param spaceLimit the maximum number of nodes that may be generated
	 * while finding one solution
	 * @param timeLimit the maximum number of milliseconds that may elapse
	 * while finding one solution
	 * @param authorTemporalLimit the maximum number of actions in the author's
	 * plan
	 * @param characterTemporalLimit the maximum number of actions in the plans
	 * characters use to justify their actions
	 * @param epistemicLimit the maximum depth in a character's theory of mind
	 * to search
	 */
	public Search(
		Problem problem,
		long searchLimit,
		long spaceLimit,
		long timeLimit,
		int authorTemporalLimit,
		int characterTemporalLimit,
		int epistemicLimit
	) {
		this.problem = problem;
		this.searchLimit = searchLimit;
		this.spaceLimit = spaceLimit;
		this.timeLimit = timeLimit;
		this.authorTemporalLimit = authorTemporalLimit;
		this.characterTemporalLimit = characterTemporalLimit;
		this.epistemicLimit = epistemicLimit;
	}
	
	@Override
	public String toString() {
		return "[" + toString("Search") + "]";
	}
	
	/**
	 * Returns a string composed of a name and a description of the search's
	 * parameters, primary for use in {@link #toString()}.
	 * 
	 * @param name the name of the type of search
	 * @return a string of the name and search's parameters
	 */
	protected String toString(String name) {
		String string = name + ": ";
		string += "p=\"" + problem.name + "\"";
		string += "; g=" + getGoal();
		string += "; vl=" + searchLimit;
		string += "; gl=" + spaceLimit;
		string += "; tl=" + timeLimit;
		string += "; atl=" + authorTemporalLimit;
		string += "; ctl=" + characterTemporalLimit;
		string += "; el=" + epistemicLimit;
		return string;
	}
	
	/**
	 * Finds the next solution to this search's problem. A solution is a {@link
	 * Plan plan} which can be executed from {@link #getStart() this search's
	 * initial state} and which increases the {@link Problem#utility author's
	 * utility} up to or above {@link #getGoal() this search's goal value}.
	 * <p>
	 * This method may take a long time to run, so the provided {@link
	 * Status status} object should be periodically updated with the method's
	 * progress.
	 * <p>
	 * If this method has already been called, and {@link #setStart(State)} has
	 * not been called to reset this search in the interim, this search should
	 * maintain its internal state after this method returns because this
	 * method may be called again to return additional solutions.
	 * <p>
	 * This search should maintain its internal state even after {@link
	 * #setGoal(Number)} is called; however, if the goal is set to a lower
	 * value, the search does not need to revisit plans which previously would
	 * not have been considered solutions but now would be due to the lowered
	 * goal.
	 * <p>
	 * This method only needs to find plans which increase the author's utility
	 * to or above what it is in the initial state. If the goal for this search
	 * is less than or equal to the author's utility in the initial state, only
	 * plans which increase the author's utility above the initial state are
	 * considered solutions (in other words, plans which do not increase the
	 * author's utility are not solutions, even if the author's utility would
	 * meet or exceed the goal after the plan is executed).
	 * <p>
	 * Practically speaking, this method is simply a call to {@link
	 * Result#Result(Search, Status) a constructor for the result object},
	 * which in turn calls {@link #run(Progress, Status)}. This is to ensure
	 * the result this method returns has already been allocated in case an
	 * {@link OutOfMemoryError} occurs while searching for a solution. Thus,
	 * this method generally should not be overridden; instead, searches
	 * should implement the abstract method {@link #run(Progress, Status)}.
	 * 
	 * @param status a status to update while the search runs
	 * @return a result object detailing the work done while finding (or
	 * failing to find) a solution to the problem
	 */
	@Override
	public Result<A> get(Status status) {
		return new Result<>(this, status);
	}
	
	/**
	 * Returns the initial state of the world from which a solution {@link
	 * Plan plan} must be executable. Unless changed by the {@link
	 * #setStart(State)} method, the initial state should be the one {@link
	 * Problem#initial defined by the planning problem}.
	 * 
	 * @return the initial state of the world before planning begins
	 */
	public abstract State getStart();
	
	/**
	 * Sets the initial state of the world from which a solution {@link
	 * Plan plan} must be executable. This method resets this search object,
	 * meaning it should discard its internal state, should start over (from
	 * the newly set initial state) the next time {@link #get(Status)} is
	 * called, and may reset counters such as {@link #getVisited() the number
	 * of nodes visited}.
	 * 
	 * @param state the new initial state of the world before planning begins
	 */
	public abstract void setStart(State state);
	
	/**
	 * Returns the {@link Problem#utility author utility} value that a solution
	 * {@link Plan plan} must reach or exceed to be considered a solution by
	 * this search.
	 * 
	 * @return the author utility value which must be reached or exceeded for a
	 * plan to be considered a solution
	 */
	public abstract Number getGoal();
	
	/**
	 * Sets the {@link Problem#utility author utility} value that a solution
	 * {@link Plan plan} must reach or exceed to be considered a solution by
	 * this search. This method does not reset this search; its internal state
	 * should be maintained.
	 * 
	 * @param goal the new author utility value which must be reached or
	 * exceeded for a plan to be considered a solution
	 */
	public abstract void setGoal(Number goal);
	
	/**
	 * Returns the total number of nodes {@link Planner#setSearchLimit(long)
	 * visited} during all calls to {@link #get(Status)} since the last time
	 * this search was reset using the {@link #setStart(State)} method.
	 * 
	 * @return the total number of nodes visited since this search was reset
	 */
	public abstract long getVisited();
	
	/**
	 * Returns the total number of nodes {@link Planner#setSpaceLimit(long)
	 * generated} during all calls to {@link #get(Status)} since the last time
	 * this search was reset using the {@link #setStart(State)} method.
	 * 
	 * @return the total number of nodes generated since this search was reset
	 */
	public abstract long getGenerated();
	
	/**
	 * This method searches for the next solution, resuming where the previous
	 * call left off (unless {@link #setStart(State)} has been called to reset
	 * the search). A {@link Progress progress object} is provided to track
	 * whether a search has finished and to count values such as nodes visited
	 * and generated (since methods like {@link #getVisited()} track total
	 * nodes visited). A {@link Status status object} should be updated
	 * periodically to reflect this method's progress.
	 * 
	 * @param progress a progress object used to track the nodes visited, nodes
	 * generated, and whether a suitable solution has been found by this search
	 * @param status a status to update while the search runs
	 */
	protected abstract void run(Progress<A> progress, Status status);
}