package edu.uky.cs.nil.sabre.search;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Number;
import edu.uky.cs.nil.sabre.Problem;
import edu.uky.cs.nil.sabre.Solution;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.logic.Comparison;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * A result object summarizes information about the outcome of a {@link Search
 * search} (like the {@link #solution solution it found}) and the effort spent
 * reaching that outcome (like the {@link #visited number of nodes visited} and
 * the {@link #time time spent searching}).
 * 
 * @param <A> the type of {@link Action action} used in this result's solution
 * @author Stephen G. Ware
 */
public class Result<A extends Action> {

	/** The problem the search was solving */
	public final Problem problem;
	
	/** The initial state the search started from */
	public final State start;
	
	/** The author utility that a solution would need to meet or exceed */
	public final Number goal;
	
	/** The solution found by the search */
	public final Solution<A> solution;
	
	/** The author utility achieved by the solution */
	public final Number utility;
	
	/** A brief message explaining the outcome */
	public final String message;
	
	/** The number of nodes visited during the search */
	public final long visited;
	
	/** The number of nodes generated during the search */
	public final long generated;
	
	/** The number of milliseconds elapsed during the search */
	public final long time;
	
	/**
	 * Constructs a new result.
	 * 
	 * @param problem the problem the search was solving
	 * @param start the initial state the search started from
	 * @param goal the author utility that a solution would need to meet or
	 * exceed
	 * @param solution the solution found by the search
	 * @param utility the author utility achieved by the solution
	 * @param message a brief message explaining the outcome
	 * @param visited the number of nodes visited during the search
	 * @param generated the number of nodes generated during the search
	 * @param time the number of milliseconds elapsed during the search
	 */
	public Result(Problem problem, State start, Number goal, Solution<A> solution, Number utility, String message, int visited, int generated, long time) {
		this.problem = problem;
		this.start = start;
		this.goal = goal;
		this.solution = solution;
		this.utility = utility;
		this.message = message;
		this.visited = visited;
		this.generated = generated;
		this.time = time;
	}
	
	/**
	 * Performs a search and constructs a result based on the outcome. This
	 * constructor is used by {@link Search#get(Status)} to ensure that this
	 * result object is allocated in memory before the search starts, in case
	 * an {@link OutOfMemoryError} occurs during search.
	 * 
	 * @param search the search whose next solution will be generated
	 * @param status a status to update while the search runs
	 */
	public Result(Search<A> search, Status status) {
		this.problem = search.problem;
		this.start = search.getStart();
		this.goal = search.getGoal();
		String message;
		String outOfMemory = "Out of memory.";
		Progress<A> progress = new Progress<>(search);
		try {
			search.run(progress, status);
			message = progress.getMessage();
		}
		catch(SearchException e) {
			message = e.getMessage();
		}
		catch(OutOfMemoryError e) {
			System.gc();
			message = outOfMemory;
		}
		this.solution = progress.getSolution();
		this.utility = progress.getUtility();
		this.message = message;
		this.visited = progress.getVisited();
		this.generated = progress.getGenerated();
		this.time = progress.getTime();
	}
	
	@Override
	public String toString() {
		String string = "[Result for \"" + problem.name + "\": " + message;
		if(solution != null)
			string += " " + solution.size() + " actions; utility " + utility + ";";
		return string + " " + visited + " visited; " + generated + " generated; " + Utilities.time(time) + "]";
	}
	
	/**
	 * Indicates whether or not this result represents success in solving the
	 * {@link #problem problem}; success means a solution was found that the
	 * {@link Result#utility utility it achieved} is greater than or equal to
	 * the {@link #goal search's goal}.
	 * 
	 * @return true if this result includes a solution which achieves a utility
	 * that is greater than or equal to the goal, false otherwise
	 */
	public boolean getSuccess() {
		return utility != null && Comparison.GREATER_THAN_OR_EQUAL_TO.test(utility, goal);
	}
}