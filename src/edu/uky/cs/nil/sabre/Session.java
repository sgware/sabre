package edu.uky.cs.nil.sabre;

import java.io.File;
import java.io.IOException;

import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.io.DefaultParser;
import edu.uky.cs.nil.sabre.io.ParseException;
import edu.uky.cs.nil.sabre.io.Parser;
import edu.uky.cs.nil.sabre.io.Printer;
import edu.uky.cs.nil.sabre.logic.Arithmetic;
import edu.uky.cs.nil.sabre.logic.Comparison;
import edu.uky.cs.nil.sabre.logic.Unknown;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.prog.ProgressionCostFactory;
import edu.uky.cs.nil.sabre.prog.ProgressionPlanner;
import edu.uky.cs.nil.sabre.prog.ProgressionPlanner.Method;
import edu.uky.cs.nil.sabre.search.Planner;
import edu.uky.cs.nil.sabre.search.Result;
import edu.uky.cs.nil.sabre.search.Search;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * A session gathers several important elements together and provides a single
 * way for them to interact with each other: a {@link Parser parser} and {@link
 * Printer printer} to read and write, a {@link Problem problem} being solved, a
 * {@link Planner planner} and its current {@link Search search}, etc.
 * 
 * @author Stephen G. Ware
 */
public class Session {
	
	/** A name for the session's {@link Status status} object */
	protected static final String STATUS = "status";
	
	/** A name for the session's {@link Parser parser} object */
	protected static final String PARSER = "parser";
	
	/** A name for the session's {@link Printer printer} object */
	protected static final String PRINTER = "printer";
	
	/** A name for the session's {@link Problem problem} object */
	protected static final String PROBLEM = "problem";
	
	/**
	 * A name for the session's {@link Planner#compile(Problem, Status)
	 * compiled} {@link Problem problem} object
	 */
	protected static final String COMPILED_PROBLEM = "compiled problem";
	
	/** A name for the session's {@link State state} object */
	protected static final String STATE = "state";
	
	/** A name for the session's {@link Search#getGoal()} setting */
	protected static final String GOAL = "goal";
	
	/** A name for the session's {@link Planner planner} object */
	protected static final String PLANNER = "planner";
	
	/**
	 * A name for the planner's {@link Planner#getSearchLimit() search limit}
	 * setting
	 */
	protected static final String SEARCH_LIMIT = "search limit";
	
	/**
	 * A name for the planner's {@link Planner#getSpaceLimit() space limit}
	 * setting
	 */
	protected static final String SPACE_LIMIT = "space limit";
	
	/**
	 * A name for the planner's {@link Planner#getTimeLimit() time limit}
	 * setting
	 */
	protected static final String TIME_LIMIT = "time limit";
	
	/**
	 * A name for the planner's {@link Planner#getAuthorTemporalLimit() author
	 * temporal limit} setting
	 */
	protected static final String AUTHOR_TEMPORAL_LIMIT = "author temporal limit";
	
	/**
	 * A name for the planner's {@link Planner#getCharacterTemporalLimit()
	 * character temporal limit} setting
	 */
	protected static final String CHARACTER_TEMPORAL_LIMIT = "character temporal limit";
	
	/**
	 * A name for the planner's {@link Planner#getEpistemicLimit() epistemic
	 * limit} setting
	 */
	protected static final String EPISTEMIC_LIMIT = "epistemic limit";
	
	/**
	 * A name for the heuristic progression planner's {@link
	 * ProgressionPlanner.Method method} setting
	 */
	protected static final String METHOD = "search method";
	
	/**
	 * A name for the heuristic progression planner's {@link
	 * ProgressionPlanner#getCost() cost} function
	 */
	protected static final String COST = "cost";
	
	/**
	 * A name for the heuristic progression planner's {@link
	 * ProgressionPlanner#getHeuristic() heuristic} function
	 */
	protected static final String HEURISTIC = "heuristic";
	
	/**
	 * A name for the heuristic progression planner's {@link
	 * ProgressionPlanner#getExplanationPruning() explanation pruning} setting
	 */
	protected static final String EXPLANATION_PRUNING = "explanation pruning";
	
	/** A name for the session's {@link Search search} object */
	protected static final String SEARCH = "search";
	
	/** A name for the search's most recent {@link Result result} object */
	protected static final String RESULT = "result";
	
	/**
	 * A name for whether or not {@link #getResult() the most recent result}
	 * succeeded in solving the problem
	 */
	protected static final String SUCCESS = "success";
	
	/**
	 * A name for the {@link Plan solution} returned {@link #getResult() the
	 * most recent result}
	 */
	protected static final String SOLUTION = "solution";
	
	/**
	 * A name for the author's {@link Plan plan} from the most {@link
	 * #getResult() result}
	 */
	protected static final String PLAN = "plan";
	
	/**
	 * The session's status object, to which status update messages will be
	 * written
	 */
	protected Status status = new Status();
	
	/** The session's parser */
	protected Parser parser;
	
	/** The session's printer */
	protected Printer printer;
	
	/** The session's problem */
	protected Problem problem;
	
	/**
	 * The session's {@link Planner#compile(Problem, Status) compiled} problem
	 */
	protected Problem compiled;
	
	/**
	 * The session's current {@link State state}, from which plans will start
	 */
	protected State state;
	
	/** The session's goal setting */
	protected Number goal;
	
	/** The session's planner */
	protected Planner<?> planner;
	
	/** The session's search */
	protected Search<?> search;
	
	/** The search's most recent result */
	protected Result<?> result;
	
	/**
	 * Constructs a new session with a {@link DefaultParser default parser},
	 * {@link edu.uky.cs.nil.sabre.io.DefaultPrinter default printer}, and a
	 * {@link ProgressionPlanner heuristic progression planner}.
	 */
	public Session() {
		setParser(new DefaultParser());
		setPrinter(Utilities.DEFAULT_PRINTER);
		setPlanner(new ProgressionPlanner());
		status.setMessage("New session started.");
	}
	
	@Override
	public String toString() {
		return "Session: " + getStatus();
	}
	
	/**
	 * Returns the session's current {@link Status status} object, to which
	 * updates are written when the session is modified, when a problem is
	 * being compiled, when the search is running, and during other
	 * time-consuming operations. If the current status has been {@link
	 * #setStatus(Status) set} to null, a new status will be created and
	 * returned.
	 * 
	 * @return the current status
	 */
	public synchronized Status getStatus() {
		if(status == null)
			status = new Status();
		return status;
	}
	
	/**
	 * Sets the session's current {@link Status status} object, to which
	 * updates will be written when the session is modified, when a problem is
	 * being compiled, when the search is running, and during other
	 * time-consuming operations.
	 * 
	 * @param status the new status to use
	 */
	public synchronized void setStatus(Status status) {
		this.status = status;
	}
	
	/**
	 * Returns the session's current {@link Parser parser}, which is used to
	 * read {@link Problem problems}, {@link edu.uky.cs.nil.sabre.logic.Logical
	 * logical formulas}, and other inputs. If the current parser has been
	 * {@link #setParser(Parser) set} to null, a new {@link DefaultParser
	 * default parser} will be created and returned.
	 * 
	 * @return the current parser
	 */
	public synchronized Parser getParser() {
		if(parser == null)
			parser = new DefaultParser();
		return parser;
	}
	
	/**
	 * Sets the session's current {@link Parser parser}.
	 * 
	 * @param parser the new parser to use
	 */
	public synchronized void setParser(Parser parser) {
		this.parser = parser;
		getStatus().setMessage(PARSER + ": " + parser);
	}
	
	/**
	 * Returns the session's current {@link Printer printer}, which is used to
	 * convert {@link Plan plans} and other outputs into text. If the current
	 * printer has been {@link #setPrinter(Printer) set} to null, the {@link
	 * Utilities#DEFAULT_PRINTER default printer} will be returned.
	 * 
	 * @return the current printer
	 */
	public synchronized Printer getPrinter() {
		if(printer == null)
			printer = Utilities.DEFAULT_PRINTER;
		return printer;
	}
	
	/**
	 * Sets the session's current printer.
	 * 
	 * @param printer the new printer to use
	 */
	public synchronized void setPrinter(Printer printer) {
		this.printer = printer;
		getStatus().setMessage(PRINTER + ": " + printer);
	}
	
	/**
	 * Returns the session's current {@link Problem} problem that the planner
	 * will solve.
	 * 
	 * @return the current problem
	 * @throws IllegalStateException if the problem has not been {@link
	 * #setProblem(Problem) set}
	 */
	public synchronized Problem getProblem() {
		return notNull(problem, PROBLEM);
	}
	
	/**
	 * Sets the session's current {@link Problem problem} and removes the
	 * session's {@link #getCompiledProblem() current compiled problem}, {@link
	 * #getState() current state}, {@link #getGoal() current goal}, {@link
	 * #getSearch() current search}, {@link #getResult() latest result}, etc.
	 * 
	 * @param problem the new problem to solve
	 */
	public synchronized void setProblem(Problem problem) {
		this.problem = problem;
		setState(null);
		setGoal(null);
		setCompiledProblem(null);
		getStatus().setMessage(PROBLEM + ": " + (problem == null ? null : problem.name));
	}
	
	/**
	 * {@link #setProblem(Problem) Sets} the session's current {@link Problem
	 * problem} after parsing a given file using the session's {@link
	 * #getParser() parser}.
	 * 
	 * @param file the problem file to be parsed
	 * @throws IOException if an exception occurs while reading the file
	 * @throws ParseException if an exception occurs while parsing the file
	 */
	public synchronized void setProblem(File file) throws IOException, ParseException {
		setProblem(getParser().parse(file, Problem.class));
	}
	
	/**
	 * Returns the session's current {@link Planner#compile(Problem, Status)
	 * compiled} {@link Problem problem}. If the {@link #getProblem() current
	 * problem} has not yet been compiled, it will be compiled.
	 * 
	 * @return the problem, pre-processed by the planner
	 * @throws IllegalStateException if the {@link #getProblem() problem} is not
	 * set
	 * @throws IllegalStateException if the {@link #getPlanner() planner} is not
	 * set
	 */
	public synchronized Problem getCompiledProblem() {
		if(compiled == null)
			compiled = getPlanner().compile(getProblem(), getStatus());
		return compiled;
	}
	
	/**
	 * Sets the session's current compiled problem and removes the session's
	 * {@link #getState() current state}, {@link #getSearch() current search},
	 * {@link #getResult() latest result}, etc. This method does not remove the
	 * {@link #getProblem() current (uncompiled) problem}; it should only be
	 * called with a compiled version of the {@link #getProblem() current
	 * problem}. This method does not remove the {@link #getGoal() current
	 * goal}, because even through the initial state may change as a result
	 * of compiling the problem differently, the goal may not change.
	 * 
	 * @param compiled the new compiled problem to solve
	 */
	public synchronized void setCompiledProblem(Problem compiled) {
		this.compiled = compiled;
		setState(null);
		setSearch(null);
		getStatus().setMessage(COMPILED_PROBLEM + ": " + (problem == null ? null : problem.name));
	}
	
	/**
	 * Returns the state in which planning will start. If the state has not
	 * been set, the {@link #getProblem() problem's} {@link Problem#initial
	 * initial state} will used.
	 * 
	 * @return the state in which planning will start
	 * @throws IllegalStateException if the initial state is not set and
	 * cannot not be calculated
	 */
	public synchronized State getState() {
		if(state == null) {
			Problem compiled = getCompiledProblem();
			if(compiled instanceof CompiledProblem)
				state = ((CompiledProblem) compiled).start;
		}
		return notNull(state, STATE);
	}
	
	/**
	 * Sets the session's current {@link State state} (the state from which
	 * planning will begin). If the session has a {@link #getSearch() current
	 * search}, its {@link Search#setStart(State) start state} will be updated.
	 * 
	 * @param state the new state to start in
	 */
	public synchronized void setState(State state) {
		this.state = state;
		if(search != null)
			search.setStart(state);
		getStatus().setMessage(STATE + ": " + state);
	}
	
	/**
	 * Returns the current {@link Search#getGoal() goal}, the author utility
	 * that a plan must achieve to be considered a solution. If the goal is not
	 * set, a default value is used, which is the {@link Problem#utility
	 * author's utility} in the {@link #getState() current state} rounded up to
	 * the nearest whole number, or the author's utility +1 if the utility is
	 * already a whole number.
	 * 
	 * @return the current goal
	 * @throws IllegalStateException if the goal is not set and cannot be
	 * calculated
	 */
	public synchronized Number getGoal() {
		if(goal == null) {
			Value goal = getCompiledProblem().utility.evaluate(getState());
			if(goal.equals(Unknown.UNKNOWN))
				goal = Number.NEGATIVE_INFINITY;
			this.goal = (Number) goal;
			double rounded = Utilities.roundUp(this.goal.value);
			if(rounded > this.goal.value)
				this.goal = Number.get(rounded);
			else
				this.goal = (Number) Arithmetic.ADD.calculate(this.goal, Number.ONE);
		}
		return notNull(goal, GOAL);
	}
	
	/**
	 * Sets the session's current {@link Search#getGoal() goal}. If the session
	 * has a {@link #getSearch() current search}, its {@link
	 * Search#setGoal(Number) goal} will updated.
	 * 
	 * @param goal the new author utility plans must reach to be considered a
	 * solution
	 */
	public synchronized void setGoal(Number goal) {
		this.goal = goal;
		if(search != null)
			search.setGoal(goal);
		getStatus().setMessage(GOAL + ": " + goal);
	}
	
	/**
	 * {@link #setGoal(double) Sets} the session's {@link #getGoal() current
	 * goal} using a Java {@code double}.
	 * 
	 * @param goal the new author utility plans must reach to be considered a
	 * solution
	 */
	public synchronized void setGoal(double goal) {
		setGoal(Number.get(goal));
	}
	
	/**
	 * Returns the session's current {@link Planner planner}.
	 * 
	 * @return the current planner
	 */
	public synchronized Planner<?> getPlanner() {
		return notNull(planner, PLANNER);
	}
	
	/**
	 * Sets the session's current {@link Planner planner} and removes the
	 * session's current {@link #getCompiledProblem() compiled problem}, {@link
	 * #getSearch() current search}, {@link #getResult() latest result}, etc.
	 * 
	 * @param planner the new planner to use
	 */
	public synchronized void setPlanner(Planner<?> planner) {
		this.planner = planner;
		setCompiledProblem(null);
		getStatus().setMessage(PLANNER + ": " + planner.name);
	}
	
	/**
	 * Returns the {@link #getPlanner() current planner's} {@link
	 * Planner#getSearchLimit() search limit} setting.
	 * 
	 * @return the current planner's search limit
	 */
	public synchronized long getSearchLimit() {
		return getPlanner().getSearchLimit();
	}
	
	/**
	 * Sets the {@link #getPlanner() current planner's} {@link
	 * Planner#setSearchLimit(long) search limit} and removes the {@link
	 * #getSearch() current search}, {@link #getResult() most recent result},
	 * etc.
	 * 
	 * @param limit the new search limit to use
	 */
	public synchronized void setSearchLimit(long limit) {
		getPlanner().setSearchLimit(limit);
		setSearch(null);
		getStatus().setMessage(SEARCH_LIMIT + ": " + limit);
	}
	
	/**
	 * Returns the {@link #getPlanner() current planner's} {@link
	 * Planner#getSpaceLimit() space limit} setting.
	 * 
	 * @return the current planner's space limit
	 */
	public synchronized long getSpaceLimit() {
		return getPlanner().getSpaceLimit();
	}
	
	/**
	 * Sets the {@link #getPlanner() current planner's} {@link
	 * Planner#setSpaceLimit(long) space limit} and removes the {@link
	 * #getSearch() current search}, {@link #getResult() most recent result},
	 * etc.
	 * 
	 * @param limit the new space limit to use
	 */
	public synchronized void setSpaceLimit(long limit) {
		getPlanner().setSpaceLimit(limit);
		setSearch(null);
		getStatus().setMessage(SPACE_LIMIT + ": " + limit);
	}
	
	/**
	 * Returns the {@link #getPlanner() current planner's} {@link
	 * Planner#getTimeLimit() time limit} setting.
	 * 
	 * @return the current planner's time limit
	 */
	public synchronized long getTimeLimit() {
		return getPlanner().getTimeLimit();
	}
	
	/**
	 * Sets the {@link #getPlanner() current planner's} {@link
	 * Planner#setTimeLimit(long) time limit} and removes the {@link
	 * #getSearch() current search}, {@link #getResult() most recent result},
	 * etc.
	 * 
	 * @param limit the new time limit to use
	 */
	public synchronized void setTimeLimit(long limit) {
		getPlanner().setTimeLimit(limit);
		setSearch(null);
		getStatus().setMessage(TIME_LIMIT + ": " + limit);
	}
	
	/**
	 * Returns the {@link #getPlanner() current planner's} {@link
	 * Planner#getAuthorTemporalLimit() author temporal limit} setting.
	 * 
	 * @return the current planner's author temporal limit
	 */
	public synchronized int getAuthorTemporalLimit() {
		return getPlanner().getAuthorTemporalLimit();
	}
	
	/**
	 * Sets the {@link #getPlanner() current planner's} {@link
	 * Planner#setAuthorTemporalLimit(int) author temporal limit} and removes the
	 * {@link #getSearch() current search}, {@link #getResult() most recent
	 * result}, etc.
	 * 
	 * @param limit the new author temporal limit to use
	 */
	public synchronized void setAuthorTemporalLimit(int limit) {
		getPlanner().setAuthorTemporalLimit(limit);
		setSearch(null);
		getStatus().setMessage(AUTHOR_TEMPORAL_LIMIT + ": " + limit);
	}
	
	/**
	 * Returns the {@link #getPlanner() current planner's} {@link
	 * Planner#getCharacterTemporalLimit() character temporal limit} setting.
	 * 
	 * @return the current planner's character temporal limit
	 */
	public synchronized int getCharacterTemporalLimit() {
		return getPlanner().getCharacterTemporalLimit();
	}
	
	/**
	 * Sets the {@link #getPlanner() current planner's} {@link
	 * Planner#setCharacterTemporalLimit(int) character temporal limit} and
	 * removes the {@link #getSearch() current search}, {@link #getResult() most
	 * recent result}, etc.
	 * 
	 * @param limit the new character temporal limit to use
	 */
	public synchronized void setCharacterTemporalLimit(int limit) {
		getPlanner().setCharacterTemporalLimit(limit);
		setSearch(null);
		getStatus().setMessage(CHARACTER_TEMPORAL_LIMIT + ": " + limit);
	}
	
	/**
	 * Returns the {@link #getPlanner() current planner's} {@link
	 * Planner#getEpistemicLimit() epistemic limit} setting.
	 * 
	 * @return the current planner's epistemic limit
	 */
	public synchronized int getEpistemicLimit() {
		return getPlanner().getEpistemicLimit();
	}
	
	/**
	 * Sets the {@link #getPlanner() current planner's} {@link
	 * Planner#setEpistemicLimit(int) epistemic limit} and removes the {@link
	 * #getSearch() current search}, {@link #getResult() most recent result},
	 * etc.
	 * 
	 * @param limit the new epistemic limit to use
	 */
	public synchronized void setEpistemicLimit(int limit) {
		getPlanner().setEpistemicLimit(limit);
		setSearch(null);
		getStatus().setMessage(EPISTEMIC_LIMIT + ": " + limit);
	}
	
	private ProgressionPlanner pp() {
		return cast(getPlanner(), PLANNER, ProgressionPlanner.class, "a heuristic progression planner");
	}
	
	/**
	 * Returns the {@link Method search method} used by a {@link
	 * ProgressionPlanner heuristic progression planner}, if the {@link
	 * #getPlanner() current planner} is that type.
	 * 
	 * @return the search method
	 * @throws IllegalStateException if the current planner is not a heuristic
	 * progression planner
	 */
	public synchronized Method getMethod() {
		return pp().getMethod();
	}
	
	/**
	 * Sets the {@link Method search method} used by a {@link
	 * ProgressionPlanner heuristic progression planner}, if the {@link
	 * #getPlanner() current planner} is that type.
	 * 
	 * @param method the new search method to use
	 * @throws IllegalStateException if the current planner is not a heuristic
	 * progression planner
	 */
	public synchronized void setMethod(Method method) {
		pp().setMethod(method);
		setSearch(null);
		getStatus().setMessage(METHOD + ": " + method);
	}
	
	/**
	 * Returns the {@link ProgressionCostFactory cost function} used by a {@link
	 * ProgressionPlanner heuristic progression planner}, if the {@link
	 * #getPlanner() current planner} is that type.
	 * 
	 * @return the cost function
	 * @throws IllegalStateException if the current planner is not a heuristic
	 * progression planner
	 */
	public synchronized ProgressionCostFactory getCost() {
		return pp().getCost();
	}
	
	/**
	 * Sets the {@link ProgressionCostFactory cost function} used by a {@link
	 * ProgressionPlanner heuristic progression planner}, if the {@link
	 * #getPlanner() current planner} is that type.
	 * 
	 * @param cost the new cost function to use
	 * @throws IllegalStateException if the current planner is not a heuristic
	 * progression planner
	 */
	public synchronized void setCost(ProgressionCostFactory cost) {
		pp().setCost(cost);
		setSearch(null);
		getStatus().setMessage(COST + ": " + cost);
	}
	
	/**
	 * Returns the {@link ProgressionCostFactory heuristic function} used by a
	 * {@link ProgressionPlanner heuristic progression planner}, if the {@link
	 * #getPlanner() current planner} is that type.
	 * 
	 * @return the heuristic function
	 * @throws IllegalStateException if the current planner is not a heuristic
	 * progression planner
	 */
	public synchronized ProgressionCostFactory getHeuristic() {
		return pp().getHeuristic();
	}
	
	/**
	 * Sets the {@link ProgressionCostFactory heuristic function} used by a
	 * {@link ProgressionPlanner heuristic progression planner}, if the {@link
	 * #getPlanner() current planner} is that type.
	 * 
	 * @param heuristic the new heuristic function to use
	 * @throws IllegalStateException if the current planner is not a heuristic
	 * progression planner
	 */
	public synchronized void setHeuristic(ProgressionCostFactory heuristic) {
		pp().setHeuristic(heuristic);
		setSearch(null);
		getStatus().setMessage(HEURISTIC + ": " + heuristic);
	}
	
	/**
	 * Returns the {@link ProgressionPlanner#getExplanationPruning() explanation
	 * pruning} setting used by a {@link ProgressionPlanner heuristic
	 * progression planner}, if the {@link #getPlanner() current planner} is
	 * that type.
	 * 
	 * @return whether explanation pruning will be used
	 * @throws IllegalStateException if the current planner is not a heuristic
	 * progression planner
	 */
	public synchronized boolean getExplanationPruning() {
		return pp().getExplanationPruning();
	}
	
	/**
	 * Sets the {@link ProgressionPlanner#getExplanationPruning() explanation
	 * pruning} setting used by a {@link ProgressionPlanner heuristic
	 * progression planner}, if the {@link #getPlanner() current planner} is
	 * that type.
	 * 
	 * @param value whether explanation pruning should be used
	 * @throws IllegalStateException if the current planner is not a heuristic
	 * progression planner
	 */
	public synchronized void setExplanationPruning(boolean value) {
		pp().setExplanationPruning(value);
		setSearch(null);
		getStatus().setMessage(EXPLANATION_PRUNING + ": " + value);
	}
	
	/**
	 * Returns the session's current {@link Search search}, which was created
	 * by the session's {@link #getPlanner() planner} for the session's {@link
	 * #getProblem() problem}. If there is no current search, one will be
	 * created.
	 * 
	 * @return the current search
	 * @throws IllegalStateException if a new search needs to be created, but
	 * the problem, state, goal, or planner are not set
	 */
	public synchronized Search<?> getSearch() {
		if(search == null) {
			Planner<?> planner = getPlanner();
			Problem problem = getCompiledProblem();
			State state = getState();
			Number goal = getGoal();
			search = planner.getSearch(problem, getStatus());
			search.setStart(state);
			search.setGoal(goal);
			setResult(null);
			getStatus().setMessage("Search created for problem \"" + problem.name + "\".");
		}
		return search;
	}
	
	/**
	 * Sets the session's current {@link Search search}, and removes the {@link
	 * #getResult() most recent result}, etc. This method is protected by
	 * default, and is only used to unset the current search (i.e. set the
	 * current search to null). If this method is given public access, it should
	 * update all of the session's other settings, such as the {@link
	 * #getProblem() current problem}, {@link #getSearchLimit() current search
	 * limit}, etc.
	 * 
	 * @param search the new search to use
	 */
	public synchronized void setSearch(Search<?> search) {
		this.search = search;
		setResult(null);
		getStatus().setMessage(SEARCH + ": " + search);
	}
	
	/**
	 * Returns the {@link Result result} of the most recent {@link
	 * Search#get(Status) run} of the session's {@link #getSearch() current
	 * search}. If the search has not been created, it will be created. If it
	 * has not been run, it will be run.
	 * 
	 * @return the result of the last search
	 * @throws IllegalStateException if the search needs to be run but it {@link
	 * #getSearch() cannot be created}
	 */
	public synchronized Result<?> getResult() {
		if(result == null)
			result = getSearch().get(getStatus());
		return result;
	}
	
	/**
	 * Sets the {@link Result result} of the most recent {@link
	 * Search#get(Status) run} of the {@link #getSearch() current search}. This
	 * method is protected by default, and is only used to unset the current
	 * result or to set it to a result of the current search. If this method is
	 * given public access, it should update all of the session's other
	 * settings, such as the {@link #getSearch() current search} {@link
	 * #getProblem() current problem}, {@link #getSearchLimit() current search
	 * limit}, etc.
	 * 
	 * @param result the new most recent result
	 */
	protected synchronized void setResult(Result<?> result) {
		this.result = result;
		getStatus().setMessage(RESULT + ": " + result);
	}
	
	/**
	 * Indicates whether or not the {@link #getSearch() most recent search}
	 * succeeded in finding a plan that meets or exceeds {@link #getGoal() the
	 * author's goal utility}. If a search finds a plan which improves the
	 * author's utility, but not up to the goal, it may still return that plan.
	 * This means that {@link #getSolution()} may still return a plan, even if
	 * this method returns false. If the search has not been created, it will be
	 * created. If it has not been run, it will be run.
	 * 
	 * @return true if the most recent search returns a result whose plan meets
	 * or exceeds the author's goal utility
	 * @throws IllegalStateException if the search needs to be run but it {@link
	 * #getSearch() cannot be created}
	 */
	public synchronized boolean getSuccess() {
		Result<?> result = getResult();
		return result.solution != null && Comparison.GREATER_THAN_OR_EQUAL_TO.test(result.utility, result.goal);
	}
	
	/**
	 * Returns the {@link Result#solution solution} from the session's current
	 * {@link #getResult() result}. If the search has not been created, it will
	 * be created. If it has not been run, it will be run.
	 * 
	 * @return the solution from the current result
	 * @throws IllegalStateException if the search needs to be run but it {@link
	 * #getSearch() cannot be created}
	 */
	public synchronized Solution<?> getSolution() {
		Result<?> result = getResult();
		return notNull(result.solution, SOLUTION);
	}
	
	/**
	 * Returns the author's {@link Plan plan} (that is, just the actions the
	 * author intends) from the session's {@link #getResult() current result}.
	 * If the search has not been created, it will be created. If it has not
	 * been run, it will be run.
	 * 
	 * @return a sequence of actions that can be taken in the current state that
	 * will improve the author's utility
	 * @throws IllegalStateException if the search needs to be run but it {@link
	 * #getSearch() cannot be created}
	 */
	public synchronized Plan<?> getPlan() {
		return getPlan(null);
	}
	
	/**
	 * Returns the current {@link Plan plan} for a given {@link Character
	 * character} from the session's {@link #getResult() current result}. If the
	 * search has not been created, it will be created. If it has not been run,
	 * it will be run.
	 * 
	 * @param character the character whose plan is desired, or null if the
	 * author's plan is desired
	 * @return a sequence of actions the given character believes can be taken
	 * from the current state to improve their utility
	 * @throws IllegalStateException if the search needs to be run but it {@link
	 * #getSearch() cannot be created}, or if the current result's solution is
	 * not a solution object
	 */
	public synchronized Plan<?> getPlan(Character character) {
		Plan<?> plan = getSolution();
		if(character != null)
			plan = cast(plan, PLAN, Solution.class, SOLUTION).getExplanation(character);
		TailPlan<Action> result = TailPlan.EMPTY;
		if(plan != null)
			for(Action action : plan)
				result = result.append(action);
		return result;
	}
	
	/**
	 * Throws an exception if the given object is null.
	 * 
	 * @param <T> the object
	 * @param object the object which should not be null
	 * @param name the object's name, which will be used in the message of the
	 * exception if the object is null
	 * @return the object, if it is not null
	 * @throws IllegalStateException if the object is null
	 */
	protected <T> T notNull(T object, String name) {
		if(object == null)
			throw Exceptions.notSet(name);
		return object;
	}
	
	/**
	 * Throws an exception if the given object cannot be cast to a desired
	 * class.
	 * 
	 * @param <T> the class to which the object should be cast
	 * @param object the object to cast
	 * @param objectName the object's name, which will be used in the message of
	 * the exception if the object cannot be cast
	 * @param type the class object of the class to which the object will be
	 * cast
	 * @param typeName the name of the class to which the object will be cast,
	 * which will be used in the message of the exception if the object cannot
	 * be cast
	 * @return the object, cast to the given class
	 * @throws IllegalStateException if the object cannot be cast
	 */
	protected <T> T cast(Object object, String objectName, Class<T> type, String typeName) {
		if(type.isAssignableFrom(object.getClass()))
			return type.cast(object);
		else
			throw Exceptions.wrongType(objectName, typeName);
	}
}