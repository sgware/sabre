package edu.uky.cs.nil.sabre;

import java.lang.reflect.Array;

import edu.uky.cs.nil.sabre.prog.GraphHeuristic;
import edu.uky.cs.nil.sabre.prog.ProgressionCostFactory;
import edu.uky.cs.nil.sabre.prog.ProgressionPlanner;
import edu.uky.cs.nil.sabre.prog.ProgressionPlanner.Method;
import edu.uky.cs.nil.sabre.prog.ReachabilityHeuristic;
import edu.uky.cs.nil.sabre.prog.RelaxedPlanHeuristic;
import edu.uky.cs.nil.sabre.prog.WeightedCost;
import edu.uky.cs.nil.sabre.search.Planner;
import edu.uky.cs.nil.sabre.search.Result;
import edu.uky.cs.nil.sabre.search.Search;
import edu.uky.cs.nil.sabre.util.CommandLineArguments;
import edu.uky.cs.nil.sabre.util.Worker;

/**
 * Configures a {@link Session session} according to command line arguments,
 * runs a {@link Search search}, and prints the {@link Result result} to
 * standard output.
 * 
 * @author Stephen G. Ware
 */
public class Main {

	/** The command line key for the usage message */
	public static final String HELP_KEY = "-help";
	
	/** The command line key for verbose output */
	public static final String VERBOSE_KEY = "-v";
	
	/** The command line key for the problem file URL */
	public static final String PROBLEM_KEY = "-p";
	
	/** The command line key for the goal utility */
	public static final String GOAL_KEY = "-g";
	
	/** The command line key for the {@link Planner#getSearchLimit() search limit} */
	public static final String SEARCH_LIMIT_KEY = "-vl";
	
	/** The command line key for the {@link Planner#getSpaceLimit() space limit} */
	public static final String SPACE_LIMIT_KEY = "-gl";
	
	/** The command line key for the {@link Planner#getTimeLimit() time limit} */
	public static final String TIME_LIMIT_KEY = "-tl";
	
	/**
	 * The command line key for the {@link Planner#getAuthorTemporalLimit()
	 * author temporal limit}
	 */
	public static final String AUTHOR_TEMPORAL_LIMIT_KEY = "-atl";
	
	/**
	 * The command line key for the {@link Planner#getCharacterTemporalLimit()
	 * character temporal limit}
	 */
	public static final String CHARACTER_TEMPORAL_LIMIT_KEY = "-ctl";
	
	/**
	 * The command line key for the {@link Planner#getEpistemicLimit() epistemic
	 * limit}
	 */
	public static final String EPISTEMIC_LIMIT_KEY = "-el";
	
	/**
	 * The command line key for the {@link ProgressionPlanner#getMethod() search
	 * method}
	 */
	public static final String METHOD_KEY = "-m";
	
	/**
	 * The command line key for the {@link ProgressionPlanner#getCost() cost
	 * function}
	 */
	public static final String COST_KEY = "-c";
	
	/**
	 * The command line key for the {@link WeightedCost weight} to apply to the
	 * cost function
	 */
	public static final String COST_WEIGHT_KEY = "-cw";
	
	/**
	 * The command line key for the {@link ProgressionPlanner#getHeuristic()
	 * heuristic function}
	 */
	public static final String HEURISTIC_KEY = "-h";
	
	/**
	 * The command line key for the {@link WeightedCost weight} to apply to the
	 * heuristic function
	 */
	public static final String HEURISTIC_WEIGHT_KEY = "-hw";
	
	/**
	 * The command line key for {@link
	 * ProgressionPlanner#getExplanationPruning() explanation pruning}
	 */
	public static final String EXPLANATION_PRUNING_KEY = "-ep";
	
	/**
	 * The abbreviation for {@link ProgressionPlanner.Method#BEST_FIRST
	 * best-first search}
	 */
	public static final String BEST_FIRST_OPTION = "bf";
	
	/**
	 * The abbreviation for {@link ProgressionPlanner.Method#EXPLANATION_FIRST
	 * explanation-first search}
	 */
	public static final String EXPLANATION_FIRST_OPTION = "ef";
	
	/**
	 * The abbreviation for {@link ProgressionPlanner.Method#GOAL_FIRST
	 * goal-first search}
	 */
	public static final String GOAL_FIRST_OPTION = "gf";
	
	/**
	 * The abbreviation for {@link
	 * edu.uky.cs.nil.sabre.prog.ProgressionCost#ZERO the zero cost function}
	 */
	public static final String ZERO_COST_OPTION = "0";
	
	/**
	 * The abbreviation for {@link
	 * edu.uky.cs.nil.sabre.prog.ProgressionCost#PLAN_SIZE the plan size
	 * function}
	 */
	public static final String PLAN_SIZE_COST_OPTION = "n";
	
	/**
	 * The abbreviation for {@link
	 * edu.uky.cs.nil.sabre.prog.ProgressionCost#TEMPORAL the temporal depth
	 * function} */
	public static final String TEMPORAL_COST_OPTION = "t";
	
	/**
	 * The abbreviation for {@link
	 * edu.uky.cs.nil.sabre.prog.ProgressionCost#ZERO the zero heuristic}
	 */
	public static final String ZERO_HEURISTIC_OPTION = "0";
	
	/**
	 * The abbreviation for {@link ReachabilityHeuristic the reachability
	 * heuristic}
	 */
	public static final String REACHABILITY_HEURISTIC_OPTION = "r";
	
	/**
	 * The abbreviation for {@link GraphHeuristic.MaxGraphHeuristic the max
	 * graph heuristic}
	 */
	public static final String MAX_HEURISTIC_OPTION = "hmax";
	
	/**
	 * The abbreviation for {@link GraphHeuristic.SumGraphHeuristic the sum
	 * graph heuristic}
	 */
	public static final String SUM_HEURISTIC_OPTION = "h+";
	
	/**
	 * The abbreviation for {@link RelaxedPlanHeuristic the relaxed plan
	 * heuristic}
	 */
	public static final String RELAXED_PLAN_HEURISTIC_OPTION = "rp";
	
	private static final String pad(String string) {
		return String.format("%-13s", string);
	}
	
	/**
	 * A string explaining the command line arguments that can be passed to
	 * {@link #main(String[]) the main method}
	 */
	public static final String USAGE =
		Settings.CREDITS + "\n" +
		pad(HELP_KEY) +									"print this message and halt\n" +
		pad(VERBOSE_KEY) +								"verbose output gives details of the search and results\n" +
		pad(PROBLEM_KEY + " PATH") +					"problem file to parse\n" +
		pad(GOAL_KEY + " NUMBER") +						"utility a solution much reach (default round up or +1)\n" +
		pad(METHOD_KEY + " OPTION") +					"heuristic search method; options include:\n" +
		pad("   " + BEST_FIRST_OPTION) +				"A* best-first (default)\n" +
		pad("   " + EXPLANATION_FIRST_OPTION) +			"explanation-first: explain actions before achieving the goal\n" +
		pad("   " + GOAL_FIRST_OPTION) +				"goal-first: achieve the goal before explaining actions\n" +
		pad(COST_KEY + " OPTION") +						"how plan cost is measured; options include:\n" +
		pad("   " + ZERO_COST_OPTION) +					"always zero\n" +
		pad("   " + PLAN_SIZE_COST_OPTION) +			"number of actions in the plan\n" +
		pad("   " + TEMPORAL_COST_OPTION) +				"number of actions before the plan and in the plan (default)\n" +
		pad(COST_WEIGHT_KEY + " NUMBER") +				"a weight to multiply the cost by\n" +
		pad(HEURISTIC_KEY + " OPTION") +				"how distance to the goal is estimated; options include:\n" +
		pad("   " + ZERO_HEURISTIC_OPTION) +			"always zero\n" +
		pad("   " + REACHABILITY_HEURISTIC_OPTION) +	"prevent some searches with impossible goals\n" +
		pad("   " + MAX_HEURISTIC_OPTION) +				"estimate cost when a conjunction costs the max of its arguments\n" +
		pad("   " + SUM_HEURISTIC_OPTION) +				"estimate cost when a conjunction costs the sum of its arguments\n" +
		pad("   " + RELAXED_PLAN_HEURISTIC_OPTION) +	"build a relaxed plan to approximate the solution (default)\n" +
		pad(HEURISTIC_WEIGHT_KEY + " NUMBER") +			"a weight to multiply the heuristic by\n" +
		pad(EXPLANATION_PRUNING_KEY + " {y|n}") +		"once one explanation has been found for an action, do not search for more (default y)\n" +
		pad(SEARCH_LIMIT_KEY + " NUMBER") +				"max nodes the search can visit; " + Planner.UNLIMITED_NODES + " for unlimited (default " + Planner.UNLIMITED_NODES + ")\n" +
		pad(SPACE_LIMIT_KEY + " NUMBER") +				"max nodes the search can generate; " + Planner.UNLIMITED_NODES + " for unlimited (default " + Planner.UNLIMITED_NODES + ")\n" +
		pad(TIME_LIMIT_KEY + " NUMBER") +				"max milliseconds the search can run; " + Planner.UNLIMITED_TIME + " for unlimited (default " + Planner.UNLIMITED_TIME + ")\n" +
		pad(AUTHOR_TEMPORAL_LIMIT_KEY + " NUMBER") +	"max actions in a plan; " + Planner.UNLIMITED_DEPTH + " for unlimited (default " + Planner.UNLIMITED_DEPTH + ")\n" +
		pad(CHARACTER_TEMPORAL_LIMIT_KEY + " NUMBER") +	"max actions in a character's explanation for an action; " + Planner.UNLIMITED_DEPTH + " for unlimited (default " + Planner.UNLIMITED_DEPTH + ")\n" +
		pad(EPISTEMIC_LIMIT_KEY + " NUMBER") +			"max depth to explore theory of mind; " + Planner.UNLIMITED_DEPTH + " for unlimited (default " + Planner.UNLIMITED_DEPTH + ")";
	
	/**
	 * A functional interface for changing a setting in a {@link Session
	 * session}.
	 * 
	 * @param <T> the type of the value that will be set
	 * @author Stephen G. Ware
	 */
	@FunctionalInterface
	private interface Setter<T> {
		
		/**
		 * Calls the appropriate setter function for the given session with the
		 * given value.
		 * 
		 * @param session the session where the setting will be set
		 * @param value the value to set
		 * @throws Exception if an exception is thrown by the session while
		 * setting the value
		 */
		public void set(Session session, T value) throws Exception;
	}
	
	/**
	 * Holds a list of named options that are the possible values that can be
	 * passed to a setter function in a {@link Session session}.
	 * 
	 * @param <T> the type of value that will be set
	 * @author Stephen G. Ware
	 */
	private static final class OptionSetter<T> {
		
		/** The command line argument key whose options this setter defines */
		public final String key;
		
		/**
		 * The list of names for each option; the object is in the corresponding
		 * index in {@link #values}
		 */
		public final String[] codes;
		
		/**
		 * The list of values; the name is in the corresponding index in {@link
		 * #codes}
		 */
		public final T[] values;
		
		/**
		 * A function that calls the appropriate setter function in the session
		 */
		private final Setter<T> setter;
		
		/**
		 * Constructs a new option setter.
		 * 
		 * @param key the command line argument key whose options this setter
		 * defines
		 * @param type the type of value that will be set
		 * @param setter a function that calls the appropriate setter function
		 * in a session
		 * @param options an alternating list of names and values; the first and
		 * every odd numbered index should be a string; the second and every
		 * even numbered index should be a value
		 */
		@SuppressWarnings("unchecked")
		public OptionSetter(String key, Class<T> type, Setter<T> setter, Object...options) {
			this.key = key;
			this.setter = setter;
			this.codes = new String[options.length / 2];
			this.values = (T[]) Array.newInstance(type, options.length / 2);
			for(int i=0; i<options.length; i+=2) {
				this.codes[i / 2] = (String) options[i];
				this.values[i / 2] = type.cast(options[i + 1]);
			}
		}
		
		/**
		 * Parses the name of the option and passes the corresponding value to
		 * the {@link #setter setter}.
		 * 
		 * @param session the session where the setter function will be called
		 * @param code the name of an option
		 * @throws Exception if an exception is thrown by the session while
		 * setting the value
		 */
		public void set(Session session, String code) throws Exception {
			for(int i=0; i<codes.length; i++) {
				if(codes[i].equalsIgnoreCase(code)) {
					setter.set(session, values[i]);
					return;
				}
			}
			throw Exceptions.failedToParseCommandLineArgument(key, code);
		}
	}
	
	/**
	 * The options for {@link ProgressionPlanner#getMethod() a heuristic
	 * progression tree planner's search method}
	 */
	private static final OptionSetter<Method> METHOD_OPTIONS = new OptionSetter<>(
		METHOD_KEY, Method.class,
		(s, v) -> s.setMethod(v),
		BEST_FIRST_OPTION, Method.BEST_FIRST,
		EXPLANATION_FIRST_OPTION, Method.EXPLANATION_FIRST,
		GOAL_FIRST_OPTION, Method.GOAL_FIRST
	);
	
	/**
	 * The options for {@link ProgressionPlanner#getCost() a heuristic
	 * progression tree planner's cost function}
	 */
	private static final OptionSetter<ProgressionCostFactory> COST_OPTIONS = new OptionSetter<>(
		COST_KEY, ProgressionCostFactory.class,
		(s, v) -> s.setCost(v),
		TEMPORAL_COST_OPTION, ProgressionCostFactory.TEMPORAL,
		PLAN_SIZE_COST_OPTION, ProgressionCostFactory.PLAN_SIZE,
		ZERO_COST_OPTION, ProgressionCostFactory.ZERO
	);
	
	/**
	 * The options for {@link HeuristicProgressionTreePlanner#getHeuristic() a
	 * heuristic progression tree planner's heuristic function}
	 */
	private static final OptionSetter<ProgressionCostFactory> HEURISTIC_OPTIONS = new OptionSetter<>(
		HEURISTIC_KEY, ProgressionCostFactory.class,
		(s, v) -> s.setHeuristic(v),
		RELAXED_PLAN_HEURISTIC_OPTION, RelaxedPlanHeuristic.FACTORY,
		SUM_HEURISTIC_OPTION, GraphHeuristic.SUM,
		MAX_HEURISTIC_OPTION, GraphHeuristic.MAX,
		REACHABILITY_HEURISTIC_OPTION, ReachabilityHeuristic.FACTORY,
		ZERO_COST_OPTION, ProgressionCostFactory.ZERO
	);
	
	/**
	 * Configures a {@link Session session} according to command line arguments,
	 * runs a {@link Search search}, and prints the {@link Result result} to
	 * standard output.
	 * 
	 * @param args the command line arguments passed to the program
	 */
	public static void main(String[] args) {
		try {
			// Print help.
			CommandLineArguments arguments = new CommandLineArguments(args);
			if(args.length == 0 || arguments.contains(HELP_KEY)) {
				System.out.println(USAGE);
				return;
			}
			// Configure session according to command line arguments.
			Session session = new Session();
			boolean verbose = arguments.contains(VERBOSE_KEY);
			if(verbose)
				System.out.println(Settings.CREDITS);
			configure(session, arguments, verbose);
			arguments.checkUnused();
			// Run planner.
			Result<?> result;
			if(verbose)
				result = Worker.get(s -> session.getResult(), session.getStatus());
			else
				result = session.getResult();
			// Print result.
			if(verbose)
				System.out.println(session.getPrinter().toString(result));
			else if(result.solution == null)
				System.out.println(result.message);
			else
				System.out.println(session.getPrinter().toString(session.getPlan(null)));
		}
		catch(Throwable t) {
			if(t instanceof RuntimeException && t.getCause() != null)
				t = t.getCause();
			System.err.println("Error: " + t.getMessage());
		}
	}
	
	/**
	 * Configures the given {@link Session session} according to the {@link
	 * CommandLineArguments command line arguments}.
	 * 
	 * @param session the session object whose settings will be configured
	 * @param arguments the command line arguments describing how to configure
	 * the session
	 * @param verbose if true, details of the problem and search will be printed
	 * to standard output while the session is configured
	 * @throws Exception if any required command line arguments are missing or
	 * formatted incorrectly, or if the session throws an exception while it is
	 * being configured (for example, if the parser fails to parse the problem)
	 */
	public static void configure(Session session, CommandLineArguments arguments, boolean verbose) throws Exception {
		// Problem
		arguments.require(PROBLEM_KEY);
		session.setProblem(arguments.getFile(PROBLEM_KEY));
		if(verbose)
			print("Problem", session.getProblem());
		// Compiled Problem
		if(verbose)
			Worker.run(s -> session.getCompiledProblem(), session.getStatus());
		else
			session.getCompiledProblem();
		if(verbose)
			print("Compiled Problem", session.getCompiledProblem());
		// Search
		session.setGoal(arguments.getDouble(GOAL_KEY, session.getGoal().value));
		session.setSearchLimit(arguments.getLong(SEARCH_LIMIT_KEY, Planner.UNLIMITED_NODES));
		session.setSpaceLimit(arguments.getLong(SPACE_LIMIT_KEY, Planner.UNLIMITED_NODES));
		session.setTimeLimit(arguments.getLong(TIME_LIMIT_KEY, Planner.UNLIMITED_TIME));
		session.setAuthorTemporalLimit(arguments.getInt(AUTHOR_TEMPORAL_LIMIT_KEY, Planner.UNLIMITED_DEPTH));
		session.setCharacterTemporalLimit(arguments.getInt(CHARACTER_TEMPORAL_LIMIT_KEY, Planner.UNLIMITED_DEPTH));
		session.setEpistemicLimit(arguments.getInt(EPISTEMIC_LIMIT_KEY, Planner.UNLIMITED_DEPTH));
		if(session.getPlanner() instanceof ProgressionPlanner) {
			METHOD_OPTIONS.set(session, arguments.getOption(METHOD_KEY, METHOD_OPTIONS.codes));
			COST_OPTIONS.set(session, arguments.getOption(COST_KEY, COST_OPTIONS.codes));
			HEURISTIC_OPTIONS.set(session, arguments.getOption(HEURISTIC_KEY, HEURISTIC_OPTIONS.codes));
			if(arguments.contains(COST_WEIGHT_KEY))
				session.setCost(new WeightedCost.Factory(session.getCost(), arguments.getDouble(COST_WEIGHT_KEY, 1)));
			if(arguments.contains(HEURISTIC_WEIGHT_KEY))
				session.setCost(new WeightedCost.Factory(session.getHeuristic(), arguments.getDouble(HEURISTIC_WEIGHT_KEY, 1)));
			session.setExplanationPruning(arguments.getBoolean(EXPLANATION_PRUNING_KEY, true));
		}
		if(verbose)
			Worker.run(s -> session.getSearch(), session.getStatus());
		else
			session.getSearch();
		if(verbose)
			System.out.println(session.getPrinter().toString(session.getSearch()));
	}
	
	/**
	 * Prints summary statistics of a {@link Problem problem} to standard
	 * output if running in verbose mode.
	 * 
	 * @param key the type of problem
	 * @param problem the problem
	 */
	private static final void print(String key, Problem problem) {
		System.out.println(key + ": " + problem.name);
		System.out.println("  characters: " + problem.universe.characters.size());
		System.out.println("  entities:   " + problem.universe.entities.size());
		System.out.println("  fluents:    " + problem.fluents.size());
		System.out.println("  actions:    " + problem.actions.size());
		System.out.println("  triggers:   " + problem.triggers.size());
	}
}