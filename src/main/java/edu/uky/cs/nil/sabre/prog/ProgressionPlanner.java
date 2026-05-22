package edu.uky.cs.nil.sabre.prog;

import edu.uky.cs.nil.sabre.Number;
import edu.uky.cs.nil.sabre.Problem;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.comp.CompiledAction;
import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.comp.FluentExplicitizer;
import edu.uky.cs.nil.sabre.comp.Grounder;
import edu.uky.cs.nil.sabre.comp.Simplifier;
import edu.uky.cs.nil.sabre.etree.EventTree;
import edu.uky.cs.nil.sabre.logic.Unknown;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.ptree.ProgressionTreeSpace;
import edu.uky.cs.nil.sabre.search.Planner;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * A progression {@link Planner planner} is a configurable factory object that
 * creates {@link ProgressionSearch progression search} objects for {@link
 * CompiledProblem compiled problems}.
 * 
 * @author Stephen G. Ware
 */
public class ProgressionPlanner extends Planner<CompiledAction> {
	
	/** The types of progression search */
	public enum Method {
		
		/**
		 * A* search that prioritizes nodes that minimize the sum of their
		 * {@link ProgressionSearch#cost cost} and {@link
		 * ProgressionSearch#heuristic heuristic}
		 */
		BEST_FIRST {
			@Override
			public String toString() {
				return "best-first";
			}
		},
		
		/**
		 * A* search that {@link ExplanationFirstSearch requires every action to
		 * be explained for other characters before it is explained for the
		 * node's character}
		 */
		EXPLANATION_FIRST {
			@Override
			public String toString() {
				return "explanation-first";
			}
		},
		
		/**
		 * A* search that {@link GoalFirstSearch requires every action to be
		 * explained for the node's character before it is explained for other
		 * characters}
		 */
		GOAL_FIRST {
			@Override
			public String toString() {
				return "goal-first";
			}
		}
	}
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The search method to use */
	private Method method = Method.BEST_FIRST;
	
	/**
	 * The factory used to create {@link ProgressionSearch#cost the search's
	 * cost function}
	 */
	private ProgressionCostFactory cost = ProgressionCostFactory.TEMPORAL;
	
	/**
	 * The factory used to create {@link ProgressionSearch#heuristic the
	 * search's heuristic function}
	 */
	private ProgressionCostFactory heuristic = RelaxedPlanHeuristic.FACTORY;
	
	/**
	 * Whether or not searches will use {@link
	 * ProgressionSearch#explanationPruning explanation pruning}
	 */
	private boolean explanationPruning = true;

	/**
	 * Constructs a new heuristic progression planner.
	 * 
	 * @param name the name of the planner
	 */
	public ProgressionPlanner(String name) {
		super(name);
	}
	
	/**
	 * Constructs a new heuristic progression planner with the default name.
	 */
	public ProgressionPlanner() {
		this("Heuristic Progression Planner");
	}
	
	@Override
	protected String toString(String name) {
		String string = super.toString(name);
		string += "; m=\"" + getMethod() + "\"";
		string += "; c=\"" + getCost() + "\"";
		string += "; h=\"" + getHeuristic() + "\"";
		string += "; ep=" + getExplanationPruning();
		return string;
	}
	
	/**
	 * Returns the {@link Method search method} that this planner will used to
	 * solve problems.
	 * 
	 * @return the search method
	 */
	public Method getMethod() {
		return method;
	}
	
	/**
	 * Sets the {@link Method search method} that this planner will use to solve
	 * problems.
	 * 
	 * @param method the new method that should be used
	 */
	public void setMethod(Method method) {
		this.method = method;
	}
	
	/**
	 * Returns the {@link ProgressionCostFactory factory} that will be used to
	 * create {@link ProgressionSearch#cost the cost used by the searches}
	 * this planner creates.
	 * 
	 * @return the cost factory
	 */
	public ProgressionCostFactory getCost() {
		return cost;
	}
	
	/**
	 * Sets the {@link ProgressionCostFactory factory} that should be used to
	 * create {@link ProgressionSearch#cost the cost used by the searches}
	 * this planner will create.
	 * 
	 * @param factory the new cost factory that should be used
	 */
	public void setCost(ProgressionCostFactory factory) {
		this.cost = factory;
	}
	
	/**
	 * Returns the {@link ProgressionCostFactory factory} that will be used to
	 * create {@link ProgressionSearch#heuristic the heuristic used by the
	 * searches} this planner creates.
	 * 
	 * @return the heuristic factory
	 */
	public ProgressionCostFactory getHeuristic() {
		return heuristic;
	}
	
	/**
	 * Sets the {@link ProgressionCostFactory factory} that should be used to
	 * create {@link ProgressionSearch#heuristic the heuristic used by the
	 * searches} this planner will create.
	 * 
	 * @param factory the new heuristic factory that should be used
	 */
	public void setHeuristic(ProgressionCostFactory factory) {
		this.heuristic = factory;
	}
	
	/**
	 * Indicates whether {@link ProgressionSearch#explanationPruning explanation
	 * pruning} will be used in the searches this planner creates.
	 * 
	 * @return true if searches will use explanation pruning, false otherwise
	 */
	public boolean getExplanationPruning() {
		return explanationPruning;
	}
	
	/**
	 * Sets whether {@link ProgressionSearch#explanationPruning explanation
	 * pruning} will be used in the searches this planner creates.
	 * 
	 * @param value true if searches should use explanation pruning, false if
	 * they should not
	 */
	public void setExplanationPruning(boolean value) {
		this.explanationPruning = value;
	}
	
	@Override
	public CompiledProblem compile(Problem problem, Status status) {
		CompiledProblem compiled = Grounder.compile(problem, status);
		compiled = FluentExplicitizer.compile(compiled, status);
		compiled = Simplifier.compile(compiled, status);
		return compiled;
	}

	@Override
	public ProgressionSearch getSearch(Problem problem, Status status) {
		CompiledProblem compiled = problem instanceof CompiledProblem ? (CompiledProblem) problem : compile(problem, status);
		EventTree<CompiledAction> actions = compiled.actions.buildTree(status);
		compiled.triggers.buildTree(status);
		ProgressionSpace<?> space = new ProgressionTreeSpace(compiled, status);
		ProgressionCost cost = getCost().getCost(compiled, status);
		ProgressionCost heuristic = getHeuristic().getCost(compiled, status);
		ProgressionSearch search;
		switch(getMethod()) {
		case EXPLANATION_FIRST:
			search = new ExplanationFirstSearch(
				compiled,
				cost,
				heuristic,
				actions,
				space,
				getSearchLimit(),
				getSpaceLimit(),
				getTimeLimit(),
				getAuthorTemporalLimit(),
				getCharacterTemporalLimit(),
				getEpistemicLimit(),
				getExplanationPruning()
			);
			break;
		case GOAL_FIRST:
			search = new GoalFirstSearch(
				compiled,
				cost,
				heuristic,
				actions,
				space,
				getSearchLimit(),
				getSpaceLimit(),
				getTimeLimit(),
				getAuthorTemporalLimit(),
				getCharacterTemporalLimit(),
				getEpistemicLimit(),
				getExplanationPruning()
			);
			break;
		default:
			search = new ProgressionSearch(
				compiled,
				cost,
				heuristic,
				actions,
				space,
				getSearchLimit(),
				getSpaceLimit(),
				getTimeLimit(),
				getAuthorTemporalLimit(),
				getCharacterTemporalLimit(),
				getEpistemicLimit(),
				getExplanationPruning()
			);		
		}
		search.setStart(compiled.start);
		Value goal = compiled.utility.evaluate(compiled.start);
		if(goal.equals(Unknown.UNKNOWN))
			goal = Number.NEGATIVE_INFINITY;
		search.setGoal((Number) goal);
		return search;
	}
}