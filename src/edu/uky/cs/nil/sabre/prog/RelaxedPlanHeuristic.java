package edu.uky.cs.nil.sabre.prog;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Plan;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.TailPlan;
import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.hg.ActionNode;
import edu.uky.cs.nil.sabre.hg.ArithmeticNode;
import edu.uky.cs.nil.sabre.hg.ClauseNode;
import edu.uky.cs.nil.sabre.hg.DisjunctionNode;
import edu.uky.cs.nil.sabre.hg.EffectNode;
import edu.uky.cs.nil.sabre.hg.EventNode;
import edu.uky.cs.nil.sabre.hg.FluentNode;
import edu.uky.cs.nil.sabre.hg.FormulaNode;
import edu.uky.cs.nil.sabre.hg.GoalNode;
import edu.uky.cs.nil.sabre.hg.Node;
import edu.uky.cs.nil.sabre.hg.PreconditionNode;
import edu.uky.cs.nil.sabre.hg.UtilityNode;
import edu.uky.cs.nil.sabre.logic.Comparison;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * A {@link GraphHeuristic graph cost function}, meant to be used as the {@link
 * ProgressionSearch#heuristic heuristic} in a {@link ProgressionSearch
 * heuristic progression search}, which solves a relaxed version of the planning
 * problem and uses the cost of the solution to that relaxed problem as an
 * approximation of the cost of solving the real problem. This heuristic is
 * inspired by J�rg Hoffmann's Fast Forward heuristic.
 * <p>
 * The relaxed plan heuristic works initializing a {@link
 * edu.uky.cs.nil.sabre.hg.MaxGraph max heuristic graph} to the state it is
 * evaluating and then extending the graph until it is possible for the {@link
 * ProgressionNode#getCharacter() node's character's} utility to be higher. It
 * then selects a sequence of actions from the heuristic graph that approximate
 * a solution to the problem. Ideally, these actions will be similar to an
 * actual solution to the problem.
 * <p>
 * The relaxed problem implicitly being solved by this method is as follows:
 * suppose that once a proposition becomes true, it stays true forever. Few
 * real problems have this property, but if we make this assumption about a
 * real problem (even when it doesn't actually hold), we can solve a relaxed
 * version of the problem that is computationally easier. Then we can use the
 * solution to the relaxed problem as an approximation of the solution to the
 * real problem.
 * 
 * @author Stephen G. Ware
 */
public class RelaxedPlanHeuristic extends GraphHeuristic.MaxGraphHeuristic {
	
	/**
	 * A {@link ProgressionCostFactory factory} for creating {@link
	 * RelaxedPlanHeuristic relaxed plan heuristic costs}.
	 */
	public static final ProgressionCostFactory FACTORY = new ProgressionCostFactory() {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1L;
		
		@Override
		public String toString() {
			return STRING;
		}

		@Override
		public RelaxedPlanHeuristic getCost(CompiledProblem problem, Status status) {
			return new RelaxedPlanHeuristic(problem, status);
		}
	};
	
	/** The name of this heuristic */
	private static final String STRING = "relaxed plan";
	
	/** Records which nodes from the graph are part of the relaxed solution */
	protected final Set<Node> subgraph = new HashSet<>();
	
	/** Counts the number of actions in the subgraph */
	private int cost = 0;
	
	/**
	 * Constructs a new relaxed plan heuristic.
	 * 
	 * @param problem that problem for which this heuristic will approximate
	 * costs
	 * @param status a status to update while building the sum graph
	 */
	public RelaxedPlanHeuristic(CompiledProblem problem, Status status) {
		super(problem, status);
	}
	
	@Override
	public String toString() {
		return STRING;
	}
	
	@Override
	public double evaluate(State state, Character character) {
		UtilityNode utility = graph.getUtility(character);
		graph.initialize(state);
		subgraph.clear();
		cost = 0;
		Value start = utility.getValue(0);
		while(utility.getCost(Comparison.GREATER_THAN, start) == Double.POSITIVE_INFINITY && graph.extend());
		if(utility.getCost(Comparison.GREATER_THAN, start) == Double.POSITIVE_INFINITY)
			return Double.POSITIVE_INFINITY;
		else {
			extract(utility, Comparison.GREATER_THAN, start);
			return cost;
		}
	}
	
	/**
	 * Returns a plan that is a solution to the relaxed problem. Recall that
	 * the relaxed problem assumes that once a fact is true, it stays true
	 * forever. The relaxed problem is unlikely to be a true solution to the
	 * real problem, but it may approximate a solution to the real problem in
	 * length and content. The relaxed plan does not account for character
	 * intentions or beliefs.
	 * <p>
	 * This method returns the relaxed plan discovered the last time {@link
	 * #evaluate(State, Character)} was called. If that method has not yet been
	 * called, the relaxed plan will be empty.
	 * 
	 * @return a solution to the relaxed problem this heuristic solves
	 */
	public Plan<Action> getRelaxedPlan() {
		ArrayList<ActionNode> actions = new ArrayList<>(cost);
		for(Node node : subgraph)
			if(node instanceof ActionNode)
				actions.add((ActionNode) node);
		Collections.sort(actions);
		TailPlan<Action> plan = TailPlan.EMPTY;
		for(ActionNode action : actions)
			plan = plan.append(action.label);
		return plan;
	}
	
	/**
	 * Adds nodes from the {@link #graph heuristic graph} to the {@link
	 * #subgraph solution subgraph} which are needed to make a {@link
	 * FormulaNode formula node} compare to a value.
	 * 
	 * @param formula the formula node
	 * @param operator the relationship between the formula and the value
	 * @param value the value
	 */
	protected void extract(FormulaNode formula, Comparison.Operator operator, Value value) {
		if(formula instanceof FluentNode)
			extract((FluentNode) formula, operator, value);
		else if(formula instanceof ArithmeticNode)
			extract((ArithmeticNode) formula, operator, value);
		else if(formula instanceof UtilityNode)
			extract((UtilityNode) formula, operator, value);
	}
	
	/**
	 * Adds nodes from the {@link #graph heuristic graph} to the {@link
	 * #subgraph solution subgraph} which are needed to make a {@link FluentNode
	 * fluent node} compare to a value.
	 * 
	 * @param fluent the fluent node
	 * @param operator the relationship between the fluent and the value
	 * @param value the value
	 */
	protected void extract(FluentNode fluent, Comparison.Operator operator, Value value) {
		if(fluent.getCost(operator, value) == 0)
			return;
		EffectNode bestEffect = null;
		Value bestValue = null;
		double bestCost = Double.POSITIVE_INFINITY;
		for(int i=0; i<fluent.effects.size(); i++) {
			EffectNode effect = fluent.effects.get(i);
			if(effect.getCost() < Double.POSITIVE_INFINITY) {
				for(int j=0; j<effect.value.size(); j++) {
					Value v = effect.value.getValue(j);
					if(fluent.getCost(v) < Double.POSITIVE_INFINITY) {
						double cost = effect.getCost() + effect.value.getCost(j);
						if(bestEffect == null || cost < bestCost) {
							bestEffect = effect;
							bestValue = v;
							bestCost = cost;
						}
					}
				}
			}
		}
		extract(bestEffect.value, Comparison.EQUAL_TO, bestValue);
		extract(bestEffect);
	}
	
	/**
	 * Adds nodes from the {@link #graph heuristic graph} to the {@link
	 * #subgraph solution subgraph} which are needed to make an {@link
	 * ArithmeticNode arithmetic node} compare to a value.
	 * 
	 * @param arithmetic the arithmetic node
	 * @param operator the relationship between the arithmetic and the value
	 * @param value the value
	 */
	protected void extract(ArithmeticNode arithmetic, Comparison.Operator operator, Value value) {
		if(arithmetic.getCost(operator, value) == 0)
			return;
		Value bestLeft = null;
		Value bestRight = null;
		double bestCost = Double.POSITIVE_INFINITY;
		for(int i=0; i<arithmetic.left.size(); i++) {
			Value left = arithmetic.left.getValue(i);
			for(int j=0; j<arithmetic.right.size(); j++) {
				Value right = arithmetic.right.getValue(j);
				double cost = arithmetic.left.getCost(i) + arithmetic.right.getCost(j);
				if(cost < Double.POSITIVE_INFINITY) {
					Value result = arithmetic.label.operator.calculate(left, right);
					if(arithmetic.getCost(result) < Double.POSITIVE_INFINITY && operator.test(result, value) && (bestLeft == null || cost < bestCost)) {
						bestLeft = left;
						bestRight = right;
						bestCost = cost;
					}
				}
			}
		}
		extract(arithmetic.left, Comparison.EQUAL_TO, bestLeft);
		extract(arithmetic.right, Comparison.EQUAL_TO, bestRight);
	}
	
	/**
	 * Adds nodes from the {@link #graph heuristic graph} to the {@link
	 * #subgraph solution subgraph} which are needed to make a {@link
	 * UtilityNode utility node} compare to a value.
	 * 
	 * @param utility the utility node
	 * @param operator the relationship between the utility and the value
	 * @param value the value
	 */
	protected void extract(UtilityNode utility, Comparison.Operator operator, Value value) {
		if(utility.getCost(operator, value) == 0)
			return;
		GoalNode best = null;
		for(int i=0; i<utility.goals.size(); i++) {
			GoalNode goal = utility.goals.get(i);
			if(goal.condition.getCost() < Double.POSITIVE_INFINITY && goal.value.getCost(operator, value) < Double.POSITIVE_INFINITY) {
				if(best == null || goal.getCost() < best.getCost())
					best = goal;
			}
		}
		extract(best.condition);
		extract(best.value, operator, value);
	}
	
	/**
	 * Adds nodes from the {@link #graph heuristic graph} to the {@link
	 * #subgraph solution subgraph} which are needed to make a {@link
	 * DisjunctionNode disjunction node} true.
	 * 
	 * @param disjunction the disjunction node
	 */
	protected void extract(DisjunctionNode disjunction) {
		if(subgraph.add(disjunction)) {
			ClauseNode best = null;
			for(int i=0; i<disjunction.clauses.size(); i++) {
				ClauseNode clause = disjunction.clauses.get(i);
				if(clause.getCost() < Double.POSITIVE_INFINITY && (best == null || clause.getCost() < best.getCost()))
					best = clause;
			}
			extract(best);
		}
	}
	
	/**
	 * Adds nodes from the {@link #graph heuristic graph} to the {@link
	 * #subgraph solution subgraph} which are needed to make a {@link ClauseNode
	 * clause node} true.
	 * 
	 * @param clause the clause node
	 */
	protected void extract(ClauseNode clause) {
		if(subgraph.add(clause))
			for(int i=0; i<clause.preconditions.size(); i++)
				extract(clause.preconditions.get(i));
	}
	
	/**
	 * Adds nodes from the {@link #graph heuristic graph} to the {@link
	 * #subgraph solution subgraph} which are needed to make a {@link
	 * PreconditionNode precondition node} true.
	 * 
	 * @param precondition the precondition node
	 */
	protected void extract(PreconditionNode precondition) {
		if(subgraph.add(precondition)) {
			Value bestLeft = null;
			Value bestRight = null;
			double bestCost = Double.POSITIVE_INFINITY;
			for(int i=0; i<precondition.fluent.size(); i++) {
				Value left = precondition.fluent.getValue(i);
				for(int j=0; j<precondition.value.size(); j++) {
					Value right = precondition.value.getValue(j);
					double cost = precondition.fluent.getCost(i) + precondition.value.getCost(j);
					if(cost < Double.POSITIVE_INFINITY && precondition.label.operator.test(left, right)) {
						if(bestLeft == null || cost < bestCost) {
							bestLeft = left;
							bestRight = right;
							bestCost = cost;
						}
					}
				}
			}
			if(bestCost > 0) {
				extract(precondition.fluent, Comparison.EQUAL_TO, bestLeft);
				extract(precondition.value, Comparison.EQUAL_TO, bestRight);
			}
		}
	}
	
	/**
	 * Adds nodes from the {@link #graph heuristic graph} to the {@link
	 * #subgraph solution subgraph} which are needed to include an {@link
	 * EffectNode effect node} in the subgraph.
	 * 
	 * @param effect the effect node
	 */
	protected void extract(EffectNode effect) {
		if(subgraph.add(effect)) {
			EventNode best = null;
			for(int i=0; i<effect.events.size(); i++) {
				EventNode event = effect.events.get(i);
				if(event.getCost() < Double.POSITIVE_INFINITY && (best == null || event.getCost() < best.getCost()))
					best = event;
			}
			extract(effect.condition);
			extract(best);
		}
	}
	
	/**
	 * Adds nodes from the {@link #graph heuristic graph} to the {@link
	 * #subgraph solution subgraph} which are needed to include an {@link
	 * EventNode event node} in the subgraph.
	 * 
	 * @param event the event node
	 */
	protected void extract(EventNode event) {
		if(subgraph.add(event)) {
			if(event.label instanceof Action)
				cost++;
			extract(event.precondition);
		}
	}
}