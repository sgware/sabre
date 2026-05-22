package edu.uky.cs.nil.sabre.hg;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * A max graph is a {@link HeuristicGraph heuristic graph} where the cost of a
 * {@link edu.uky.cs.nil.sabre.logic.Conjunction conjunction} is defined to be
 * the maximum cost of and of its conjuncts. Costs in a max graph are meant to
 * approximate the lowest cost an expression can have.  A max graph should
 * never overestimate. A max graph imitates Blum and Furst's plan graph data
 * structure (without mutexes) and can be used to calculate things like
 * Hoffmann's Fast Forward heuristic.
 * <p>
 * Specifically:
 * <ul>
 * <li>The cost of a {@link ClauseNode clause node} being true is cost of its
 * highest cost {@link ClauseNode#preconditions precondition}.</li>
 * <li>The cost of a {@link PreconditionNode precondition node} being true is
 * higher of the costs of the values for the {@link PreconditionNode#fluent
 * left} and {@link PreconditionNode#value right} sides that make its {@link
 * edu.uky.cs.nil.sabre.logic.Comparison.Operator#test(Value, Value)
 * comparison} true.</li>
 * <li>The cost of an {@link ArithmeticNode arithmetic node} having a value is
 * the higher of the costs of the values for the {@link ArithmeticNode#left
 * left} and {@link ArithmeticNode#right right} sides of the expression that
 * cause the arithmetic expression to have that value.</li>
 * <li>The cost of a {@link UtilityNode utility node} having a value is the
 * higher of the cost of {@link GoalNode the branch of the utility expression}
 * having that {@link GoalNode#value value} and the cost of {@link
 * GoalNode#condition that goal node's condition}.</li>
 * <li>The cost of an {@link EffectNode effect node} is the higher of the cost
 * of {@link EffectNode#condition its condition} and the lowest cost of any of
 * {@link EventNode the events that have that effect}.</li>
 * <li>The cost of an {@link EffectNode effect node} assigning a value to
 * {@link EffectNode#fluent its fluent} is the higher of the cost of the {@link
 * EffectNode#getCost() effect node} and the cost of {@link EffectNode#value
 * that value}.</li>
 * </ul>
 * By default, {@link Range numeric ranges} in a max graph are modeled by
 * {@link InfiniteSpan infinite spans}.
 * 
 * @author Stephen G. Ware
 */
public class MaxGraph extends HeuristicGraph {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;

	/**
	 * Constructs a new max graph which contains only the nodes representing
	 * {@link False#FALSE false}, {@link True#TRUE true}, and {@link
	 * Clause#EMPTY the empty clause}.
	 * 
	 * @param status a status to update while building the graph
	 */
	public MaxGraph(Status status) {
		super(status);
	}
	
	/**
	 * Constructs a new max graph which contains nodes for every fluent, event,
	 * and utility in a given problem.
	 * 
	 * @param problem the problem whose fluents, events, and utilities will be
	 * represented in the graph
	 * @param status a status to update while building the graph
	 */
	public MaxGraph(CompiledProblem problem, Status status) {
		super(problem, status);
	}

	@Override
	protected double cost(ArithmeticNode node, Value leftValue, double leftCost, Value rightValue, double rightCost) {
		return Math.max(leftCost, rightCost);
	}
	
	@Override
	protected double cost(PreconditionNode node, Value leftValue, double leftCost, Value rightValue, double rightCost) {
		return Math.max(leftCost, rightCost);
	}

	@Override
	protected double cost(ClauseNode node) {
		double max = 0;
		for(int i=0; i<node.preconditions.size(); i++)
			max = Math.max(max, node.preconditions.get(i).getCost());
		return max;
	}

	@Override
	protected double cost(GoalNode node, Value value, double cost) {
		return Math.max(node.getCost(), cost);
	}

	@Override
	protected double cost(EffectNode node) {
		double min = Double.POSITIVE_INFINITY;
		for(int i=0; i<node.events.size(); i++)
			min = Math.min(min, node.events.get(i).getCost());
		return Math.max(min, node.condition.getCost());
	}

	@Override
	protected double cost(EffectNode node, Value value, double cost) {
		return Math.max(node.getCost(), cost);
	}
}