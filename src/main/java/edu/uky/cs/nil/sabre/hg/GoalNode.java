package edu.uky.cs.nil.sabre.hg;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Conditional;
import edu.uky.cs.nil.sabre.logic.Disjunction;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A {@link GoalNode goal node} represents one {@link Conditional#branches
 * branch} of a {@link Conditional conditional} {@link UtilityNode utility
 * node} {@link UtilityNode#label expression}. A goal is a {@link #condition
 * condition} under which a utility node will have new values of finite cost.
 * 
 * @author Stephen G. Ware
 */
public class GoalNode extends CostNode {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * The condition and utility value this node represents. The condition is
	 * represented by the first {@link Conditional#conditions condition} of
	 * this expression and the utility value by the first {@link
	 * Conditional#branches branch}. All other conditions and branches of this
	 * expression are ignored and not represented by this node.
	 */
	public final Conditional<Disjunction<Clause<Precondition>>> label;
	
	/**
	 * The utility node this goal will add values to if its condition has a
	 * finite cost
	 */
	public final UtilityNode utility;
	
	/**
	 * The condition which, if it has a finite cost, will add values to the
	 * utility expression
	 */
	public final DisjunctionNode condition;
	
	/**
	 * The value(s) that will be added to the utility expression if this
	 * condition has a finite cost
	 */
	public final FormulaNode value;
	
	/**
	 * Constructs a new goal node that represents a given branch of a utility
	 * node's expression.
	 * 
	 * @param utility the utility node for which this goal is a branch
	 * @param label a conditional expression whose first condition will be
	 * used as this node's condition and whose first branch will be used as the
	 * utility values (all other conditions and branches are not represented by
	 * this node)
	 */
	protected GoalNode(UtilityNode utility, Conditional<Disjunction<Clause<Precondition>>> label) {
		super(utility.graph, label);
		this.label = label;
		this.utility = utility;
		this.condition = graph.getDisjunction(label.conditions.get(0));
		this.condition.conditionals.add(this);
		this.value = (FormulaNode) graph.getNode(label.branches.get(0));
		this.value.formulas.add(this);
	}
	
	@Override
	protected boolean setCost(double cost) {
		if(super.setCost(cost)) {
			for(int i=0; i<value.size(); i++)
				utility.setCost(value.getValue(i), graph.cost(this, value.getValue(i), value.getCost(i)));
			return true;
		}
		else
			return false;
	}
	
	@Override
	protected void notify(Node node, Value value, double cost) {
		if(node.equals(condition))
			setCost(cost);
		else if(node.equals(this.value))
			utility.setCost(value, graph.cost(this, value, cost));
	}
}