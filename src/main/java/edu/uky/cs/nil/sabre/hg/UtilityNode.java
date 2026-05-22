package edu.uky.cs.nil.sabre.hg;

import java.util.Iterator;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Comparison.Operator;
import edu.uky.cs.nil.sabre.logic.Conditional;
import edu.uky.cs.nil.sabre.logic.Conjunction;
import edu.uky.cs.nil.sabre.logic.Disjunction;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.logic.Unknown;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A {@link CostSet cost set} {@link FormulaNode node} that represents a
 * utility expression. When a utility node's {@link #character character} is
 * null, it represents the {@link edu.uky.cs.nil.sabre.Problem#utility author's
 * utility}; otherwise, it represents {@link
 * edu.uky.cs.nil.sabre.Problem#utilities that character's utility}. Each {@link
 * Conditional#branches branch} of the {@link Conditional conditional} utility
 * expression is represented by a {@link GoalNode goal node}.
 * 
 * @author Stephen G. Ware
 */
public class UtilityNode extends FormulaNode {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * The character whose utility this node represents, or null for the
	 * author's utility
	 */
	public final Character character;
	
	/**
	 * The conditional numeric expression that defines a utility value depending
	 * on the situation
	 */
	public final Conditional<Disjunction<Clause<Precondition>>> label;
	
	/**
	 * A list of {@link GoalNode goal nodes} that correspond to the {@link
	 * Conditional#branches branches} of the {@link #label conditional utility
	 * expression} and which can cause this node to have different values
	 */
	public final List<GoalNode> goals = new List<>();
	
	/**
	 * The set of possible values this utility expression can have and their
	 * associated costs
	 */
	protected final Range range;
	
	/**
	 * Constructs a new utility node that belongs to a given graph and
	 * represents a given utility.
	 * 
	 * @param graph the graph this node belongs to
	 * @param character the character whose utility this node represents, or
	 * null for the author
	 * @param label the numeric utility expression this node represents
	 */
	protected UtilityNode(HeuristicGraph graph, Character character, Conditional<Disjunction<Clause<Precondition>>> label) {
		super(graph, label);
		this.character = character;
		this.label = label;
		for(int i=0; i<label.branches.size(); i++) {
			Conditional<Disjunction<Clause<Precondition>>> branch = serialize(label, i);
			goals.add(graph.makeGoalNode(this, branch));
		}
		this.range = graph.makeRange(label);
	}
	
	private static final Conditional<Disjunction<Clause<Precondition>>> serialize(Conditional<Disjunction<Clause<Precondition>>> utility, int branch) {
		Expression condition = utility.getCondition(branch);
		for(int i=0; i<branch; i++)
			condition = new Conjunction<>(condition, utility.getCondition(i).negate());
		return new Conditional<>(condition.toPrecondition(), utility.getBranch(branch), Unknown.UNKNOWN);
	}

	@Override
	public final int size() {
		return range.size();
	}
	
	@Override
	public final Iterator<Entry> iterator() {
		return range.iterator();
	}

	@Override
	public final Value getValue(int index) {
		return range.getValue(index);
	}

	@Override
	public final double getCost(int index) {
		return range.getCost(index);
	}

	@Override
	public final double getCost(Operator operator, Value value) {
		return range.getCost(operator, value);
	}
	
	/**
	 * Sets the cost of this node's utility expression having the given {@link
	 * Value value}. The value's cost will only be updated if the given cost is
	 * less than the current cost of that value.
	 * 
	 * @param value a value that the utility expression can now have
	 * @param cost the cost of the utility expression having that value
	 * @return true if the cost of that value was updated, or false if this
	 * node's state did not change
	 */
	@Override
	protected boolean setCost(Value value, double cost) {
		value = range.setCost(value, cost);
		if(value != null) {
			markForReset();
			super.setCost(value, cost);
			return true;
		}
		else
			return false;
	}
	
	@Override
	protected void reset() {
		super.reset();
		range.reset();
	}
}