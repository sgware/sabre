package edu.uky.cs.nil.sabre.hg;

import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A node which represents an {@link Precondition atomic precondition}, which
 * is a {@link edu.uky.cs.nil.sabre.logic.Comparison comparison} between a
 * {@link FluentNode fluent} on the left and a {@link FormulaNode value} on the
 * right.
 * 
 * @author Stephen G. Ware
 */
public class PreconditionNode extends CostNode {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The atomic precondition this node represents */
	public final Precondition label;
	
	/** The node representing the fluent on the left side of the comparison */
	public final FluentNode fluent;
	
	/** The node representing the value on the right side of the comparison */
	public final FormulaNode value;
	
	/**
	 * A list of all {@link EffectNode effect nodes} which might assign values
	 * to this node's fluent
	 */
	public final List<EffectNode> effects;
	
	/**
	 * A list of all {@link ClauseNode clause nodes} that this precondition node
	 * appears in
	 */
	public final List<ClauseNode> clauses = new List<>();

	/**
	 * Constructs a new precondition node that belongs to a given graph and
	 * represents a given atomic precondition.
	 * 
	 * @param graph the graph this node belongs to
	 * @param label the atomic precondition this node represents
	 */
	protected PreconditionNode(HeuristicGraph graph, Precondition label) {
		super(graph, label);
		this.label = label;
		this.fluent = graph.getFluent((Fluent) label.left);
		if(this.fluent.label.isNumber())
			this.fluent.formulas.add(this);
		this.value = (FormulaNode) graph.getNode(label.right);
		this.value.formulas.add(this);
		if(this.fluent.label.isNumber())
			this.effects = this.fluent.effects;
		else
			this.effects = new List<>();
		this.fluent.register(this);
	}
	
	@Override
	protected boolean setCost(double cost) {
		if(super.setCost(cost)) {
			// Notify all clauses this node appears in that this precondition has become true.
			for(int i=0; i<clauses.size(); i++)
				clauses.get(i).notify(this, True.TRUE, cost);
			return true;
		}
		else
			return false;
	}
	
	@Override
	protected void notify(Node node, Value value, double cost) {
		// The fluent on the left has a new value, so check this comparison.
		if(node.equals(fluent)) {
			for(int i=0; i<this.value.size(); i++)
				if(fluent.getCost(label.operator, this.value.getValue(i)) != Double.POSITIVE_INFINITY)
					setCost(graph.cost(this, value, cost, this.value.getValue(i), this.value.getCost(i)));
		}
		// The value expression on the right has a new value, so check this
		// comparison. This should only occur for numeric fluents, because the
		// value of a precondition on a nominal fluent should always be a
		// constant.
		else if(node.equals(this.value)) {			
			for(int i=0; i<fluent.size(); i++)
				if(this.value.getCost(label.operator.flip(), fluent.getValue(i)) != Double.POSITIVE_INFINITY)
					setCost(graph.cost(this, fluent.getValue(i), fluent.getCost(i), value, cost));
		}
	}
}