package edu.uky.cs.nil.sabre.hg;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A node which represents an {@link Effect atomic effect} of an {@link
 * EventNode event}.
 * 
 * @author Stephen G. Ware
 */
public class EffectNode extends CostNode {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;

	/** The atomic effect this node represents */
	public final Effect label;
	
	/**
	 * The node representing the fluent on {@link
	 * edu.uky.cs.nil.sabre.logic.Assignment#fluent the left side of the
	 * assignment}
	 */
	public final FluentNode fluent;
	
	/**
	 * The node representing the value expression on the {@link
	 * edu.uky.cs.nil.sabre.logic.Assignment#value right side of the assignment}
	 */
	public final FormulaNode value;
	
	/** A list of {@link EventNode event nodes} that have this effect */
	public final List<EventNode> events = new List<>();
	
	/**
	 * The node representing the {@link Effect#condition condition} under which
	 * the effect can happen
	 */
	public final DisjunctionNode condition;
	
	/**
	 * A list of all {@link PreconditionNode precondition nodes} whose fluent
	 * this effect might assign values to
	 */
	public final List<PreconditionNode> preconditions;
	
	/**
	 * Constructs a new effect node that belongs to a given graph and
	 * represents a given atomic effect.
	 * 
	 * @param graph the graph this node belongs to
	 * @param label the atomic effect this node represents
	 */
	protected EffectNode(HeuristicGraph graph, Effect label) {
		super(graph, label);
		this.label = label;
		this.fluent = graph.getFluent(label.fluent);
		this.value = (FormulaNode) graph.getNode(label.value);
		this.value.formulas.add(this);
		this.condition = graph.getDisjunction(label.condition);
		this.condition.conditionals.add(this);
		if(label.fluent.isNumber())
			this.preconditions = this.fluent.preconditions;
		else
			this.preconditions = ((NominalFluentNode) this.fluent).getGroup((Value) label.value);
		this.fluent.register(this);
	}
	
	@Override
	protected boolean setCost(double cost) {
		if(super.setCost(cost)) {
			for(int i=0; i<value.size(); i++)
				fluent.setCost(value.getValue(i), graph.cost(this, value.getValue(i), value.getCost(i)));
			return true;
		}
		else
			return false;
	}
	
	@Override
	protected void notify(Node node, Value value, double cost) {
		if(node.equals(this.value))
			fluent.setCost(value, graph.cost(this, value, cost));
		else
			setCost(graph.cost(this));
	}
}