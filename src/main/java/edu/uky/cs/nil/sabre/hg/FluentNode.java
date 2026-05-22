package edu.uky.cs.nil.sabre.hg;

import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A {@link CostSet cost set} {@link FormulaNode node} representing a {@link
 * Fluent fluent}, a feature of a {@link edu.uky.cs.nil.sabre.State state} that
 * can have one of several {@link Value values}.
 * 
 * @author Stephen G. Ware
 */
public abstract class FluentNode extends FormulaNode {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The fluent this node represents */
	public final Fluent label;
	
	/**
	 * All {@link PreconditionNode precondition nodes} where this node's fluent
	 * appears on {@link PreconditionNode#fluent the left side}
	 */
	public final List<PreconditionNode> preconditions = new List<>();
	
	/**
	 * All {@link EffectNode effect nodes} which {@link EffectNode#fluent
	 * assign} values to this node's fluent
	 */
	public final List<EffectNode> effects = new List<>();

	/**
	 * Constructs a new fluent node that belongs to a given graph and
	 * represents a given fluent.
	 * 
	 * @param graph the graph this node belongs to
	 * @param label the fluent this node represents
	 */
	protected FluentNode(HeuristicGraph graph, Fluent label) {
		super(graph, label);
		this.label = label;
		graph.fluents.add(this);
	}
	
	/**
	 * Adds a {@link PreconditionNode precondition nodes} to this node's {@link
	 * #preconditions list of precondition nodes} that have this node's fluent
	 * on the left.
	 * 
	 * @param precondition the precondition node to add to the list
	 */
	protected void register(PreconditionNode precondition) {
		preconditions.add(precondition);
	}
	
	/**
	 * Adds an {@link EffectNode effect node} to this node's {@link #effects
	 * list of effect nodes} that assign values to this node's fluent.
	 * 
	 * @param effect the effect node to add to the list
	 */
	protected void register(EffectNode effect) {
		effects.add(effect);
	}
	
	/**
	 * Sets the cost of this node's fluent having the given {@link Value
	 * value}. The value's cost will only be updated if the given cost is less
	 * than the current cost of that value.
	 * 
	 * @param value a value that the fluent can now have
	 * @param cost the cost of the fluent having that value
	 * @return true if the cost of that value was updated, or false if this
	 * node's state did not change
	 */
	@Override
	public boolean setCost(Value value, double cost) {
		return super.setCost(value, cost);
	}
}