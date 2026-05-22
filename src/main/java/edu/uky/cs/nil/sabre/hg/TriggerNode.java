package edu.uky.cs.nil.sabre.hg;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Trigger;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * An {@link EventNode event node} representing a {@link Trigger trigger}.
 * Once a trigger node's {@link EventNode#precondition precondition} has a
 * finite {@link #getCost() cost} that trigger automatically has that same
 * cost.
 * 
 * @author Stephen G. Ware
 */
public class TriggerNode extends EventNode {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The trigger this node represents */
	public final Trigger label;

	/**
	 * Constructs a new trigger node that belongs to a given graph and
	 * represents a given trigger.
	 * 
	 * @param graph the graph this node belongs to
	 * @param label the trigger this node represents
	 */
	protected TriggerNode(HeuristicGraph graph, Trigger label) {
		super(graph, label);
		this.label = label;
	}
	
	@Override
	protected void notify(Node node, Value value, double cost) {
		setCost(cost);
	}
}