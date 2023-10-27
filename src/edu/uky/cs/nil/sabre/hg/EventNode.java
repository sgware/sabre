package edu.uky.cs.nil.sabre.hg;

import edu.uky.cs.nil.sabre.Event;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.True;

/**
 * A node which represents an {@link Event event}, which has a {@link
 * #precondition precondition} that must have a finite cost before it can occur
 * and {@link #effects effects} which may cause other nodes to take on finite
 * values.
 * 
 * @author Stephen G. Ware
 */
public abstract class EventNode extends CostNode {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;

	/** The event this node represents */
	public final Event label;
	
	/** The node representing the event's precondition */
	public final DisjunctionNode precondition;
	
	/** The nodes representing the event's effects */
	public final List<EffectNode> effects = new List<>();
	
	/**
	 * Constructs a new event node that belongs to a given graph and
	 * represents a given event.
	 * 
	 * @param graph the graph this node belongs to
	 * @param label the event this node represents
	 */
	protected EventNode(HeuristicGraph graph, Event label) {
		super(graph, label);
		this.label = label;
		this.precondition = graph.getDisjunction(label.getPrecondition().toPrecondition());
		this.precondition.conditionals.add(this);
		for(Effect effect : label.getEffect().toEffect()) {
			EffectNode node = graph.getEffect(effect);
			effects.add(node);
			node.events.add(this);
		}
	}
	
	@Override
	protected boolean setCost(double cost) {
		if(super.setCost(cost)) {
			for(int i=0; i<effects.size(); i++)
				effects.get(i).notify(this, True.TRUE, cost);
			return true;
		}
		else
			return false;
	}
}