package edu.uky.cs.nil.sabre.hg;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * An {@link EventNode event node} that represents an {@link Action action}.
 * An action node does not automatically have a finite {@link #getCost()} once
 * its {@link EventNode#precondition precondition} has a finite cost; rather,
 * once an action's precondition has a finite cost the action is added to a
 * list of actions that will be {@link #setCost(double) assigned} a finite cost
 * next time the {@link HeuristicGraph heuristic graph's} {@link
 * HeuristicGraph#extend()} method is called. When that method is called, all
 * waiting action nodes will be assigned the cost of their preconditions plus
 * one, where the plus one represents the additional effort of taking that
 * action.
 * 
 * @author Stephen G. Ware
 */
public class ActionNode extends EventNode {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The action this node represents */
	public final Action label;

	/**
	 * Constructs a new action node that belongs to a given graph and
	 * represents a given event.
	 * 
	 * @param graph the graph this node belongs to
	 * @param label the action this node represents
	 */
	protected ActionNode(HeuristicGraph graph, Action label) {
		super(graph, label);
		this.label = label;
		graph.actions.add(this);
	}
	
	@Override
	public boolean setCost(double cost) {
		return super.setCost(cost);
	}
	
	@Override
	protected void notify(Node node, Value value, double cost) {
		graph.next.add(this);
	}
}