package edu.uky.cs.nil.sabre.hg;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Disjunction;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A node which represents a logical expression in {@link
 * edu.uky.cs.nil.sabre.logic.Expression#toPrecondition() disjunctive normal}
 * form, a disjunction of {@link #clauses conjunctive clauses} where at least
 * one clause must have a finite cost before the disjunction can have a finite
 * cost. The {@link #getCost() cost} of a disjunction node is the cost of its
 * lowest cost clause.
 * 
 * @author Stephen G. Ware
 */
public class DisjunctionNode extends CostNode {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;

	/** The disjunctive normal form expression this node represents */
	public final Disjunction<Clause<Precondition>> label;
	
	/**
	 * The nodes representing the conjunctive clauses that make up the
	 * disjunction
	 */
	public final List<ClauseNode> clauses = new List<>();
	
	/** All nodes which depend on this disjunction as a condition */
	public final List<Node> conditionals = new List<>();
	
	/**
	 * Constructs a new disjunction node that belongs to a given graph and
	 * represents a given disjunctive normal form expression.
	 * 
	 * @param graph the graph this node belongs to
	 * @param label the disjunctive normal form expression this node represents
	 */
	protected DisjunctionNode(HeuristicGraph graph, Disjunction<Clause<Precondition>> label) {
		super(graph, label);
		this.label = label;
		for(Clause<Precondition> clause : label) {
			ClauseNode node = graph.getClause(clause);
			this.clauses.add(node);
			node.disjunctions.add(this);
		}
	}
	
	@Override
	protected boolean setCost(double cost) {
		if(super.setCost(cost)) {
			// Notify all nodes that depend on this disjunction that the disjunction has become true.
			for(int i=0; i<conditionals.size(); i++)
				conditionals.get(i).notify(this, True.TRUE, cost);
			return true;
		}
		else
			return false;
	}
	
	@Override
	protected void notify(Node node, Value value, double cost) {
		setCost(Math.min(getCost(), cost));
	}
}