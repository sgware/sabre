package edu.uky.cs.nil.sabre.hg;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A node which represents a {@link Clause conjunctive clause}, a conjunction
 * of {@link #preconditions atomic preconditions} that must all have a finite
 * cost before the clause can have a finite cost.
 * 
 * @author Stephen G. Ware
 */
public class ClauseNode extends CostNode {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The conjunctive clause this node represents */
	public final Clause<Precondition> label;
	
	/** The nodes representing the preconditions that make up the clause */
	public final List<PreconditionNode> preconditions = new List<>();
	
	/** The nodes representing all disjunctions that this clause appears in */
	public final List<DisjunctionNode> disjunctions = new List<>();
	
	/**
	 * The number of times {@link #notify(Node, Value, double)} has been
	 * called (which, in an ideal situation, is also the number of
	 * preconditions in the clause that are true, though this is not
	 * guaranteed to be the case)
	 */
	private int count = 0;

	/**
	 * Constructs a new clause node that belongs to a given graph and
	 * represents a given conjunctive clause.
	 * 
	 * @param graph the graph this node belongs to
	 * @param label the conjunctive clause this node represents
	 */
	protected ClauseNode(HeuristicGraph graph, Clause<Precondition> label) {
		super(graph, label);
		this.label = label;
		for(Precondition precondition : label) {
			PreconditionNode node = graph.getPrecondition(precondition);
			this.preconditions.add(node);
			node.clauses.add(this);
		}
	}
	
	@Override
	protected boolean setCost(double cost) {
		if(super.setCost(cost)) {
			// Notify all disjunctions this clause appears in that this clause has become true.
			for(int i=0; i<disjunctions.size(); i++)
				disjunctions.get(i).notify(this, True.TRUE, cost);
			return true;
		}
		else
			return false;
	}
	
	@Override
	protected void notify(Node node, Value value, double cost) {
		markForReset();
		if(++count >= preconditions.size() && preconditions.size() > 0)
			setCost(graph.cost(this));
	}
	
	@Override
	protected void reset() {
		super.reset();
		count = 0;
	}
}