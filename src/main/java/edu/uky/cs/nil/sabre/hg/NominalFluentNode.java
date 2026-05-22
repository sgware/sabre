package edu.uky.cs.nil.sabre.hg;

import java.util.Arrays;
import java.util.Iterator;

import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Comparison;
import edu.uky.cs.nil.sabre.logic.Comparison.Operator;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ArrayIterator;

/**
 * A {@link FluentNode fluent node} representing a {@link
 * edu.uky.cs.nil.sabre.logic.Expression#mustNotBeNumber() non-numeric} {@link
 * Fluent fluent}, which is any fluent that has a discrete, finite set of
 * possible values.
 * 
 * @author Stephen G. Ware
 */
public class NominalFluentNode extends FluentNode {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * An array of {@link #getGroup(Value) value groups}, indexed by the hash
	 * code of the group's value
	 */
	protected final List<PreconditionNode>[] groups;

	/**
	 * Constructs a new fluent node that belongs to a given graph and
	 * represents a given nominal fluent.
	 * 
	 * @param graph the graph this node belongs to
	 * @param label the nominal fluent this node represents
	 * @throws edu.uky.cs.nil.sabre.FormatException if the fluent is numeric
	 */
	@SuppressWarnings("unchecked")
	protected NominalFluentNode(HeuristicGraph graph, Fluent label) {
		super(graph, label);
		label.mustNotBeNumber();
		this.groups = new List[label.type.universe.entities.size() + 1];
	}
	
	@Override
	protected void register(PreconditionNode precondition) {
		super.register(precondition);
		if(precondition.label.operator.equals(Comparison.EQUAL_TO) && precondition.label.right instanceof Value) {
			List<PreconditionNode> group = getGroup((Value) precondition.label.right);
			group.add(precondition);
			for(PreconditionNode other : preconditions)
				register(group, other);
		}
		else if(precondition.label.operator.equals(Comparison.NOT_EQUAL_TO))
			for(List<PreconditionNode> group : groups)
				if(group != null)
					register(group, precondition);
		for(EffectNode effect : effects)
			register(precondition, effect);
	}
	
	@Override
	protected void register(EffectNode effect) {
		super.register(effect);
		for(PreconditionNode precondition : preconditions)
			register(precondition, effect);
	}
	
	private final void register(List<PreconditionNode> group, PreconditionNode precondition) {
		if(group.get(0).label.combine(precondition.label) != null)
			group.add(precondition);
	}
	
	private final void register(PreconditionNode precondition, EffectNode effect) {
		Precondition p = new Precondition(Comparison.EQUAL_TO, effect.label.fluent, effect.label.value);
		if(precondition.label.combine(p) != null)
			precondition.effects.add(effect);
	}
	
	/**
	 * Returns the value group for a given value. A value group is all
	 * precondition nodes that will have a finite cost once this node's fluent
	 * has a finite {@link FluentNode#getCost(Value) cost for some value}. It
	 * is a list of nodes where the first node is always an {@link
	 * edu.uky.cs.nil.sabre.logic.Comparison#EQUAL_TO equal to} {@link
	 * PreconditionNode precondition node} with this node's fluent on the left
	 * and a {@link ConstantNode constant node} for the group's value on the
	 * right. All other nodes in the list are {@link
	 * edu.uky.cs.nil.sabre.logic.Comparison#NOT_EQUAL_TO not equal to}
	 * precondition nodes with this node's fluent on the left and some other
	 * value on the right.
	 * 
	 * @param value the value whose group of precondition nodes is desired
	 * @return a list of precondition nodes which will have a finite cost once
	 * this node's fluent has a finite cost for that value
	 */
	protected List<PreconditionNode> getGroup(Value value) {
		List<PreconditionNode> group = groups[value.hashCode() + 1];
		if(group == null) {
			group = new List<>();
			groups[value.hashCode() + 1] = group;
			graph.getPrecondition(new Precondition(Comparison.EQUAL_TO, label, value));
		}
		return group;
	}
	
	@Override
	public int size() {
		return entries().length;
	}
	
	@Override
	public Iterator<Entry> iterator() {
		return new ArrayIterator<>(entries());
	}

	@Override
	public Value getValue(int index) {
		return entries()[index].value;
	}

	@Override
	public double getCost(int index) {
		return entries()[index].cost;
	}
	
	@Override
	public double getCost(Value value) {
		return getGroup(value).get(0).getCost();
	}

	@Override
	public double getCost(Operator operator, Value value) {
		for(Entry entry : entries())
			if(operator.test(entry.value, value))
				return entry.cost;
		return Double.POSITIVE_INFINITY;
	}
	
	/**
	 * Returns an array of {@link CostSet.Entry cost set entries} for every
	 * value (and its associated cost) that this fluent can have.
	 * 
	 * @return an array of cost set entries
	 */
	protected Entry[] entries() {
		Entry[] entries = entries(0, 0);
		Arrays.sort(entries);
		return entries;
	}

	private final Entry[] entries(int index, int size) {
		if(index == groups.length)
			return new Entry[size];
		else if(groups[index] == null || groups[index].get(0).getCost() == Double.POSITIVE_INFINITY)
			return entries(index + 1, size);
		else {
			PreconditionNode node = groups[index].get(0);
			Entry[] array = entries(index + 1, size + 1);
			array[size] = new Entry((Value) node.label.right, node.getCost());
			return array;
		}
	}
	
	@Override
	public boolean setCost(Value value, double cost) {
		List<PreconditionNode> group = getGroup(value);
		if(group.get(0).setCost(cost)) {
			super.setCost(value, cost);
			for(int i=1; i<group.size(); i++)
				group.get(i).setCost(cost);
			return true;
		}
		else
			return false;
	}
}