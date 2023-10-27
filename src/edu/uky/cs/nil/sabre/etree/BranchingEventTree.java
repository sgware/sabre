package edu.uky.cs.nil.sabre.etree;

import java.util.ArrayList;
import java.util.function.Consumer;

import edu.uky.cs.nil.sabre.Event;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * For each possible value that the tree's {@link EventTree#expression
 * expression} can have, a branching event tree provides a branch containing
 * the events whose preconditions require the expression to have that value,
 * as well as an {@link #irrelevant irrelevant branch} which contains the
 * events that do not use the expression in their preconditions.
 * <p>
 * To find all events whose preconditions are satisfied in some state, one
 * should evaluate the tree's expression in that state and then follow two
 * of the tree's branches: the branch corresponding to the expression's
 * value and the irrelevant branch. When one encounters a tree which defines
 * some {@link EventTree#events events}, it means that event's preconditions
 * are known to be satisfied.
 * <p>
 * Here is some simple pseudocode for using event tree {@code t} to collect all
 * events whose preconditions are satisfied in state {@code s} into a list
 * {@code e}:<br>
 * <pre>collect(state s, event tree t, list e):
 *     Add all of t's events to e.
 *     Let v be the result of evaluating t's expression in s.
 *     Let b be the branch of t for value v.
 *     Let i be the irrelevant branch of t.
 *     collect(s, b, e).
 *     collect(s, i, e).
 * }</pre>
 * 
 * @param <E> the type of event in this tree
 * @author Stephen G. Ware
 */
public abstract class BranchingEventTree<E extends Event> extends EventTree<E> {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** Events which do not have this tree's expression in their precondition */
	public final EventTree<E> irrelevant;

	/**
	 * Constructs a new branching event tree.
	 * 
	 * @param events events whose preconditions are satisfied
	 * @param expression the expression on which the tree will branch
	 * @param irrelevant the branch of the tree containing events whose
	 * preconditions do not involve the expression
	 */
	public BranchingEventTree(ImmutableSet<E> events, Expression expression, EventTree<E> irrelevant) {
		super(events, expression);
		this.irrelevant = irrelevant;
	}
	
	@Override
	public abstract EventTree<E> getBranch(Value value);
	
	@Override
	public Iterable<E> getEvery(State state) {
		ArrayList<E> list = new ArrayList<>();
		forEvery(state, e -> list.add(e));
		return list;
	}
	
	@Override
	public void forEvery(State state, Consumer<? super E> consumer) {
		forEvery(state, this, consumer);
	}
	
	private static final <E extends Event> void forEvery(State state, EventTree<E> tree, Consumer<? super E> consumer) {
		if(tree == null)
			return;
		for(int i=0; i<tree.events.size(); i++)
			consumer.accept(tree.events.get(i));
		forEvery(state, tree.getBranch(tree.expression.evaluate(state)), consumer);
		forEvery(state, tree.getBranch(null), consumer);
	}
	
	@Override
	public E getAny(State state) {
		return forAny(state, this);
	}
	
	private static final <E extends Event> E forAny(State state, EventTree<E> tree) {
		if(tree == null)
			return null;
		else if(tree.events.size() > 0)
			return tree.events.get(0);
		E result = forAny(state, tree.getBranch(tree.expression.evaluate(state)));
		if(result == null)
			result = forAny(state, tree.getBranch(null));
		return result;
	}
}