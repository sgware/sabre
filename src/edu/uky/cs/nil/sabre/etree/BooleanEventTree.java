package edu.uky.cs.nil.sabre.etree;

import edu.uky.cs.nil.sabre.Event;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * A {@link BranchingEventTree branching event tree} whose {@link
 * EventTree#expression expression} is of type {@code boolean} and will have
 * three possible branches: one where the expression is {@link False#FALSE
 * false}, one where the expression is {@link True#TRUE true}, and the {@link
 * BranchingEventTree#irrelevant irrelevant branch}.
 * <p>
 * Boolean event trees can be used for numeric preconditions. An event tree's
 * {@link EventTree#expression expression} cannot be of type {@code number},
 * because the tree must have a branch for every possible value of the
 * expression, and there are infinitely many numbers, however preconditions on
 * numeric values, like {@code (n > 5)}, are of type {@code boolean}, so they
 * can be the expression for a Boolean event tree.
 * 
 * @param <E> the type of event in this tree
 * @author Stephen G. Ware
 */
public class BooleanEventTree<E extends Event> extends BranchingEventTree<E> {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** Events whose preconditions require this tree's expression to be false */
	public final EventTree<E> falseBranch;
	
	/** Events whose preconditions require this tree's expression to be true */
	public final EventTree<E> trueBranch;

	/**
	 * Constructs a new Boolean event tree.
	 * 
	 * @param events events whose preconditions are satisfied
	 * @param expression the expression on which the tree will branch
	 * @param falseBranch the branch of the tree containing events whose
	 * preconditions require the expression to be false
	 * @param trueBranch the branch of the tree containing events whose
	 * preconditions require the expression to be true
	 * @param irrelevant the branch of the tree containing events whose
	 * preconditions do not involve the expression
	 */
	public BooleanEventTree(ImmutableSet<E> events, Expression expression, EventTree<E> falseBranch, EventTree<E> trueBranch, EventTree<E> irrelevant) {
		super(events, expression, irrelevant);
		expression.mustBeBoolean();
		this.falseBranch = falseBranch;
		this.trueBranch = trueBranch;
	}

	@Override
	public EventTree<E> getBranch(Value value) {
		if(value == null)
			return irrelevant;
		else if(value.equals(False.FALSE))
			return falseBranch;
		else if(value.equals(True.TRUE))
			return trueBranch;
		else
			return null;
	}
}