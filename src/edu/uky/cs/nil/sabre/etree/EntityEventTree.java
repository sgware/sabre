package edu.uky.cs.nil.sabre.etree;

import edu.uky.cs.nil.sabre.Entity;
import edu.uky.cs.nil.sabre.Event;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Unknown;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ImmutableSet;
import edu.uky.cs.nil.sabre.util.UniqueMap;

/**
 * A {@link BranchingEventTree branching event tree} whose {@link
 * EventTree#expression expression} is a {@link Fluent fluent} of type {@code
 * entity} and will have several possible branches: one for each possible value
 * the fluent could have, one where the fluent's value is {@link Unknown
 * unknown}, and the {@link BranchingEventTree#irrelevant irrelevant branch}.
 * 
 * @param <E> the type of event in this tree
 * @author Stephen G. Ware
 */
public class EntityEventTree<E extends Event> extends BranchingEventTree<E> {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * The fluent of type {@code entity} on which this event tree will branch
	 */
	public final Fluent expression;
	
	/**
	 * For each possible value of the fluent, this map contains a branch with
	 * the events whose preconditions require the fluent to have that value
	 */
	protected final UniqueMap<Entity, EventTree<E>> branches;
	
	/** Events whose preconditions require this tree's fluent to be unknown */
	protected final EventTree<E> unknownBranch;

	/**
	 * Constructs a new entity event tree.
	 * 
	 * @param events events whose preconditions are satisfied
	 * @param fluent the fluent on whose value this tree will branch
	 * @param branches maps each possible value of the fluent to a branch
	 * containing events whose preconditions require the fluent to have that
	 * value
	 * @param unknownBranch events whose preconditions require the fluent to be
	 * unknown
	 * @param irrelevant the branch of the tree containing events whose
	 * preconditions do not involve the fluent
	 */
	public EntityEventTree(ImmutableSet<E> events, Fluent fluent, UniqueMap<Entity, EventTree<E>> branches, EventTree<E> unknownBranch, EventTree<E> irrelevant) {
		super(events, fluent, irrelevant);
		fluent.mustBeEntity();
		this.expression = fluent;
		this.branches = branches;
		this.unknownBranch = unknownBranch;
	}

	@Override
	public EventTree<E> getBranch(Value value) {
		if(value == null)
			return irrelevant;
		else if(value.equals(Unknown.UNKNOWN))
			return unknownBranch;
		else if(value instanceof Entity)
			return branches.get((Entity) value);
		else
			return null;
	}
}