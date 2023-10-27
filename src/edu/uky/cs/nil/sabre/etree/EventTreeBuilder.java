package edu.uky.cs.nil.sabre.etree;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import edu.uky.cs.nil.sabre.Entity;
import edu.uky.cs.nil.sabre.Event;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Disjunction;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Unknown;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ImmutableSet;
import edu.uky.cs.nil.sabre.util.UniqueMap;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * A factory object used to build an {@link EventTree event tree} from a set
 * of {@link Event events}. This class can be extended to modify how event
 * trees are built or to add new types of branches.
 * 
 * @param <E> the type of event in the tree to be built
 * @author Stephen G. Ware
 */
public class EventTreeBuilder<E extends Event> {

	/**
	 * Maps the events this tree (and its branches) will contain to their
	 * preconditions. Note a precondition in this map may not match the key
	 * event's precondition if this builder is being used to construct a branch
	 * where some expressions have been previously tested.
	 */
	protected final Map<E, Disjunction<Clause<Precondition>>> events;
	
	/**
	 * Constructs a new event tree builder that will build an event tree from
	 * a given collection of events.
	 * 
	 * @param events the events to be included in the tree
	 */
	public EventTreeBuilder(Iterable<E> events) {
		this(toMap(events));
	}
	
	/**
	 * Converts a collection of events into a map which maps each event to its
	 * {@link Event#getPrecondition() precondition}.
	 * 
	 * @param <E> the type of event in the tree to be built
	 * @param events the events to be included in the tree
	 * @return a map of events to preconditions
	 */
	protected static <E extends Event> Map<E, Disjunction<Clause<Precondition>>> toMap(Iterable<E> events) {
		LinkedHashMap<E, Disjunction<Clause<Precondition>>> map = new LinkedHashMap<>();
		for(E event : events)
			map.put(event, event.getPrecondition().toPrecondition());
		return map;
	}
	
	/**
	 * Constructs a new event tree builder using the given map of events to
	 * preconditions.
	 * 
	 * @param events a map of events to preconditions
	 */
	protected EventTreeBuilder(Map<E, Disjunction<Clause<Precondition>>> events) {
		this.events = events;
	}
	
	/**
	 * Constructs the event tree and all its branches. For large sets of events
	 * this method may take a long time to run. If the set of events for this
	 * builder is empty, this method will return {@link EventTree#EMPTY the
	 * empty tree}.
	 * 
	 * @param status the status object to be updated as the tree is built
	 * @return the event tree
	 */
	@SuppressWarnings("unchecked")
	public EventTree<E> build(Status status) {
		if(events.size() == 0)
			return (EventTree<E>) (EventTree<?>) EventTree.EMPTY;
		else
			return build(new EventTreeStatus(status));
	}
	
	/**
	 * Constructs the event tree and all its branches. If the set of events for
	 * this builder is empty, this method returns null, meaning this method
	 * should be used for building all branches except the root (use {@link
	 * #build(Status)} to build the root, since it will return the empty tree
	 * if the builder has no events).
	 * <p>
	 * If the preconditions of all events in this tree are either {@link
	 * False#FALSE false} or {@link True#TRUE true}, this method will build a
	 * leaf node containing all events whose preconditions are true. Otherwise,
	 * this method will {@link #chooseExpression(EventTreeStatus) choose an
	 * expression on which to branch} and create the appropriate event tree
	 * type and all of its branches for that expression.
	 * <p>
	 * This method should {@link EventTreeStatus#incrementTotalBranches()
	 * increment the total number of branches} as soon as it starts running
	 * and {@link EventTreeStatus#incrementCompleteBranches() increment the
	 * number of completed branches} right before it returns.
	 * 
	 * @param status the status object tracking how many branches have been
	 * started and completed
	 * @return the event tree
	 */
	protected EventTree<E> build(EventTreeStatus status) {
		EventTree<E> result = null;
		status.incrementTotalBranches();
		if(events.size() > 0) {
			if(events.values().stream().allMatch(p -> p.equals(False.FALSE) || p.equals(True.TRUE)))
				result = new EventTree<>(getSatisfiedEvents());
			else {
				Expression expression = chooseExpression(status);
				if(expression != null) {
					if(expression.isBoolean())
						result = buildBooleanEventTree(expression, status);
					else if(expression instanceof Fluent)
						result = buildEntityEventTree((Fluent) expression, status);
				}
			}
		}
		status.incrementCompleteBranches();
		return result;
	}
	
	/**
	 * Chooses an expression on which to branch. An ideal expression is one
	 * which splits the events in this tree into a large number of groups of
	 * about equal size.
	 * 
	 * @param status the status object
	 * @return the expression on which to branch
	 */
	protected Expression chooseExpression(EventTreeStatus status) {
		Expression best = null;
		int mostRelevant = 0;
		int mostValues = 0;
		for(Expression candidate : getCandidateExpressions()) {
			int relevant = countRelevantEvents(candidate);
			int values = countValues(candidate);
			if(relevant > mostRelevant || (relevant == mostRelevant && values > mostValues)) {
				best = candidate;
				mostRelevant = relevant;
				mostValues = values;
			}
		}
		return best;
	}
	
	private final Set<Expression> getCandidateExpressions() {
		LinkedHashSet<Expression> candidates = new LinkedHashSet<>();
		for(Entry<E, Disjunction<Clause<Precondition>>> entry : events.entrySet()) {
			for(Clause<Precondition> clause : entry.getValue()) {
				for(Precondition atom : clause) {
					if(atom.left.isNumber())
						candidates.add(atom);
					else
						candidates.add(atom.left);
				}
			}
		}
		return candidates;
	}
	
	private final int countRelevantEvents(Expression expression) {
		int count = 0;
		for(Entry<E, Disjunction<Clause<Precondition>>> entry : events.entrySet())
			if(entry.getValue().occurs(expression))
				count++;
		return count;
	}
	
	private final int countValues(Expression expression) {
		if(expression.isBoolean())
			return 2;
		else
			return ((Fluent) expression).type.getValues().size() + 1;
	}
	
	/**
	 * Builds a tree which branches based an expression of type {@code
	 * boolean}.
	 * 
	 * @param expression an expression of type {@code boolean}
	 * @param status the status object
	 * @return an event tree which branches on the expression
	 */
	protected EventTree<E> buildBooleanEventTree(Expression expression, EventTreeStatus status) {
		ImmutableSet<E> satisfied = getSatisfiedEvents();
		EventTree<E> falseBranch = new EventTreeBuilder<>(getRelevantEvents(expression, False.FALSE)).build(status);
		EventTree<E> trueBranch = new EventTreeBuilder<>(getRelevantEvents(expression, True.TRUE)).build(status);
		EventTree<E> irrelevant = new EventTreeBuilder<>(getIrrelevantEvents(expression)).build(status);
		return new BooleanEventTree<>(satisfied, expression, falseBranch, trueBranch, irrelevant);
	}
	
	/**
	 * Builds a tree which branches based on a fluent of type {@code entity}.
	 * 
	 * @param fluent the fluent of type {@code entity}
	 * @param status the status object
	 * @return an event tree which branches on the fluent
	 */
	protected EventTree<E> buildEntityEventTree(Fluent fluent, EventTreeStatus status) {
		ImmutableSet<E> satisfied = getSatisfiedEvents();
		UniqueMap<Entity, EventTree<E>> branches = new UniqueMap<>();
		for(Value value : fluent.type.getValues())
			branches.put((Entity) value, new EventTreeBuilder<>(getRelevantEvents(fluent, value)).build(status));
		EventTree<E> unknownBranch = new EventTreeBuilder<>(getRelevantEvents(fluent, Unknown.UNKNOWN)).build(status);
		EventTree<E> irrelevant = new EventTreeBuilder<>(getIrrelevantEvents(fluent)).build(status);
		return new EntityEventTree<>(satisfied, fluent, branches, unknownBranch, irrelevant);
	}
	
	/**
	 * Returns all events in this builder whose preconditions are {@link
	 * True#TRUE true} as a {@link ImmutableSet set}.
	 * 
	 * @return a set of events whose preconditions are satisfied
	 */
	protected ImmutableSet<E> getSatisfiedEvents() {
		LinkedHashSet<E> satisfied = new LinkedHashSet<>();
		for(Entry<E, Disjunction<Clause<Precondition>>> entry : events.entrySet())
			if(entry.getValue().equals(True.TRUE))
				satisfied.add(entry.getKey());
		return new ImmutableSet<>(satisfied);
	}
	
	/**
	 * Returns a new {@link LinkedHashMap map} of events from this builder
	 * which has preconditions that contain the given expression. The
	 * preconditions the events are mapped to will have the expression replaced
	 * with the given value.
	 * 
	 * @param expression the expression which needs to appear in an event's
	 * precondition
	 * @param value the value to replace that expression with
	 * @return a map of relevant events with their updated preconditions
	 */
	protected LinkedHashMap<E, Disjunction<Clause<Precondition>>> getRelevantEvents(Expression expression, Value value) {
		LinkedHashMap<E, Disjunction<Clause<Precondition>>> relevant = new LinkedHashMap<>();
		for(Entry<E, Disjunction<Clause<Precondition>>> entry : events.entrySet()) {
			Expression precondition = (Expression) entry.getValue().substitute(expression, value);
			if(precondition != entry.getValue()) {
				precondition = precondition.simplify();
				if(!precondition.equals(False.FALSE))
					relevant.put(entry.getKey(), precondition.toPrecondition());
			}
		}
		return relevant;
	}
	
	/**
	 * Returns a new {@link LinkedHashMap map} of events from this builder
	 * whose preconditions do not contain the given expressions. The
	 * preconditions the events are mapped to will be the same as their are
	 * in this builder. This method is generally used to build the {@link
	 * BranchingEventTree#irrelevant irrelevant branch}.
	 * 
	 * @param expression the expression which does not appear in an event's
	 * precondition
	 * @return a map of events for which this expression is irrelevant with
	 * the same preconditions they currently have
	 */
	protected LinkedHashMap<E, Disjunction<Clause<Precondition>>> getIrrelevantEvents(Expression expression) {
		LinkedHashMap<E, Disjunction<Clause<Precondition>>> irrelevant = new LinkedHashMap<>();
		for(Entry<E, Disjunction<Clause<Precondition>>> entry : events.entrySet())
			if(!entry.getValue().equals(False.FALSE) && !entry.getValue().equals(True.TRUE) && !entry.getValue().occurs(expression))
				irrelevant.put(entry.getKey(), entry.getValue());
		return irrelevant;
	}
}