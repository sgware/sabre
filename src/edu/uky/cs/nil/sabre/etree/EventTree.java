package edu.uky.cs.nil.sabre.etree;

import java.io.Serializable;
import java.util.Set;
import java.util.function.Consumer;

import edu.uky.cs.nil.sabre.Event;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Unknown;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * An event tree is a stateless data structure used to efficiently determine
 * which {@link Event events} have preconditions satisfied in a given {@link
 * State state}.
 * <p>
 * The parent {@link EventTree} class is used the {@link #EMPTY empty tree} and
 * for leaf nodes which contain events but not branches. Most event tree are
 * {@link BranchingEventTree branching events trees}.
 * 
 * @param <E> the type of event in this tree
 * @author Stephen G. Ware
 */
public class EventTree<E extends Event> implements Serializable {

	/** An empty event tree with no events and no branches */
	public static final EventTree<Event> EMPTY = new EventTree<>();
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** A set of no values */
	private static final ImmutableSet<Value> NO_VALUES = new ImmutableSet<>();
	
	/** A set of both Boolean values: false and true */
	private static final ImmutableSet<Value> BOOLEAN_VALUES = new ImmutableSet<>(False.FALSE, True.TRUE);
	
	/**
	 * Returns a set of all possible {@link Value values} that a given {@link
	 * Expression expression} could have. If the expression given is of type
	 * {@code boolean}, this method returns {@code {false, true}}. If the
	 * expression is of type {@code number}, this method throws an exception,
	 * since there are infinitely many possible values. Otherwise, this method
	 * assumes the expression given is a {@link Fluent fluent}, and it returns
	 * {@link
	 * edu.uky.cs.nil.sabre.Universe#getValues(edu.uky.cs.nil.sabre.Type) all
	 * possible values} of that fluent's type (including {@link Unknown
	 * unknown} if the fluent is of type {@code entity}).
	 * 
	 * @param expression the expression for which all possible values are
	 * wanted
	 * @return a set of possible values the expression could evaluate to
	 */
	public static ImmutableSet<Value> getBranches(Expression expression) {
		if(expression.equals(True.TRUE))
			return NO_VALUES;
		else if(expression.isBoolean())
			return BOOLEAN_VALUES;
		else
			return ((Fluent) expression).type.getValues().add(Unknown.UNKNOWN);
	}
	
	/** The events whose preconditions are satisfied */
	public final ImmutableSet<E> events;
	
	/** The expression on which this event tree will branch */
	public final Expression expression;
	
	/**
	 * Constructs a new event tree from a {@link ImmutableSet set} of events
	 * with a given expression.
	 * 
	 * @param events events whose preconditions are satisfied
	 * @param expression the expression on which the tree will branch
	 */
	protected EventTree(ImmutableSet<E> events, Expression expression) {
		this.events = events;
		this.expression = expression;
	}
	
	/**
	 * Constructs a new event tree from a {@link ImmutableSet set} of events.
	 * 
	 * @param events events whose preconditions are satisfied
	 */
	public EventTree(ImmutableSet<E> events) {
		this(events, True.TRUE);
	}
	
	/**
	 * Constructs a new event tree from a {@link Set set} of events.
	 * 
	 * @param events events whose preconditions are satisfied
	 */
	public EventTree(Set<E> events) {
		this(new ImmutableSet<>(events));
	}
	
	/**
	 * Constructs a new event tree from an array of events.
	 * 
	 * @param events events whose preconditions are satisfied
	 */
	@SuppressWarnings("unchecked")
	public EventTree(E...events) {
		this(new ImmutableSet<>(events));
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	/**
	 * Event trees branch based on the value of their {@link #expression
	 * expression} in some state. This method is used to get the branch of the
	 * tree for which the expression has the given value. When this method is
	 * called with {@code null} as its argument, it will return the {@link
	 * BranchingEventTree#irrelevant irrelevant branch} if one exists. If this
	 * tree has no branches for the given value, this method returns {@code
	 * null}.
	 * 
	 * @param value the value of the tree's expression in some state
	 * @return the branch corresponding to the tree's expression have that
	 * value
	 */
	public EventTree<E> getBranch(Value value) {
		return null;
	}
	
	/**
	 * Returns a collection of every event in this tree whose precondition is
	 * satisfied in a given state.
	 * 
	 * @param state the state
	 * @return every event whose precondition is satisfied
	 */
	public Iterable<E> getEvery(State state) {
		return events;
	}
	
	/**
	 * Every event in this tree whose precondition is satisfied in a given
	 * state will be passed to the given consumer.
	 * 
	 * @param state the state
	 * @param consumer the consumer which will receive every event whose
	 * precondition is satisfied
	 */
	public void forEvery(State state, Consumer<? super E> consumer) {
		for(int i=0; i<events.size(); i++)
			consumer.accept(events.get(i));
	}
	
	/**
	 * Returns the first event found in this tree whose precondition is
	 * satisfied in a given state.
	 * 
	 * @param state the state
	 * @return the first event whose precondition is satisfied, or null if no
	 * event's preconditions are satisfied
	 */
	public E getAny(State state) {
		if(events.size() == 0)
			return null;
		else
			return events.get(0);
	}
}