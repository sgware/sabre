package edu.uky.cs.nil.sabre.etree;

import java.util.function.Consumer;

import edu.uky.cs.nil.sabre.Event;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.comp.CompiledEvent;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.util.ImmutableArray;
import edu.uky.cs.nil.sabre.util.ImmutableSet;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * An {@link ImmutableSet immutable set} of {@link
 * edu.uky.cs.nil.sabre.logic.Logical#isGround() ground} {@link Event events}
 * which does not initially build an {@link EventTree event tree}, but allows
 * one to be {@link #buildTree(Status) built} later and the result {@link
 * #getTree() cached}. Building an event tree can be expensive in both time and
 * memory, so some objects that use a set of events may not want to build one.
 * Also, once a tree is built, it is helpful to save the tree and reuse it later
 * if more than one object needs the tree.
 * <p>
 * This method provides the same event operations that {@link EventTree event
 * tree} provides: {@link #getEvery(State)} and {@link #getAny(State)}. If the
 * tree has been built for this set, that tree will be used to perform these
 * operations efficiently. If the tree has not been built, those methods will
 * be less efficient, because they require iteration through event event in the
 * set.
 * 
 * @param <E> the type of event in this set
 * @author Stephen G. Ware
 */
public class EventSet<E extends Event> extends ImmutableSet<E> {
	
	/** Serial version ID */
	private static final long serialVersionUID = 1L;
	
	/** The event tree built from this set */
	private EventTree<E> tree = null;

	/**
	 * Constructs a new event set by reusing the array underlying an immutable
	 * set of ground events.
	 * 
	 * @param events the set of events
	 * @throws edu.uky.cs.nil.sabre.FormatException if any events are not ground
	 */
	public EventSet(ImmutableSet<E> events) {
		super(events);
		check();
	}
	
	/**
	 * Constructs a new event set from an array of ground events.
	 * 
	 * @param events the array of events
	 * @throws edu.uky.cs.nil.sabre.FormatException if any events are not ground
	 */
	@SuppressWarnings("unchecked")
	public EventSet(E...events) {
		super(events);
		check();
	}
	
	/**
	 * Constructs a new event set from an {@link Iterable iterable} of ground
	 * events.
	 * 
	 * @param events the events
	 * @throws edu.uky.cs.nil.sabre.FormatException if any events are not ground
	 */
	public EventSet(Iterable<E> events) {
		super(events);
		check();
	}
	
	private final void check() {
		for(int i=0; i<size(); i++)
			get(i).mustBeGround();
	}
	
	/**
	 * Returns the {@link EventTree event tree} built from this set of events,
	 * or null if the tree has not yet been {@link #buildTree(Status) built}.
	 * 
	 * @return the event tree, or null if the tree has not been built
	 */
	public EventTree<E> getTree() {
		return tree;
	}
	
	/**
	 * Returns the {@link EventTree event tree} built from this set of events,
	 * building it if it did not already exist.
	 * 
	 * @param status a status to update while the tree is built
	 * @return the existing event tree, if one has been build before, or the
	 * newly built event tree
	 */
	public EventTree<E> buildTree(Status status) {
		if(tree == null)
			tree = new EventTreeBuilder<>(this).build(status);
		return tree;
	}
	
	/**
	 * Returns a collection of every event in this tree whose precondition is
	 * satisfied in a given state.
	 * 
	 * @param state the state
	 * @return every event whose precondition is satisfied
	 */
	@SuppressWarnings("unchecked")
	public Iterable<E> getEvery(State state) {
		EventTree<E> tree = getTree();
		if(tree == null)
			return (Iterable<E>) new ImmutableArray<>(every(this, state, 0, 0));
		else
			return tree.getEvery(state);
	}
	
	private static final Event[] every(ImmutableSet<? extends Event> set, State state, int index, int count) {
		if(index == set.size())
			return new CompiledEvent[count];
		else if(set.get(index).getPrecondition().evaluate(state).equals(True.TRUE)) {
			Event[] array = every(set, state, index + 1, count + 1);
			array[count] = set.get(index);
			return array;
		}
		else
			return every(set, state, index + 1, count);
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
		EventTree<E> tree = getTree();
		if(tree == null) {
			for(int i=0; i<size(); i++)
				if(get(i).getPrecondition().evaluate(state).equals(True.TRUE))
					consumer.accept(get(i));
		}
		else
			tree.forEvery(state, consumer);
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
		EventTree<E> tree = getTree();
		if(tree == null) {
			for(int i=0; i<size(); i++)
				if(get(i).getPrecondition().evaluate(state).equals(True.TRUE))
					return get(i);
			return null;
		}
		else
			return tree.getAny(state);
	}
}