package edu.uky.cs.nil.sabre.graph;

import java.io.Serializable;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Event;
import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.FiniteState;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Trigger;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.Unique;

/**
 * A state node represents an assignment of a {@link Value value} to every
 * {@link StateGraph#fluents fluent tracked by a state graph} as well as the
 * beliefs of every {@link StateGraph#characters character tracked by the
 * graph}. Permanent (that is, non-{@link TemporaryNode temporary}) nodes are
 * unique, meaning there are no other state nodes in the same graph which
 * represent exactly the same state. Note that a state is not defined only by
 * the values assigned to fluents but also includes all beliefs about fluents,
 * beliefs about beliefs, and so on. This means that a state is actually defined
 * by a node and all nodes which can be reached via {@link EpistemicEdge
 * epistemic edges} from this node.
 * 
 * @author Stephen G. Ware
 */
public class StateNode implements Serializable, Unique, FiniteState {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The graph to which this node belongs */
	public final StateGraph graph;
	
	/** A unique, sequential ID number for this node in the graph */
	public final int id;
	
	/**
	 * The values for each of the fluents tracked by this graph, where the
	 * value at index i is the value for the fluent at index i in {@link
	 * StateGraph#fluents graph's set of fluents}
	 */
	Value[] values;
	
	/** The first temporal edge that has this node as its parent */
	TemporalEdge temporalOut = null;
	
	/** The first temporal edge that has this node as its child */
	TemporalEdge temporalIn = null;
	
	/** The first epistemic edge that has this node as its parent */
	EpistemicEdge epistemicOut = null;
	
	/** The first epistemic edge that has this node as its child */
	EpistemicEdge epistemicIn = null;
	
	/**
	 * Constructs a new state node with a given ID number and set of values.
	 * 
	 * @param graph the graph to which this node belongs
	 * @param id a unique sequential ID number
	 * @param values the values of each of the graph's fluents
	 */
	StateNode(StateGraph graph, int id, Value[] values) {
		this.graph = graph;
		this.id = id;
		this.values = values;
	}
	
	/**
	 * Constructs a new permanent state node from a {@link TemporaryNode
	 * temporary} or {@link InitialNode initial} state node.
	 * 
	 * @param temporary the non-permanent state node this permanent node will
	 * replace
	 */
	StateNode(StateNode temporary) {
		this(temporary.graph, temporary.graph.states.size(), temporary.values);
		copy(this, temporary.epistemicOut);
		graph.states.put(new StateKey(this), this);
	}
	
	private static final void copy(StateNode parent, EpistemicEdge edge) {
		if(edge != null) {
			copy(parent, (EpistemicEdge) edge.nextOut);
			new EpistemicEdge(parent, edge.label, edge.child);
		}
	}
	
	@Override
	public int hashCode() {
		return id;
	}
	
	@Override
	public String toString() {
		return Integer.toString(id);
	}

	@Override
	public Value getValue(Fluent fluent) {
		if(fluent.characters.size() == 0) {
			Integer index = graph.index.get(fluent);
			if(index == null)
				throw Exceptions.notDefined("Fluent", fluent.toString());
			else
				return values[index.intValue()];
		}
		else
			return getBeliefs((Character) fluent.characters.get(0)).getValue(fluent.removeFirstCharacter());
	}
	
	@Override
	public StateNode getBeliefs(Character character) {
		if(character == null)
			return this;
		else
			return getEpistemicChild(character).child;
	}
	
	/**
	 * Returns a {@link EdgeIterable collection} of all {@link TemporalEdge
	 * temporal edges} in the graph that have this node as their {@link
	 * Edge#parent parent}.
	 * 
	 * @return a collection of temporal nodes with this node as parent
	 */
	public EdgeIterable<TemporalEdge> getTemporalChildren() {
		return new EdgeIterable<>(temporalOut, Edge.OUT);
	}
	
	/**
	 * Returns a {@link TemporalEdge temporal edge} that has this node as its
	 * {@link Edge#parent parent} and a given {@link Event event} as its {@link
	 * TemporalEdge#label label}, if one exists in the graph. The {@link
	 * Edge#child child} of the edge will be the state of the world after
	 * taking the given event in the state represented by this node. If the
	 * edge could exist but does not, this method will not create it. See
	 * {@link #after(Action)}.
	 * 
	 * @param event an event which can occur in this state
	 * @return the temporal edge with this node as its parent, the given event
	 * as its label, and the state after this event as its child, or null if no
	 * such edge has been created yet
	 */
	public TemporalEdge getTemporalChild(Event event) {
		return (TemporalEdge) find(event, temporalOut);
	}
	
	void removeTemporalChild(TemporalEdge edge) {
		temporalOut = (TemporalEdge) remove(edge, temporalOut, Edge.OUT);
	}
	
	/**
	 * Returns a {@link EdgeIterable collection} of all {@link TemporalEdge
	 * temporal edges} that have this node as the {@link Edge#child child} in
	 * the graph. If edges could exist but do not, this method will not create
	 * them.
	 * 
	 * @return all temporal edges with this node as the child
	 */
	public EdgeIterable<TemporalEdge> getTemporalParents() {
		return new EdgeIterable<>(temporalIn, Edge.IN);
	}
	
	void removeTemporalParent(TemporalEdge edge) {
		temporalIn = (TemporalEdge) remove(edge, temporalIn, Edge.IN);
	}
	
	/**
	 * Returns a {@link EdgeIterable collection} of all {@link EpistemicEdge
	 * epistemic edges} that have this node as the {@link Edge#parent parent}
	 * in the graph. There will always be exactly as many edges as there are
	 * {@link StateGraph#characters characters tracked by the graph}. These
	 * edges represent the state of the world these characters believe to be
	 * that case when the actual state is this node.
	 * 
	 * @return all epistemic edges with this node as the parent
	 */
	public EdgeIterable<EpistemicEdge> getEpistemicChildren() {
		return new EdgeIterable<>(epistemicOut, Edge.OUT);
	}
	
	/**
	 * Returns the {@link EpistemicEdge epistemic edge} that has this node as
	 * the {@link Edge#parent parent} and a given {@link Character character} as
	 * the {@link EpistemicEdge#label label}. The {@link Edge#child child} of
	 * the edge will be the state the character believes the world to be in when
	 * the world is actually in the state represented by this node. The edge
	 * will always exist, unless the character is not one of the {@link
	 * StateGraph#characters characters tracked by the graph}.
	 * 
	 * @param character the character whose beliefs are desired
	 * @return an edge with this node as the parent, the given character as the
	 * label, and the character's beliefs as the child
	 */
	public EpistemicEdge getEpistemicChild(Character character) {
		return (EpistemicEdge) find(character, epistemicOut);
	}
	
	void removeEpistemicChild(Edge edge) {
		epistemicOut = (EpistemicEdge) remove(edge, epistemicOut, Edge.OUT);
	}
	
	/**
	 * Returns a {@link EdgeIterable collection} of all {@link EpistemicEdge
	 * epistemic edges} that have this node as the {@link Edge#child child} in
	 * the graph.
	 * 
	 * @return all epistemic edges with this node as the child
	 */
	public EdgeIterable<EpistemicEdge> getEpistemicParents() {
		return new EdgeIterable<>(epistemicIn, Edge.IN);
	}
	
	void removeEpistemicParent(EpistemicEdge edge) {
		epistemicIn = (EpistemicEdge) remove(edge, epistemicIn, Edge.IN);
	}
	
	/**
	 * Returns the state that would result from applying the effects of the
	 * given action to this state. In other words, this action finds a {@link
	 * TemporalEdge temporal edge} with this node as the {@link Edge#parent
	 * parent}, the given action as the {@link TemporalEdge#label label}, and
	 * returns the edge's {@link Edge#child child}. If the edge does not
	 * already exist, it will be created. This method does not check whether
	 * the {@link Action#precondition precondition of the action} is satisfied
	 * in this state; it simply applies {@link Action#effect effect of the
	 * action}.
	 * 
	 * @param action the action whose effect will modify this state
	 * @return the state after applying the action's effect
	 */
	public StateNode after(Action action) {
		return after((Event) action);
	}
	
	StateNode after(Event event) {
		TemporalEdge edge = getTemporalChild(event);
		if(edge == null)
			edge = new TemporalEdge(this, event, new TemporaryNode(this, event)).resolve();
		return edge.child;
	}
	
	/**
	 * Returns the state after applying all {@link StateGraph#triggers triggers
	 * tracked by the graph} to this state. Specifically, this method finds a
	 * trigger whose precondition is satisfied in this state (or any
	 * epistemically accessible state), applies its effect, and then
	 * recursively does the same thing in the resulting state, stopping when no
	 * more triggers can occur. This method should generally be called after
	 * {@link #after(Action)} to find the state of the world after taking an
	 * action.
	 * 
	 * @return the state after applying all relevant triggers
	 */
	public StateNode afterTriggers() {
		TemporalEdge edge = temporalOut;
		while(edge != null) {
			if(edge.label instanceof Trigger)
				return edge.child.afterTriggers();
			else if(edge.label == null) {
				if(edge.child == this)
					return resolve();
				else
					return edge.child.afterTriggers();
			}
			edge = (TemporalEdge) edge.nextOut;
		}
		Trigger trigger = graph.triggers.getAny(this);
		if(trigger != null)
			return after(trigger).afterTriggers();
		StateNode after = after(null);
		if(after == this)
			return this;
		else
			return after.afterTriggers();
	}
	
	/**
	 * Returns a permanent state node from the graph which is equivalent to
	 * this state node. If this node is a permanent state, this method returns
	 * this node; otherwise, it finds or creates a permanent state node in the
	 * graph and returns it.
	 * 
	 * @return a permanent state node equivalent to this state node
	 */
	StateNode resolve() {
		return this;
	}
	
	private static final Edge find(Object label, Edge first) {
		while(first != null) {
			if(Utilities.equals(label, first.label))
				return first;
			first = first.nextOut;
		}
		return null;
	}
	
	private static final Edge remove(Edge edge, Edge first, Edge.Group group) {
		if(first == null)
			return null;
		else if(first == edge)
			return group.getNext(first);
		Edge previous = first;
		Edge current = group.getNext(first);
		while(current != null) {
			if(current == edge) {
				group.setNext(previous, group.getNext(current));
				break;
			}
			previous = current;
			current = group.getNext(current);
		}
		return first;
	}
}