package edu.uky.cs.nil.sabre.graph;

import java.io.Serializable;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Event;
import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.FiniteState;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Plan;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Solution;
import edu.uky.cs.nil.sabre.Trigger;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.logic.Comparison;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Value;

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
public class StateNode implements Serializable, FiniteState {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The state graph to which this node belongs */
	public final StateGraph graph;
	
	/** A unique, sequential ID number for this node in the graph */
	public final long id;
	
	/**
	 * The values for each of the fluents tracked by this graph, where the
	 * value at index i is the value for the fluent at index i in {@link
	 * StateGraph#fluents graph's set of fluents}
	 */
	protected Value[] values;
	
	/** The first epistemic edge that has this node as its parent */
	private EpistemicEdge epistemicOut = null;
	
	/** The first epistemic edge that has this node as its child */
	private EpistemicEdge epistemicIn = null;
	
	/** The first temporal edge that has this node as its parent */
	private TemporalEdge temporalOut = null;
	
	/** The first temporal edge that has this node as its child */
	private TemporalEdge temporalIn = null;
	
	/**
	 * Constructs a new state node with a given ID number and set of values.
	 * 
	 * @param graph the graph to which this node belongs
	 * @param id a unique sequential ID number
	 * @param values the values of each of the graph's fluents
	 */
	StateNode(StateGraph graph, long id, Value[] values) {
		this.graph = graph;
		this.id = id;
		this.values = values;
	}
	
	@Override
	public String toString() {
		return Long.toString(id);
	}
	
	@Override
	public int hashCode() {
		return Long.hashCode(id);
	}

	@Override
	public Value getValue(Fluent fluent) {
		if(fluent.characters.size() == 0) {
			Integer index = graph.index.apply(fluent);
			if(index == null)
				throw Exceptions.notDefined("Fluent", fluent.toString());
			else
				return values[index];
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
	 * Returns the utility value of the given character in the state represented
	 * by this node, where utility is a numeric representation of how satisfied
	 * the character is with the current state.
	 * 
	 * @param character the character whose utility value is desired
	 * @return the character's utility value in this state
	 */
	public Value getUtility(Character character) {
		if(character == null)
			return graph.utility.evaluate(this);
		else
			return graph.utilities.get(character).evaluate(this);
	}
	
	/**
	 * Returns a collection of all {@link EpistemicEdge epistemic edges} that
	 * have this node as the {@link Edge#parent parent} in the graph. There will
	 * always be exactly as many edges as there are {@link StateGraph#characters
	 * characters tracked by the graph}. These edges represent the state of the
	 * world these characters believe to be the case when the actual state is
	 * this node.
	 * 
	 * @return all epistemic edges with this node as the parent
	 */
	public Iterable<EpistemicEdge> getEpistemicChildren() {
		return () -> new EdgeIterator<>(Edge.OUT, epistemicOut);
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
		return findOutEdge(epistemicOut, character);
	}
	
	/**
	 * Adds an epistemic edge to this node's epistemic children.
	 * 
	 * @param edge the epistemic edge to add
	 */
	void addOutEdge(EpistemicEdge edge) {
		epistemicOut = addEdge(Edge.OUT, epistemicOut, edge);
	}
	
	/**
	 * Removes an epistemic edge to this node's epistemic children.
	 * 
	 * @param edge the epistemic edge to remove
	 */
	void removeOutEdge(EpistemicEdge edge) {
		epistemicOut = removeEdge(Edge.OUT, epistemicOut, edge);
	}
	
	/**
	 * Returns a collection of all {@link EpistemicEdge epistemic edges} that
	 * have this node as the {@link Edge#child child} in the graph.
	 * 
	 * @return all epistemic edges with this node as the child
	 */
	public Iterable<EpistemicEdge> getEpistemicParents() {
		return () -> new EdgeIterator<>(Edge.IN, epistemicIn);
	}
	
	/**
	 * Adds an epistemic edge to this node's epistemic parents.
	 * 
	 * @param edge the epistemic edge to add
	 */
	void addInEdge(EpistemicEdge edge) {
		epistemicIn = addEdge(Edge.IN, epistemicIn, edge);
	}
	
	/**
	 * Removes an epistemic edge from this node's epistemic parents.
	 * 
	 * @param edge the epistemic edge to remove
	 */
	void removeInEdge(EpistemicEdge edge) {
		epistemicIn = removeEdge(Edge.IN, epistemicIn, edge);
	}
	
	/**
	 * Returns a collection of all {@link TemporalEdge temporal edges} in the
	 * graph that have this node as their {@link Edge#parent parent}.
	 * 
	 * @return a collection of temporal nodes with this node as parent
	 */
	public Iterable<TemporalEdge> getTemporalChildren() {
		return () -> new EdgeIterator<>(Edge.OUT, temporalOut);
	}
	
	/**
	 * Returns a {@link TemporalEdge temporal edge} that has this node as its
	 * {@link Edge#parent parent} and a given {@link Event event} as its {@link
	 * TemporalEdge#label label}, if one exists in the graph. The {@link
	 * Edge#child child} of the edge will be the state of the world after
	 * taking the given event in the state represented by this node. If the
	 * edge could exist but does not, this method will not create it. See
	 * {@link #getAfter(Action)}.
	 * 
	 * @param event an event which can occur in this state
	 * @return the temporal edge with this node as its parent, the given event
	 * as its label, and the state after this event as its child, or null if no
	 * such edge has been created yet
	 */
	public TemporalEdge getTemporalChild(Event event) {
		return findOutEdge(temporalOut, event);
	}
	
	/**
	 * Adds a temporal edge to this node's temporal children.
	 * 
	 * @param edge the temporal edge to add
	 */
	void addOutEdge(TemporalEdge edge) {
		temporalOut = addEdge(Edge.OUT, temporalOut, edge);
	}
	
	/**
	 * Removes a temporal edge to this node's temporal children.
	 * 
	 * @param edge the temporal edge to remove
	 */
	void removeOutEdge(TemporalEdge edge) {
		temporalOut = removeEdge(Edge.OUT, temporalOut, edge);
	}
	
	/**
	 * Returns a collection of all {@link TemporalEdge temporal edges} that have
	 * this node as the {@link Edge#child child} in the graph. If edges could
	 * exist but do not, this method will not create them.
	 * 
	 * @return all temporal edges with this node as the child
	 */
	public Iterable<TemporalEdge> getTemporalParents() {
		return () -> new EdgeIterator<>(Edge.IN, temporalIn);
	}
	
	/**
	 * Adds a temporal edge to this node's temporal parents.
	 * 
	 * @param edge the temporal edge to add
	 */
	void addInEdge(TemporalEdge edge) {
		temporalIn = addEdge(Edge.IN, temporalIn, edge);
	}
	
	/**
	 * Removes a temporal edge from this node's temporal parents.
	 * 
	 * @param edge the temporal edge to remove
	 */
	void removeInEdge(TemporalEdge edge) {
		temporalIn = removeEdge(Edge.IN, temporalIn, edge);
	}
	
	@SuppressWarnings("unchecked")
	private static final <E extends Edge> E findOutEdge(E first, Object label) {
		if(first == null)
			return null;
		else if(Utilities.equals(first.label, label))
			return first;
		else
			return findOutEdge((E) Edge.OUT.getNext(first), label);
	}
	
	private static final <E extends Edge> E addEdge(Edge.Group group, E first, E toAdd) {
		group.setNext(toAdd, first);
		return toAdd;
	}
	
	@SuppressWarnings("unchecked")
	private static final <E extends Edge> E removeEdge(Edge.Group group, E first, E toRemove) {
		if(first == null)
			return null;
		else if(first.equals(toRemove))
			return (E) group.getNext(first);
		else {
			group.setNext(first, removeEdge(group, group.getNext(first), toRemove));
			return first;
		}
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
	public StateNode getAfter(Action action) {
		return getAfter((Event) action);
	}
	
	final StateNode getAfter(Event event) {
		TemporalEdge temporal = getTemporalChild(event);
		if(temporal != null)
			return temporal.child;
		else {
			boolean clean = graph.newEdges.isEmpty();
			StateNode child = new EventNode(this, event);
			new TemporalEdge(this, event, child);
			for(Character character : graph.characters)
				child.getBeliefs(character);
			if(clean) {
				graph.clean();
				child = child.intern();
			}
			return child;
		}
	}
	
	/**
	 * Returns the state after applying all {@link StateGraph#triggers triggers
	 * tracked by the graph} to this state. Specifically, this method finds a
	 * trigger whose precondition is satisfied in this state (or any
	 * epistemically accessible state), applies its effect, and then
	 * recursively does the same thing in the resulting state, stopping when no
	 * more triggers can occur. This method should generally be called after
	 * {@link #getAfter(Action)} to find the state of the world after taking an
	 * action.
	 * 
	 * @return the state after applying all relevant triggers
	 */
	public StateNode getAfterTriggers() {
		for(TemporalEdge temporal : getTemporalChildren()) {
			if(temporal.label instanceof Trigger)
				return temporal.child.getAfterTriggers();
			else if(temporal.label == null && temporal.child == this)
				return this;
			else if(temporal.label == null)
				return temporal.child.getAfterTriggers();
		}
		return getAfter(graph.triggers.getAny(this)).getAfterTriggers();
	}
	
	/**
	 * Returns the state after taking each action in a plan, if the actions are
	 * possible. If all actions are possible, this method takes them one at a
	 * time, adding nodes and edges to the graph if necessary and applying
	 * triggers after each action. If any action's precondition is not met in
	 * that state where it needs to be taken, this method returns null.
	 * 
	 * @param plan the plan to execute
	 * @return a node representing the state after taking all actions in the
	 * plan, or null if some action's precondition is not met
	 */
	public StateNode getAfter(Plan<? extends Action> plan) {
		StateNode after = this;
		for(Action action : plan) {
			if(action.getPrecondition().evaluate(after).equals(True.TRUE))
				after = after.getAfter(action).getAfterTriggers();
			else
				return null;
		}
		return after;
	}
	
	/**
	 * Returns a permanent node in the graph which is equivalent to this node.
	 * 
	 * @return an equivalent permanent node
	 */
	StateNode intern() {
		return this;
	}
	
	/**
	 * Determines whether a {@link Solution solution} is valid in this state.
	 * A valid solution:
	 * <ul>
	 * <li>is a non-empty plan</li>
	 * <li>that is possible to execute in this state</li>
	 * <li>that raises the utility of {@link Solution#getCharacter() its
	 * character}</li>
	 * <li>that is composed of actions which are explained for all the other
	 * consenting characters who need to take them</li>
	 * <li>is minimal, meaning none of the actions can be left out while still
	 * achieving the same or better utility</li>
	 * </ul>
	 * This method may add nodes and edges to the graph.
	 * 
	 * @param solution the solution to test in this state
	 * @return true if the solution is valid in this state, false otherwise
	 */
	public boolean isValid(Solution<? extends Action> solution) {
		// The plan must not be null.
		if(solution == null)
			return false;
		// The plan must not be empty.
		if(solution.size() == 0)
			return false;
		// The plan must be possible.
		StateNode after = getAfter(solution);
		if(after == null)
			return false;
		// The plan must improve utility for its character.
		if(!Comparison.GREATER_THAN.test(after.getUtility(solution.getCharacter()), this.getUtility(solution.getCharacter())))
			return false;
		// Every action must be explained for all other consenting characters who take them.
		StateNode current = this;
		Solution<? extends Action> plan = solution;
		while(plan.size() > 0) {
			TemporalEdge edge = current.getTemporalChild(plan.get(0));
			if(!current.isExplainedForOthers(plan))
				return false;
			current = edge.child.getAfterTriggers();
			plan = plan.next();
		}
		// The plan must be minimal.
		return isMinimal(solution);
	}
	
	private final boolean isMinimal(Solution<? extends Action> solution) {
		Value goal = getAfter(solution).getUtility(solution.getCharacter());
		return !findSubsequence(this, solution, goal, false);
	}
	
	static final boolean findSubsequence(StateNode state, Solution<? extends Action> solution, Value goal, boolean shorter) {
		if(solution.size() == 0)
			return shorter && Comparison.GREATER_THAN_OR_EQUAL_TO.test(state.getUtility(solution.getCharacter()), goal);
		else if(findSubsequence(state, solution.next(), goal, true))
			return true;
		else {
			Action action = solution.get(0);
			if(shorter && (!action.getPrecondition().evaluate(state).equals(True.TRUE) || !state.isExplainedForOthers(solution)))
				return false;
			return findSubsequence(state.getAfter(action).getAfterTriggers(), solution.next(), goal, shorter);
		}
	}
	
	private boolean isExplainedForOthers(Solution<? extends Action> solution) {
		Action action = solution.get(0);
		for(Parameter consenting : action.consenting) {
			if(!Utilities.equals(consenting, solution.getCharacter()) && consenting instanceof Character other) {
				StateNode beliefs = getBeliefs(other);
				if(!action.getPrecondition().evaluate(beliefs).equals(True.TRUE))
					return false;
				beliefs.getAfter(action);
				TemporalEdge branch = beliefs.getTemporalChild(action);
				Solution<? extends Action> explanation = solution.getExplanation(other);
				if(!branch.isValid(explanation))
					return false;
			}
		}
		return true;
	}
}