package edu.uky.cs.nil.sabre.graph;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.FiniteState;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Trigger;
import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.etree.EventSet;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * A state graph is made up of {@link StateNode nodes} which are {@link
 * edu.uky.cs.nil.sabre.State world states} that are liked by two kinds of
 * edges: {@link EpistemicEdge epistemic edges} that define what each {@link
 * Character character} believes the world state to be, and {@link TemporalEdge
 * temporal edges} that define how the world state changes as a result of
 * {@link edu.uky.cs.nil.sabre.Event an event}.
 * <p>
 * From every node there extends exactly one epistemic edge for each character.
 * In other words, these graphs cannot model characters with uncertain beliefs.
 * Characters must always commit to a belief about every {@link Fluent fluent},
 * but those beliefs may be wrong. This requirement also implies that these
 * graphs represent an arbitrarily nested theory of mind (what {@code x}
 * believes {@code y} believes, and so on).
 * <p>
 * The graph does not contain duplicate nodes; in other words, the graph will
 * never have two nodes which represent exactly the same world state. When
 * adding edges to the graph, if the edge would create a new node which is a
 * duplicate of an existing node, this is detected, and the original node is
 * used instead. A graph can be (and often is) infinite, meaning that there are
 * always more nodes which could be added to it by adding more temporal edges;
 * however, adding a single temporal edge to the graph will always add only a
 * finite number of new nodes and edges. This means that state graphs are an
 * implementation of the {@link FiniteState finite state interface}. In other
 * words, every individual world state in the graph can always be represented
 * by a finite number of nodes.
 * <p>
 * Planning can be done directly in a state graph if the overhead of detecting
 * duplicate nodes is acceptable. State graphs are also useful for representing
 * the true initial state of a problem as a finite structure if any {@link
 * Trigger triggers} apply in the {@link edu.uky.cs.nil.sabre.Problem#initial
 * initial state originally defined by the problem}, since applying triggers
 * can lead to an initial state which cannot be represented as a finite {@link
 * edu.uky.cs.nil.sabre.logic.Proposition proposition}.
 * 
 * @author Stephen G. Ware
 */
public class StateGraph implements Serializable {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * The set of all fluents tracked by the state nodes in this graph (with
	 * all {@link Fluent#characters} removed). This set contains only
	 * non-espitemic fluents; to check the value of an epistemic fluent at a
	 * node, follow {@link EpistemicEdge epistemic edges} for the characters in
	 * order from that node and check the value of the fluent minus all
	 * characters in the final destination node.
	 */
	public final ImmutableSet<Fluent> fluents;
	
	/** The set of all characters whose beliefs are represented in this graph */
	public final ImmutableSet<Character> characters;
	
	/**
	 * The set of all ground triggers which can occur in the states represented
	 * in this graph
	 */
	public final EventSet<Trigger> triggers;
	
	/**
	 * The first state represented by this graph (usually a problem's initial
	 * state)
	 */
	public final StateNode root;
	
	/**
	 * A mapping of each {@link #fluents fluent} to its integer index in the set
	 * of fluents
	 */
	final HashMap<Fluent, Integer> index = new HashMap<>();
	
	/**
	 * A hashtable containing every unique combination of fluents values used in
	 * the nodes of this graph
	 */
	final HashMap<ValuesKey, Value[]> values = new HashMap<>();
	
	/**
	 * A hashtable for storing all state nodes in the graph which uses a hash
	 * function which can detect when nodes are duplicates
	 */
	final HashMap<StateKey, StateNode> states = new HashMap<>();
	
	/**
	 * A reusable list of node pairs used when checking whether two nodes are
	 * duplicates
	 */
	final ArrayList<StateNode> pairs = new ArrayList<>();
	
	/**
	 * Constructs a new state graph that tracks a given set of fluents,
	 * characters, and triggers, and which starts with a given state.
	 * 
	 * @param fluents the set of all fluents to be tracked by the state nodes
	 * in this graph
	 * @param characters the set of all characters whose beliefs are to be
	 * represented in this graph
	 * @param triggers the set of all ground triggers which can occur in the
	 * states represented by this graph
	 * @param root the first state to be represented by this graph (usually but
	 * not necessarily the {@link edu.uky.cs.nil.sabre.InitialState initial
	 * state} of a {@link edu.uky.cs.nil.sabre.Problem planning problem}
	 */
	@SuppressWarnings("unchecked")
	public StateGraph(ImmutableSet<? extends Fluent> fluents, ImmutableSet<Character> characters, EventSet<? extends Trigger> triggers, FiniteState root) {
		this.fluents = fluents.apply(object -> {
			Fluent fluent = (Fluent) object;
			while(fluent.characters.size() > 0)
				fluent = fluent.removeFirstCharacter();
			return fluent;
		}).cast(Fluent.class);
		for(int i=0; i<this.fluents.size(); i++)
			this.index.put(this.fluents.get(i), i);
		this.characters = characters;
		this.triggers = (EventSet<Trigger>) triggers;
		this.root = new InitialNode(this, root, new HashMap<>()).resolve();
	}
	
	/**
	 * Constructs a new state graph based on a compiled planning problem.
	 * The fluent tracked in this graph will be {@link CompiledProblem#fluents
	 * all fluent defined by the problem}. The characters tracked in this graph
	 * will be {@link edu.uky.cs.nil.sabre.Problem#universe all characters
	 * defined by the problem's universe}. The triggers tracked will be {@link
	 * CompiledProblem#triggers all trigger defined by the problem}. The root
	 * state will be the {@link CompiledProblem#initial problem's initial
	 * state}.
	 * 
	 * @param problem the compiled planning problem
	 */
	public StateGraph(CompiledProblem problem) {
		this(problem.fluents, problem.universe.characters, problem.triggers, problem.start);
	}
	
	@Override
	public String toString() {
		return "[" + getClass().getSimpleName() + ": " + states.size() + " nodes]";
	}
	
	/**
	 * Interns a combination of values for the fluents tracked in this graph.
	 * 
	 * @param values an array of values
	 * @return the first such array of these values ever interned by this graph
	 */
	Value[] resolve(Value[] values) {
		ValuesKey key = new ValuesKey(values);
		values = this.values.get(key);
		if(values == null) {
			values = key.values;
			this.values.put(key, values);
		}
		return values;
	}
	
	/**
	 * Interns a state node in the graph. If a state that is a duplicate of the
	 * given node exists, the earlier existing node will be returned.
	 * Otherwise, a new permanent state node will be constructed and added to
	 * the graph. If this method is called with {@link a permanent StateNode
	 * state node}, it will always return that node, but when this method is
	 * called with a {@link TemporaryNode temporary state node}, it will
	 * return a permanent state node that is equivalent to the temporary node.
	 * This method is used to detect and avoid duplicate state nodes.
	 * 
	 * @param state a state node which will be interned in the graph
	 * @return a permanent state node in the graph
	 */
	StateNode resolve(StateNode state) {
		StateKey key = new StateKey(state);
		state = states.get(key);
		if(state == null)
			state = new StateNode(key.node);
		return state;
	}
}