package edu.uky.cs.nil.sabre.graph;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.FiniteState;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Mapping;
import edu.uky.cs.nil.sabre.Problem;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Trigger;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.etree.EventSet;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ImmutableArray;
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
	
	/** A numeric expression that the author will try to maximize */
	public final Expression utility;
	
	/**
	 * A mapping, for each {@link #characters character}, to a numeric
	 * expression that the character will try to maximize
	 */
	public final Mapping<Expression> utilities;
	
	/**
	 * The set of all ground triggers which can occur in the states represented
	 * in this graph
	 */
	public final EventSet<Trigger> triggers;
	
	/**
	 * The first state represented by this graph (such as a problem's initial
	 * state)
	 */
	public final StateNode root;
	
	/**
	 * A mapping of each {@link #fluents fluent} to its integer index in the set
	 * of fluents
	 */
	final Function<Fluent, Integer> index;
	
	/**
	 * A list of newly created nodes that need to be checked during {@link
	 * #clean() cleaning} to determine if they are duplicates
	 */
	final ArrayList<TemporaryNode> newNodes = new ArrayList<>();
	
	/**
	 * A list of newly created edges that need to be checked during {@link
	 * #clean() cleaning} to determine if they need to be modified
	 */
	final ArrayList<Edge> newEdges = new ArrayList<>();
	
	/**
	 * A reusable list of node pairs used when {@link NodeKey#equals(Object)
	 * detecting duplicate states}
	 */
	final PairList pairs = new PairList();
	
	/**
	 * A hashtable containing every unique combination of fluent {@link
	 * StateNode#values values} used in the nodes of this graph
	 */
	private final HashMap<ImmutableArray<Value>, Value[]> values = new HashMap<>();
	
	/**
	 * A hashtable for storing all state nodes in the graph that uses a hash
	 * function which can detect when nodes are duplicates
	 */
	private final HashMap<NodeKey, StateNode> nodes = new HashMap<>();
	
	/** The next ID number to assign to a permanent state node */
	private long nextID = 0;
	
	/**
	 * Constructs a new state graph that tracks a given set of fluents,
	 * characters, utility functions, and triggers, and which starts in a given
	 * state.
	 * 
	 * @param fluents the set of all fluents to be tracked by the state nodes
	 * in this graph
	 * @param characters the set of all characters whose beliefs are to be
	 * represented in this graph
	 * @param utility the author's utility expression
	 * @param utilities utility expressions for each character
	 * @param triggers the set of all ground triggers which can occur in the
	 * states represented by this graph
	 * @param root the first state to be represented by this graph (usually but
	 * not necessarily the {@link edu.uky.cs.nil.sabre.InitialState initial
	 * state} of a {@link edu.uky.cs.nil.sabre.Problem planning problem}
	 * @throws edu.uky.cs.nil.sabre.FormatException if any fluents, utility
	 * expressions, or triggers are not ground
	 */
	@SuppressWarnings("unchecked")
	public StateGraph(ImmutableSet<? extends Fluent> fluents, ImmutableSet<Character> characters, Expression utility, Mapping<? extends Expression> utilities, Iterable<? extends Trigger> triggers, FiniteState root) {
		this.fluents = fluents.apply(object -> {
			Fluent fluent = (Fluent) object;
			while(fluent.characters.size() > 0)
				fluent = fluent.removeFirstCharacter();
			return fluent;
		}).cast(Fluent.class);
		for(Fluent fluent : this.fluents)
			fluent.mustBeGround();
		HashMap<Fluent, Integer> index = new HashMap<>();
		for(Fluent fluent : this.fluents)
			index.put(fluent, index.size());
		this.index = fluent -> index.get(fluent);
		this.characters = characters;
		utility.mustBeGround();
		this.utility = utility;
		for(Character character : characters)
			utilities.get(character).mustBeGround();
		this.utilities = utilities.cast(Expression.class);
		for(Trigger trigger : triggers)
			trigger.mustBeGround();
		this.triggers = triggers instanceof EventSet ? (EventSet<Trigger>) triggers : new EventSet<Trigger>(Utilities.toArray(triggers, Trigger.class));
		InitialNode initial = new InitialNode(this, root, new HashMap<>());
		clean();
		this.root = initial.intern();
	}
	
	/**
	 * Constructs a new state graph based on a compiled planning problem.
	 * The fluent tracked in this graph will be {@link Problem#fluents all
	 * fluents defined by the problem}. The characters tracked in this graph
	 * will be {@link edu.uky.cs.nil.sabre.Problem#universe all characters
	 * defined by the problem's universe}. The utilities will be {@link
	 * Problem#utilities the utilities defined in the problem}. The triggers
	 * tracked will be {@link CompiledProblem#triggers all triggers defined by
	 * the problem}. Note that all element of the problem must be
	 * ground.
	 * 
	 * @param problem the planning problem
	 * @param root the first state to be represented by this graph
	 */
	public StateGraph(Problem problem, FiniteState root) {
		this(
			problem.fluents,
			problem.universe.characters,
			problem.utility,
			problem.utilities,
			problem.triggers,
			root
		);
	}
	
	/**
	 * Constructs a new state graph {@link #StateGraph(Problem, FiniteState)
	 * based on a compiled planning problem} and using the {@link
	 * CompiledProblem#start problem's state state} as the graph's {@link #root
	 * root state}.
	 * 
	 * @param problem the compiled planning problem
	 */
	public StateGraph(CompiledProblem problem) {
		this(problem, problem.start);
	}
	
	/**
	 * This method is called after {@link TemporaryNode temporary nodes} and
	 * edges have been added to the graph to detect and replace duplicate nodes
	 * and remap the edges to and from those nodes.
	 */
	final void clean() {
		// Intern all value arrays first.
		for(TemporaryNode node : newNodes)
			node.clean();
		// Find replacements for any nodes which are duplicate states.
		for(TemporaryNode node : newNodes)
			node.intern();
		newNodes.clear();
		// Update all new edges.
		for(int i=0; i<newEdges.size(); i++)
			newEdges.get(i).clean();
		newEdges.clear();
	}
	
	/**
	 * Interns a unique combination of values for the fluents tracked in this
	 * graph.
	 * 
	 * @param values an array of values
	 * @return the first such array of these values ever interned by this graph
	 */
	final Value[] intern(Value[] values) {
		ImmutableArray<Value> key = new ImmutableArray<>(values);
		Value[] interned = this.values.get(key);
		if(interned == null) {
			interned = values;
			this.values.put(key, interned);
		}
		return interned;
	}
	
	/**
	 * For a given temporary node, this method finds and returns an existing
	 * permanent node in the graph that is equivalent or, if no such node
	 * exists, creates a new permanent node from the temporary node and returns
	 * it. The temporary node's epistemic edges will be recreated in the new
	 * permanent node, since these are required to intern the node in a
	 * hashtable, but the temporal nodes will not be recreated in this method;
	 * recreating temporary nodes happens when edges are {@link #clean()
	 * cleaned}.
	 * 
	 * @param node the temporary node whose equivalent permanent node is desired
	 * @return a permanent node equivalent to the given temporary node
	 */
	final StateNode intern(TemporaryNode node) {
		StateNode interned = nodes.get(new NodeKey(node));
		if(interned == null) {
			interned = new StateNode(this, nextID++, node.values);
			for(int i=characters.size()-1; i>=0; i--)
				new EpistemicEdge(interned, characters.get(i), node.getBeliefs(characters.get(i)));
			nodes.put(new NodeKey(interned), interned);
		}
		return interned;
	}
}