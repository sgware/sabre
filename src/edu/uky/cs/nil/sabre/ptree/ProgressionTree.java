package edu.uky.cs.nil.sabre.ptree;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.HeadPlan;
import edu.uky.cs.nil.sabre.Number;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Solution;
import edu.uky.cs.nil.sabre.SolutionGoal;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.comp.CompiledAction;
import edu.uky.cs.nil.sabre.comp.CompiledEvent;
import edu.uky.cs.nil.sabre.comp.CompiledFluent;
import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.comp.CompiledTrigger;
import edu.uky.cs.nil.sabre.etree.EventTree;
import edu.uky.cs.nil.sabre.logic.Arithmetic;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Comparison;
import edu.uky.cs.nil.sabre.logic.Conditional;
import edu.uky.cs.nil.sabre.logic.Conjunction;
import edu.uky.cs.nil.sabre.logic.Disjunction;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Unknown;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.BigArrayLong;
import edu.uky.cs.nil.sabre.util.ImmutableSet;
import edu.uky.cs.nil.sabre.util.MemoryBudget;

/**
 * A progression search space is a large table that stores the data for each
 * node of a {@link ProgressionTreeSearch progression tree search} through the
 * space of {@link State states}. States are not stored as objects to avoid
 * excessive allocation on the heap during search; rather, data for each state
 * is stored in a {@link BigArrayLong big array} and each node is identified by
 * an ID number. Most methods have a state ID number as a parameter.
 * <p>
 * A node can be thought of as an {@link CompiledEvent event}/{@link State
 * state} pair, meaning each node has an {@link #getEvent(long) event associated
 * with it} and can also be {@link #getValue(long, CompiledFluent) treated like
 * a state}. The event is the event that occurred immediately before the state.
 * If the event is {@link CompiledAction an action}, it means the state is the
 * state that results from applying the {@link CompiledAction#effect acton's
 * effect} to the {@link #getBefore(long) previous state}. If the event is
 * {@link CompiledTrigger a trigger}, it means some trigger applied in the
 * previous state and this state is the result. A node which is an initial state
 * node has no associated event, so {@link #getEvent(long)} will return null.
 * <p>
 * The node with ID number 0 is always {@link #initialize(State) the state that
 * was used to initialize this tree}. Node that {@link #initialize(State)} may
 * return a value other than 0 if {@link #getAfterTriggers(long) triggers
 * apply} in that state, in which case the method will return the ID number of
 * the node after all triggers have been applied.
 * <p>
 * Every node is {@link #getCharacter(long) associated with a character}. Node 0
 * and its descendants are associated with the author, and {@link
 * #getCharacter(long)} will return null for those nodes. Every other node is
 * reached by {@link #getBeliefs(long, Character) requesting the beliefs of some
 * character in some state}, and the resulting node and its descendants will be
 * associated with that character. For example, if you call {@link
 * #getBeliefs(long, Character)} with node ID 0 and the first character from a
 * problem, you will get the state that character believes the initial state to
 * be.
 * <p>
 * To save memory (and the time required to allocate new memory), nodes do not
 * store the {@link Value value} of each {@link CompiledFluent fluent}, but
 * rather the sequence of {@link CompiledEvent events} that led to that state;
 * the value of each fluent can then be derived from looking as the effects of
 * each event starting from the most recent. This means that nodes take up
 * little memory but methods like {@link #getValue(long, CompiledFluent)} are
 * relatively expensive.
 * <p>
 * Yes, I know this is a single class with 1500+ lines of code. Fight me, you
 * cowards. It is good code, perhaps even beautiful in its way. 
 * 
 * @author Stephen G. Ware
 */
public class ProgressionTree implements Serializable {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * Feature index for the {@link edu.uky.cs.nil.sabre.Entity#id ID number}
	 * of the character whose beliefs the node represents, or -1 for the author.
	 */
	private static final int CHARACTER = 0;
	
	/**
	 * Feature index for the {@link EventList#getEvent(int) ID number} of the
	 * event immediately before the state a node represents. The value is the
	 * {@link CompiledEvent#getID() ID number} of a compiled event if the event
	 * is one of the actions or triggers defined in the {@link #problem
	 * problem}, but the event could also be one of the {@link
	 * EventList#getDummy(Character, CompiledFluent, Value) dummy actions}
	 * used to update beliefs. If the event is a dummy action, the value is
	 * {@link ProgressionProblem#getEvent(int) its ID in the progression
	 * problem}. If the node is an initial state node, the value is the index
	 * in {@link #initial the list of initial states} of the node's {@link
	 * InitialState initial state object}.
	 */
	private static final int EVENT = 1;
	
	/**
	 * Feature index for the utility of the node's character. Utility is a
	 * {@code double} but is encoded as a {@code long} to be stored in the
	 * array. The value is initially {@link #UTILITY_NOT_SET} until the value is
	 * calculated by {@link #getUtility(long)}, and then the value will be that
	 * value. If the utility is {@link Unknown#UNKNOWN unknown}, the value will
	 * be {@link #UTILITY_UNKNOWN}.
	 */
	private static final int UTILITY = 2;
	
	/** 
	 * Feature index for the ID number of the node immediately before this
	 * node. If the node {@link #isRoot(long) is a root}, the value is the
	 * node's own ID number.
	 */
	private static final int BEFORE = 3;
	
	/**
	 * Feature index for the ID number of the last {@link
	 * #getChild(long, CompiledEvent) child} of this node. This value is a
	 * pointer to the start of a linked list of children; {@link
	 * #PREVIOUS_SIBLING} gives the next node in the list. The value is -1 if
	 * there are no children. A linked list of children is possible because
	 * each node has at most one parent.
	 */
	private static final int LAST_CHILD = 4;
	
	/**
	 * Feature index for the ID number of the next node in the linked list of
	 * a node's children. {@link #LAST_CHILD} is the start of the list. The
	 * value is -1 if there are no more nodes in the list.
	 */
	private static final int PREVIOUS_SIBLING = 5;
	
	/**
	 * Feature index for the ID number of the node representing the state after
	 * {@link #getAfterTriggers(long) applying any relevant triggers} to this
	 * state. The value is the node's own ID number if no triggers apply.
	 */
	private static final int AFTER_TRIGGERS = 6;
	
	/**
	 * Feature index for the ID number of a descendant node that achieves the
	 * best utility discovered so far. The path to the descendant must be of
	 * explained actions. This feature may sometimes be the same as {@link
	 * #EXPLANATION} but not always, because the best known descendant does
	 * not require this node's action to be explained. The value is this node's
	 * own ID number if no better descendant has been generated yet.
	 */
	private static final int BEST = 7;
	
	/**
	 * Feature index for the ID number of a descendant node that achieves the
	 * worst utility discovered so far. The path to the descendant must be of
	 * explained actions. The value is this node's own ID number if no worse
	 * descendant has been generated yet.
	 */
	private static final int WORST = 8;
	
	/**
	 * Feature index for the ID number of a descendant node that explains (i.e.
	 * leads to a higher utility for) this node's action for this node's
	 * character. The path to the descendant must be minimal (i.e. no actions
	 * can be left out and still reach the same or higher utility), and all
	 * actions on the path except this node's action must be explained. This
	 * feature may sometimes be the same as {@link #BEST} but not always,
	 * because this feature is only set when a descendant has been generated
	 * that improves utility and explains this node's action for this node's
	 * character. The value is -1 if no descendant has been generated yet that
	 * explains the node's action for the node's character.
	 */
	private static final int EXPLANATION = 9;
	
	/**
	 * Feature index for the ID number of the node that is the top of a LIFO
	 * stack (implemented as a link list) of explained descendants that are
	 * waiting for this node's action to become explained so they can be
	 * propagated further back. When a node has descendants that are potential
	 * explanations, but the node's action is not yet explained, those
	 * descendants wait in this list until the node's action becomes explained.
	 * If the node becomes explained, the list is drained an all its members
	 * are propagated backwards. {@link #PREVIOUS_IN_QUEUE} is the ID number of
	 * the next nodes of the list. The value is -1 if there are no waiting
	 * descendants. A linked list of waiting nodes is possible because a node
	 * can be waiting on at most one action at a time. This list for the
	 * author's root node (node 0) stores solutions to the problem waiting to
	 * be returned by {@link #getNextSolution()}.
	 */
	private static final int END_OF_QUEUE = 10;
	
	/**
	 * Feature index for the ID number of the node that is next in the stack
	 * (linked list) of explained descendants waiting to for this node's action
	 * to become explained so they can be propagated further back. The value is
	 * -1 if there are no more descendants in the list.
	 */
	private static final int PREVIOUS_IN_QUEUE = 11;
	
	/**
	 * Feature index for the ID number of the node that is the node's most
	 * recently generated trunk. A trunk is the node from which a {@link
	 * #getBranch(long, Character) branch} extends. A list of trunks is needed
	 * because when a node becomes {@link #isExplained(long) explained} it
	 * needs to check whether its trunks are now also explained as a result.
	 * This value is the pointer to the start of a linked list of trunks; the
	 * next trunk in the list is stored at {@link #PREVIOUS_TRUNK} plus the
	 * {@link edu.uky.cs.nil.sabre.Entity#id ID number} of {@link
	 * #getCharacter(long) the node's character}. The value is -1 if this node
	 * has no trunks. The start of the list does not need to specify a character
	 * because a node can only be a branch for its character; however, the next
	 * node in the list does need to specify an character because a node can
	 * appear in more than one list of trunks. A linked list of trunks is
	 * possible because a node can have at most one branch per character.
	 */
	private static final int LAST_TRUNK = 12;
	
	/**
	 * Feature index for the ID number of the next node in the list of trunks
	 * for the first {@link Character character} in the {@link #problem
	 * problem's} {@link edu.uky.cs.nil.sabre.Universe universe}. There are as
	 * many previous trunk indices as characters; this is the index of the first
	 * one. The index of the previous trunk for a given character is obtained by
	 * adding the {@link edu.uky.cs.nil.sabre.Entity#id character's ID number}
	 * to this index. The value is -1 if there are no more trunks.
	 */
	private static final int PREVIOUS_TRUNK = 13;
	
	/**
	 * Feature index for the ID number of the node that represents the beliefs
	 * of the first {@link Character character} in the {@link #problem
	 * problem's} {@link edu.uky.cs.nil.sabre.Universe universe}. There are as
	 * many belief indices as characters; this is the index of the first one.
	 * The index of the beliefs for a given character is obtained by adding the
	 * {@link edu.uky.cs.nil.sabre.Entity#id character's ID number} to this
	 * index. The value is -1 until the beliefs of an character are generated by
	 * {@link #getBeliefs(long, Character)}, and after that the value will be
	 * the ID number of that belief node.
 	 */
	private final int BELIEFS;
	
	/**
	 * The total number of features a node has. The amount of memory that a
	 * node occupies can be obtained by multiplying this number by {@link
	 * java.lang.Long#BYTES}.
	 */
	private final int NODE_SIZE;
	
	/**
	 * The value stored in the {@link #UTILITY} feature when {@link
	 * #getUtility(long) a node's utility} is {@link Unknown#UNKNOWN unknown}.
	 */
	private static final long UTILITY_UNKNOWN = Double.doubleToLongBits(Double.NaN);
	
	/**
	 * The value stored in the {@link #UTILITY} feature before {@link
	 * #getUtility(long) a node's utility} has been calculated. This value
	 * should not be {@link Double#longBitsToDouble(long) decoded}, because it
	 * would also decode as {@link Double#NaN NaN} which would cause the node's
	 * utility to read as {@link Unknown#UNKNOWN unknown}.
	 */
	private static final long UTILITY_NOT_SET = UTILITY_UNKNOWN + 1;
	
	/** The problem whose search space this table stores */
	public final CompiledProblem problem;
	
	/** A list of events, including dummy belief update actions */
	private final EventList events;
	
	/** An event tree of triggers to efficiently detect when triggers apply */
	private final EventTree<CompiledTrigger> triggers;
	
	/** The table in which data is stored */
	private final BigArrayLong nodes;
	
	/** The list of special initial state nodes */
	private final ArrayList<InitialState> initial;
	
	/** The number of nodes in this state space */
	private transient long size = 0;
	
	/**
	 * Constructs a new progression state space for a given problem with a given
	 * chunk size and {@link MemoryBudget memory budget} for its {@link
	 * BigArrayLong big array}.
	 * 
	 * @param problem the problem whose search space this table will store
	 * @param triggers an event tree of all triggers in the problem
	 * @param chunkSize the chunk size in bytes for that {@link BigArrayLong
	 * big array} that data will be stored in
	 * @param budget the memory budget which limits the size of the big array
	 * that data will be stored in
	 */
	public ProgressionTree(CompiledProblem problem, EventTree<CompiledTrigger> triggers, long chunkSize, MemoryBudget budget) {
		this.BELIEFS = PREVIOUS_TRUNK + problem.universe.characters.size();
		this.NODE_SIZE = BELIEFS + problem.universe.characters.size();
		this.problem = problem;
		this.events = new EventList(problem);
		this.triggers = triggers;
		this.nodes = new BigArrayLong(chunkSize, budget);
		this.initial = new ArrayList<>((problem.universe.characters.size() * problem.universe.characters.size()) + 1);
		initialize(problem.start);
	}
	
	/**
	 * Constructs a new progression state space for a given problem with a
	 * default chunk size and unlimited {@link MemoryBudget memory budget} for
	 * its {@link BigArrayLong big array}.
	 * 
	 * @param problem the problem whose search space this table will store
	 * @param triggers an event tree of all triggers in the problem
	 */
	public ProgressionTree(CompiledProblem problem, EventTree<CompiledTrigger> triggers) {
		this(problem, triggers, BigArrayLong.DEFAULT_CHUNK_SIZE, new MemoryBudget());
	}
	
	private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
		in.defaultReadObject();
		State start = initial.get(0).state;
		initial.clear();
		initialize(start);
	}
	
	@Override
	public String toString() {
		StringWriter string = new StringWriter();
		try { write(string); }
		catch(IOException e) {/* do nothing */}
		return string.toString();
	}
	
	/**
	 * Returns a string with detailed information about the data for a node.
	 * This method is generally used for debugging purposes.
	 * 
	 * @param node the ID number of a node
	 * @return a string with information about that node's data
	 */
	public String toString(long node) {
		String string = "[" + node + ":";
		string += " character=" + getCharacter(node);
		string += " event=" + getEvent(node);
		string += " utility=" + (get(node, UTILITY) == UTILITY_NOT_SET ? "x" : utility(node));
		string += " before=" + getBefore(node);
		string += " triggers=" + get(node, AFTER_TRIGGERS);
		string += " best=" + get(node, BEST);
		string += " worst=" + get(node, WORST);
		string += " exp=" + get(node, EXPLANATION);
		String trunks = "";
		long trunk = get(node, LAST_TRUNK);
		while(trunk != -1) {
			if(!trunks.isEmpty())
				trunks = "," + trunks;
			trunks = trunk + trunks;
			trunk = getPreviousTrunk(trunk, getCharacter(node));
		}
		string += " trunks=" + trunks;
		for(Character character : problem.universe.characters)
			string += " b(" + character + ")=" + get(node, BELIEFS, character);
		return string + "]";
	}
	
	/**
	 * Writes {@link #write(long, Writer) data for each node} in the space to a
	 * {@link Writer writer}. This method is generally used for debugging
	 * purposes.
	 * 
	 * @param writer the write to which data will be sent
	 * @throws IOException if an exception occurs while writing to the writer
	 */
	public void write(Writer writer) throws IOException {
		for(long i=0; i<size(); i++) {
			write(i, writer);
			writer.append("\n");
		}
	}
	
	/**
	 * Writes data for a node to a {@link Writer writer}. This method is
	 * generally used for debugging purposes.
	 * 
	 * @param node the ID number of the desired node
	 * @param writer the writer to which data will be sent
	 * @throws IOException if an exception occurs while writing to the writer
	 */
	public void write(long node, Writer writer) throws IOException {
		writer.append(toString(node));
	}
	
	/**
	 * Returns the number of nodes in the state space.
	 * 
	 * @return the number of nodes
	 */
	public long size() {
		return size;
	}
	
	/**
	 * Returns the value of a feature for a node. Feature are identified by
	 * indices; for example, the {@link #CHARACTER} feature is where the ID number
	 * of {@link #getCharacter(long) a node's character} is stored.
	 * 
	 * @param node the ID number of the node for which the given feature is
	 * desired
	 * @param feature the index of the feature desired
	 * @return the value of that feature for that node
	 */
	private final long get(long node, int feature) {
		return nodes.get(node * NODE_SIZE + feature);
	}
	
	/**
	 * Returns the value of a feature for a node when that feature exists for
	 * each {@link Character character}, such as {@link #BELIEFS}.
	 * 
	 * @param node the ID number of the node for which the given feature is
	 * desired
	 * @param feature the index of the feature desired
	 * @param character the character for which that feature is desired
	 * @return the value of that feature for that node and that character
	 */
	private final long get(long node, int feature, Character character) {
		return get(node, feature + character.id);
	}
	
	/**
	 * Sets the value of a feature for a node.
	 * 
	 * @param node the ID number of the node whose feature will be set
	 * @param feature the index of feature to be set
	 * @param value the value to set the feature to
	 */
	private final void set(long node, int feature, long value) {
		nodes.set(node * NODE_SIZE + feature, value);
	}
	
	/**
	 * Sets the value of a feature for a node when that feature exists for
	 * each {@link Character character}, such as {@link #BELIEFS}.
	 * 
	 * @param node the ID number of the node whose feature will be set
	 * @param feature the index of the feature to be set
	 * @param character the character for which that feature will be set
	 * @param value the value to set the feature to
	 */
	private final void set(long node, int feature, Character character, long value) {
		set(node, feature + character.id, value);
	}
	
	/**
	 * Initializes this state space by specifying the initial {@link State
	 * state} that will form the root (node ID 0) of the tree; all other data
	 * will be erased.
	 * 
	 * @param initial the state which will form the root of the tree
	 * @return the ID number of the node where the search should start, which
	 * will usually be 0, unless some {@link CompiledTrigger triggers} apply in
	 * given initial state, in which case the node ID returned will be the
	 * result after {@link #getAfterTriggers(long) applying those triggers} to
	 * node 0
	 */
	public long initialize(State initial) {
		size = 0;
		this.initial.clear();
		return initial(-1, null, new InitialState(problem, initial));
	}
	
	/**
	 * Creates a new initial state node and initializes all its features.
	 * 
	 * @param trunk the ID number of the node for which this node is a branch,
	 * or -1 if initializing the author root
	 * @param character the character for which this node is the trunk's branch,
	 * or null if initializing the author root
	 * @param state the initial state object for the node
	 * @return the ID number of the newly created initial state node
	 */
	private final long initial(long trunk, Character character, InitialState state) {
		long node = size++;
		set(node, CHARACTER, character == null ? -1 : character.id);
		set(node, EVENT, initial.size());
		initial.add(state);
		set(node, UTILITY, UTILITY_NOT_SET);
		set(node, BEFORE, node);
		for(int i=BEFORE+1; i<NODE_SIZE; i++)
			set(node, i, -1);
		if(trunk != -1)
			addTrunk(node, trunk);
		return getAfterTriggers(node);
	}
	
	/**
	 * If any new solutions to the problem being modeled by this tree have been
	 * generated, this method returns the node ID of the a solution. A solution
	 * is a sequence of {@link #isExplained(long) explained events} starting at
	 * the root (node ID 0) that improves {@link
	 * edu.uky.cs.nil.sabre.Problem#utility the author's utility}. Once a
	 * solution has been returned by this method, it will not be returned
	 * again. 
	 * 
	 * @return the node ID of a solution which has been generated and has not
	 * yet been returned, or -1 if there are no solutions waiting to be
	 * returned
	 */
	public long getNextSolution() {
		long solution = pop(0);
		while(solution != -1) {
			if(utility(solution) > utility(0))
				break;
			solution = pop(0);
		}
		return solution;
	}
	
	/**
	 * Returns the ID number of a node's root, which is found by going {@link
	 * #getBefore(long) back into the past} until the initial state is reached
	 * or an unforeseen update was made to the state via a {@link
	 * edu.uky.cs.nil.sabre.graph.DummyAction dummy action}. A tree can contain
	 * many roots. A root can be thought of as the starting place for a branch
	 * of the search.
	 * 
	 * @param node the ID number of the node whose root is desired
	 * @return the ID number of the node's root
	 */
	public long getRoot(long node) {
		CompiledEvent event = getEvent(node);
		if(event == null || isDummy(event))
			return node;
		else
			return getRoot(getBefore(node));
	}
	
	/**
	 * Checks whether a node is an initial state node, which is signified by
	 * the {@link #BEFORE} feature being set to the node's own ID number. This
	 * definition of a root node differs slightly from the method {@link
	 * #getRoot(long)}, which considers states after dummy actions to be root
	 * nodes as well. This private method is typically used to signal that it
	 * is not possible to go back any farther into the past.
	 * 
	 * @param node the ID number of the node to check
	 * @return true if the node is an initial state root, false otherwise
	 */
	private final boolean isRoot(long node) {
		return getBefore(node) == node;
	}
	
	/**
	 * Returns the {@link Character character} associated with this node, or
	 * null if the node is associated with the author. Descendants of node 0 are
	 * all author nodes (for which this method returns null), but every other
	 * node represents the beliefs of some character. This method returns the
	 * character whose beliefs this node represents.
	 * 
	 * @param node the ID number of the node whose character is desired
	 * @return the character or null if this node is associated with the author
	 */
	public Character getCharacter(long node) {
		long index = get(node, CHARACTER);
		if(index == -1)
			return null;
		else
			return problem.universe.characters.get((int) index);
	}
	
	/**
	 * Returns the {@link CompiledEvent event} that occurred immediately before
	 * the state represented by this node, or null if the node is an initial
	 * state node.
	 * 
	 * @param node the ID number of the node whose event is requested
	 * @return the event, or null if the node is an initial state node
	 */
	public CompiledEvent getEvent(long node) {
		if(isRoot(node))
			return null;
		else
			return events.getEvent((int) get(node, EVENT));
	}
	
	/**
	 * Returns the most recent {@link CompiledAction action} that occurred
	 * before the state represented by this node, or null if the node is an
	 * initial state node. If {@link #getAfterTriggers(long) triggers apply}
	 * after an action, the event returned by {@link #getEvent(long)} will be
	 * the last trigger. This method is a convenient way to back up past any
	 * number of triggers to get the most recent action that occurred.
	 * 
	 * @param node the ID number of the node whose action is requested
	 * @return the action, or null if the node is an initial state node
	 */
	public CompiledAction getAction(long node) {
		CompiledEvent event = getEvent(node);
		if(event == null || isDummy(event))
			return null;
		else if(event instanceof CompiledTrigger)
			return getAction(getBefore(node));
		else
			return (CompiledAction) event;
	}
	
	/**
	 * Checks whether an {@link CompiledEvent event} is a dummy event by
	 * checking its {@link CompiledEvent#getID() ID number}; if the number is
	 * above the number of the last trigger defined in the original problem,
	 * the event must be one of the dummy events generated during search for
	 * purposes such as belief updates.
	 * 
	 * @param event the event to be checked
	 * @return true if the event is a dummy event, false otherwise
	 */
	private final boolean isDummy(CompiledEvent event) {
		return event.getID() >= ((CompiledProblem) problem).events.size();
	}
	
	/**
	 * Returns the sequence of {@link CompiledAction actions} that have
	 * occurred since {@link #getRoot(long) the root} that led to the state
	 * represented by a given node.
	 * 
	 * @param node the ID number of the node where the plan should stop
	 * @return the sequence of actions starting at the root and leading up to
	 * node
	 */
	public HeadPlan<CompiledAction> getPlan(long node) {
		return getPlan(getRoot(node), node);
	}
	
	/**
	 * Returns the sequence of {@link CompiledAction actions} that have
	 * occurred between the given start and end nodes. This method will return
	 * null if the end node is not a descendant of the start node or if there
	 * is {@link #getRoot(long) a root node} between the start and end (i.e.
	 * some unexpected state update happened between the start and end).
	 * 
	 * @param start the ID number of the node representing the state where the
	 * plan should start
	 * @param end the ID number of the node representing the state where the
	 * plan should end
	 * @return the sequence of actions between the start and end (which will be
	 * empty if no actions have occurred between the nodes) or null if the
	 * state and end pair is somehow invalid
	 */
	public HeadPlan<CompiledAction> getPlan(long start, long end) {
		return getPlan(start, end, HeadPlan.EMPTY.cast(CompiledAction.class));
	}
	
	/**
	 * Returns a plan composed of the {@link CompiledAction actions} after the
	 * start node and before the end node. This method starts with {@link
	 * HeadPlan#EMPTY an empty plan} and works backwards from the end, {@link
	 * HeadPlan#prepend(edu.uky.cs.nil.sabre.Action) prepending} each action it
	 * finds to the plan until it reaches the start. If {@link #isRoot(long) a
	 * root node} is encountered before reaching the start, this method returns
	 * null.
	 * 
	 * @param start the state after which actions are desired
	 * @param end the state before which the actions are desired
	 * @param plan the plan so far
	 * @return a sequence of actions after start and before end
	 */
	private final HeadPlan<CompiledAction> getPlan(long start, long end, HeadPlan<CompiledAction> plan) {
		if(start == end)
			return plan;
		CompiledEvent event = getEvent(end);
		if(event == null || isDummy(event))
			return null;
		else if(event instanceof CompiledAction)
			plan = plan.prepend((CompiledAction) event);
		return getPlan(start, getBefore(end), plan);
	}
	
	/**
	 * Returns the {@link Value value} a {@link Expression logical expression}
	 * would have when {@link Expression#evaluate(State) evaluated} in a given
	 * state.
	 * 
	 * @param node the ID number of the node representing the state in which
	 * the logical expression will be evaluated
	 * @param expression the logical expression to be evaluated
	 * @return the result of evaluating the expression in that state
	 */
	@SuppressWarnings("unchecked")
	public Value getValue(long node, Expression expression) {
		if(expression instanceof Value)
			return (Value) expression;
		else if(expression instanceof Fluent)
			return getValue(node, (CompiledFluent) expression);
		else if(expression instanceof Arithmetic)
			return getValue(node, (Arithmetic) expression);
		else if(expression instanceof Precondition)
			return getValue(node, (Precondition) expression);
		else if(expression instanceof Clause)
			return getValue(node, (Clause<Precondition>) expression);
		else
			return getValue(node, expression.toValued());
	}
	
	/**
	 * Returns the {@link Value value} of a {@link CompiledFluent compiled
	 * fluent} in a given state.
	 * 
	 * @param node the ID number of the node representing the state in which
	 * the fluent will be evaluated
	 * @param fluent the compiled fluent
	 * @return the value of the fluent in that state
	 */
	public Value getValue(long node, CompiledFluent fluent) {
		if(isRoot(node))
			return getInitialState(node).getValue(fluent);
		long before = getBefore(node);
		Clause<Effect> effect = events.getEffect(getEvent(node), fluent);
		if(effect != null)
			for(int i=0; i<effect.size(); i++)
				if(getValue(before, effect.get(i).condition).equals(True.TRUE))
					return getValue(before, effect.get(i).value);
		return getValue(before, fluent);
	}
	
	/**
	 * Returns the {@link InitialState initial state object} for an {@link
	 * #isRoot(long) initial state node}. This method should only be called
	 * with nodes that are known to be initial state nodes.
	 * 
	 * @param node the ID number of an initial state node
	 * @return the node's initial state object
	 */
	private final InitialState getInitialState(long node) {
		return initial.get((int) get(node, EVENT));
	}
	
	/**
	 * Returns the {@link Value value} an {@link Arithmetic arithmetic
	 * expression} would have when {@link Expression#evaluate(State) evaluated}
	 * in a given state.
	 * 
	 * @param node the ID number of the node representing the state in which
	 * the arithmetic expression will be evaluated
	 * @param arithmetic the arithmetic expression to be evaluated
	 * @return the result of evaluating the expression in that state
	 */
	public Value getValue(long node, Arithmetic arithmetic) {
		return arithmetic.operator.calculate(getValue(node, arithmetic.left), getValue(node, arithmetic.right));
	}
	
	/**
	 * Returns the {@link Value value} a {@link Conditional conditional
	 * expression} whose conditions are in {@link Expression#toPrecondition()
	 * disjunctive normal form} would have when {@link
	 * Expression#evaluate(State) evaluated} in a given state.
	 * 
	 * @param node the ID number of the node representing the state in which
	 * the conditional expression will be evaluated
	 * @param conditional the conditional expression to be evaluated
	 * @return the result of evaluating the expression in that state
	 */
	public Value getValue(long node, Conditional<Disjunction<Clause<Precondition>>> conditional) {
		for(int i=0; i<conditional.conditions.size(); i++)
			if(getValue(node, conditional.conditions.get(i)).equals(True.TRUE))
				return getValue(node, conditional.branches.get(i));
		return getValue(node, conditional.branches.get(conditional.branches.size() - 1));
	}
	
	/**
	 * Returns the {@link Value value} a {@link Expression logical expression}
	 * in {@link Expression#toPrecondition() disjunctive normal form} would
	 * have when {@link Expression#evaluate(State) evaluated} in a given state.
	 * 
	 * @param node the ID number of the node representing the state in which
	 * the expression will be evaluated
	 * @param disjunction the expression to be evaluated
	 * @return the result of evaluating the expression in that state
	 */
	public Value getValue(long node, Disjunction<Clause<Precondition>> disjunction) {
		for(int i=0; i<disjunction.size(); i++)
			if(getValue(node, disjunction.get(i)).equals(True.TRUE))
				return True.TRUE;
		return False.FALSE;
	}
	
	/**
	 * Returns the {@link Value value} a {@link Clause clause} would have when
	 * {@link Expression#evaluate(State) evaluated} in a given state.
	 * 
	 * @param node the ID number of the node representing the state in which
	 * the clause will be evaluated
	 * @param clause the clause to be evaluated
	 * @return the result of evaluating the clause in that state
	 */
	public Value getValue(long node, Clause<Precondition> clause) {
		for(int i=0; i<clause.size(); i++)
			if(getValue(node, clause.get(i)).equals(False.FALSE))
				return False.FALSE;
		return True.TRUE;
	}
	
	/**
	 * Returns the {@link Value value} an {@link Precondition atomic
	 * precondition} would have when {@link Expression#evaluate(State)
	 * evaluated} in a given state.
	 * 
	 * @param node the ID number of the node representing the state in which
	 * the precondition will be evaluated
	 * @param precondition the precondition to be evaluated
	 * @return the result of evaluating the precondition in that state
	 */
	public Value getValue(long node, Precondition precondition) {
		if(precondition.operator.test(getValue(node, (CompiledFluent) precondition.left), getValue(node, precondition.right)))
			return True.TRUE;
		else
			return False.FALSE;
	}
	
	/**
	 * Returns the utility of the {@link #getCharacter(long) character}
	 * associated with a given node, or the {@link
	 * edu.uky.cs.nil.sabre.Problem#utility author's utility} if the node is
	 * associated with the author.
	 * 
	 * @param node the ID number of a node where the utility is desired
	 * @return the utility of the node's character in the state represented by
	 * that node
	 */
	public Value getUtility(long node) {
		double value = utility(node);
		if(Double.isNaN(value))
			return Unknown.UNKNOWN;
		else
			return Number.get(value);
	}
	
	/**
	 * Returns the utility of the {@link #getCharacter(long) character}
	 * associated with a given node as a double, calculating it if it has not
	 * been previously calculated.
	 * 
	 * @param node the ID number of a node
	 * @return the utility of the node's character in the state represented by
	 * that node
	 */
	private final double utility(long node) {
		long utility = get(node, UTILITY);
		if(utility == UTILITY_NOT_SET) {
			Character character = getCharacter(node);
			Value value = getValue(node, character == null ? problem.utility : problem.utilities.get(character));
			if(value == Unknown.UNKNOWN)
				utility = UTILITY_UNKNOWN;
			else
				utility = Double.doubleToLongBits(((Number) value).value);
			set(node, UTILITY, utility);
		}
		return Double.longBitsToDouble(utility);
	}
	
	/**
	 * Returns the ID number of the node immediately before a given node (i.e.
	 * the state before {@link #getEvent(long) the node's event}). If the node
	 * is an initial state node, the node's own ID will be returned.
	 * 
	 * @param node the ID number of a node
	 * @return the ID number of the node immediately before the given node
	 */
	public long getBefore(long node) {
		return get(node, BEFORE);
	}
	
	/**
	 * Returns the ID number of the node immediately before the most recent
	 * non-dummy {@link CompiledAction action} (i.e. the state the most recent
	 * non-dummy action was taken in). If there have been no non-dummy actions
	 * since the initial state, this method will return the ID number of the
	 * initial state node. This method skips over {@link
	 * edu.uky.cs.nil.sabre.graph.DummyAction dummy actions} that represent
	 * belief updates, such the belief updates that occur before a {@link
	 * #surprise(long, CompiledAction, long) surprise action}.
	 * 
	 * @param node the ID number of a node
	 * @return the ID number of the node immediately before the most recent
	 * non-dummy action, or the initial state node if there have been no such
	 * actions
	 */
	public long getBeforeAction(long node) {
		return getBeforeAction(node, false);
	}
	
	private final long getBeforeAction(long node, boolean found) {
		CompiledEvent event = getEvent(node);
		if(event == null)
			return node;
		else if(isDummy(event))
			return getBeforeAction(getBefore(node), found);
		else if(found)
			return node;
		else
			return getBeforeAction(getBefore(node), event instanceof CompiledAction);
	}
	
	/**
	 * Returns the number of child nodes that have been {@link
	 * #getChild(long, CompiledEvent) generated} so far for a given node.
	 * Children nodes are generated using the {@link
	 * #getChild(long, CompiledEvent)} method. The {@link
	 * #getAfter(long, CompiledAction)} method also generates children, but may
	 * return a descendant if {@link #getAfterTriggers(long) triggers apply}
	 * after the action.
	 * 
	 * @param node the ID number of a node
	 * @return the number of children the node currently has
	 */
	public int getChildren(long node) {
		int count = 0;
		long child = getLastChild(node);
		while(child != -1) {
			count++;
			child = getPreviousSibling(child);
		}
		return count;
	}
	
	/**
	 * Returns the ID number of the most recently {@link
	 * #getAfter(long, CompiledAction) generated} child node of a given node.
	 * This method can be used as the first step in traversing the list of all
	 * children; after getting the last child, the second to last child can be
	 * gotten using {@link #getPreviousSibling(long)}, and so on. To extend the
	 * metaphor, this method returns the ID of the youngest child, and each
	 * child knows the ID of it's next older sibling. This method returns -1 if
	 * the node has no children.
	 * 
	 * @param node the ID number of a node whose most recent child is desired
	 * @return the ID number of the most recently generated child node, or -1
	 * if the node has no children
	 */
	public long getLastChild(long node) {
		return get(node, LAST_CHILD);
	}
	
	/**
	 * Returns the ID number of the child node of this node's parent that was
	 * {@link #getAfter(long, CompiledAction) generated} immediately before the
	 * given node. In other words, this method returns a child's next older
	 * sibling. This method can be used as the second, third, etc. step in
	 * traversing the list of a node's children; the first step is {@link
	 * #getLastChild(long)}. This method returns -1 if the node has no sibling.
	 * 
	 * @param node the ID number of a node whose older sibling is desired
	 * @return the ID number of the older sibling, or -1 if no such node exists
	 */
	public long getPreviousSibling(long node) {
		return get(node, PREVIOUS_SIBLING);
	}
	
	/**
	 * Returns the node that results from taking a given {@link CompiledAction
	 * action} in a given state and then {@link #getAfterTriggers(long)
	 * applying any relevant triggers}. If such a node has been generated
	 * before, this method will return it; otherwise this method creates a new
	 * node.
	 * 
	 * @param node the ID number of a node
	 * @param action the action to be taken in the state represented by the
	 * node
	 * @return the ID number of a node whose state is the state after taking
	 * that action in that state (and then applying any relevant triggers)
	 */
	public long getAfter(long node, CompiledAction action) {
		return getAfterTriggers(getChild(node, action));
	}
	
	/**
	 * Returns the node that results when a given {@link CompiledEvent event}
	 * occurs in a given state. The node returned will have the given event as
	 * {@link #getEvent(long) its event} and will represent the state after
	 * applying {@link CompiledEvent#getEffect() the event's effect} to the
	 * state represented by given node. This method does not check whether
	 * {@link CompiledEvent#getPrecondition() the event's precondition} is
	 * satisfied. If such a node has already been generated, it will be
	 * returned; otherwise this method will create a new node. The returned
	 * node is called a child of the given node, which is called the parent.
	 * A node's children, and its children's children, etc. are called
	 * descendants.
	 * 
	 * @param parent the ID number of a node
	 * @param event the event to be taken in the state represented by the node
	 * @return the ID number of the child node whose event will be the event
	 * and whose state will be the state after applying the event's effect
	 */
	public long getChild(long parent, CompiledEvent event) {
		long child = findChild(parent, event);
		if(child == -1)
			child = makeChild(parent, event);
		return child;
	}
	
	/**
	 * Searches for the child node that would result from taking a given event
	 * in a given state. If the child node does not exist, this method returns
	 * -1.
	 * 
	 * @param parent the ID number of the node that is the parent of the desired
	 * child
	 * @param event the event that leads to the desired child node
	 * @return the ID number of the child node created by taking the event in
	 * the parent state, or -1 if no such child exists
	 */
	final long findChild(long parent, CompiledEvent event) {
		long child = getLastChild(parent);
		while(child != -1) {
			if(get(child, EVENT) == event.getID())
				return child;
			child = getPreviousSibling(child);
		}
		return -1;
	}
	
	/**
	 * Creates a child node from a given parent node and event. This method does
	 * not check if the event's precondition is true, and it does not check
	 * whether the parent already has a child node for this event, so it should
	 * only be called if {@link #findChild(long, CompiledEvent)} returns -1 for
	 * this parent and event.
	 * 
	 * @param parent the ID number of the node that will be the parent of the
	 * new node that will be created
	 * @param event the event that will be taken in the parent state that leads
	 * to the new node
	 * @return the ID number of the new child node
	 */
	final long makeChild(long parent, CompiledEvent event) {
		long child = size++;
		set(child, CHARACTER, get(parent, CHARACTER));
		set(child, EVENT, event.getID());
		set(child, UTILITY, UTILITY_NOT_SET);
		set(child, BEFORE, parent);
		set(child, LAST_CHILD, -1);
		set(child, PREVIOUS_SIBLING, getLastChild(parent));
		for(int i=PREVIOUS_SIBLING+1; i<NODE_SIZE; i++)
			set(child, i, -1);
		set(parent, LAST_CHILD, child);
		return child;
	}
	
	/**
	 * Returns the ID number of the node that represents the state after
	 * applying any relevant triggers to the given node. If no triggers apply,
	 * the same node is returned.
	 * 
	 * @param node the ID number of a node
	 * @return the ID number of the node after applying any relevant triggers,
	 * or the same node if no triggers apply
	 */
	public long getAfterTriggers(long node) {
		long after = get(node, AFTER_TRIGGERS);
		if(after == -1) {
			CompiledTrigger trigger = getTrigger(node);
			if(trigger == null) {
				after = node;
				set(node, AFTER_TRIGGERS, node);
				propagate(node, node);
			}
			else {
				after = getAfterTriggers(getChild(node, trigger));
				set(node, AFTER_TRIGGERS, after);
			}
		}
		return after;
	}
	
	/**
	 * Efficiently finds and returns a {@link CompiledTrigger trigger} whose
	 * {@link CompiledTrigger#precondition precondition} is met in the given
	 * state.
	 * 
	 * @param node the ID number of a node
	 * @return a trigger that applies in the state represented by that node, or
	 * null if no triggers apply
	 */
	private final CompiledTrigger getTrigger(long node) {
		return getTrigger(node, triggers);
	}
	
	private final CompiledTrigger getTrigger(long node, EventTree<CompiledTrigger> tree) {
		if(tree == null)
			return null;
		else if(tree.events.size() > 0)
			return tree.events.get(0);
		CompiledTrigger trigger = null;
		if(tree.expression instanceof Precondition)
			trigger = getTrigger(node, tree.getBranch(getValue(node, (Precondition) tree.expression)));
		else
			trigger = getTrigger(node, tree.getBranch(getValue(node, tree.expression)));
		if(trigger == null)
			trigger = getTrigger(node, tree.getBranch(null));
		return trigger;
	}
	
	/**
	 * Returns the ID number of a node that represents the beliefs of an {@link
	 * Character character} in a given state. If the character's beliefs have
	 * already been generated for the given node, the same result will be
	 * returned again; otherwise new nodes will be generated. Any triggers that
	 * apply in the character's beliefs will have already been applied---in
	 * other words, the node ID returned will always be the result of calling
	 * {@link #getAfterTriggers(long)} on whatever new state is generated. If
	 * the character given is null and the node given is associated with the
	 * author (i.e. {@link #getCharacter(long)} returns null for that node),
	 * this method returns the result of {@link #getAfterTriggers(long)};
	 * otherwise this method returns -1 any time the character is null, because
	 * the model does not define what a character believes the author believes.
	 * <p>
	 * Here is how beliefs are derived. If the node given is an initial state
	 * node, then consult the {@link InitialState initial state} to {@link
	 * InitialState#getBeliefs(Character) derive the character's beliefs}.
	 * Otherwise:
	 * <ol>
	 * <li>Let N be the node and C the character given as input to this method.
	 * </li>
	 * <li>Starting at N, {@link #getBefore(long) back up to the state before}
	 * N and {@link #getBeliefs(long, Character) get C's beliefs} for that node.
	 * Call this node B.</li>
	 * <li>If the {@link #getEvent(long) event associated} with N is an {@link 
	 * CompiledAction action} and C {@link CompiledAction#observing observes}
	 * it:
	 * <ol>
	 * <li>If the action's precondition is not met in B, then create {@link
	 * edu.uky.cs.nil.sabre.graph.DummyAction#surprise(State, edu.uky.cs.nil.sabre.Event, State)
	 * a dummy action to update C's beliefs so they will believe the action's
	 * precondition is met}. Update B to be the state after that dummy action.
	 * </li>
	 * <li>Update B to be the state after the action.</li>
	 * </ol></li>
	 * <li>If the event has any effects which directly modify C's beliefs,
	 * create {@link EventList#getUpdate(Character, CompiledFluent, Value) dummy
	 * belief updates actions} to update C's beliefs with those modifications.
	 * Update B to be the state after those dummy action.</li>
	 * <li>Update B to be the state after {@link #getAfterTriggers(long)
	 * applying any relevant triggers}.</li>
	 * <li>Return B.</li>
	 * </ol>
	 * 
	 * @param node the ID number of a node representing some state
	 * @param character the character whose beliefs are desired, or null if the
	 * author's beliefs are desired
	 * @return the ID number of a node representing a state that is the beliefs
	 * of the given character in the given state, or -1 if the character given
	 * was null and the node given was not associated with the author
	 */
	public long getBeliefs(long node, Character character) {
		if(character == null) {
			if(getCharacter(node) == null)
				return getAfterTriggers(node);
			else
				return -1;
		}
		long beliefs = get(node, BELIEFS, character);
		if(beliefs == -1) {
			if(isRoot(node))
				beliefs = initial(node, character, getInitialState(node).getBeliefs(character));
			else {
				long before = getBefore(node);
				beliefs = getBeliefs(before, character);
				CompiledEvent event = getEvent(node);
				if(observes(character, event, before)) {
					beliefs = surprise(before, (CompiledAction) event, beliefs);
					//beliefs = getAfterTriggers(beliefs);
					beliefs = getChild(beliefs, event);
				}
				beliefs = update(node, event, beliefs);
				beliefs = getAfterTriggers(beliefs);
			}
			set(node, BELIEFS, character, beliefs);
		}
		return beliefs;
	}
	
	/**
	 * Tests whether a character observes and event. A character observes an
	 * event if the event is an {@link CompiledAction action} and if the {@link
	 * CompiledAction#observing observation function} for that character {@link
	 * Expression#evaluate(State) evaluates} to {@link True#TRUE true}.
	 * 
	 * @param character the character who may or may not observe the event
	 * @param event the event
	 * @param node the ID number of a node representing the state before the
	 * event was taken
	 * @return true if the event is an action and the character's observation
	 * function evaluates to true in the given state
	 */
	private final boolean observes(Character character, CompiledEvent event, long node) {
		return event instanceof CompiledAction && getValue(node, ((CompiledAction) event).observing.get(character)).equals(True.TRUE);
	}
	
	/**
	 * If necessary, creates and applies {@link
	 * EventList#getUpdate(Character, CompiledFluent, Value) dummy belief update
	 * actions} that update a character's beliefs before an {@link
	 * CompiledAction action} they observe but believe to be impossible
	 * happens. When an action occurs, every character whose {@link
	 * CompiledAction#observing observation function} {@link
	 * Expression#evaluate(State) evaluates} to {@link True#TRUE true} must
	 * update their beliefs based on the action. It is possible for characters
	 * to observe an action they did not believe was possible (i.e. {@link
	 * CompiledAction#precondition the action's precondition} was not met).
	 * This is called a surprise action. When a surprise action happens, the
	 * character should first update their beliefs so the action is possible,
	 * and then update their beliefs based on the action. This method first
	 * checks is a given action is a surprise action. If not, it returns the
	 * character's beliefs unmodified. If the action is a surprise action, it
	 * creates and applies dummy actions which update the character's beliefs in
	 * preparation for the surprise action.
	 * <p>
	 * Some {@link Precondition preconditions} of surprise actions cannot be
	 * made true, such as {@link Comparison#NOT_EQUAL_TO not equal
	 * comparisons}, since it may not be clear how the state should be modified
	 * to make the precondition true. In these cases, the relevant {@link
	 * CompiledFluent fluents} are set to {@link Unknown#UNKNOWN unknown}.
	 * 
	 * @param node the ID number of a node representing the state in which the
	 * action will happen
	 * @param action the action
	 * @param beliefs the ID number of a node representing an character's
	 * beliefs in the state represented by the node
	 * @return the character's unmodified beliefs if they believe the action is
	 * possible, otherwise the ID number of a node representing the character's
	 * beliefs after a dummy belief update actions have been applied in
	 * preparation for the surprise action to occur
	 */
	private final long surprise(long node, CompiledAction action, long beliefs) {
		if(getValue(beliefs, action.getPrecondition()).equals(False.FALSE)) {
			Character character = getCharacter(beliefs);
			for(int i=0; i<action.getPrecondition().size(); i++) {
				Clause<Precondition> clause = action.getPrecondition().get(i);
				if(getValue(node, clause).equals(True.TRUE)) {
					for(int j=0; j<clause.size(); j++) {
						Precondition precondition = clause.get(j);
						if(getValue(beliefs, precondition).equals(False.FALSE)) {
							if(precondition.operator.equals(Comparison.EQUAL_TO))
								beliefs = getChild(beliefs, events.getUpdate(character, (CompiledFluent) precondition.left, getValue(node, precondition.right)));
							else
								beliefs = getChild(beliefs, events.getUpdate(character, (CompiledFluent) precondition.left, Unknown.UNKNOWN));
						}
					}
				}
			}
		}
		return beliefs;
	}
	
	/**
	 * If necessary, creates and applies {@link
	 * EventList#getUpdate(Character, CompiledFluent, Value) dummy belief update
	 * actions} that modify a character's beliefs according to any direct belief
	 * updates that occurs due to {@link CompiledEvent#getEffect() the effects
	 * of an event}. When an event has an effect that directly modifies a
	 * character's beliefs, such as any time {@link
	 * edu.uky.cs.nil.sabre.logic.Assignment#fluent an effect's fluent} has any
	 * number of {@link CompiledFluent#characters characters}, that belief
	 * update is accomplished by applying a dummy action to the character's
	 * beliefs. The dummy action's precondition is {@link False#FALSE false} and
	 * its effects set the state to reflect the direct belief update. This
	 * method first checks whether an event causes any direct belief updates. If
	 * not, the character's unmodified beliefs are returned; otherwise, the
	 * necessary dummy actions are created and applied, and this method returns
	 * the ID number of the state that represents the character's updated
	 * beliefs.
	 * 
	 * @param node the ID number of a node representing the state in which the
	 * event will happen
	 * @param event the event
	 * @param beliefs the ID number of a node representing an character's
	 * beliefs in the state represented by the node
	 * @return the character's unmodified beliefs if the event does not cause
	 * any direct belief updates, otherwise the ID number of a node representing
	 * the character's beliefs after the direct belief updates have been applied
	 */
	private final long update(long node, CompiledEvent event, long beliefs) {
		long before = getBefore(node);
		Character character = getCharacter(beliefs);
		for(int i=0; i<event.getEffect().size(); i++) {
			Effect effect = event.getEffect().get(i);
			if(effect.fluent.characters.size() > 0 && effect.fluent.characters.get(0).equals(character) && getValue(before, effect.condition).equals(True.TRUE)) {
				CompiledFluent fluent = ((CompiledFluent) effect.fluent).removeFirstCharacter();
				Value value = getValue(before, effect.value);
				if(!getValue(beliefs, fluent).equals(value))
					beliefs = getChild(beliefs, events.getUpdate(character, fluent, value));
			}
		}
		return beliefs;
	}
	
	/**
	 * Returns the ID number of a node's most recently {@link
	 * #getBranch(long, Character) generated} trunk. A trunk is the node from
	 * which {@link #getBranch(long, Character) a branch} extends. This method
	 * can be used as the first step in traversing the list of all trunks; after
	 * getting the last trunk, the second to last trunk can be gotten using
	 * {@link #getPreviousTrunk(long, Character)}, and so on. This method
	 * returns -1 if the node has no trunks.
	 * 
	 * @param node the ID number of the node whose last trunk is desired
	 * @return the ID number of the node's most recently generated trunk, or
	 * -1 if the node has no trunks
	 */
	public long getLastTrunk(long node) {
		CompiledEvent event = getEvent(node);
		if(event == null)
			return get(node, LAST_TRUNK);
		else if(isDummy(event))
			return -1;
		else if(event instanceof CompiledTrigger)
			return getLastTrunk(getBefore(node));
		else
			return get(node, LAST_TRUNK);
	}
	
	/**
	 * Returns the ID number of the trunk node for the same character's {@link
	 * #getBranch(long, Character) branch} that was generated immediately before
	 * the given node. In other words, this method returns a branch's next
	 * oldest trunk (for some character). This method can be used as the second,
	 * third, etc. step in traversing the list of a node's trunks; the first
	 * step is {@link #getLastTrunk(long)}. This method returns -1 if the node
	 * has no next oldest trunk for that character.
	 * 
	 * @param node the ID number of a node whose older trunk is desired
	 * @param character the character for whom the trunk branches
	 * @return the ID number of the older trunk, or -1 if no such node exists
	 */
	public long getPreviousTrunk(long node, Character character) {
		return get(node, PREVIOUS_TRUNK, character);
	}
	
	/**
	 * Adds a new trunk to a branch's linked list of trunks.
	 * 
	 * @param node the ID number of the branch node
	 * @param trunk the ID number of the trunk to be added to the branch's list
	 * of trunks
	 * @return true if the trunk was added to the list, or false if the trunk
	 * was already in the list
	 */
	private final boolean addTrunk(long node, long trunk) {
		Character character = getCharacter(node);
		long current = getLastTrunk(node);
		while(current != -1 && current != trunk)
			current = getPreviousTrunk(current, character);
		if(current == -1) {
			set(trunk, PREVIOUS_TRUNK, character, getLastTrunk(node));
			set(node, LAST_TRUNK, trunk);
			return true;
		}
		else
			return false;
	}
	
	/**
	 * Returns the ID number of a node's branch for a given {@link Character
	 * character}. A branch is the state one of {@link CompiledAction#consenting
	 * an action's consenting characters} believes the world to be in after
	 * taking {@link #getAction(long) the most recent action}. The node from
	 * which the branch extends is called the {@link #getLastTrunk(long) trunk}.
	 * A branch exists for a given character if the character is a consenting
	 * character to the most recent action and if that character believes the
	 * action is possible. If the branch already exists, this method will return
	 * it. If the branch can be generated, this method will generate it. If the
	 * branch does not exist (because the character is not a consenting
	 * character or because they do not believe the action is possible) this
	 * method returns -1. If a branch exists, the node ID returned will be after
	 * {@link #getAfterTriggers(long) after applying any relevant triggers}.
	 * <p>
	 * If the given character matches {@link #getCharacter(long) the character
	 * associated with the given node}, the node returned will be the given node
	 * {@link #getAfterTriggers(long) after applying triggers}. In other words,
	 * the branch used to explain an action for a character in that character's
	 * own mind is the trunk. This also applies if the given node is associated
	 * with the author ({@link #getCharacter(long)} returns null) and the given
	 * character is null.
	 * <p>
	 * Branches are used to {@link #isExplained(long) explain} actions. An
	 * action is explained if a branch exists for each of its consenting
	 * characters and if each of those branches {@link
	 * #getExplanation(long, Character) has an explanation} for {@link
	 * #getCharacter(long) that branch's character}. It is important to generate
	 * the branches for an action so that it can become explained. It is
	 * possible to obtain the same node ID by {@link #getBeforeAction(long)
	 * backing up to the state before the action}, {@link
	 * #getBeliefs(long, Character) getting the character's beliefs}, and {@link
	 * #getAfter(long, CompiledAction) taking the action}, but it is important
	 * to use this method because it registers the branch as belonging to this
	 * trunk. This way, when the branch becomes explained, the trunk action
	 * will be notified that it has been explained for the branch character.
	 * 
	 * @param node the ID number of the trunk node for which a branch is
	 * desired
	 * @param character the character for whom the branch is desired
	 * @return the ID number of the branch node, or -1 if the branch does not
	 * exist
	 */
	public long getBranch(long node, Character character) {
		if(character == null) {
			if(getCharacter(node) == null)
				return getAfterTriggers(node);
			else
				return -1;
		}
		else
			return getBranch(node, character, true);
	}
	
	/**
	 * Finds the branch from a given trunk for a given character, with the
	 * option to create the branch if it does not already exist.
	 * 
	 * @param node the ID number of the trunk node for which a branch is
	 * desired
	 * @param character the character for whom the branch is desired
	 * @param create whether or not to create the branch if it does not exist
	 * @return the ID number of the branch node, or -1 if the branch does not
	 * exist and was not created
	 */
	private final long getBranch(long node, Character character, boolean create) {
		CompiledEvent event = getEvent(node);
		if(event == null)
			return -1;
		else if(event instanceof CompiledTrigger)
			return getBranch(getBefore(node), character, create);
		else if(Utilities.equals(character, getCharacter(node)))
			return getAfterTriggers(node);
		else if(!consents(character, event))
			return -1;
		long branch = getBeliefs(getBefore(node), character);
		if(create) {
			long child = findChild(branch, event);
			if(child == -1 && getValue(branch, event.getPrecondition()).equals(True.TRUE))
				child = makeChild(branch, event);
			branch = child;
		}
		else
			branch = findChild(branch, event);
		if(branch == -1)
			return -1;
		if(branch != node && addTrunk(branch, node) && isExplained(node))
			drain(node);
		return getAfterTriggers(branch);
	}
	
	/**
	 * Tests whether a given event is an {@link CompiledAction action} and
	 * whether a given {@link Character character} is one of its {@link
	 * CompiledAction#consenting consenting characters}.
	 * 
	 * @param character the character which may or may not be a consenting
	 * character
	 * @param event the event which may or may not be an action
	 * @return true if the event is an action and the character is one of its
	 * consenting characters, false otherwise
	 */
	private final boolean consents(Character character, CompiledEvent event) {
		return event instanceof CompiledAction && ((CompiledAction) event).consenting.contains(character);
	}
	
	/**
	 * Returns the ID number of a node that is the best (i.e. increases
	 * {@link #getUtility(long) utility} the most) known explained descendant
	 * of a given node. An "explained descendant" means all of the actions
	 * between the given node and the descendant are {@link #isExplained(long)
	 * explained}. If there are no explained descendants, or if none have a
	 * higher utility than the given node, the node ID itself is returned.
	 * 
	 * @param node the ID number of a node for whom a best explained
	 * descendant is desired
	 * @return the ID number of one of the node's best explained descendants,
	 * or the given node ID if no such descendant exists
	 */
	public long getBest(long node) {
		return get(node, BEST);
	}
	
	/**
	 * Returns the ID number of a node that is the worst (i.e. decreases
	 * {@link #getUtility(long) utility} the most) known explained descendant
	 * of a given node. An "explained descendant" means all of the actions
	 * between the given node and the descendant are {@link #isExplained(long)
	 * explained}. If there are no explained descendants, or if none have a
	 * lower utility than the given node, the node ID itself is returned.
	 * 
	 * @param node the ID number of a node for whom a worst explained
	 * descendant is desired
	 * @return the ID number of one of the node's worst explained descendants,
	 * or the given node ID if no such descendant exists
	 */
	public long getWorst(long node) {
		return get(node, WORST);
	}
	
	/**
	 * Checks whether a given node make sense, which means that for every
	 * {@link CompiledAction#consenting consenting character} of the {@link
	 * #getAction(long) the most recent action} nodes have been generated that
	 * demonstrate the character can imagine a plan starting with that action
	 * which increases their {@link #getUtility(long) utility}.
	 * <p>
	 * If the most recent action is a {@link
	 * edu.uky.cs.nil.sabre.graph.DummyAction dummy action} (which are used to
	 * impose unexpected belief updates), this method will return false,
	 * because an unexpected action cannot be explained.
	 * 
	 * @param node the ID number of a node
	 * @return true if the most recent action makes sense for all its
	 * consenting characters, false otherwise
	 */
	public boolean isExplained(long node) {
		CompiledEvent event = getEvent(node);
		if(event == null || isDummy(event))
			return false;
		else if(event instanceof CompiledTrigger)
			return isExplained(getBefore(node));
		else {
			ImmutableSet<Character> consenting = ((CompiledAction) event).consenting;
			for(int i=0; i<consenting.size(); i++)
				if(!isExplained(node, consenting.get(i)))
					return false;
			return true;
		}
	}
	
	/**
	 * Checks whether a given node make sense for a given {@link Character
	 * character}, which means the character is a {@link
	 * CompiledAction#consenting consenting character} for the {@link
	 * #getAction(long) most recent action} and node have been generated that
	 * demonstrate the character can imagine a plan starting with that action
	 * which increases their {@link #getUtility(long) utility}.
	 * 
	 * @param node the ID number of a node
	 * @param character a character from whom the node may be explained
	 * @return true if the character is a consenting character for the most
	 * recent action and the action makes sense for that character, false
	 * otherwise
	 */
	public boolean isExplained(long node, Character character) {
		return getExplanation(node, character) != -1;
	}
	
	/**
	 * Returns the ID number of a node representing the explanation of {@link
	 * #getAction(long) the most recent action} for a given {@link Character
	 * character}. An explanation for an action for a character is the state
	 * after a {@link edu.uky.cs.nil.sabre.Plan plan} the character believes is
	 * possible that starts with the action and increases the character's {@link
	 * #getUtility(long) utility} the most. If no explanation for this character
	 * has yet been generated this method returns -1.
	 * <p>
	 * Note that an explanation is either {@link #getBranch(long, Character) the
	 * branch for the given character} or one of its descendants. This means the
	 * state the node represents and the plan to get there may not actually be
	 * possible in the state represented by the given node; it only needs to be
	 * something the character believes is possible.
	 * 
	 * @param node the ID number of a node for which an explanation of the most
	 * recent action is desired
	 * @param character the character for whom the explanation is desired
	 * @return the ID number of a node that represents the state after
	 * executing a plan the character believes is possible that starts with the
	 * action and which they believe will increase their utility the most, or
	 * -1 if no such node has been generated
	 */
	public long getExplanation(long node, Character character) {
		if(character == null && getCharacter(node) == null)
			return getExplanation(node);
		long branch = getBranch(node, character, false);
		if(branch == -1)
			return -1;
		else
			return getExplanation(branch);
	}
	
	/**
	 * Returns the value of the {@link #EXPLANATION} feature for a given node.
	 * A node's explanation is the ID number of itself or a descendant that
	 * represents a state after a plan of {@link #isExplained(long) explained}
	 * actions that starts with {@link #getAction(long) the most recent action}
	 * and that increase the {@link #getUtility(long) utility} of the {@link
	 * #getCharacter(long) character associated with the node} the most. This
	 * value may often be similar to {@link #getBest(long)}, except that it will
	 * be -1 if no explanation has yet been generated and the plan must start in
	 * {@link #getBeforeAction(long) the previous state} and must include the
	 * most recent action. This method may return the node ID itself if the most
	 * recent action improves the character's utility and no triggers apply.
	 * 
	 * @param node the ID number of the node for which the explanation is
	 * desired
	 * @return the ID number of the node itself or one of its descendants that
	 * represents the state after taking a sequence of explained actions that
	 * includes the most recent action and which increases the utility of the
	 * character associated with the given node the most, or -1 if no such node
	 * has been generated
	 */
	private final long getExplanation(long node) {
		CompiledEvent event = getEvent(node);
		if(event instanceof CompiledTrigger)
			return getExplanation(getBefore(node));
		else
			return get(node, EXPLANATION);
	}
	
	/**
	 * Returns a {@link Clause clause} from {@link #getBest(long) the best
	 * known descendant} of a given node that is sufficient to ensure {@link
	 * edu.uky.cs.nil.sabre.Problem#utilities the utility expression} of the
	 * {@link #getCharacter(long) character associated with the node} evaluates
	 * to their {@link #getUtility(long) utility} in that state. In other words,
	 * this method returns a clause the character wants to make true (if there
	 * is a way to improve their utility) or a clause which expresses why their
	 * utility is what it currently is in the given state.
	 * 
	 * @param node the ID number of a node representing a state
	 * @return a clause that is true in the best known descendant state which
	 * is sufficient to explain the character's utility in that state
	 */
	public Clause<Precondition> getGoal(long node) {
		return getGoalClause(getBest(node));
	}
	
	/**
	 * Creates a {@link Clause clause} that is true in the given state and
	 * which is sufficient to explain why the {@link #getUtility(long) the
	 * utility of the character associate with the node} is its value in that
	 * state.
	 * 
	 * @param node the ID number of the node from which the clause is desired
	 * @return a clause which is true in the given state and which explains the
	 * utility of the character associated with that node
	 */
	private final Clause<Precondition> getGoalClause(long node) {
		Conditional<Disjunction<Clause<Precondition>>> utility = getCharacter(node) == null ? problem.utility : problem.utilities.get(getCharacter(node));
		Expression goal = True.TRUE;
		for(int i=0; i<utility.branches.size(); i++) {
			Disjunction<Clause<Precondition>> condition = utility.getCondition(i);
			if(getValue(node, condition).equals(True.TRUE)) {
				for(Clause<Precondition> clause : condition) {
					if(getValue(node, clause).equals(True.TRUE)) {
						goal = new Conjunction<>(goal, clause);
						break;
					}
				}
				for(CompiledFluent fluent : utility.branches.get(i).collect(CompiledFluent.class))
					goal = new Conjunction<>(goal, new Precondition(Comparison.EQUAL_TO, fluent, getValue(node, fluent)));
				break;
			}
			else
				goal = new Conjunction<>(goal, condition.negate());
		}
		return goal.toPrecondition().get(0);
	}
	
	/**
	 * Returns a {@link Solution solution plan} that starts at a given node and
	 * ends at the {@link #getBest(long) best known descendant} that improves
	 * the {@link #getUtility(long) utility of the character associated with the
	 * node}. If there is no known way to improve utility from the given node,
	 * this method returns null.
	 * 
	 * @param node the ID number of the node representing the state in which
	 * the solution should start
	 * @return a solution starting in the given state and leading to a state
	 * where the utility of the character associated with the node is improved,
	 * or null if no such solution has been generated
	 */
	public Solution<CompiledAction> getSolution(long node) {
		return getSolution(node, getBest(node));
	}

	/**
	 * Returns a {@link Solution solution plan} that starts at a given node and
	 * passes through a second given node on the way to a descendant that
	 * {@link #getBest(long) most improves} the {@link #getUtility(long) the
	 * utility of the character associated with the nodes}. If the second node
	 * is a descendant of the first that improves the character's utility, the
	 * solution will end there; otherwise, the solution will end in the state
	 * that explains the {@link #getAction(long) action associated with the
	 * second node}, and as a result the solution will both include that action
	 * and pass through the state after that action. If the second node is not
	 * a descendant of the first, or if there is no solution which starts at
	 * the first node and passes through the second, this method returns null.
	 * 
	 * @param start the ID number of the node representing the state in which
	 * the solution should start
	 * @param end the ID number of a node representing a state which the
	 * solution should pass through
	 * @return a solution starting in the first given state and passing through
	 * the second, or null if no such solution exists
	 */
	public Solution<CompiledAction> getSolution(long start, long end) {
		if(start == -1 || end == -1)
			return null;
		else if(getAfterTriggers(start) != start)
			return getSolution(getAfterTriggers(start), end);
		else if(getAfterTriggers(end) != end)
			return getSolution(start, getAfterTriggers(end));
		else if(utility(start) >= utility(end) && getExplanation(end) != end)
			return getSolution(start, getExplanation(end));
		else
			return getSolution(start, end, utility(end), new SolutionGoal<>(getCharacter(end), getGoalClause(end)));
	}

	private final Solution<CompiledAction> getSolution(long start, long end, double goal, Solution<CompiledAction> solution) {
		if(start == end)
			return solution;
		else if(solution.size() > 0 && getAfterTriggers(end) == end && utility(end) >= goal)
			return null;
		CompiledEvent event = getEvent(end);
		if(event == null || isDummy(event))
			return null;
		else if(event instanceof CompiledAction) {
			CompiledAction action = (CompiledAction) event;
			solution = solution.prepend(action);
			for(Character other : action.consenting) {
				if(!Utilities.equals(other, getCharacter(end))) {
					long branch = getBranch(end, other, false);
					if(branch != -1) {
						Solution<CompiledAction> explanation = getSolution(getJustBeforeAction(branch), branch);
						if(explanation != null)
							solution = solution.setExplanation(explanation);
					}
				}
			}
		}
		return getSolution(start, getBefore(end), goal, solution);
	}
	
	/**
	 * Returns the ID number of the node immediately before the most recent
	 * {@link CompiledAction action}, or -1 if the most recent action was a
	 * {@link edu.uky.cs.nil.sabre.graph.DummyAction dummy action}. This
	 * method is almost identical to {@link #getBeforeAction(long, boolean)},
	 * except that it will not back up past dummy actions. If it encounters a
	 * dummy action before a non-dummy action, it returns -1. If it encounters
	 * a surprise action, it returns the state immediately before the action
	 * (i.e. the state immediately after the dummy surprise action belief
	 * updates that happened before the surprise action).
	 * 
	 * @param node the ID number of a node
	 * @return the ID number of the node representing the state immediately
	 * before the most recent non-dummy action, or -1 if the most recent action
	 * was a dummy action, or the ID number of the initial state if there have
	 * been no recent actions
	 */
	final long getJustBeforeAction(long node) {
		CompiledEvent event = getEvent(node);
		if(event == null)
			return node;
		else if(isDummy(event))
			return -1;
		else if(event instanceof CompiledAction)
			return getBefore(node);
		else
			return getJustBeforeAction(getBefore(node));
	}
	
	/**
	 * This method notifies a node about one of its descendants, in case that
	 * node might be the new {@link #getBest(long) best} or {@link
	 * #getWorst(long) worst} descendant, or in case the node might be an
	 * {@link #getExplanation(long) explanation} for the first node's action.
	 * When an action becomes newly explained, this method also notifies all the
	 * {@link #getLastTrunk(long) trunks} of that node, in case they have now
	 * become explained as a result.
	 * 
	 * @param node the ID number of the node to be notified of a descendant
	 * @param future the ID number of a node that is a descendant of the first
	 */
	private final void propagate(long node, long future) {
		// Check if the descendant is the new best node.
		if(getBest(node) == -1 || utility(future) > utility(getBest(node)))
			set(node, BEST, future);
		// Check if the descendant is the new worst node.
		if(getWorst(node) == -1 || utility(future) < utility(getWorst(node)))
			set(node, WORST, future);
		// Depending on the node's event...
		CompiledEvent event = getEvent(node);
		// If the node is an initial state node, add the descendant to the
		// queue of waiting nodes. This is how solutions end up accumulating
		// at node 0 for the getSolution() method.
		if(event == null)
			push(node, future);
		// If the node's event is a trigger, propagate the descendant up.
		else if(event instanceof CompiledTrigger)
			propagate(getBefore(node), future);
		// If the node's event is a dummy action, it cannot be explained; stop.
		else if(isDummy(event))
			return;
		// If the node's event is an action, and the descendant state has a
		// different utility, and the path to the descendant is minimal...
		// (note: it is important to know about both best and worst outcomes)
		else if(utility(getBefore(node)) != utility(future) && isMinimal(node, future)) {
			// Add the descendant to the queue of nodes waiting at this node.
			// The queue will eventually be emptied if the node is explained.
			push(node, future);
			// If the descendant is an explanation, make note of it.
			if(better(node, future))
				set(node, EXPLANATION, future);
			// If the node is explained (either it was before or has just now
			// become so), drain the queue of waiting nodes and propagate them
			// all up.
			if(isExplained(node))
				drain(node);
			// Also check all of the trunks for which the node is a branch;
			// they may now have become explained.
			long trunk = getLastTrunk(node);
			while(trunk != -1) {
				if(isExplained(trunk))
					drain(trunk);
				trunk = getPreviousTrunk(trunk, getCharacter(node));
			}
		}
	}
	
	/**
	 * Checks whether the path from one node to a descendant is the shortest
	 * path of explained nodes that achieves that utility. The actions that
	 * happens between the start node represent a plan. This method works by
	 * trying to find a strict subsequence of that plan which achieve the same
	 * utility. If it cannot find one, the plan is minimal.
	 * <p>
	 * Note that this method searches for a subsequence of actions that
	 * achieves the exact utility (not higher or lower), because this method
	 * can be used to check the minimality of both best and worst plans.
	 * <p>
	 * A plan is minimal if all its actions are explained and there is no
	 * shorter sequence of explained actions that achieves the same utility.
	 * Technically, this method does not fully check this requirement, because
	 * a shorter plan might exist that has different explanations that have not
	 * yet been discovered. This method only checks whether a shorter plan can
	 * be found that uses the same explanations or other already-discovered
	 * explanations; checking for undiscovered explanations would be equivalent
	 * to a smaller planning problem and thus too expensive.
	 * 
	 * @param node the ID number of a node representing the state to start in
	 * @param future the ID number of a node that is a descendant of the first
	 * and which represents the state to end in
	 * @return false if there is a subsequence of explained actions that can be
	 * taken from the start node to reach a state which achieves the same
	 * utility, true otherwise
	 */
	private final boolean isMinimal(long node, long future) {
		return !findSubsequence(getBefore(node), getBefore(node), future, false, utility(future));
	}
	
	/**
	 * Searches for a subsequence of actions to achieve a goal utility.
	 * 
	 * @param node the ID number of the current state of the subsequence being
	 * explored
	 * @param start the ID number of the next node in the original action
	 * sequence whose subsequence are being checked
	 * @param end the ID number of the last node in the original action
	 * sequence whose subsequence are being checked
	 * @param shorter true if at least one action has been left out so far, or
	 * false if no actions have been left out yet
	 * @param goal the utility at the end of the original action sequence
	 * @return true if a subsequence of the original sequence was found which
	 * achieves the same utility, false otherwise
	 */
	private final boolean findSubsequence(long node, long start, long end, boolean shorter, double goal) {
		// If the end of the original sequence has been reached, the current
		// state of the subsequence achieves the target utility, and at least
		// one action has been left out along the way, return success.
		if(start == end)
			return shorter && utility(node) == goal;
		// Get the next action and the next state from the original action
		// sequence.
		CompiledAction action = first(start, end);
		long next = getAfter(start, action);
		// If that action cab be left out of the subsequence and the target
		// utility reached, return success.
		if(findSubsequence(node, next, end, true, goal))
			return true;
		// Else, make sure the action is possible in the subsequence.
		if(getValue(node, action.getPrecondition()).equals(True.TRUE)) {
			// Use the explanations from the original action sequence to
			// explain the remaining actions in the new subsequence.
			expand(node, start, getExplanation(next));
			// Take the action in the subsequence.
			long after = getAfter(node, action);
			// Recursively check if a subsequence can be found.
			return isExplained(after) && findSubsequence(after, next, end, shorter, goal);
		}
		return false;
	}
	
	/**
	 * Attempts to use explanations from one action sequence to {@link
	 * #isExplained(long) explain} actions in a newly formed subsequence of
	 * those actions.
	 * 
	 * @param node the ID number of a node that represents the start state of
	 * the subsequence of actions
	 * @param start the ID number of a node that represents the start state of
	 * the original action sequence
	 * @param end the ID number of a node that represents the end state of the
	 * original action sequence
	 */
	private final void expand(long node, long start, long end) {
		// Stop if any of the given nodes do not exist.
		if(node != -1 && start != -1 && end != -1 && start != end) {
			// Get the first action of the original sequence.
			CompiledAction action = first(start, end);
			// If that action can be taken in the new subsequence...
			if(getValue(node, action.getPrecondition()).equals(True.TRUE)) {
				// Take the action in the new subsequence.
				long after = getAfter(node, action);
				// Follow the action in the original sequence.
				long next = getAfter(start, action);
				// Expand all future actions in the original sequence too.
				expand(after, next, end);
				// For each consenting character of the action, find the
				// explanation for that character in the original action
				// sequence and expand it in the new subsequence.
				for(int i=0; i<action.consenting.size(); i++) {
					Character character = action.consenting.get(i);
					expand(getBranch(after, character), getBranch(next, character), getExplanation(next, character));
				}
			}
		}
	}
	
	/**
	 * Returns the first action in the implied plan that exists between a node
	 * and one of its descendants. This method starts at the end state and
	 * looks backwards until it finds the start state, then returns the last
	 * action it encountered before reaching that start state.
	 * 
	 * @param start the ID number of a node representing the state in which the
	 * sequence starts
	 * @param end the ID number of a node representing the state in which the
	 * sequence ends
	 * @return the first action in the implied plan between the start and end
	 * states, or null if there are no actions between the nodes
	 */
	private final CompiledAction first(long start, long end) {
		return first(start, end, null);
	}
	
	private final CompiledAction first(long start, long end, CompiledAction action) {
		if(start == end)
			return action;
		CompiledEvent event = getEvent(end);
		if(event instanceof CompiledAction)
			action = (CompiledAction) event;
		return first(start, getBefore(end), action);
	}
	
	/**
	 * Checks whether a given descendant is a better explanation for a given
	 * node than that node's current explanation (if any). There are three
	 * cases in which this method returns true:
	 * <ol>
	 * <li>The descendant improves {@link #getUtility(long) utility} and the
	 * node has no current explanations, so something is better than nothing.
	 * </li>
	 * <li>The descendant improves utility more than the current explanation.
	 * </li>
	 * <li>The descendant improves utility the same amount, but the new
	 * explanation is shorter than the current explanation.</li>
	 * </ol>
	 * 
	 * @param node the ID number of a node for whom the descendant is an
	 * explanation
	 * @param future the ID number of a node which is a descendant of the first
	 * node and which might be a better explanation
	 * @return true if the descendant is a better explanation than the node's
	 * current explanation, false otherwise
	 */
	private final boolean better(long node, long future) {
		if(utility(future) <= utility(getBefore(node)))
			return false;
		long explanation = getExplanation(node);
		if(explanation == -1 || utility(future) > utility(explanation))
			return true;
		else
			return utility(future) == utility(explanation) && length(node, future) < length(node, explanation);
	}
	
	/**
	 * Counts the actions between a node and one of its descendants.
	 * 
	 * @param node the ID number of a node representing the state in which a
	 * sequence of actions starts
	 * @param future the ID number of a node representing the state in which a
	 * sequence of action stops
	 * @return the number of actions between the states
	 */
	private final int length(long node, long future) {
		if(node == future)
			return 0;
		CompiledEvent event = getEvent(future);
		if(event instanceof CompiledAction)
			return 1 + length(node, getBefore(future));
		else
			return length(node, getBefore(future));
	}
	
	/**
	 * Adds a descendant to the queue of nodes waiting for a given node to
	 * become explained. Methods like {@link #getBest(long)}, {@link
	 * #getWorst(long)}, and {@link #getExplanation(long)} are only interested
	 * in descendants which can be reached via a sequence of explained actions.
	 * When {@link #propagate(long, long) a descendant is reported to its
	 * ancestor}, but that ancestor is not yet explained, the descendants wait
	 * at that ancestor until the ancestor becomes explained before they are
	 * propagated further up. This method adds a descendant to the queue of
	 * waiting nodes.
	 * 
	 * @param node the ID number of the node where the queue is formed
	 * @param future the ID number of the node to be added to the queue
	 */
	private final void push(long node, long future) {
		set(future, PREVIOUS_IN_QUEUE, get(node, END_OF_QUEUE));
		set(node, END_OF_QUEUE, future);
	}
	
	/**
	 * Removes a descendant from the front of the queue of descendants waiting
	 * for the node to become explained.
	 * 
	 * @param node the ID number of the node where the queue is formed
	 * @return the ID number of the first node in queue, or -1 if the queue is
	 * empty
	 */
	private final long pop(long node) {
		long child = get(node, END_OF_QUEUE);
		if(child == -1)
			return -1;
		long previous = -1;
		while(get(child, PREVIOUS_IN_QUEUE) != -1) {
			previous = child;
			child = get(child, PREVIOUS_IN_QUEUE);
		}
		if(previous == -1)
			set(node, END_OF_QUEUE, -1);
		else
			set(previous, PREVIOUS_IN_QUEUE, -1);
		return child;
	}
	
	/**
	 * Removes all descendants from the queue of descendants waiting for a node
	 * to become explained and {@link #propagate(long, long) propagates} them
	 * up to the node's parent. This method is used when a node becomes
	 * explained; all descendants who were waiting on it to become explained
	 * are propagated up.
	 * 
	 * @param node the ID number of the node  where the queue is formed
	 */
	private final void drain(long node) {
		long child = get(node, END_OF_QUEUE);
		set(node, END_OF_QUEUE, -1);
		drain(child, getBefore(node));
	}
	
	private final void drain(long child, long before) {
		if(child != -1) {
			drain(get(child, PREVIOUS_IN_QUEUE), before);
			set(child, PREVIOUS_IN_QUEUE, -1);
			propagate(before, child);
		}
	}
}