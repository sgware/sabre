package edu.uky.cs.nil.sabre.prog;

import java.util.Comparator;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Solution;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.comp.CompiledAction;
import edu.uky.cs.nil.sabre.comp.CompiledFluent;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A progression space is a method of implementing the graph of states that is
 * searched by a {@link ProgressionSearch progression search}. A progression
 * space defines a type of object that represents a node in the graph and
 * provides ways of interacting with nodes to gain information about their
 * relationship to the whole space.
 * <p>
 * A node can be considered an action/state pair. In other words, each node
 * needs to identify both a {@link State state} and the {@link CompiledAction
 * action} which occurred immediately before that state. A node in a state space
 * can be thought of as a temporal edge in the state graph; it represents both
 * an action and the state that follows that action (and thus it implicitly also
 * represents the state before taking the action as well).
 * <p> 
 * Different progression spaces will have tradeoffs in size and speed. For
 * example, a progression space that detects duplicate states will be smaller,
 * but the added overhead needed to detect duplicates may or may not make the
 * space faster to search.
 * 
 * @param <N> the type of object used to represent a state in this space
 * @author Stephen G. Ware
 *
 */
public interface ProgressionSpace<N> {

	/**
	 * Returns the total number of states in the whole state space.
	 * 
	 * @return the number of states
	 */
	public long size();
	
	/**
	 * Reset and initializes the state space to begin at a given initial state.
	 * The nodes previously represented in this structure may become invalid
	 * after calling this method.
	 * 
	 * @param state the initial state where search will begin
	 * @return an object representing that initial state in this space
	 */
	public N initialize(State state);
	
	/**
	 * Returns the next {@link Solution solution} in the space that has not yet
	 * been returned. A solution is any plan which improves the {@link
	 * edu.uky.cs.nil.sabre.Problem#utility author's utility} where every {@link
	 * CompiledAction action} is {@link #isExplained(Object) explained}.
	 * 
	 * @return the next solution which has not yet been returned, or null if no
	 * new solutions have been discovered
	 */
	public Solution<CompiledAction> getNextSolution();
	
	/**
	 * Compares two objects that represent nodes in this search space. This
	 * method follows the contract of {@link
	 * Comparator#compare(Object, Object)}, except that it returns a {@code
	 * double} instead of an {@code int}.
	 * 
	 * @param node1 the first node in the state space to compare
	 * @param node2 the second node in the state space to compare
	 * @return a negative double, zero, or a positive double as the first
	 * node is higher priority, the same priority, or lower priority than the
	 * second node
	 */
	public double compare(N node1, N node2);
	
	/**
	 * Returns the {@link CompiledAction action} associated with the given node
	 * in the state space. Recall that a node is an action/state pair where the
	 * action leads to the state. This method returns the action of that pair.
	 * If a node has no action because it represents an initial state, this
	 * method returns null.
	 * 
	 * @param node the node in the state space whose action is desired
	 * @return the action associated with the node, or null if the node
	 * represents an initial state
	 */
	public CompiledAction getAction(N node);
	
	/**
	 * Returns the {@link Value value} of a {@link CompiledFluent fluent} in the
	 * state represented by a node in the state space. Recall that a node is an
	 * action/state pair where the action leads to the state. This method
	 * returns the value of a fluent in the state of that pair.
	 * 
	 * @param node the node in the state space to be treated as a state
	 * @param fluent the fluent to be evaluated
	 * @return the value of the fluent in that node's state
	 */
	public Value getValue(N node, CompiledFluent fluent);
	
	/**
	 * Indicates whether the {@link CompiledAction action} that led to the state
	 * for the given node is fully explained (that is, explained for all its
	 * {@link CompiledAction#consenting consenting characters}).
	 * 
	 * @param node the node in the state space whose action may be explained
	 * @return true if the node's action is explained for all its consenting
	 * characters, false otherwise
	 */
	public boolean isExplained(N node);
	
	/**
	 * Indicates whether the {@link CompiledAction action} that led to the state
	 * for the given node is explained for a given character. Note that if the
	 * character is not a {@link CompiledAction#consenting consenting character}
	 * for the action, this method may return false even if the action does
	 * make sense for that character to want.
	 * 
	 * @param node the node in the state space whose action may be explained
	 * @param character the character for which it may be explained
	 * @return true if the action associated with the node is explained for that
	 * character
	 */
	public boolean isExplained(N node, Character character);
	
	/**
	 * Returns a collection of nodes from this state space that represent states
	 * that may have come before the given node. Recall that a node is an
	 * action/state pair where the action leads to the state. This means that,
	 * unless {@link #getAction(Object) the action} is null, a node also
	 * implicitly represents the state before taking the action that led to its
	 * state. This method returns a list of other action/state pairs that led to
	 * this before state. In other words, if we think of the given node as the
	 * second action in a plan leading to the third state in a plan, this method
	 * returns a collection of nodes that could be the first action in the plan
	 * leading to the second state in the plan. If the given node has no
	 * parents, for example because it is an initial state, this collection will
	 * be empty.
	 * 
	 * @param node a node in the state space
	 * @return a collection of nodes in the state space that represent
	 * action/state pairs before this node, or an empty collection is no such
	 * nodes exist
	 */
	public Iterable<N> getParents(N node);
	
	/**
	 * Returns the node that would result from taking the given action in the
	 * state represented by the given node. Recall that a node is an
	 * action/state pair where the action leads to the state. The node returned
	 * by this method will have the given action as its action, the state after
	 * that action as its state, and the given node will appear in the {@link
	 * #getParents(Object) parents} of the resulting node.
	 * 
	 * @param node a node in the state space
	 * @param action the action to take in the state represented by the node
	 * @return the node that results from taking that action in that state
	 */
	public N getChild(N node, CompiledAction action);
	
	/**
	 * Returns the node that represents what a {@link Character character}
	 * believes will happen when taking an {@link CompiledAction action}. Recall
	 * that a node is an action/state pair where the action leads to the state. 
	 * The branch returned will be for the action of that pair. A branch is used
	 * to explain why an action's {@link CompiledAction#consenting consenting
	 * characters} would want to take the action. This method will return if the
	 * branch does not exist. A branch does not exist if the given character
	 * does not believe the action is possible. In some implementations,
	 * branches may only exist for consenting characters.
	 * 
	 * @param node the node in the state space whose action needs to be
	 * explained for some character
	 * @param character the character for which the action needs to be
	 * explained
	 * @return a node in the state space which represents that character's
	 * beliefs about the state after taking the action, or null if such a branch
	 * does not exist
	 */
	public N getBranch(N node, Character character);
}