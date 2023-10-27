package edu.uky.cs.nil.sabre.hg;

import java.io.Serializable;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

import edu.uky.cs.nil.sabre.Action;
import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Event;
import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Trigger;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.logic.Arithmetic;
import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Conditional;
import edu.uky.cs.nil.sabre.logic.Disjunction;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.Logical;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * A heuristic graph is a data structure that represents the relationships
 * between {@link edu.uky.cs.nil.sabre.logic.Proposition propositions} and
 * {@link Event events} in a {@link CompiledProblem ground problem} to enable
 * various kinds of analysis.
 * <p>
 * Heuristic graphs are inspired by Avrim Blum and Merrick Furst's plan graph
 * data structure. It represents a relaxed model of a planning problem where
 * once a proposition becomes true it stays true forever (sometimes called the
 * "ignore delete lists" relaxation). However, these are several important
 * differences:
 * <ul>
 * <li>Since Sabre problems use {@link edu.uky.cs.nil.sabre.comp.CompiledFluent
 * fluents} and are not limited to Boolean propositions, heuristic graphs
 * represent each fluent as {@link FluentNode a set} of {@link Value values}
 * where values can be added to the set but never removed (sometimes called
 * monotonic or accumulating semantics).</li>
 * <li>Heuristic graphs can represent numeric values, like {@link
 * NumericFluentNode numeric fluents} and {@link ArithmeticNode arithmetic
 * expressions}. By default, numeric values are represented as a {@link Range
 * range} which implicitly includes all values between the lowest and highest
 * value.</li>
 * <li>A heuristic graph does not model mutex relations. This is because
 * mutex relations are costly to check, and heuristic graphs are designed to be
 * fast enough to be reused frequently, such as in Jörg Hoffmann's Fast Forward
 * heuristic.</li>
 * <li>Heuristic graphs generalize Blum and Furst's plan graphs as well as Blai
 * Bonet and Héctor Geffner's additive heuristic by allowing multiple possible
 * definitions of {@link CostNode a node's cost}. A {@link MaxGraph max graph}
 * is a heuristic graph where the cost of a conjunction is the maximum cost of
 * any of its conjuncts, similar to the levels of Blum and Furst's plan graph.
 * A {@link SumGraph sum graph} is a heuristic graph where the cost of a
 * conjunction is the sum of the cost of its conjuncts (similar to Bonet and
 * Geffner's additive heuristic).</li>
 * </ul>
 * Nodes in a heuristic graph can be treated like a monotonically increasing
 * set of values that are each associated with a cost. The most common
 * operation on a heuristic graph is {@link CostSet#getCost(Value) to query a
 * node about the cost of a value}. For example, you can ask a {@link
 * NominalFluentNode nominal fluent} how much it would cost for it to take on
 * one of its many possible values. When a heuristic graph is {@link
 * #initialize(State) initialized to a state}, every value will have either a
 * cost of 0 (meaning the node's expression has that value in the initial
 * state) or {@link Double#POSITIVE_INFINITY positive infinity} (meaning the
 * node's expression cannot yet have that value). Each time a heuristic graph
 * is {@link #extend() extended}, every {@link ActionNode action} whose
 * precondition has a finite cost will be added to the graph (that is, its
 * {@link CostNode#getCost() cost} will go from infinite to finite). Adding
 * new actions may cause new expressions that previously had an infinite cost
 * to have a finite cost. If those new actions enable new propositions, then
 * extending the graph again may add new actions, and those actions will add
 * new propositions, and so on. Eventually, the graph will level off, which
 * means that no new actions can be added. Once a graph has leveled off, 
 * {@link #extend()} will return false.
 * <p>
 * Some nodes represent {@link CostNode Boolean propositions}, whose sets of
 * values can only be true and false. Examples include {@link ClauseNode
 * disjunctions}, {@link ClauseNode clauses}, {@link PreconditionNode
 * preconditions}, and {@link EffectNode effects}. These nodes only have a
 * {@link CostNode#getCost()} method that returns the cost of that expression
 * being true. {@link EventNode Event nodes} are like this too; their {@link
 * CostNode#getCost()} method returns the cost of reaching a state where
 * that event could occur.
 * <p>
 * Heuristic graphs can be used to explore the structural properties of a
 * problem, such as which actions can possibly satisfy certain propositions.
 * Heuristic graphs can also be used to check a necessary but not sufficient
 * condition for whether a proposition is possible. Once a heuristic graph has
 * leveled off, if a proposition is not in the graph, we can be confident that
 * the proposition is impossible (however, we cannot be certain that a
 * proposition is possible just because it is in the graph).
 * <p>
 * Heuristic graphs are commonly used to {@link
 * edu.uky.cs.nil.sabre.comp.Simplifier simplify} compiled problems and in
 * {@link edu.uky.cs.nil.sabre.prog.GraphHeuristic heuristics} for {@link
 * edu.uky.cs.nil.sabre.prog.ProgressionPlanner heuristic search}.
 * 
 * @author Stephen G. Ware
 */
public abstract class HeuristicGraph implements Serializable {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * Nodes representing all {@link edu.uky.cs.nil.sabre.comp.CompiledFluent
	 * fluents} in the graph
	 */
	public final List<FluentNode> fluents = new List<>();

	/**
	 * Nodes representing all {@link edu.uky.cs.nil.sabre.comp.CompiledAction
	 * actions} in the graph
	 */
	public final List<ActionNode> actions = new List<>();
	
	/** Maps the labels of nodes to their corresponding nodes */
	protected final HashMap<Object, Node> nodes = new HashMap<>();
	
	/**
	 * A node representing the {@link edu.uky.cs.nil.sabre.Problem#utility
	 * author's utility}
	 */
	protected UtilityNode utility = null;
	
	/**
	 * Nodes representing each {@link edu.uky.cs.nil.sabre.Problem#utilities
	 * character's utility}
	 */
	protected final HashMap<Character, UtilityNode> utilities = new HashMap<>();
	
	/**
	 * A node representing the constant {@link False#FALSE}. This field is
	 * necessary because false is both a {@link Value value} and an empty
	 * {@link Disjunction disjunction}, so the same label can map to two
	 * different nodes. This field points to the {@link ConstantNode constant}
	 * for false; the mapping in {@link #nodes} will be to the {@link
	 * DisjunctionNode disjunction} for false.
	 */
	final ConstantNode falseValueNode;
	
	/**
	 * A node representing the constant {@link True#TRUE}. This field is
	 * necessary because true is both a {@link Value value} and a {@link
	 * Disjunction disjunction} of {@link Clause#EMPTY the empty clause}, so
	 * the same label can map to two different nodes. This field points to the
	 * {@link ConstantNode constant} for true; the mapping in {@link #nodes}
	 * will be to the {@link DisjunctionNode disjunction} for true.
	 */
	final ConstantNode trueValueNode;
	
	/** A node representing {@link Clause#EMPTY the empty clause} */
	final ClauseNode emptyClauseNode;
	
	/**
	 * A list of {@link ActionNode actions} whose preconditions have a finite
	 * cost but which have not yet been added to the graph
	 */
	final List<ActionNode> next = new List<>();
	
	/**
	 * A sentinel node that represents the end of the list of nodes that need to
	 * be {@link #reset() reset}
	 */
	private final Node lastToReset = new Node(this, null) {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1L;
	};
	
	/**
	 * The start of a linked list of nodes that need to be {@link #reset()
	 * reset}
	 */
	Node firstToReset = lastToReset;
	
	/**
	 * Constructs a heuristic graph which contains only the nodes representing
	 * {@link False#FALSE false}, {@link True#TRUE true}, and {@link
	 * Clause#EMPTY the empty clause}.
	 * 
	 * @param status a status to update while building the graph
	 */
	@SuppressWarnings("unchecked")
	public HeuristicGraph(Status status) {
		status.setMessage("Building empty heuristic graph...");
		this.falseValueNode = makeConstantNode(False.FALSE);
		this.trueValueNode = makeConstantNode(True.TRUE);
		this.emptyClauseNode = getClause((Clause<Precondition>) (Clause<?>) Clause.EMPTY);
		makeDisjunctionNode(False.FALSE.toPrecondition());
		makeDisjunctionNode(True.TRUE.toPrecondition());
	}
	
	/**
	 * Constructs a heuristic graph which contains nodes for every fluent,
	 * event, and utility in a given problem.
	 * 
	 * @param problem the problem whose fluents, events, and utilities will be
	 * represented in the graph
	 * @param status a status to update while building the graph
	 */
	public HeuristicGraph(CompiledProblem problem, Status status) {
		this(status);
		status.setMessage("Building heuristic graph for \"" + problem.name + "\"...");
		for(Fluent fluent : problem.fluents)
			getFluent(fluent);
		for(Event event : problem.events)
			getEvent(event);
		setUtility(null, problem.utility);
		for(Character character : problem.universe.characters)
			setUtility(character, problem.utilities.get(character));
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	/**
	 * Returns the {@link Node heuristic graph node} associated with the given
	 * {@link Logical logical expression}. If the node does not exist, it will
	 * be created.
	 * 
	 * @param logical the logical expression to represent in the graph
	 * @return a node representing that logical expression (which will have
	 * that expression as its {@link Node#label its label})
	 * @throws IllegalArgumentException if the graph cannot create a node for
	 * the given expression
	 * @throws edu.uky.cs.nil.sabre.FormatException if the logical expression
	 * is not ground
	 */
	@SuppressWarnings("unchecked")
	public Node getNode(Logical logical) {
		if(logical instanceof Value)
			return getConstant((Value) logical);
		if(logical instanceof Fluent)
			return getFluent((Fluent) logical);
		else if(logical instanceof Arithmetic)
			return getArithmetic((Arithmetic) logical);
		else if(logical instanceof Precondition)
			return getPrecondition((Precondition) logical);
		else if(isClause(logical))
			return getClause((Clause<Precondition>) logical);
		else if(isDNF(logical))
			return getDisjunction((Disjunction<Clause<Precondition>>) logical);
		else if(logical instanceof Event)
			return getEvent((Event) logical);
		else if(logical instanceof Effect)
			return getEffect((Effect) logical);
		else if(logical instanceof Expression)
			return getDisjunction(((Expression) logical).toPrecondition());
		else
			throw Exceptions.noHeuristicGraphNode(logical);
	}
	
	private static final boolean isClause(Logical logical) {
		if(!(logical instanceof Clause))
			return false;
		Clause<?> clause = (Clause<?>) logical;
		for(int i=0; i<clause.size(); i++)
			if(!(clause.get(i) instanceof Precondition))
				return false;
		return true;
	}
	
	private static final boolean isDNF(Logical logical) {
		if(!(logical instanceof Disjunction))
			return false;
		Disjunction<?> disjunction = (Disjunction<?>) logical;
		for(int i=0; i<disjunction.size(); i++)
			if(!isClause(disjunction.get(i)))
				return false;
		return true;
	}
	
	/**
	 * This method is used to create all {@link Range ranges} in the graph
	 * so that subclasses of {@link HeuristicGraph} have the option to
	 * return subclasses of {@link Range} to modify the graph's behavior. By
	 * default, this method calls {@link #makeNumericRange()} for all {@link
	 * edu.uky.cs.nil.sabre.logic.Typed#isNumber() numeric expressions}, and
	 * {@link #makeNominalRange()} for all other expressions.
	 * 
	 * @param expression the logical expression whose set of values will be
	 * represented by the range
	 * @return a range object representing the possible values of the logical
	 * expression
	 */
	protected Range makeRange(Expression expression) {
		if(expression.isNumber())
			return makeNumericRange();
		else
			return makeNominalRange();
	}
	
	/**
	 * This method is used to create all nominal {@link Range ranges} (that is,
	 * range objects that hold a set of discrete {@link Value values}) so that
	 * subclasses of {@link HeuristicGraph} have the option to return
	 * subclasses of {@link Range} to modify the graph's behavior. By default,
	 * this method returns a new {@link ArrayRange}.
	 * 
	 * @return a range object representing the possible values of a logical
	 * expression that has discrete nominal values
	 */
	protected Range makeNominalRange() {
		return new ArrayRange();
	}
	
	/**
	 * This method is used to create all numeric {@link Range ranges} (that is,
	 * range objects that hold a potentially infinite set of {@link
	 * edu.uky.cs.nil.sabre.logic.Numeric numeric values}) so that subclasses
	 * of {@link HeuristicGraph} have the option to return subclasses of {@link
	 * DisjunctionNode} to modify the graph's behavior. By default, this method
	 * returns a new {@link InfiniteSpan}.
	 * 
	 * @return a range object representing the possible values of a logical
	 * expression that has discrete nominal values
	 */
	protected Range makeNumericRange() {
		return new InfiniteSpan();
	}
	
	/**
	 * Returns the {@link ConstantNode constant node} associated with the given
	 * {@link Value logical value}. If the node does not exist, it will be
	 * created.
	 * 
	 * @param value the logical value to represent in the graph
	 * @return a constant node representing that value
	 */
	public ConstantNode getConstant(Value value) {
		if(value.equals(False.FALSE))
			return falseValueNode;
		else if(value.equals(True.TRUE))
			return trueValueNode;
		ConstantNode node = (ConstantNode) nodes.get(value);
		if(node == null)
			node = makeConstantNode(value);
		return node;
	}
	
	/**
	 * This method is used to create all {@link ConstantNode constant nodes} so
	 * that subclasses of {@link HeuristicGraph} have the option to return
	 * subclasses of {@link ConstantNode} to modify the graph's behavior.
	 * 
	 * @param value the logical value to represent in the graph
	 * @return a new constant node
	 */
	protected ConstantNode makeConstantNode(Value value) {
		return new ConstantNode(this, value);
	}
	
	/**
	 * Returns the {@link FluentNode fluent node} associated with the given
	 * {@link Fluent fluent}. If the node does not exist, it will be created.
	 * 
	 * @param fluent the fluent to represent in the graph
	 * @return a fluent node representing the fluent
	 * @throws edu.uky.cs.nil.sabre.FormatException if the fluent is not ground
	 */
	public FluentNode getFluent(Fluent fluent) {
		if(fluent.isNumber())
			return getNumericFluent(fluent);
		else
			return getNominalFluent(fluent);
	}
	
	/**
	 * Returns the {@link NominalFluentNode fluent node} associated with the
	 * given nominal fluent (that is, any fluent which not {@link
	 * edu.uky.cs.nil.sabre.logic.Typed#isNumber() numeric}). If the node does
	 * not exist, it will be created.
	 * 
	 * @param fluent the nominal fluent to represent in the graph
	 * @return the fluent node representing the fluent
	 * @throws edu.uky.cs.nil.sabre.FormatException if the fluent is not ground
	 * @throws edu.uky.cs.nil.sabre.FormatException if the fluent is numeric
	 */
	public NominalFluentNode getNominalFluent(Fluent fluent) {
		NominalFluentNode node = (NominalFluentNode) nodes.get(fluent);
		if(node == null)
			node = makeNominalFluentNode(fluent);
		return node;
	}
	
	/**
	 * This method is used to create all {@link NominalFluentNode nominal
	 * fluent nodes} so that subclasses of {@link HeuristicGraph} have the
	 * option to return subclasses of {@link NominalFluentNode} to modify the
	 * graph's behavior.
	 * 
	 * @param fluent the nominal fluent to represent in the graph
	 * @return a new nominal fluent node
	 */
	protected NominalFluentNode makeNominalFluentNode(Fluent fluent) {
		return new NominalFluentNode(this, fluent);
	}
	
	/**
	 * Returns the {@link NumericFluentNode fluent node} associated with the
	 * given {@link edu.uky.cs.nil.sabre.logic.Typed#isNumber() numeric}
	 * fluent. If the node does not exist, it will be created.
	 * 
	 * @param fluent the numeric fluent to represent in the graph
	 * @return the fluent node representing the fluent
	 * @throws edu.uky.cs.nil.sabre.FormatException if the fluent is not ground
	 * @throws edu.uky.cs.nil.sabre.FormatException if the fluent is not
	 * numeric
	 */
	public NumericFluentNode getNumericFluent(Fluent fluent) {
		NumericFluentNode node = (NumericFluentNode) nodes.get(fluent);
		if(node == null)
			node = makeNumericFluentNode(fluent);
		return node;
	}
	
	/**
	 * This method is used to create all {@link NumericFluentNode numeric
	 * fluent nodes} so that subclasses of {@link HeuristicGraph} have the
	 * option to return subclasses of {@link NumericFluentNode} to modify the
	 * graph's behavior.
	 * 
	 * @param fluent the numeric fluent to represent in the graph
	 * @return a new numeric fluent node
	 */
	protected NumericFluentNode makeNumericFluentNode(Fluent fluent) {
		return new NumericFluentNode(this, fluent);
	}
	
	/**
	 * Returns the {@link ArithmeticNode arithmetic node} associated with the
	 * given {@link Arithmetic arithmetic expression}. If the node does not
	 * exist, it will be created.
	 * 
	 * @param arithmetic the arithmetic expression to represent in the graph
	 * @return the arithmetic node representing the arithmetic expression
	 * @throws edu.uky.cs.nil.sabre.FormatException if the arithmetic
	 * expression is not ground
	 */
	public ArithmeticNode getArithmetic(Arithmetic arithmetic) {
		ArithmeticNode node = (ArithmeticNode) nodes.get(arithmetic);
		if(node == null)
			node = makeArithmeticNode(arithmetic);
		return node;
	}
	
	/**
	 * This method is used to create all {@link ArithmeticNode arithmetic
	 * nodes} so that subclasses of {@link HeuristicGraph} have the option to
	 * return subclasses of {@link ArithmeticNode} to modify the graph's
	 * behavior.
	 * 
	 * @param arithmetic the arithmetic expression to represent in the graph
	 * @return a new arithmetic node
	 */
	protected ArithmeticNode makeArithmeticNode(Arithmetic arithmetic) {
		return new ArithmeticNode(this, arithmetic);
	}
	
	/**
	 * Defines the {@link CostSet#getCost(Value) cost} of a value of an {@link
	 * ArithmeticNode arithmetic node} based on the values and costs of the
	 * left and right sides of {@link ArithmeticNode#label the node's
	 * arithmetic expression}.
	 * 
	 * @param node the arithmetic node for which the cost of a value is being
	 * calculated
	 * @param leftValue the value of the left side of the arithmetic expression
	 * @param leftCost the cost of the value on the left side
	 * @param rightValue the value of the right side of the arithmetic
	 * expression
	 * @param rightCost the cost of the value on the right side
	 * @return the cost of the value that results from evaluating the node's
	 * arithmetic expression with the given left and right side values
	 */
	protected abstract double cost(ArithmeticNode node, Value leftValue, double leftCost, Value rightValue, double rightCost);
	
	/**
	 * Returns the {@link PreconditionNode precondition nodes} associated with
	 * the given {@link Precondition atomic precondition}. If the node does not
	 * exist, it will be created.
	 * 
	 * @param precondition the atomic precondition to represent in the graph
	 * @return the precondition node representing the precondition
	 * @throws edu.uky.cs.nil.sabre.FormatException if the precondition is not
	 * ground
	 */
	public PreconditionNode getPrecondition(Precondition precondition) {
		PreconditionNode node = (PreconditionNode) nodes.get(precondition);
		if(node == null)
			node = makePreconditionNode(precondition);
		return node;
	}
	
	/**
	 * This method is used to create all {@link PreconditionNode precondition
	 * nodes} so that subclasses of {@link HeuristicGraph} have the option to
	 * return subclasses of {@link PreconditionNode} to modify the graph's
	 * behavior.
	 * 
	 * @param precondition the precondition to represent in the graph
	 * @return a new precondition node
	 */
	protected PreconditionNode makePreconditionNode(Precondition precondition) {
		return new PreconditionNode(this, precondition);
	}
	
	/**
	 * Defines the {@link CostNode#getCost() cost} of a {@link PreconditionNode
	 * precondition node} being {@link True#TRUE true} based on the values and
	 * costs of the left and right sides of its {@link PreconditionNode#label
	 * comparison}. This method will only be called with values that make the
	 * precondition {@link Expression#evaluate(State) evaluate} to true.
	 * 
	 * @param node the precondition node for which the cost of being true is
	 * being calculated
	 * @param leftValue the value on the left side of the comparison
	 * @param leftCost the cost of the value on the left
	 * @param rightValue the value on the right side of the comparison
	 * @param rightCost the cost of the value on the right
	 * @return the cost of the precondition being true with the given left
	 * and right values
	 */
	protected abstract double cost(PreconditionNode node, Value leftValue, double leftCost, Value rightValue, double rightCost);
	
	/**
	 * Returns the {@link ClauseNode clause node} associated with the given
	 * {@link Clause conjunctive clause}. If the node does not exist, it will
	 * be created.
	 * 
	 * @param clause the conjunctive clause to represent in the graph
	 * @return the clause node representing the clause
	 * @throws edu.uky.cs.nil.sabre.FormatException if the clause is not ground
	 */
	public ClauseNode getClause(Clause<Precondition> clause) {
		ClauseNode node = (ClauseNode) nodes.get(clause);
		if(node == null)
			node = makeClauseNode(clause);
		return node;
	}
	
	/**
	 * This method is used to create all {@link ClauseNode clause nodes} so
	 * that subclasses of {@link HeuristicGraph} have the option to
	 * return subclasses of {@link ClauseNode} to modify the graph's behavior.
	 * 
	 * @param clause the conjunctive clause to represent in the graph
	 * @return a new clause node
	 */
	protected ClauseNode makeClauseNode(Clause<Precondition> clause) {
		return new ClauseNode(this, clause);
	}
	
	/**
	 * Defines the {@link CostNode#getCost() cost} of a {@link ClauseNode
	 * clause node} being {@link True#TRUE true}. The {@link
	 * ClauseNode#preconditions preconditions} that make up the clause and
	 * their costs can be accessed from the node itself. This method will be
	 * called after {@link ClauseNode#notify(Node, Value, double)} has been
	 * called as many times as it has preconditions. Usually, this implies
	 * this method will not be called until all preconditions in the clause
	 * have a finite cost, but this cannot be guaranteed.
	 * 
	 * @param node the clause node for which the cost of bring true is being
	 * calculated
	 * @return the cost of a clause being true
	 */
	protected abstract double cost(ClauseNode node);
	
	/**
	 * Returns the {@link DisjunctionNode disjunction node} associated with the
	 * given {@link Expression#toPrecondition() disjunctive normal form
	 * expression}. If the node does not exist, it will be created.
	 * 
	 * @param disjunction the disjunctive normal form expression to represent
	 * in the graph
	 * @return the disjunction node representing the expression
	 * @throws edu.uky.cs.nil.sabre.FormatException if the disjunction is not
	 * ground
	 */
	public DisjunctionNode getDisjunction(Disjunction<Clause<Precondition>> disjunction) {
		DisjunctionNode node = (DisjunctionNode) nodes.get(disjunction);
		if(node == null)
			node = makeDisjunctionNode(disjunction);
		return node;
	}
	
	/**
	 * This method is used to create all {@link DisjunctionNode disjunction
	 * nodes} so that subclasses of {@link HeuristicGraph} have the option to
	 * return subclasses of {@link DisjunctionNode} to modify the graph's
	 * behavior.
	 * 
	 * @param disjunction the disjunctive normal form expression to represent
	 * in the graph
	 * @return a new disjunction node
	 */
	protected DisjunctionNode makeDisjunctionNode(Disjunction<Clause<Precondition>> disjunction) {
		return new DisjunctionNode(this, disjunction);
	}
	
	/**
	 * Returns the {@link EventNode event node} associated with the given
	 * {@link Event event}. If the node does not exist, it will be created.
	 * 
	 * @param event the event to represent in the graph
	 * @return the event node representing the event
	 * @throws edu.uky.cs.nil.sabre.FormatException if the event is not ground
	 */
	public EventNode getEvent(Event event) {
		if(event instanceof Action)
			return getAction((Action) event);
		else
			return getTrigger((Trigger) event);
	}
	
	/**
	 * Returns the {@link ActionNode action node} associated with the given
	 * {@link Action action}. If the node does not exist, it will be created.
	 * 
	 * @param action the action to represent in the graph
	 * @return the action node representing the action
	 * @throws edu.uky.cs.nil.sabre.FormatException if the action is not ground
	 */
	public ActionNode getAction(Action action) {
		ActionNode node = (ActionNode) nodes.get(action);
		if(node == null)
			node = makeActionNode(action);
		return node;
	}
	
	/**
	 * This method is used to create all {@link ActionNode action nodes} so
	 * that subclasses of {@link HeuristicGraph} have the option to return
	 * subclasses of {@link ActionNode} to modify the graph's behavior.
	 * 
	 * @param action the action to represent in the graph
	 * @return a new action node
	 */
	protected ActionNode makeActionNode(Action action) {
		return new ActionNode(this, action);
	}
	
	/**
	 * Returns the {@link TriggerNode trigger node} associated with the given
	 * {@link Trigger trigger}. If the node does not exist, it will be created.
	 * 
	 * @param trigger the trigger to represent in the graph
	 * @return the node representing the trigger
	 * @throws edu.uky.cs.nil.sabre.FormatException if the trigger is not
	 * ground
	 */
	public TriggerNode getTrigger(Trigger trigger) {
		TriggerNode node = (TriggerNode) nodes.get(trigger);
		if(node == null)
			node = makeTriggerNode(trigger);
		return node;
	}
	
	/**
	 * This method is used to create all {@link TriggerNode trigger nodes} so
	 * that subclasses of {@link HeuristicGraph} have the option to return
	 * subclasses of {@link TriggerNode} to modify the graph's behavior.
	 * 
	 * @param trigger the trigger to represent in the graph
	 * @return a new trigger node
	 */
	protected TriggerNode makeTriggerNode(Trigger trigger) {
		return new TriggerNode(this, trigger);
	}
	
	/**
	 * Returns the {@link EffectNode effect node} associated with the given
	 * {@link Effect atomic effect}. If the node does not exist, it will be
	 * created.
	 * 
	 * @param effect the effect to represent in the graph
	 * @return an effect node representing the effect
	 * @throws edu.uky.cs.nil.sabre.FormatException if the effect is not ground
	 */
	public EffectNode getEffect(Effect effect) {
		EffectNode node = (EffectNode) nodes.get(effect);
		if(node == null)
			node = makeEffectNode(effect);
		return node;
	}
	
	/**
	 * This method is used to create all {@link EffectNode effect nodes} so
	 * that subclasses of {@link HeuristicGraph} have the option to return
	 * subclasses of {@link EffectNode} to modify the graph's behavior.
	 * 
	 * @param effect the effect to represent in the graph
	 * @return a new effect node
	 */
	protected EffectNode makeEffectNode(Effect effect) {
		return new EffectNode(this, effect);
	}
	
	/**
	 * Defines the {@link CostNode#getCost() cost} of an {@link EffectNode
	 * effect node} being included in the graph, which means that at least
	 * one of {@link EffectNode#events the events that has this effect} has
	 * a finite cost and {@link EffectNode#condition the effect's condition}
	 * also has a finite cost. The events that have this effect and the
	 * condition can be accessed from the node itself. This method may be
	 * called before some event has a finite cost and before the condition
	 * has a finite cost.
	 * 
	 * @param node the effect node for which the cost of being true is being
	 * calculated
	 * @return the cost of the effect being true
	 */
	protected abstract double cost(EffectNode node);
	
	/**
	 * Defines the {@link CostSet#getCost(Value) cost} of {@link
	 * EffectNode#fluent an effect node's fluent} having a given {@link Value
	 * value} from the {@link EffectNode#value right hand side of an effect
	 * node's assignment} based on the value and cost of the right hand side.
	 * 
	 * @param node the effect node which is assigning the value to its fluent
	 * @param value the value being assigned to the fluent
	 * @param cost the cost of the value being assigned to the fluent
	 * @return the cost of the fluent having that value
	 */
	protected abstract double cost(EffectNode node, Value value, double cost);
	
	/**
	 * Returns the {@link Character characters} which have {@link UtilityNode
	 * utility nodes} represented in the graph. The resulting collection will
	 * not include {@code null}, even if {@link #getUtility() the author's
	 * utility} is represented.
	 * 
	 * @return a collection of characters whose utilities are defined in the
	 * graph
	 */
	public Iterable<Character> getCharacters() {
		return utilities.keySet();
	}
	
	/**
	 * Returns the {@link UtilityNode utility node} representing {@link 
	 * edu.uky.cs.nil.sabre.Problem#utility the author's utility} if it {@link
	 * #setUtility(Character, Conditional) has been defined}.
	 * 
	 * @return a utility node representing the author's utility, or null if the
	 * author's utility has not been represented in this graph yet
	 */
	public UtilityNode getUtility() {
		return getUtility(null);
	}
	
	/**
	 * Returns the {@link UtilityNode utility node} representing {@link
	 * edu.uky.cs.nil.sabre.Problem#utilities an character's utility} if it
	 * {@link #setUtility(Character, Conditional) has been defined}.
	 * 
	 * @param character the character whose utility representation is desired
	 * @return a utility node representing the character's utility, or null if
	 * the character's utility has not been represented in this graph yet
	 */
	public UtilityNode getUtility(Character character) {
		if(character == null)
			return utility;
		else
			return utilities.get(character);
	}
	
	/**
	 * Defines a {@link UtilityNode utility node} for the given character using
	 * the given utility expression. To define the author's utility, the
	 * character argument should be {@code null}. This method should only be
	 * called once per character; calling it more than once per character will
	 * cause an {@link IllegalStateException}. To access an already defined
	 * utility node, use {@link #getUtility()}.
	 * 
	 * @param character the character whose utility is being represented
	 * @param utility a numeric expression which can be evaluated to determine
	 * how satisfied the character is with a given situation
	 * @return a utility node for that character representing the utility
	 * expression
	 * @throws IllegalStateException if a utility node has already been defined
	 * for the character
	 */
	public UtilityNode setUtility(Character character, Conditional<Disjunction<Clause<Precondition>>> utility) {
		UtilityNode node = getUtility(character);
		if(node != null)
			throw Exceptions.utilityNodeAlreadyDefined(character);
		node = makeUtilityNode(character, utility);
		if(character == null)
			this.utility = node;
		else
			utilities.put(character, node);
		return node;
	}
	
	/**
	 * This method is used to create all {@link UtilityNode utility nodes} so
	 * that subclasses of {@link HeuristicGraph} have the option to return
	 * subclasses of {@link UtilityNode} to modify the graph's behavior.
	 * 
	 * @param character the character whose utility is being defined
	 * @param conditional a numeric expression which can be evaluated to
	 * determine how satisfied the character is with a given situation
	 * @return a new utility node
	 */
	protected UtilityNode makeUtilityNode(Character character, Conditional<Disjunction<Clause<Precondition>>> conditional) {
		return new UtilityNode(this, character, conditional);
	}

	/**
	 * This method is used to create all {@link GoalNode goal nodes} so that
	 * subclasses of {@link HeuristicGraph} have the option to return
	 * subclasses of {@link GoalNode} to modify the graph's behavior.
	 * 
	 * @param utility the utility node for which this goal represents a branch
	 * @param label a conditional expression where the first condition and
	 * branch (and only the first condition and branch) will be used as the
	 * condition and value expressions for one branch of a utility node
	 * @return a goal node for the given utility node with the given condition
	 */
	protected GoalNode makeGoalNode(UtilityNode utility, Conditional<Disjunction<Clause<Precondition>>> label) {
		return new GoalNode(utility, label);
	}
	
	/**
	 * Defines the {@link CostSet#getCost(Value) cost} of a {@link UtilityNode
	 * utility node} having a given {@link Value value} based on the cost of
	 * the {@link GoalNode goal condition} that was met and the value and cost
	 * of the {@link GoalNode#value branch of the utility expression that was
	 * evaluated}. A utility node has {@link UtilityNode#goals has many goals},
	 * which represent the conditions and branches of a {@link Conditional
	 * conditional utility expression}. When a {@link GoalNode#condition goal
	 * node's condition} becomes finite, it becomes possible for {@link
	 * GoalNode#utility the goal's utility node} to take on every value that
	 * the {@link GoalNode#value goal node's value expression can have}.
	 * This method will only be called after the goal node has a finite {@link
	 * CostNode#getCost() cost}.
	 * 
	 * @param node the goal node which represents a {@link GoalNode#condition
	 * condition} of the utility expression that has a finite cost
	 * @param value a value that the {@link GoalNode#value goal node's value
	 * expression} can have
	 * @param cost the cost of the goal node's value expression having that
	 * value
	 * @return the cost of the goal node's utility node having that value
	 */
	protected abstract double cost(GoalNode node, Value value, double cost);
	
	/**
	 * {@link Node#reset() Resets} event node in the graph so that all costs
	 * are {@link Double#POSITIVE_INFINITY positive infinity}, except for costs
	 * which are trivially 0 (the {@link ConstantNode constant node}
	 * representing true and the {@link ClauseNode clause node} representing
	 * {@link Clause#EMPTY the empty clause}).
	 */
	public void reset() {
		Node current = null;
		Node next = firstToReset;
		while(next != lastToReset) {
			current = next;
			next = current.nextToReset;
			current.reset();
		}
		firstToReset = lastToReset;
		emptyClauseNode.setCost(0);
	}
	
	/**
	 * Returns an array of all finite costs that appear anywhere in the graph
	 * (that is, the cost of any {@link CostNode cost nodes} with a finite
	 * {@link CostNode#getCost() cost} and the costs of any {@link CostSet cost
	 * sets} that have {@link CostSet#getCost(Value) values with finite
	 * costs}).
	 * 
	 * @return a sorted array of finite costs that appear in this graph
	 */
	public double[] getLevels() {
		HashSet<Double> costs = new HashSet<>();
		for(FluentNode fluent : fluents)
			for(CostSet.Entry entry : fluent)
				costs.add(entry.cost);
		for(ActionNode action : actions)
			costs.add(action.getCost());
		costs.remove(Double.POSITIVE_INFINITY);
		double[] levels = new double[costs.size()];
		int index = 0;
		for(Double cost : costs)
			levels[index++] = cost;
		Arrays.sort(levels);
		return levels;
	}
	
	/**
	 * {@link #reset() Resets} all nodes in the graph and then initializes all
	 * {@link FluentNode fluent nodes} by setting the cost of the value that
	 * fluent has in the given state to 0. After initialization, {@link
	 * CostNode nodes that represent Boolean propositions} will have a cost of
	 * 0 if they are true in the given state or {@link Double#POSITIVE_INFINITY
	 * positive infinity} otherwise. Numeric nodes like {@link ArithmeticNode
	 * arithmetic nodes} and {@link UtilityNode utility nodes} will have the
	 * cost of their value in the given state set of 0, and the cost of all
	 * other values set to positive infinity.
	 * 
	 * @param state the state that defines the initial value of each fluent
	 */
	public void initialize(State state) {
		reset();
		for(int i=0; i<fluents.size(); i++) {
			FluentNode node = fluents.get(i);
			node.setCost(state.getValue(node.label), 0);
		}
	}
	
	/**
	 * Sets the cost of any {@link ActionNode action node} which currently
	 * has an infinite {@link CostNode#getCost() cost} but whose {@link
	 * EventNode#precondition precondition} has a finite {@link
	 * CostNode#getCost() cost} to the cost of its precondition plus one.
	 * This method returns true if at least one new action was assigned a
	 * finite cost; otherwise, this method returns false. Once this method
	 * returns false, the heuristic graph is said to have leveled off.
	 * 
	 * @return true if at least one action that previously has an infinite cost
	 * was assigned a finite cost, or false if the graph has leveled off
	 */
	public boolean extend() {
		if(next.size() == 0)
			return false;
		else {
			extend(next.size() - 1);
			return true;
		}
	}
	
	private final void extend(int index) {
		if(index >= 0) {
			ActionNode node = next.get(index);
			extend(index - 1);
			node.setCost(node.precondition.getCost() + 1);
		}
		else
			next.clear();
	}
}