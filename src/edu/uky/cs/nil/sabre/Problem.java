package edu.uky.cs.nil.sabre;

import java.io.Serializable;
import java.util.LinkedHashSet;

import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.HashSubstitution;
import edu.uky.cs.nil.sabre.logic.Unknown;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * Represents all the definitions needed to define a full planning task.
 * A problem defines:
 * <ul>
 * <li>a {@link #name name}</li>
 * <li>a list of {@link Type types} and {@link Entity entities} defined in a
 * {@link Universe universe}</li>
 * <li>a list of {@link Fluent fluents}</li>
 * <li>a list of {@link Event events}, as well as smaller lists of {@link
 * Action actions} and {@link Trigger triggers}</li>
 * <li>a {@link Expression logical expression} defining the initial state</li>
 * <li>an {@link #utility author utility}</li>
 * <li>a {@link #utilities character utility} for each character</li>
 * </ul>
 * Problem objects usually should not be constructed directly. They should be
 * defined using a {@link ProblemBuilder} to ensure correct formatting.
 * <p>
 * The fluents and events defined in a problem can be templates, meaning their
 * signatures contain {@link edu.uky.cs.nil.sabre.logic.Variable variables}. A
 * fluent or event template implicitly defines all possible {@link
 * Expression#isGround() ground} versions of that template. Every ground fluent
 * or event must have a unique {@link Signature signature}; A
 * {@link ProblemBuilder problem builder} enforces this by preventing two
 * templates from ambiguously defining two ground objects that would be
 * different but have the same signature.
 * 
 * @author Stephen G. Ware
 */
public class Problem implements Serializable {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The name of the problem */
	public final String name;
	
	/**
	 * The universe defining the problem's {@link Type types} and {@link Entity
	 * entities}
	 */
	public final Universe universe;
	
	/** The {@link Fluent fluents} tracked in every state of the problem */
	public final ImmutableSet<Fluent> fluents;
	
	/** A set of all {@link Event events} that can occur */
	public final ImmutableSet<Event> events;
	
	/**
	 * All the {@link Action actions} defined in this problem (a subset of
	 * {@link #events events})
	 */
	public final ImmutableSet<Action> actions;
	
	/**
	 * All the {@link Trigger triggers} defined in this problem (a subset of
	 * {@link #events events})
	 */
	public final ImmutableSet<Trigger> triggers;
	
	/**
	 * A {@link Expression logical expression} representing the initial state
	 */
	public final Expression initial;
	
	/** The author's utility {@link Expression expression} */
	public final Expression utility;
	
	/**
	 * A {@link Mapping mapping} which returns a utility {@link Expression
	 * expression} for each {@link Character character}
	 */
	public final Mapping<Expression> utilities;
	
	/** The comment associated with this problem in its definition */
	public final String comment;
	
	/**
	 * Constructs a new problem.
	 * 
	 * @param name the problem's name
	 * @param universe the universe of types and entities used in this problem
	 * @param fluents all fluents defined in this problem
	 * @param actions all actions defined in this problem
	 * @param triggers all triggers defined in this problem
	 * @param initial an expression describing the initial state, which sets
	 * values of all fluents
	 * @param utility an expression that can be evaluated to determine how
	 * satisfied the author is with a given state
	 * @param utilities a mapping of expressions that can be evaluated to
	 * determine how satisfied some character is with a given state
	 * @param comment a text comment associated with the problem
	 */
	protected Problem(
		String name,
		Universe universe,
		ImmutableSet<Fluent> fluents,
		ImmutableSet<Action> actions,
		ImmutableSet<Trigger> triggers,
		Expression initial,
		Expression utility,
		Mapping<Expression> utilities,
		String comment
	) {
		this.name = name;
		this.universe = universe;
		this.fluents = fluents;
		LinkedHashSet<Event> events = new LinkedHashSet<>();
		for(Action action : actions)
			events.add(action);
		for(Trigger axiom : triggers)
			events.add(axiom);
		this.events = new ImmutableSet<>(events);
		this.actions = actions;
		this.triggers = triggers;
		this.initial = initial;
		this.utility = utility;
		this.utilities = utilities;
		this.comment = comment;
	}
	
	/**
	 * Constructs a new problem using the definitions specified in a {@link
	 * ProblemBuilder}.
	 * 
	 * @param builder the problem builder
	 */
	public Problem(ProblemBuilder builder) {
		this(
			builder.getName(),
			builder.getUniverse(),
			builder.getFluents(),
			builder.getActions(),
			builder.getTriggers(),
			builder.getInitialState(),
			builder.getUtility(),
			builder.getUtilities(),
			builder.getComment()
		);
	}
	
	@Override
	public String toString() {
		return "[Problem \"" + name + "\": " + fluents.size() + " fluents; " + actions.size() + " actions; " + triggers.size() + " triggers; " + (1 + universe.characters.size()) + " utilities]";
	}
	
	/**
	 * Returns a fluent defined in this problem based on the given signature.
	 * The signature does not need to match a fluent explicitly defined in this
	 * problem; it can match a fluent implicitly defined by a template.
	 * Any types and entities in the signature will be replaced with types and
	 * entities of the same name in this problem's universe, if they exist.
	 * 
	 * @param signature the signature of the fluent to find
	 * @return the defined fluent
	 * @throws FormatException if no fluent matching this signature is defined
	 */
	public Fluent getFluent(Signature signature) {
		return find("Fluent", signature, fluents);
	}
	
	/**
	 * Returns an event defined in this problem based on the given signature.
	 * The signature does not need to match an event explicitly defined in this
	 * problem; it can match an event implicitly defined by a template.
	 * Any types and entities in the signature will be replaced with types and
	 * entities of the same name in this problem's universe, if they exist.
	 * 
	 * @param signature the signature of the event to find
	 * @return the defined event
	 * @throws FormatException if no event matching this signature is defined
	 */
	public Event getEvent(Signature signature) {
		return find("Event", signature, events);
	}
	
	/**
	 * Returns an action defined in this problem based on the given signature.
	 * The signature does not need to match an action explicitly defined in this
	 * problem; it can match an action implicitly defined by a template.
	 * Any types and entities in the signature will be replaced with types and
	 * entities of the same name in this problem's universe, if they exist.
	 * 
	 * @param signature the signature of the action to find
	 * @return the defined action
	 * @throws FormatException if no action matching this signature is defined
	 */
	public Action getAction(Signature signature) {
		return find("Action", signature, actions);
	}
	
	/**
	 * Returns a trigger defined in this problem based on the given signature.
	 * The signature does not need to match a trigger explicitly defined in
	 * this problem; it can match an action implicitly defined by a template.
	 * Any types and entities in the signature will be replaced with types and
	 * entities of the same name in this problem's universe, if they exist.
	 * 
	 * @param signature the signature of the trigger to find
	 * @return the defined trigger
	 * @throws FormatException if no trigger matching this signature is defined
	 */
	public Trigger getTrigger(Signature signature) {
		return find("Trigger", signature, triggers);
	}
	
	@SuppressWarnings("unchecked")
	private final <S extends Signed> S find(String type, Signature signature, ImmutableSet<S> defined) {
		signature = (Signature) signature.substitute(universe);
		for(S existing : defined) {
			if(existing.getSignature().covers(signature)) {
				existing = (S) existing.distinguish(signature);
				return (S) existing.substitute(new HashSubstitution(existing.getSignature().arguments, signature.arguments));
			}
		}
		throw Exceptions.notDefined(type, signature.toString());
	}
	
	/**
	 * Returns the value of the {@link #utility author utility expression} in a
	 * given state.
	 * 
	 * @param state the state in which to evaluate the author's utility
	 * @return the author's utility in that state
	 */
	public Value getUtility(State state) {
		return getUtility(utility, state);
	}
	
	/**
	 * Returns the value of a {@link Character character's} {@link #utilities
	 * utility expression} in a given state.
	 * 
	 * @param character the character whose utility is desired
	 * @param state the state in which to evaluate the character's utility
	 * @return the character's utility in that state
	 */
	public Value getUtility(Character character, State state) {
		if(character == null)
			return getUtility(state);
		else
			return getUtility(utilities.get(character), state);
	}
	
	private static final Number getUtility(Expression expression, State state) {
		Value value = expression.evaluate(state);
		if(value.equals(Unknown.UNKNOWN))
			return Number.ZERO;
		else
			return (Number) value;
	}
}