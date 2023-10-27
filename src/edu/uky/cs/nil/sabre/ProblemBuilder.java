package edu.uky.cs.nil.sabre;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;

import edu.uky.cs.nil.sabre.comp.CompiledMapping;
import edu.uky.cs.nil.sabre.logic.Conditional;
import edu.uky.cs.nil.sabre.logic.Conjunction;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.HashSubstitution;
import edu.uky.cs.nil.sabre.logic.Logical;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.logic.Substitution;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.logic.Variable;
import edu.uky.cs.nil.sabre.util.ImmutableSet;
import edu.uky.cs.nil.sabre.util.UniqueMap;

/**
 * A problem builder assists in defining a {@link Problem planning problem} by
 * enforcing requirements on the various parts needed to define a problem.
 * 
 * @author Stephen G. Ware
 */
public class ProblemBuilder {
	
	private String name;
	private final Universe universe;
	private final LinkedHashSet<Fluent> fluents = new LinkedHashSet<>();
	private final LinkedHashSet<Event> events = new LinkedHashSet<>();
	private Expression initial = True.TRUE;
	private Expression utility = Number.ZERO;
	private final UniqueMap<Character, Expression> utilities = new UniqueMap<>();
	private String comment = Settings.DEFAULT_PROBLEM_COMMENT;
	
	/**
	 * Constructs a new problem builder that will begin with all the
	 * definitions in the provided problem and can be used to define
	 * a new problem which is an extension or modification of that
	 * problem.
	 * 
	 * @param problem a pre-defined problem
	 */
	public ProblemBuilder(Problem problem) {
		this.name = problem.name;
		this.universe = problem.universe;
		for(Fluent fluent : problem.fluents)
			this.fluents.add(fluent);
		for(Event event : problem.events)
			this.events.add(event);
		this.initial = problem.initial;
		this.utility = problem.utility;
		for(Character character : problem.universe.characters)
			this.utilities.put(character, problem.utilities.get(character));
		this.comment = problem.comment;
	}
	
	/**
	 * Constructs a new empty problem builder with a given name and universe.
	 * All parts of the problem will be initialized to default or empty values.
	 * 
	 * @param name the problem name
	 * @param universe the universe of types and entities
	 */
	public ProblemBuilder(String name, Universe universe) {
		this.name = name;
		this.universe = universe;
		for(Character character : universe.characters)
			utilities.put(character, Number.ZERO);
	}
	
	/**
	 * Returns the current problem name.
	 * 
	 * @return the name
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Sets the name of the problem.
	 * 
	 * @param name the new name
	 * @return the name
	 */
	public String setName(String name) {
		this.name = name;
		return this.name;
	}
	
	/**
	 * Returns the universe of types and entities being used in the current
	 * problem.
	 * 
	 * @return the universe
	 */
	public Universe getUniverse() {
		return universe;
	}
	
	/**
	 * Returns a fluent defined in the current problem based on the given
	 * signature. The signature does not need to match a fluent explicitly
	 * defined in the current problem; it can match a fluent implicitly defined
	 * by a template. Any types and entities in the signature will be replaced
	 * with types and entities of the same name in the current problem's
	 * universe, if they exist.
	 * 
	 * @param signature the signature of the fluent to find
	 * @return the defined fluent
	 * @throws FormatException if no fluent matching this signature is defined
	 */
	public Fluent getFluent(Signature signature) {
		return find("Fluent", signature, fluents);
	}
	
	/**
	 * Returns a set of all currently defined fluents.
	 * 
	 * @return the fluents
	 */
	public ImmutableSet<Fluent> getFluents() {
		return new ImmutableSet<>(fluents);
	}
	
	/**
	 * Defines a new fluent. If the signature contains {@link Variable
	 * variables}, this will implicitly define all possible ground versions of
	 * the fluent.
	 * 
	 * @param signature the new fluent's signature
	 * @param type the type of value the new fluent can be assigned
	 * @param comment a comment to associate with the fluent
	 * @return the newly defined fluent
	 * @throws FormatException if the signature matches an already defined
	 * fluent or if it implicitly defines a fluent already defined
	 */
	public Fluent defineFluent(Signature signature, Type type, String comment) {
		signature = localize(signature);
		type = localize(type);
		Signature overlap = findOverlap(signature, fluents);
		if(overlap != null)
			throw Exceptions.makesAmbiguous("fluent", signature, overlap);
		Fluent fluent = new Fluent(signature, type, comment);
		fluents.add(fluent);
		return fluent;
	}
	
	/**
	 * Returns an event defined in the current problem based on the given
	 * signature. The signature does not need to match an event explicitly
	 * defined in the current problem; it can match an event implicitly defined
	 * by a template. Any types and entities in the signature will be replaced
	 * with types and entities of the same name in the current problem's
	 * universe, if they exist.
	 * 
	 * @param signature the signature of the event to find
	 * @return the defined event
	 * @throws FormatException if no event matching this signature is defined
	 */
	public Event getEvent(Signature signature) {
		return find("Event", signature, events);
	}
	
	/**
	 * Returns an action defined in the current problem based on the given
	 * signature. The signature does not need to match an action explicitly
	 * defined in the current problem; it can match an action implicitly defined
	 * by a template. Any types and entities in the signature will be replaced
	 * with types and entities of the same name in the current problem's
	 * universe, if they exist.
	 * 
	 * @param signature the signature of the action to find
	 * @return the defined action
	 * @throws FormatException if no action matching this signature is defined
	 */
	public Action getAction(Signature signature) {
		return (Action) find("Action", signature, events);
	}
	
	/**
	 * Returns a set of all currently defined actions.
	 * 
	 * @return the actions
	 */
	public ImmutableSet<Action> getActions() {
		return Utilities.collect(Action.class, events);
	}
	
	/**
	 * Defines a new action. If the signature contains {@link Variable
	 * variables}, this will implicitly define all possible ground versions of
	 * the action.
	 * 
	 * @param signature the new action's signature
	 * @param precondition the new action's precondition
	 * @param effect the new action's effect
	 * @param consenting the new action's set of consenting characters
	 * @param observing the new action's observation function
	 * @param comment a comment to associate with the action
	 * @return the newly defined action
	 * @throws FormatException if the signature matches an already defined
	 * action or if it implicitly defines an action already defined
	 */
	@SuppressWarnings("unchecked")
	public Action defineAction(Signature signature, Expression precondition, Expression effect, Iterable<? extends Parameter> consenting, Mapping<Expression> observing, String comment) {
		signature = localize(signature);
		precondition = localize(precondition);
		effect = localize(effect);
		ImmutableSet<Parameter> _consenting = (ImmutableSet<Parameter>) new ImmutableSet<>(consenting).apply(p -> localize((Parameter) p));
		observing = localize(observing);
		Signature overlap = findOverlap(signature, events);
		if(overlap != null)
			throw Exceptions.makesAmbiguous("action", signature, overlap);
		Action action = new Action(signature, precondition, effect, _consenting, observing, comment);
		events.add(action);
		return action;
	}
	
	/**
	 * Returns a trigger defined in the current problem based on the given
	 * signature. The signature does not need to match a trigger explicitly
	 * defined in the current problem; it can match a trigger implicitly
	 * defined by a template. Any types and entities in the signature will be
	 * replaced with types and entities of the same name in the current
	 * problem's universe, if they exist.
	 * 
	 * @param signature the signature of the trigger to find
	 * @return the defined trigger
	 * @throws FormatException if no trigger matching this signature is defined
	 */
	public Trigger getTrigger(Signature signature) {
		return (Trigger) find("Trigger", signature, events);
	}
	
	/**
	 * Returns a set of all currently defined triggers.
	 * 
	 * @return the triggers
	 */
	public ImmutableSet<Trigger> getTriggers() {
		return Utilities.collect(Trigger.class, events);
	}
	
	/**
	 * Defines a new trigger. If the signature contains {@link Variable
	 * variables}, this will implicitly define all possible ground versions of
	 * the trigger.
	 * 
	 * @param signature the new trigger's signature
	 * @param precondition the new trigger's precondition
	 * @param effect the new trigger's effect
	 * @param comment a comment to associate with the trigger
	 * @return the newly defined trigger
	 * @throws FormatException if the signature matches an already defined
	 * trigger or if it implicitly defines a trigger already defined
	 */
	public Trigger defineTrigger(Signature signature, Expression precondition, Expression effect, String comment) {
		signature = localize(signature);
		precondition = localize(precondition);
		effect = localize(effect);
		Signature overlap = findOverlap(signature, events);
		if(overlap != null)
			throw Exceptions.makesAmbiguous("trigger", signature, overlap);
		Trigger trigger = new Trigger(signature, precondition, effect, comment);
		events.add(trigger);
		return trigger;
	}
	
	/**
	 * Returns a {@link Expression logical expression} used to set the initial
	 * state, which should define the values of all fluents in a problem,
	 * including any character beliefs that do not match the true world state.
	 * 
	 * @return the expression that sets the initial state
	 */
	public Expression getInitialState() {
		return initial;
	}
	
	/**
	 * Adds an expression to the initial state of the problem. The expression
	 * is first {@link Expression#toEffect() converted to an effect}, then the
	 * new initial state expression becomes a conjunction of the current
	 * expression and the new expression.
	 * 
	 * @param expression the new expression to add to the initial state
	 * @return the new initial state expression
	 * @throws FormatException if the expression cannot be converted to an
	 * effect
	 */
	public Expression addToInitialState(Expression expression) {
		expression = localize(expression);
		expression.toEffect();
		ArrayList<Expression> arguments = new ArrayList<>();
		add(initial, arguments);
		add(expression, arguments);
		if(arguments.size() == 0)
			initial = True.TRUE;
		else if(arguments.size() == 1)
			initial = arguments.get(0);
		else
			initial = new Conjunction<>(arguments);
		return initial;
	}
	
	private static final void add(Expression argument, Collection<Expression> arguments) {
		if(argument.equals(True.TRUE))
			return;
		else if(argument instanceof Conjunction<?>)
			for(Expression a : (Conjunction<?>) argument)
				add(a, arguments);
		else
			arguments.add(argument);
	}
	
	/**
	 * Returns the currently defined author utility function.
	 * 
	 * @return the utility function
	 */
	public Expression getUtility() {
		return utility;
	}
	
	/**
	 * Sets the author utility function. Utility expressions should be of type
	 * {@code number}, but if an expression of type {@code boolean} is given,
	 * it will automatically be converted into a conditional numeric
	 * expression like so: {@code if expression then 1 else 0}.
	 * 
	 * @param expression the new author utility function
	 * @return the author utility function
	 */
	public Expression setUtility(Expression expression) {
		utility = toUtility(localize(expression));
		return utility;
	}
	
	/**
	 * Returns the currently defined utility function for a given character.
	 * 
	 * @param character the character whose utility function is desired
	 * @return the character's utility function
	 */
	public Expression getUtility(Character character) {
		return utilities.get(localize(character));
	}
	
	/**
	 * Returns a {@link Mapping mapping} representing the utility functions of
	 * all characters in the universe.
	 * 
	 * @return the mapping
	 */
	public Mapping<Expression> getUtilities() {
		Expression[] utilities = new Expression[universe.characters.size()];
		for(Character character : universe.characters)
			utilities[character.id] = this.utilities.get(character);
		return new CompiledMapping<>(utilities);
	}
	
	/**
	 * Sets the utility function for a given character. Utility expressions
	 * should be of type {@code number}, but if an expression of type {@code
	 * boolean} is given, it will automatically be converted into a conditional
	 * numeric expression like so: {@code if expression then 1 else 0}.
	 * 
	 * @param character the character whose utility is being defined
	 * @param expression the new utility function
	 * @return the utility function
	 */
	public Expression setUtility(Character character, Expression expression) {
		character = localize(character);
		utilities.put(character, toUtility(localize(expression)));
		return utilities.get(character);
	}
	
	private static final Expression toUtility(Expression expression) {
		if(expression.isBoolean())
			return new Conditional<>(expression, Number.ONE, Number.ZERO);
		else if(expression.isNumber())
			return expression;
		else
			throw Exceptions.utilityMustBePropositionOrNumeric(expression);
	}
	
	/**
	 * Returns the current comment associated with the problem.
	 * 
	 * @return the comment
	 */
	public String getComment() {
		return comment;
	}
	
	/**
	 * Sets the comment associated with the problem.
	 * 
	 * @param comment the new comment
	 * @return the comment
	 */
	public String setComment(String comment) {
		this.comment = comment;
		return this.comment;
	}
	
	@SuppressWarnings("unchecked")
	private final <L extends Logical> L localize(L logical) {
		return (L) logical.substitute(object -> {
			if(object instanceof Fluent) {
				Fluent fluent = (Fluent) object;
				Fluent replacement = getFluent(fluent.signature);
				for(int i=fluent.characters.size()-1; i>=0; i--)
					replacement = replacement.prepend(localize(fluent.characters.get(i)));
				return replacement;
			}
			else if(object instanceof Event)
				return getEvent(((Event) object).getSignature());
			else
				return universe.apply(object);
		});
	}
	
	@SuppressWarnings("unchecked")
	private final <S extends Signed> S find(String type, Signature signature, Iterable<S> defined) {
		signature = localize(signature);
		for(S existing : defined) {
			if(existing.getSignature().covers(signature)) {
				existing = (S) existing.distinguish(signature);
				return (S) existing.substitute(new HashSubstitution(existing.getSignature().arguments, signature.arguments));
			}
		}
		throw Exceptions.notDefined(type, signature.toString());
	}
	
	private final <S extends Signed> Signature findOverlap(Signature signature, Iterable<S> defined) {
		for(S existing : defined) {
			Signature overlap = findOverlap(signature, existing.getSignature());
			if(overlap != null)
				return overlap;
		}
		return null;
	}
	
	private final Signature findOverlap(Signature s1, Signature s2) {
		if(s1.name.equals(s2.name) && s1.arguments.size() == s2.arguments.size())
			return findOverlap((Signature) s1.distinguish(s2), s2, 0, new HashSubstitution());
		else
			return null;
	}
	
	private final Signature findOverlap(Signature s1, Signature s2, int index, HashSubstitution substitution) {
		if(index == s1.arguments.size())
			return (Signature) s1.substitute(substitution);
		else if(findOverlap(s1.arguments.get(index), s2.arguments.get(index), substitution))
			return findOverlap(s1, s2, index + 1, substitution);
		else
			return null;
	}
	
	private final boolean findOverlap(Parameter p1, Parameter p2, HashSubstitution substitution) {
		if(p1 instanceof Value && p2 instanceof Value)
			return p1.equals(p2);
		else if(p1 instanceof Value) {
			if(Substitution.canSubstitute(p2, p1, substitution)) {
				substitution.replace(p2, p1);
				return true;
			}
			else
				return false;
		}
		else if(p2 instanceof Value) {
			if(Substitution.canSubstitute(p1, p2, substitution)) {
				substitution.replace(p1, p2);
				return true;
			}
			else
				return false;
		}
		else {
			for(Value value : universe.getValues(((Variable) p1).type)) {
				if(universe.getValues(((Variable) p2).type).contains(value) && Substitution.canSubstitute(value, p1) && Substitution.canSubstitute(value, p2)) {
					substitution.replace(p1, value);
					substitution.replace(p2, value);
					return true;
				}
			}
			return false;
		}
	}
}
