package edu.uky.cs.nil.sabre;

import java.util.NoSuchElementException;

import edu.uky.cs.nil.sabre.io.DefaultParser;
import edu.uky.cs.nil.sabre.io.List;
import edu.uky.cs.nil.sabre.io.ParseException;
import edu.uky.cs.nil.sabre.io.Token;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.Logical;
import edu.uky.cs.nil.sabre.logic.Typed;
import edu.uky.cs.nil.sabre.logic.Variable;
import edu.uky.cs.nil.sabre.util.ImmutableList;
import edu.uky.cs.nil.sabre.util.MemoryBudget;
import edu.uky.cs.nil.sabre.util.Unique;

/**
 * All exceptions are generated from this utility class so that exception
 * messages can be kept in one place for easy comparison and modification.
 * 
 * @author Stephen G. Ware
 */
public class Exceptions {
	
	/**
	 * Thrown any time an index in a list, array, etc. is accessed which does
	 * not exist.
	 * 
	 * @param index the index
	 * @return an IndexOutOfBoundsException
	 */
	public static final IndexOutOfBoundsException indexOutOfBounds(int index) {
		return new IndexOutOfBoundsException("The index " + index + " does not exist in this collection.");
	}

	/**
	 * Thrown any time {@link java.util.Iterator#next()} is called on an {@link
	 * java.util.Iterator} that has no more elements.
	 * 
	 * @return a NoSuchElementException
	 */
	public static final NoSuchElementException iteratorOutOfElements() {
		return new NoSuchElementException("Iterator has no more elements.");
	}
	
	/**
	 * Thrown when a {@link Unique unique object's} {@link Object#hashCode()
	 * hash code} returns a negative number.
	 * 
	 * @param unique the unique object with a negative hash code
	 * @return an IllegalArgumentException
	 */
	public static final IllegalArgumentException uniqueHashCodesNotNegative(Unique unique) {
		return new IllegalArgumentException("Unique hash code for \"" + unique + "\" should not be negative.");
	}
	
	/**
	 * Thrown when a {@link MemoryBudget memory budget} fails to parse a string
	 * expressing an amount of memory.
	 * 
	 * @param string the string which cannot be parsed
	 * @return an IllegalArgumentException
	 */
	public static final IllegalArgumentException cannotParseMemory(String string) {
		return new IllegalArgumentException("The string \"" + string + "\" cannot be parsed as a quantity of memory.");
	}
	
	/**
	 * Thrown when a {@link MemoryBudget memory budget} is asked to allocate
	 * memory despite having 0 bytes left.
	 * 
	 * @param size the full size of the original memory budget
	 * @return an OutOfMemoryError
	 */
	public static final OutOfMemoryError memoryBudgetExceeded(long size) {
		return new OutOfMemoryError("All " + MemoryBudget.toString(size) + " of memory has been used.");
	}
	
	/**
	 * Thrown when a {@link Logical logical formula} should be ground (that is,
	 * should contain no variables) but is not.
	 * 
	 * @param logical a logical formula which is not ground
	 * @return a FormatException
	 */
	public static final FormatException mustBeGround(Logical logical) {
		return new FormatException("\"" + logical + "\" is not ground.");
	}
	
	/**
	 * Thrown when a {@link Typed typed logical formula} should be of a certain
	 * {@link Type type} but is not.
	 * 
	 * @param typed the formula which should be of a certain type
	 * @param type the type it should be
	 * @return a FormatException
	 */
	public static final FormatException mustBe(Typed typed, String type) {
		return new FormatException("\"" + typed + "\" must be of type \"" + type + "\".");
	}
	
	/**
	 * Thrown when a {@link Typed typed logical formula} should not be of a
	 * certain {@link Type type} but is.
	 * 
	 * @param typed the formula which should not be of a certain type
	 * @param type the type it should not be
	 * @return a FormatException
	 */
	public static final FormatException mustNotBe(Typed typed, String type) {
		return new FormatException("\"" + typed + "\" must not be of type \"" + type + "\".");
	}
	
	/**
	 * Thrown when a {@link Expression logical expression} should be {@link
	 * Expression#toValued() formatted as a value} but is not.
	 * 
	 * @param value the expression which should be formatted as a value
	 * @return a FormatException
	 */
	public static final FormatException mustByValued(Expression value) {
		return new FormatException("The expression \"" + value + "\" is not formatted as a value.");
	}
	
	/**
	 * Thrown when {@link Expression#negate() negating} a {@link Expression
	 * logical expression} which cannot be negated.
	 * 
	 * @param expression the expression that cannot be negated
	 * @return a FormatException
	 */
	public static final FormatException cannotNegate(Expression expression) {
		return new FormatException("The expression \"" + expression + "\" cannot be negated.");
	}
	
	/**
	 * Thrown when {@link Expression#toValued() converting a logical expression
	 * to a value} which cannot be converted to a value.
	 * 
	 * @param expression the expression which cannot be converted to a value
	 * @return a FormatException
	 */
	public static final FormatException cannotConvertToValued(Expression expression) {
		return new FormatException("The expression \"" + expression + "\" cannot be converted to value.");
	}
	
	/**
	 * Thrown when {@link Expression#toPrecondition() converting a logical
	 * expression} to a precondition which cannot be converted to a
	 * precondition.
	 * 
	 * @param expression the expression which cannot be converted to a
	 * precondition
	 * @return a FormatException
	 */
	public static final FormatException cannotConvertToPrecondition(Expression expression) {
		return new FormatException("The expression \"" + expression + "\" cannot be converted to a precondition.");
	}
	
	/**
	 * Thrown when {@link Expression#toEffect() converting a logical expression
	 * to an effect} which cannot be converted to an effect.
	 * 
	 * @param expression the expression which cannot be converted to an effect
	 * @return a FormatException
	 */
	public static final FormatException cannotConvertToEffect(Expression expression) {
		return new FormatException("The expression \"" + expression + "\" cannot be converted to an effect.");
	}
	
	/**
	 * Thrown when a {@link Variable variable} that is explicitly or implicitly
	 * quantified  is of type {@code number}, which is not allowed, since there
	 * are infinitely many numbers.
	 * 
	 * @return a FormatException
	 */
	public static final FormatException variableMayNotBeNumeric() {
		return new FormatException("Variables may not be of type \"" + Settings.NUMBER_TYPE_NAME + ", because they must have a finite number of possible values.");
	}
	
	/**
	 * Thrown when a logical expression would {@link
	 * edu.uky.cs.nil.sabre.logic.Assignment assign} a {@link
	 * edu.uky.cs.nil.sabre.logic.Value value} to a {@link Fluent fluent} but
	 * the fluent's type does not allow it to have that value.
	 * 
	 * @param fluent the fluent being assigned
	 * @param value the value which is the wrong type to be assigned to the
	 * fluent
	 * @return a FormatException
	 */
	public static final FormatException cannotAssign(Fluent fluent, Expression value) {
		return new FormatException("The expression \"" + value + "\" cannot be assigned to the fluent \"" + fluent + "\".");
	}
	
	/**
	 * Thrown when a {@link edu.uky.cs.nil.sabre.logic.Conditional conditional
	 * logical expression} has a mismatch between the number of conditions and
	 * branches (the number of branches must always be exactly 1 more than the
	 * number of conditions).
	 * 
	 * @param conditions the number of conditions given to the conditional
	 * @param branches the number of branches given to the conditional
	 * @return a FormatException
	 */
	public static final FormatException conditionBranchCount(int conditions, int branches) {
		return new FormatException("A conditional expression with \"" + conditions + "\" conditions must have exactly " + (conditions + 1) + " branches, but " + branches + " were given.");
	}
	
	/**
	 * Thrown when a query (usually a {@link Fluent fluent}) cannot be isolated
	 * by itself on the left side of a {@link
	 * edu.uky.cs.nil.sabre.logic.Comparison comparison} by applying simple
	 * transformations to the comparison.
	 * 
	 * @param query the expression that cannot be isolated
	 * @param expression the expression in which the query occurs
	 * @return a FormatException
	 */
	public static final FormatException cannotIsolate(Expression query, Expression expression) {
		return new FormatException("The expression \"" + query + "\" cannot be isolated in the expression \"" + expression + "\".");
	}
	
	/**
	 * Thrown when an {@link edu.uky.cs.nil.sabre.logic.Effect effect} is
	 * created with {@link edu.uky.cs.nil.sabre.logic.False#FALSE false} as its
	 * condition.
	 * 
	 * @return a FormatException
	 */
	public static final FormatException effectConditionCannotBeFalse() {
		return new FormatException("Effect condition cannot be \"" + DefaultParser.FALSE_KEYWORD + "\".");
	}
	
	/**
	 * Thrown when the number of original objects in a {@link
	 * edu.uky.cs.nil.sabre.logic.Substitution substitution} does not match
	 * the number of replacement objects.
	 * 
	 * @param originals the number of original objects
	 * @param replacements the number of replacement objects
	 * @return an IllegalArgumentException
	 */
	public static final IllegalArgumentException substitutionCount(int originals, int replacements) {
		return new IllegalArgumentException("Cannot substitute " + replacements + " thing" + (replacements == 1 ? "" : "s") + " for " + originals + " thing" + (originals == 1 ? "" : "s") + ".");
	}
	
	/**
	 * Thrown when some object of a given type and (optionally) with a given
	 * name should be defined, but no objects of that type with that name are
	 * defined. Example situations include requesting a {@link Type type} that
	 * does not exist from a {@link Universe universe} or when an object
	 * should be defined in a {@link edu.uky.cs.nil.sabre.io.Definitions list
	 * of definitions} but is not.
	 * 
	 * @param type the type of thing that should be defined
	 * @param name the name of the thing, or null if the name is not relevant
	 * @return a FormatException
	 */
	public static final FormatException notDefined(String type, String name) {
		return new FormatException(type + (name == null ? "" : " \"" + name + "\"") + " not defined.");
	}
	
	/**
	 * Thrown when trying to create a subtype of a type which cannot have any
	 * subtypes. For example, the type {@code boolean} cannot have any
	 * subtypes.
	 * 
	 * @param type the name of the type which cannot have any subtypes
	 * @return a FormatException
	 */
	public static final FormatException typeMayNotBeExtended(Type type) {
		return new FormatException("Type \"" + type.name + "\" may not be extended.");
	}
	
	/**
	 * Thrown when trying to make a type which cannot have any supertypes a
	 * supertype of some other type. For example, the type {@code boolean} may
	 * not have any supertypes.
	 * 
	 * @param type the name of the type which may not have any supertypes
	 * @return a FormatException
	 */
	public static final FormatException typeMayNotExtend(Type type) {
		return new FormatException("Type \"" + type.name + "\" may not extend from another type.");
	}
	
	/**
	 * Thrown when a cycle has been created in a {@link Universe universe's}
	 * type ontology.
	 * 
	 * @param child the child type of the relationship which created the cycle
	 * @param parent the parent type of the relationship which created the
	 * cycle
	 * @return a FormatException
	 */
	public static final FormatException typeCycle(Type child, Type parent) {
		return new FormatException("Type \"" + child.name + "\" may not extend \"" + parent.name + "\" because \"" + parent.name + "\" already extends \"" + child.name + "\".");
	}
	
	/**
	 * Thrown when a Java number cannot be converted to a {@link Number numeric
	 * value}.
	 * 
	 * @param value the Java number which cannot be converted
	 * @return an IllegalArgumentException
	 */
	public static final IllegalArgumentException notANumber(java.lang.Number value) {
		return new IllegalArgumentException("The value \"" + value + "\" cannot be represented as a number.");
	}
	
	/**
	 * Thrown when {@link Expression#evaluate(State) evaluating a logical
	 * expression} (usually an {@link edu.uky.cs.nil.sabre.logic.Arithmetic
	 * arithmetic expression}) would cause division by zero.
	 * 
	 * @param numerator the number that would be divided by zero
	 * @return an ArithmeticException
	 */
	public static final ArithmeticException divideByZero(Expression numerator) {
		return new ArithmeticException("Cannot divide \"" + numerator + "\" by zero.");
	}
	
	/**
	 * Thrown when attempting to assign an {@link Entity entity} a {@link Type
	 * type} which it cannot have. Entities cannot be of type {@code boolean}
	 * or {@code number}.
	 * 
	 * @param entity the entity
	 * @param type a type the entity may not have
	 * @return a FormatException
	 */
	public static final FormatException entityMayNotBeOfType(Entity entity, Type type) {
		return new FormatException("Entity \"" + entity.name + "\" may not be of type \"" + type.name + "\".");
	}
	
	/**
	 * Thrown when requesting a list of all values of type {@code number},
	 * which is infinitely many values.
	 * 
	 * @return a FormatException
	 */
	public static final FormatException infiniteValues() {
		return new FormatException("Type \"" + Settings.NUMBER_TYPE_NAME + "\" has infinitely many possible values.");
	}
	
	/**
	 * Thrown when defining a parameter (usually in a {@link Signature
	 * signature}) to be the value {@link
	 * edu.uky.cs.nil.sabre.logic.Unknown#UNKNOWN unknown}.
	 * 
	 * @param index the index of the parameter
	 * @return a FormatException
	 */
	public static final FormatException parameterMayNotBeNull(int index) {
		return new FormatException("Parameter " + index + " may not be \"" + DefaultParser.UNKNOWN_KEYWORD + "\".");
	}
	
	/**
	 * Thrown when defining a new thing would make a previously defined thing
	 * ambiguous. This usually happens when a {@link Fluent fluent} or {@link
	 * Event event} template implicitly defines something which was already
	 * implicitly defined by another fluent or event template.
	 * 
	 * @param type the type of thing being ambiguously defined
	 * @param definition the name or definition of the thing being ambiguously
	 * defined
	 * @param ambiguous the already defined thing which is not being redefined
	 * @return a FormatException
	 */
	public static final FormatException makesAmbiguous(String type, Signature definition, Signature ambiguous) {
		return new FormatException("Defining " + type + " \"" + definition + "\" would make \"" + ambiguous + "\" ambiguous.");
	}
	
	/**
	 * Thrown when an {@link edu.uky.cs.nil.sabre.logic.Effect effect} is a
	 * contradiction, such as when a fluent is assigned two different values
	 * in the same effect. An effect expression is a contradiction if {@link
	 * Expression#toEffect() converting it to an effect} returns the {@link
	 * edu.uky.cs.nil.sabre.logic.Clause#NULL null clause}.
	 * 
	 * @param expression the contradictory effect
	 * @return a FormatException
	 */
	public static final FormatException illegalEffect(Expression expression) {
		return new FormatException("The effect \"" + expression + "\" is contradictory.");
	}
	
	/**
	 * Thrown when a {@link Variable variable} must of of type {@code character}
	 * or a supertype of {@code character}, generally when defining a {@link
	 * Mapping.Function mapping function}.
	 * 
	 * @param variable the variable which needs to be of type character or some
	 * supertype of character
	 * @return a FormatException
	 */
	public static final FormatException variableMustBeCharacter(Variable variable) {
		return new FormatException("The variable \"" + variable.name + "\" must be of type \"" + Settings.CHARACTER_TYPE_NAME + "\" or a supertype of \"" + Settings.CHARACTER_TYPE_NAME + "\".");
	}
	
	/**
	 * Thrown when {@link ProblemBuilder#setUtility(Expression) defining a
	 * utility function} that is neither of type {@code number} nor of type
	 * {@code boolean}.
	 * 
	 * @param expression the invalid utility expression
	 * @return a FormatException
	 */
	public static final FormatException utilityMustBePropositionOrNumeric(Expression expression) {
		return new FormatException("The utility expression \"" + expression + "\" must be either a proposition or a numberic expression.");
	}
	
	/**
	 * Thrown when an expression cannot be imposed on a {@link State state} as
	 * an effect.
	 * 
	 * @param expression the expression which cannot be converted into a valid
	 * effect
	 * @return an IllegalStateException
	 */
	public static final IllegalStateException cannotImpose(Expression expression) {
		return new IllegalStateException("Cannot impose \"" + expression + "\".");
	}
	
	/**
	 * Thrown when attempting to add an explanation to an empty plan.
	 * 
	 * @return an IllegalArgumentException
	 */
	public static final IllegalArgumentException noExplanationForEmptyPlan() {
		return new IllegalArgumentException("No explanation is needed because this plan has no actions.");
	}
	
	/**
	 * Thrown when attempting to explain an action which does not require an
	 * explanation for a given character.
	 * 
	 * @param character the character for whom no explanation is needed
	 * @param action the action that does not need to be explained for that
	 * character
	 * @return an IllegalArgumentException
	 */
	public static final IllegalArgumentException noExplanationRequired(Character character, Action action) {
		return new IllegalArgumentException("The action \"" + action + "\" does not need an explanation for " + (character == null ? "the author" : "\"" + character + "\"") + ".");
	}
	
	/**
	 * Thrown when attempting to use a {@link Plan plan} to explain an {@link
	 * Action action} but the plan does not start with that action.
	 * 
	 * @param explanation the explanation which does not start with the action
	 * @param action the action
	 * @return an IllegalArgumentException
	 */
	public static final IllegalArgumentException explanationMustStartWithAction(Plan<?> explanation, Action action) {
		String exp = "";
		for(int i=0; i<explanation.size(); i++) {
			if(i > 0)
				exp += " ";
			exp += explanation.get(i);
		}
		return new IllegalArgumentException("The explanation \"" + exp + "\" cannot be used to explain the action \"" + action + "\" because it does not start with that action.");
	}
	
	/**
	 * Thrown when {@link
	 * edu.uky.cs.nil.sabre.hg.HeuristicGraph#getNode(Logical) requesting a
	 * node from a heuristic graph} that the graph does not define.
	 * 
	 * @param logical the expression which has no equivalent node in the
	 * heuristic graph
	 * @return an IllegalArgumentException
	 */
	public static final IllegalArgumentException noHeuristicGraphNode(Logical logical) {
		return new IllegalArgumentException("There is no heuristic graph node type defined for \"" + logical + "\".");
	}
	
	/**
	 * Thrown when {@link
	 * edu.uky.cs.nil.sabre.hg.HeuristicGraph#setUtility(Character,
	 * edu.uky.cs.nil.sabre.logic.Conditional) creating a utility node in a
	 * heuristic graph} for an character who already has a utility node.
	 * 
	 * @param character the character who already has a utility node
	 * @return an IllegalStateException
	 */
	public static final IllegalStateException utilityNodeAlreadyDefined(Character character) {
		return new IllegalStateException("A utility node has already been created for character \"" + character + "\".");
	}
	
	/**
	 * Thrown when constructing a {@link edu.uky.cs.nil.sabre.io.Sentinel
	 * sentinel pattern} for a {@link edu.uky.cs.nil.sabre.io.Parser parser}
	 * which does not have a sentinel keyword between each pattern.
	 * 
	 * @param pattern the pattern followed by another pattern that should be
	 * followed by a sentinel
	 * @return an IllegalArgumentException
	 */
	public static final IllegalArgumentException noSentinel(Object pattern) {
		return new IllegalArgumentException("The pattern \"" + pattern + "\" must be followed by a sentinel keyword.");
	}
	
	/**
	 * Thrown when a {@link edu.uky.cs.nil.sabre.io.Parser parser} has been
	 * asked to parse a symbol for which no parsing rules are defined.
	 * 
	 * @param symbol the symbol for which there are no parsing rules
	 * @param tokens the tokens being parsed
	 * @return a ParseException
	 */
	public static final ParseException noPattern(String symbol, ImmutableList<Token> tokens) {
		return new ParseException("The parser does not define any rules for parsing " + symbol + ".", tokens);
	}
	
	/**
	 * Thrown when a {@link edu.uky.cs.nil.sabre.io.Parser parser} tries to
	 * build an object after successfully parsing it, but no {@link
	 * edu.uky.cs.nil.sabre.io.Builder builders} are defined that specify how
	 * to create the object.
	 * 
	 * @param symbol the type of object for where there are no builders
	 * @param tokens the tokens parsed
	 * @return a ParseException
	 */
	public static final ParseException noBuilder(String symbol, ImmutableList<Token> tokens) {
		return new ParseException("The parser does not define a method for building " + symbol + ".", tokens);
	}
	
	/**
	 * Thrown when a {@link edu.uky.cs.nil.sabre.io.Parser parser} expected to
	 * find nothing but instead found something.
	 * 
	 * @param unexpected the thing the parser did not expect to encounter
	 * @param tokens the tokens being parsed
	 * @return a ParseException
	 */
	public static final ParseException parseUnexpected(String unexpected, ImmutableList<Token> tokens) {
		return new ParseException("Unexpected " + unexpected + ".", tokens);
	}
	
	/**
	 * Thrown when a {@link edu.uky.cs.nil.sabre.io.Parser parser} expected to
	 * find something specific next, but something else was found instead.
	 * 
	 * @param expected the thing that parser expected to find
	 * @param unexpected the thing the parser found instead
	 * @param tokens the tokens being parsed
	 * @return a ParseException
	 */
	public static final ParseException parseUnexpected(String expected, String unexpected, ImmutableList<Token> tokens) {
		return new ParseException("Expected to find " + expected + ", but found " + unexpected + ".", tokens);
	}
	
	/**
	 * Thrown when {@link edu.uky.cs.nil.sabre.io.Parser parsing} a {@link
	 * edu.uky.cs.nil.sabre.io.List list} that has the wrong number of
	 * elements.
	 * 
	 * @param count the number of elements in the list
	 * @param min the minimum number of elements the list should have
	 * @param max the maximum number of elements the list should have
	 * @param tokens the tokens being parsed
	 * @return a ParseException
	 */
	public static final ParseException parseListCount(int count, int min, int max, ImmutableList<Token> tokens) {
		String message;
		if(min != List.NO_LIMIT && min == max)
			message = Integer.toString(count);
		else if(min != List.NO_LIMIT && max != List.NO_LIMIT)
			message = "between " + min + " and " + max;
		else if(min != List.NO_LIMIT)
			message = "at least " + min;
		else
			message = "at most " + max;
		return new ParseException("Expected " + message + " elements, but found " + count + ".", tokens);
	}
	
	/**
	 * Thrown when a {@link edu.uky.cs.nil.sabre.io.Printer printer} tries to
	 * write an object, but no {@link edu.uky.cs.nil.sabre.io.ObjectPrinter
	 * object printers} are defined that accept that kind of object.
	 * 
	 * @param object the object which cannot be printed
	 * @return an IllegalArgumentException
	 */
	public static final IllegalArgumentException noPrinter(String object) {
		return new IllegalArgumentException("The printer does not define any rules for printing " + object + ".");
	}
	
	/**
	 * Thrown when {@link edu.uky.cs.nil.sabre.util.CommandLineArguments a set
	 * of command line arguments} should contain a key but does not.
	 * 
	 * @param argument the command line argument that should exist
	 * @return an IllegalArgumentException
	 */
	public static final IllegalArgumentException missingCommandLineArgument(String argument) {
		return new IllegalArgumentException("Expected the command line argument \"" + argument + "\" but it was not found.");
	}
	
	/**
	 * Thrown when parsing a value from {@link
	 * edu.uky.cs.nil.sabre.util.CommandLineArguments a set of command line
	 * arguments} fails. For example, if an argument should be an integer but
	 * cannot be parsed as an integer, this exception would be thrown.
	 * 
	 * @param argument the key for which the argument is a value
	 * @param value the value following the key
	 * @return an IllegalArgumentException
	 */
	public static final IllegalArgumentException failedToParseCommandLineArgument(String argument, String value) {
		return new IllegalArgumentException("The command line argument \"" + argument + " " + value + "\" could not be parsed.");
	}
	
	/**
	 * Thrown when a {@link edu.uky.cs.nil.sabre.util.CommandLineArguments
	 * command line argument} is a file, but the file cannot be found.
	 * 
	 * @param path the path to the file which does not exist
	 * @return an IllegalArgumentException
	 */
	public static final IllegalArgumentException fileNotFound(String path) {
		return new IllegalArgumentException("The file \"" + path + "\" was not found.");
	}
	
	/**
	 * Thrown when a {@link edu.uky.cs.nil.sabre.util.CommandLineArguments a
	 * command line argument} is not used for anything.
	 * 
	 * @param argument the unused argument
	 * @return an IllegalArgumentException
	 */
	public static final IllegalArgumentException unusedCommandLineArgument(String argument) {
		return new IllegalArgumentException("The command line argument \"" + argument + "\" either does not exist or is not used in this context.");
	}
	
	/**
	 * Thrown when an object is null but should not be.
	 * 
	 * @param name the name of the thing that is null but should not be
	 * @return an IllegalStateException
	 */
	public static final IllegalStateException notSet(String name) {
		return new IllegalStateException("A value for \"" + name + "\" has not been set.");
	}
	
	/**
	 * Thrown when an object is expected to be of a certain type but it is not
	 * (such as when an illegal cast is about to occur).
	 * 
	 * @param object the object that is expected to be of a certain type
	 * @param type the name of the type it should be but is not
	 * @return an IllegalStateException
	 */
	public static final IllegalStateException wrongType(Object object, String type) {
		return new IllegalStateException("Expected \"" + object + "\" to be " + type + " but it was not.");
	}
}