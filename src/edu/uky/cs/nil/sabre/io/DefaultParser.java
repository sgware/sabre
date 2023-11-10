package edu.uky.cs.nil.sabre.io;

import java.math.BigDecimal;
import java.util.ArrayList;

import edu.uky.cs.nil.sabre.*;
import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Number;
import edu.uky.cs.nil.sabre.logic.*;
import edu.uky.cs.nil.sabre.util.ImmutableArray;
import edu.uky.cs.nil.sabre.util.ImmutableList;

/**
 * A {@link Parser parser} which defines rules and builders for parsing many
 * kinds of common objects, including {@link Problem problems} and {@link
 * Logical logical formulas}.
 * <p>
 * When parsing {@link Logical logical formulas}, the following precedence
 * rules are used, from highest precedence to lowest:
 * <ul>
 * <li>{@link Epistemic belief expressions}</li>
 * <li>{@link Conjunction conjunctions}</li>
 * <li>{@link Disjunction disjunctions}</li>
 * <li>{@link Negation negations}</li>
 * <li>{@link Conditional conditionals}</li>
 * <li>{@link Quantified quantified expressions}</li>
 * <li>{@link Assignment assignments}</li>
 * <li>{@link Comparison#EQUAL_TO equal to comparisons}</li>
 * <li>{@link Comparison#NOT_EQUAL_TO not equal to comparisons}</li>
 * <li>{@link Comparison#GREATER_THAN_OR_EQUAL_TO greater than or equal to
 * comparisons}</li>
 * <li>{@link Comparison#LESS_THAN_OR_EQUAL_TO less than or equal to
 * comparisons}</li>
 * <li>{@link Comparison#GREATER_THAN greater than comparisons}</li>
 * <li>{@link Comparison#LESS_THAN less than comparisons}</li>
 * <li>{@link TypeConstraint type constraints}</li>
 * <li>{@link ArithmeticExpansion#PRODUCT products}</li>
 * <li>{@link ArithmeticExpansion#SUM summations}</li>
 * <li>{@link Arithmetic#MULTIPLY multiplication}</li>
 * <li>{@link Arithmetic#DIVIDE division}</li>
 * <li>{@link Arithmetic#ADD addition}</li>
 * <li>{@link Arithmetic#SUBTRACT subtraction}</li>
 * </ul>
 * Note that conjunctions and disjunctions have high precedence. If
 * <code>X</code>, <code>Y</code>, and <code>Z</code> are {@link
 * Typed#isBoolean() Boolean expressions} the string
 * <code>"if(X) Y &amp; Z"</code> will be parsed as
 * <code>"(if(X) Y) &amp; (Z)"</code>.
 * 
 * @author Stephen G. Ware
 */
public class DefaultParser extends Parser {

	/** Keyword for {@link Unknown#UNKNOWN unknown} */
	public static final String UNKNOWN_KEYWORD = "?";
	
	/** Keyword for {@link False#FALSE false} */
	public static final String FALSE_KEYWORD = "False";
	
	/** Keyword for {@link True#TRUE true} */
	public static final String TRUE_KEYWORD = "True";
	
	/** Keyword for the opening bracket of a logical expression in brackets */
	public static final String LOGICAL_OPEN_BRACKET = "(";
	
	/** Keyword for the closing bracket of a logical expression in brackets */
	public static final String LOGICAL_CLOSE_BRACKET = ")";
	
	/** Keyword for {@link Clause#NULL the null clause} */
	public static final String NULL_CLAUSE_KEYWORD = "nil";
	
	/** Keyword for the start of a list of parameters or arguments */
	public static final String PARAMETER_LIST_OPEN_BRACKET = "(";
	
	/** Keyword for the end of a list of parameters or arguments */
	public static final String PARAMETER_LIST_CLOSE_BRACKET = ")";
	
	/**
	 * Keyword for that separates elements in a list of parameters or arguments
	 */
	public static final String PARAMETER_SEPARATOR = ",";
	
	/** Keyword for {@link Epistemic epistemic modal expressions} */
	public static final String EPISTEMIC_KEYWORD = "believes";
	
	/** Keyword for {@link Conjunction the logical conjunction operator} */
	public static final String CONJUNCTION_KEYWORD = "&";
	
	/** Keyword for {@link Disjunction the logical disjunction operator} */
	public static final String DISJUNCTION_KEYWORD = "|";
	
	/** Keyword for {@link Negation the logical negation operator} */
	public static final String NEGATION_KEYWORD = "!";
	
	/**
	 * Keyword for the first branch of {@link Conditional a conditional
	 * expression}
	 */
	public static final String CONDITIONAL_FIRST_BRANCH_KEYWORD = "if";
	
	/**
	 * Keyword for all middle branches of {@link Conditional a conditional
	 * expression}
	 */
	public static final String CONDITIONAL_MIDDLE_BRANCH_KEYWORD = "elseif";
	
	/**
	 * Keyword for the last branch of {@link Conditional a conditional
	 * expression}
	 */
	public static final String CONDITIONAL_LAST_BRANCH_KEYWORD = "else";
	
	/** Keyword for {@link Effect#condition a conditional effect} */
	public static final String CONDITIONAL_EFFECT_KEYWORD = "when";
	
	/** Keyword for {@link Quantified#UNIVERSAL the universal quantifier} */
	public static final String UNIVERSAL_QUANTIFICATION_KEYWORD = "forall";
	
	/** Keyword for {@link Quantified#EXISTENTIAL the existential quantifier} */
	public static final String EXISTENTIAL_QUANTIFICATION_KEYWORD = "exists";
	
	/** Keyword for {@link Assignment assignment} */
	public static final String ASSIGNMENT_KEYWORD = "=";
	
	/** Keyword for {@link Comparison#EQUAL_TO equal to comparison} */
	public static final String EQUAL_TO_KEYWORD = "==";
	
	/** Keyword for {@link Comparison#NOT_EQUAL_TO not equal to comparison} */
	public static final String NOT_EQUAL_TO_KEYWORD = "!=";
	
	/**
	 * Keyword for {@link Comparison#GREATER_THAN_OR_EQUAL_TO greater than or
	 * equal to comparison}
	 */
	public static final String GREATER_THAN_OR_EQUAL_TO_KEYWORD = ">=";
	
	/**
	 * Keyword for {@link Comparison#LESS_THAN_OR_EQUAL_TO less than or equal to
	 * comparison}
	 */
	public static final String LESS_THAN_OR_EQUAL_TO_KEYWORD = "<=";
	
	/** Keyword for {@link Comparison#GREATER_THAN greater than comparison} */
	public static final String GREATER_THAN_KEYWORD = ">";
	
	/** Keyword for {@link Comparison#LESS_THAN less than comparison} */
	public static final String LESS_THAN_KEYWORD = "<";
	
	/** Keyword for {@link TypeConstraint type constraints} */
	public static final String TYPE_CONSTRAINT_KEYWORD = ":";
	
	/** Keyword for {@link ArithmeticExpansion#PRODUCT product} */
	public static final String PRODUCT_KEYWORD = "product";
	
	/** Keyword for {@link ArithmeticExpansion#SUM summation} */
	public static final String SUM_KEYWORD = "sum";
	
	/** Keyword for {@link Arithmetic#MULTIPLY the multiplication operator} */
	public static final String MULTIPLICATION_KEYWORD = "*";
	
	/** Keyword for {@link Arithmetic#DIVIDE the division operator} */
	public static final String DIVISION_KEYWORD = "/";
	
	/** Keyword for {@link Arithmetic#ADD the addition operator} */
	public static final String ADDITION_KEYWORD = "+";
	
	/** Keyword for {@link Arithmetic#SUBTRACT the subtraction operator} */
	public static final String SUBTRACTION_KEYWORD = "-";
	
	/** Keyword for ending a definition */
	public static final String DEFINITION_SEPARATOR = ";";
	
	/** Keyword for beginning {@link Type a type} definition */
	public static final String TYPE_DEFINITION_KEYWORD = "type";
	
	/** Keyword for expressing a type inheritance relationship */
	public static final String INHERITANCE_KEYWORD = ":";
	
	/** Keyword for beginning {@link Entity an entity} definition */
	public static final String ENTITY_DEFINITION_KEYWORD = "entity";
	
	/** Keyword for beginning {@link Fluent a fluent} definition */
	public static final String FLUENT_DEFINITION_KEYWORD = "property";
	
	/** Keyword for beginning {@link Action an action} definition */
	public static final String ACTION_DEFINITION_KEYWORD = "action";
	
	/** Keyword for beginning {@link Trigger a trigger} definition */
	public static final String TRIGGER_DEFINITION_KEYWORD = "trigger";
	
	/** Keyword that starts the block of definitions of an event's elements */
	public static final String EVENT_DEFINITION_OPEN_BRACKET = "{";
	
	/** Keyword that end the block of definitions of an event's elements */
	public static final String EVENT_DEFINITION_CLOSE_BRACKET = "}";
	
	/**
	 * Keyword to the right of a definition's label that indicates the
	 * definition appears to the right of this keyword
	 */
	public static final String DEFINITION_INDICATOR = ":";
	
	/**
	 * Keyword for beginning {@link Event#getPrecondition() an event
	 * precondition}
	 */
	public static final String PRECONDITION_KEYWORD = "precondition";
	
	/** Keyword for beginning {@link Event#getEffect() an event effect} */
	public static final String EFFECT_KEYWORD = "effect";
	
	/**
	 * Keyword for beginning {@link Action#consenting an action's consenting
	 * characters}
	 */
	public static final String CONSENTING_KEYWORD = "consenting";
	
	/**
	 * Keyword for beginning {@link Action#observing an action's observing
	 * characters}
	 */
	public static final String OBSERVING_KEYWORD = "observing";
	
	/**
	 * Keyword for beginning {@link Problem#getUtility(Character, State) a
	 * utility expression}
	 */
	public static final String UTILITY_DEFINITION_KEYWORD = "utility";
	
	/** Keyword used to signify a goal has been achieved */
	public static final String GOAL_KEYWORD = "goal";
	
	/** Keyword used to indent branches in a {@link Solution solution} */
	public static final String BRANCH_INDENT_KEYWORD = "|";
	
	/** Non-terminal symbol representing {@link Object a Java object} */
	protected static final NonTerminal OBJECT = new NonTerminal(Object.class, "an object");
	
	/**
	 * Non-terminal symbol representing {@link Quantified#quantifier a
	 * quantifier}
	 */
	protected static final NonTerminal QUANTIFIER = new NonTerminal(Quantified.Quantifier.class, "a quantifier");
	
	/**
	 * Non-terminal symbol representing {@link Quantified#UNIVERSAL the
	 * universal quantifier}
	 */
	protected static final NonTerminal UNIVERSAL_QUANTIFIER = new NonTerminal(Quantified.UNIVERSAL, "the universal quantifier");
	
	/**
	 * Non-terminal symbol representing {@link Quantified#EXISTENTIAL the
	 * existential quantifier}
	 */
	protected static final NonTerminal EXISTENTIAL_QUANTIFIER = new NonTerminal(Quantified.EXISTENTIAL, "the existential quantifier");
	
	/**
	 * Non-terminal sybmol representing {@link Comparison#operator a comparison
	 * operator}
	 */
	protected static final NonTerminal COMPARISON_OPERATOR = new NonTerminal(Comparison.Operator.class, "a comparison operator");
	
	/**
	 * Non-terminal symbol representing {@link Comparison#EQUAL_TO the equal to
	 * comparison operator}
	 */
	protected static final NonTerminal EQUAL_TO_OPERATOR = new NonTerminal(Comparison.EQUAL_TO, "the equal to operator");
	
	/**
	 * Non-terminal symbol representing {@link Comparison#NOT_EQUAL_TO the not
	 * equal to comparison operator}
	 */
	protected static final NonTerminal NOT_EQUAL_TO_OPERATOR = new NonTerminal(Comparison.NOT_EQUAL_TO, "the not equal to operator");
	
	/**
	 * Non-terminal symbol representing {@link
	 * Comparison#GREATER_THAN_OR_EQUAL_TO the greater than or equal to
	 * comparison operator}
	 */
	protected static final NonTerminal GREATER_THAN_OR_EQUAL_TO_OPERATOR = new NonTerminal(Comparison.GREATER_THAN_OR_EQUAL_TO, "the greater than or equal to operator");
	
	/**
	 * Non-terminal symbol representing {@link Comparison#LESS_THAN_OR_EQUAL_TO
	 * the less than or equal to comparison operator}
	 */
	protected static final NonTerminal LESS_THAN_OR_EQUAL_TO_OPERATOR = new NonTerminal(Comparison.LESS_THAN_OR_EQUAL_TO, "the less than or equal to operator");
	
	/**
	 * Non-terminal symbol representing {@link Comparison#GREATER_THAN the
	 * greater than comparison operator}
	 */
	protected static final NonTerminal GREATER_THAN_OPERATOR = new NonTerminal(Comparison.GREATER_THAN, "the greater than operator");
	
	/**
	 * Non-terminal symbol representing {@link Comparison#LESS_THAN the less
	 * than comparison operator}
	 */
	protected static final NonTerminal LESS_THAN_OPERATOR = new NonTerminal(Comparison.LESS_THAN, "the less than operator");
	
	/**
	 * Non-terminal symbol representing {@link Arithmetic#operator an arithmetic
	 * expansion operator}
	 */
	protected static final NonTerminal ARITHMETIC_EXPANSION_OPERATOR = new NonTerminal(ArithmeticExpansion.Operator.class, "an arithmetic expansion operator");
	
	/**
	 * Non-terminal symbol representing {@link ArithmeticExpansion#PRODUCT the
	 * product operator}
	 */
	protected static final NonTerminal PRODUCT_OPERATOR = new NonTerminal(ArithmeticExpansion.PRODUCT, "the product operator");
	
	/**
	 * Non-terminal symbol representing {@link ArithmeticExpansion#SUM the
	 * summation operator}
	 */
	protected static final NonTerminal SUM_OPERATOR = new NonTerminal(ArithmeticExpansion.SUM, "the summation operator");
	
	/**
	 * Non-terminal symbol representing {@link Arithmetic#operator an arithmetic
	 * operator}
	 */
	protected static final NonTerminal ARITHMETIC_OPERATOR = new NonTerminal(Arithmetic.Operator.class, "an arithmetic operator");
	
	/**
	 * Non-terminal symbol representing {@link Arithmetic#MULTIPLY the
	 * multiplication operator}
	 */
	protected static final NonTerminal MULTIPLY_OPERATOR = new NonTerminal(Arithmetic.MULTIPLY, "the multiplication operator");
	
	/**
	 * Non-terminal symbol representing {@link Arithmetic#DIVIDE the division
	 * operator}
	 */
	protected static final NonTerminal DIVIDE_OPERATOR = new NonTerminal(Arithmetic.DIVIDE, "the division operator");
	
	/**
	 * Non-terminal symbol representing {@link Arithmetic#ADD the addition
	 * operator}
	 */
	protected static final NonTerminal ADD_OPERATOR = new NonTerminal(Arithmetic.ADD, "the addition operator");
	
	/**
	 * Non-terminal symbol representing {@link Arithmetic#SUBTRACT the
	 * subtraction operator}
	 */
	protected static final NonTerminal SUBTRACT_OPERATOR = new NonTerminal(Arithmetic.SUBTRACT, "the subtraction operator");
	
	/** Non-terminal symbol representing {@link Logical a logical formula} */
	protected static final NonTerminal LOGICAL = new NonTerminal(Logical.class, "a logical formula");
	
	/**
	 * Non-terminal symbol representing {@link Typed a typed logical formula}
	 */
	protected static final NonTerminal TYPED = new NonTerminal(Typed.class, "a typed formula");
	
	/**
	 * Non-terminal symbol representing the definition of {@link Typed a typed
	 * logical formula}
	 */
	protected static final NonTerminal DEFINED_TYPED = new NonTerminal("a defined typed formula");
	
	/** Non-terminal symbol representing {@link Type a type} */
	protected static final NonTerminal TYPE = new NonTerminal(Type.class, "a type");
	
	/**
	 * Non-terminal symbol representing {@link Expression a logical expression}
	 */
	protected static final NonTerminal EXPRESSION = new NonTerminal(Expression.class, "a logical expression");
	
	/**
	 * Non-terminal symbol representing {@link Typed#isBoolean() a Boolean
	 * logical expression}
	 */
	protected static final NonTerminal BOOLEAN_EXPRESSION = new NonTerminal("a logical expression that evaluates to a Boolean value");
	
	/**
	 * Non-terminal symbol representing {@link Typed#isNumber() is numeric
	 * logical expression}
	 */
	protected static final NonTerminal NUMBER_EXPRESSION = new NonTerminal("a logical expression that evaluates to a number");
	
	/**
	 * Non-terminal symbol representing {@link Typed#isEntity() a logical
	 * expression that evaluates to an entity}
	 */
	protected static final NonTerminal ENTITY_EXPRESSION = new NonTerminal("an expression with an entity value");
	
	/**
	 * Non-terminal symbol representing {@link Typed#isCharacter() a logical
	 * expression that evaluates to an character}
	 */
	protected static final NonTerminal CHARACTER_EXPRESSION = new NonTerminal("an expression with a character value");
	
	/** Non-terminal symbol representing {@link Parameter a parameter} */
	protected static final NonTerminal PARAMETER = new NonTerminal(Parameter.class, "a parameter");
	
	/** 
	 * Non-terminal symbol representing {@link Parameter a parameter} of type
	 * character
	 */
	protected static final NonTerminal CHARACTER_PARAMETER = new NonTerminal("a character parameter");
	
	/**
	 * Non-terminal symbol representing {@link Parameter a parameter} with a
	 * name
	 */
	protected static final NonTerminal NAMED_PARAMETER = new NonTerminal("a named parameter");
	
	/** Non-terminal symbol representing {@link Value a value} */
	protected static final NonTerminal VALUE = new NonTerminal(Value.class, "a value");
	
	/** Non-terminal symbol representing {@link Value a value} with a name */
	protected static final NonTerminal NAMED_VALUE = new NonTerminal("a named value");
	
	/**
	 * Non-terminal symbol representing {@link Unknown#UNKNOWN the unknown
	 * value}
	 */
	protected static final NonTerminal UNKNOWN = new NonTerminal(Unknown.class, "the constant Unknown");
	
	/** Non-terminal symbol representing {@link False#FALSE false} */
	protected static final NonTerminal FALSE = new NonTerminal(False.class, "the constant False");
	
	/** Non-terminal symbol representing {@link True#TRUE true} */
	protected static final NonTerminal TRUE = new NonTerminal(True.class, "the constant True");
	
	/** Non-terminal symbol representing {@link Number a number} */
	protected static final NonTerminal NUMBER = new NonTerminal(Number.class, "a number");
	
	/** Non-terminal symbol representing {@link Entity an entity} */
	protected static final NonTerminal ENTITY = new NonTerminal(Entity.class, "an entity");
	
	/** Non-terminal symbol representing {@link Character a character} */
	protected static final NonTerminal CHARACTER = new NonTerminal(Character.class, "a character");
	
	/** Non-terminal symbol representing {@link Variable a variable} */
	protected static final NonTerminal VARIABLE = new NonTerminal(Variable.class, "a variable");
	
	/**
	 * Non-terminal symbol representing {@link Signature the signature} of a
	 * {@link edu.uky.cs.nil.sabre.Fluent fluent} or {@link
	 * edu.uky.cs.nil.sabre.Event event}
	 */
	protected static final NonTerminal SIGNATURE = new NonTerminal(Signature.class, "a property or event signature");
	
	/** Non-terminal symbol representing {@link Fluent a fluent} */
	protected static final NonTerminal FLUENT = new NonTerminal(Fluent.class, "a fluent");
	
	/**
	 * Non-terminal symbol representing {@link Fluent a fluent} with some number
	 * of {@link Fluent#characters characters}
	 */
	protected static final NonTerminal EPISTEMIC_FLUENT = new NonTerminal("an espitemic fluent");
	
	/**
	 * Non-terminal symbol representing {@link Fluent a fluent} with no {@link
	 * Fluent#characters characters}
	 */
	protected static final NonTerminal NONEPISTEMIC_FLUENT = new NonTerminal("a non-epistemic fluent");
	
	/**
	 * Non-terminal symbol representing {@link Epistemic a belief expression}
	 */
	protected static final NonTerminal EPISTEMIC = new NonTerminal(Epistemic.class, "an epistemic expression");
	
	/**
	 * Non-terminal symbol representing {@link Proposition a logical proposition}
	 */
	protected static final NonTerminal PROPOSITION = new NonTerminal(Proposition.class, "a proposition");
	
	/**
	 * Non-terminal symbol representing {@link Proposition#isBoolean() a logical
	 * proposition that uses Boolean operators}
	 */
	protected static final NonTerminal BOOLEAN_PROPOSITION = new NonTerminal("a Boolean proposition");
	
	/** Non-terminal symbol representing {@link Conjunction a conjunction} */
	protected static final NonTerminal CONJUNCTION = new NonTerminal(Conjunction.class, "a conjunction");
	
	/**
	 * Non-terminal symbol representing {@link Conjunction a conjunction} with
	 * two or more {@link Conjunction#arguments} arguments
	 */
	protected static final NonTerminal EXPLICIT_CONJUNCTION = new NonTerminal("a conjunction of 2 or more elements");
	
	/** Non-terminal symbol representing {@link Clause a clause} */
	protected static final NonTerminal CLAUSE = new NonTerminal(Clause.class, "a clause");
	
	/** Non-terminal symbol representing {@link Disjunction a disjunction} */
	protected static final NonTerminal DISJUNCTION = new NonTerminal(Disjunction.class, "a disjunction");
	
	/**
	 * Non-terminal symbol representing {@link Disjunction a disjunction} with
	 * two or more {@link Disjunction#arguments arguments}
	 */
	protected static final NonTerminal EXPLICIT_DISJUNCTION = new NonTerminal("a disjunction or 2 or more element");
	
	/** Non-terminal symbol representing {@link Negation a negation} */
	protected static final NonTerminal NEGATION = new NonTerminal(Negation.class, "a negation");
	
	/** Non-terminal symbol representing {@link Conditional a conditional} */
	protected static final NonTerminal CONDITIONAL = new NonTerminal(Conditional.class, "a conditional expression");
	
	/**
	 * Non-terminal symbol representing the first branch of {@link Conditional a
	 * conditional}
	 */
	protected static final NonTerminal CONDITIONAL_FIRST_BRANCH = new NonTerminal("the first branch of a conditional expression");
	
	/**
	 * Non-terminal symbol representing the middle branches of {@link
	 * Conditional a conditional}
	 */
	protected static final NonTerminal CONDITIONAL_MIDDLE_BRANCH = new NonTerminal("an elseif branch in a conditional expression");
	
	/**
	 * Non-terminal symbol representing the last branch of {@link Conditional a
	 * conditional}
	 */
	protected static final NonTerminal CONDITIONAL_LAST_BRANCH = new NonTerminal("the else branch of a conditional expression");
	
	/**
	 * Non-terminal symbol representing {@link Quantified a quantified
	 * expression}
	 */
	protected static final NonTerminal QUANTIFIED = new NonTerminal(Quantified.class, "a quantified proposition");
	
	/**
	 * Non-terminal symbol representing a universally {@link Quantified
	 * quantified expression}
	 */
	protected static final NonTerminal UNIVERSAL = new NonTerminal("a universal quantification");
	
	/**
	 * Non-terminal symbol representing an existentially {@link Quantified
	 * quantified expression}
	 */
	protected static final NonTerminal EXISTENTIAL = new NonTerminal("an existential quantification");
	
	/** Non-terminal symbol representing {@link Assignment an assignment} */
	protected static final NonTerminal ASSIGNMENT = new NonTerminal(Assignment.class, "an assignment");
	
	/** Non-terminal symbol representing {@link Comparison a comparison} */
	protected static final NonTerminal COMPARISON = new NonTerminal(Comparison.class, "a comparison");
	
	/**
	 * Non-terminal symbol representing an equal to {@link Comparison
	 * comparison}
	 */
	protected static final NonTerminal EQUAL_TO_COMPARISON = new NonTerminal("an equal to comparison");
	
	/**
	 * Non-terminal symbol representing a not equal to {@link Comparison
	 * comparison}
	 */
	protected static final NonTerminal NOT_EQUAL_TO_COMPARISON = new NonTerminal("a not equal to comparison");
	
	/**
	 * Non-terminal symbol representing a greater than or equal to {@link
	 * Comparison comparison}
	 */
	protected static final NonTerminal GREATER_THAN_OR_EQUAL_TO_COMPARISON = new NonTerminal("a greater than or equal to comparison");
	
	/**
	 * Non-terminal symbol representing a less than or equal to {@link
	 * Comparison comparison}
	 */
	protected static final NonTerminal LESS_THAN_OR_EQUAL_TO_COMPARISON = new NonTerminal("a less than or equal to comparison");
	
	/**
	 * Non-terminal symbol representing a greater than {@link Comparison
	 * comparison}
	 */
	protected static final NonTerminal GREATER_THAN_COMPARISON = new NonTerminal("a greater than comparison");
	
	/**
	 * Non-terminal symbol representing a less than {@link Comparison
	 * comparison}
	 */
	protected static final NonTerminal LESS_THAN_COMPARISON = new NonTerminal("a less than comparison");
	
	/**
	 * Non-terminal symbol representing {@link TypeConstraint a type constraint}
	 */
	protected static final NonTerminal TYPE_CONSTRAINT = new NonTerminal(TypeConstraint.class, "a type constraint");
	
	/** Non-terminal symbol representing {@link Numeric a numeric expression} */
	protected static final NonTerminal NUMERIC = new NonTerminal(Numeric.class, "a numeric expression");
	
	/**
	 * Non-terminal symbol representing {@link ArithmeticExpansion an arithmetic
	 * expansion}
	 */
	protected static final NonTerminal ARITHMETIC_EXPANSION = new NonTerminal(ArithmeticExpansion.class, "an arithmetic expansion");
	
	/**
	 * Non-terminal symbol representing a product {@link ArithmeticExpansion
	 * expansion}
	 */
	protected static final NonTerminal PRODUCT = new NonTerminal("a product");
	
	/**
	 * Non-terminal symbol representing a summation {@link ArithmeticExpansion
	 * expansion}
	 */
	protected static final NonTerminal SUM = new NonTerminal("a summation");
	
	/**
	 * Non-terminal symbol representing {@link Arithmetic an arithmetic
	 * expression}
	 */
	protected static final NonTerminal ARITHMETIC = new NonTerminal(Arithmetic.class, "an arithmetic expression");
	
	/**
	 * Non-terminal symbol representing a multiplication {@link Arithmetic
	 * arithmetic expression}
	 */
	protected static final NonTerminal MULTIPLICATION = new NonTerminal("a multiplication expression");
	
	/**
	 * Non-terminal symbol representing a division {@link Arithmetic arithmetic
	 * expression}
	 */
	protected static final NonTerminal DIVISION = new NonTerminal("a division expression");
	
	/**
	 * Non-terminal symbol representing an addition {@link Arithmetic arithmetic
	 * expression}
	 */
	protected static final NonTerminal ADDITION = new NonTerminal("an addition expression");
	
	/**
	 * Non-terminal symbol representing a subtraction {@link Arithmetic
	 * arithmetic expression}
	 */
	protected static final NonTerminal SUBTRACTION = new NonTerminal("a subtraction expression");
	
	/** Non-terminal symbol representing {@link Atom an atomic proposition} */
	protected static final NonTerminal ATOM = new NonTerminal(Atom.class, "an atomic proposition");
	
	/**
	 * Non-terminal symbol representing {@link Precondition a logical
	 * precondition}
	 */
	protected static final NonTerminal PRECONDITION = new NonTerminal(Precondition.class, "a precondition");
	
	/**
	 * Non-terminal symbol representing an equal to {@link Precondition
	 * precondition}
	 */
	protected static final NonTerminal EQUAL_TO_PRECONDITION = new NonTerminal("an equal to precondition");
	
	/**
	 * Non-terminal symbol representing a not equal to {@link Precondition
	 * precondition}
	 */
	protected static final NonTerminal NOT_EQUAL_TO_PRECONDITION = new NonTerminal("a not equal to precondition");
	
	/**
	 * Non-terminal symbol representing a greater than or equal to {@link
	 * Precondition precondition}
	 */
	protected static final NonTerminal GREATER_THAN_OR_EQUAL_TO_PRECONDITION = new NonTerminal("a greater than or equal to precondition");
	
	/**
	 * Non-terminal symbol representing a less than or equal to {@link
	 * Precondition precondition}
	 */
	protected static final NonTerminal LESS_THAN_OR_EQUAL_TO_PRECONDITION = new NonTerminal("a less than or equal to precondition");
	
	/**
	 * Non-terminal symbol representing a greater than {@link Precondition
	 * precondition}
	 */
	protected static final NonTerminal GREATER_THAN_PRECONDITION = new NonTerminal("a greater than precondition");
	
	/**
	 * Non-terminal symbol representing a less than {@link Precondition
	 * precondition}
	 */
	protected static final NonTerminal LESS_THAN_PRECONDITION = new NonTerminal("a less than precondition");
	
	/** Non-terminal symbol representing {@link Effect a logical effect} */
	protected static final NonTerminal EFFECT = new NonTerminal(Effect.class, "an effect");
	
	/**
	 * Non-terminal symbol representing {@link Effect a logical effect} with a
	 * {@link Effect#condition condition}
	 */
	protected static final NonTerminal CONDITIONAL_EFFECT = new NonTerminal("a conditional effect");
	
	/**
	 * Non-terminal symbol representing {@link Effect a logical effect} with no
	 * {@link Effect#condition condition}
	 */
	protected static final NonTerminal NONCONDITIONAL_EFFECT = new NonTerminal("a non-conditional effect");
	
	/** Non-terminal symbol representing {@link Problem a planning problem} */
	protected static final NonTerminal PROBLEM = new NonTerminal(Problem.class, "a problem");
	
	/**
	 * Non-terminal symbol representing the definition of some part of {@link
	 * Problem a planning problem}
	 */
	protected static final NonTerminal DEFINITION = new NonTerminal("a definition");
	
	/**
	 * Non-terminal symbol representing the definition of {@link Type a type}
	 */
	protected static final NonTerminal TYPE_DEFINITION = new NonTerminal("a type definition");
	
	/**
	 * Non-terminal symbol representing the definition of {@link Entity an
	 * entity}
	 */
	protected static final NonTerminal ENTITY_DEFINITION = new NonTerminal("an entity definition");
	
	/**
	 * Non-terminal symbol representing the definition of {@link Signature a
	 * signature}
	 */
	protected static final NonTerminal SIGNATURE_DEFINITION = new NonTerminal("a signature definition");
	
	/**
	 * Non-terminal symbol representing the definition of {@link Parameter a
	 * parameter} in {@link Signed something with a signature}
	 */
	protected static final NonTerminal PARAMETER_DEFINITION = new NonTerminal("a parameter definition");
	
	/**
	 * Non-terminal symbol representing the definition of {@link Variable a
	 * variable}
	 */
	protected static final NonTerminal VARIABLE_DEFINITION = new NonTerminal("a variable definition");
	
	/**
	 * Non-terminal symbol representing the definition of {@link Fluent a
	 * fluent}
	 */
	protected static final NonTerminal PROPERTY_DEFINITION = new NonTerminal("a property definition");
	
	/**
	 * Non-terminal symbol representing the definition of {@link Event an event}
	 */
	protected static final NonTerminal EVENT_DEFINITION = new NonTerminal("an event definition");
	
	/**
	 * Non-terminal symbol representing the definition of {@link Action an
	 * action}
	 */
	protected static final NonTerminal ACTION_DEFINITION = new NonTerminal("an action definition");
	
	/**
	 * Non-terminal symbol representing the definition of {@link Trigger a
	 * trigger}
	 */
	protected static final NonTerminal TRIGGER_DEFINITION = new NonTerminal("a trigger definition");
	
	/**
	 * Non-terminal symbol representing the definition of some part of {@link
	 * Action an action}
	 */
	protected static final NonTerminal ACTION_ELEMENT = new NonTerminal("part of an action definition");
	
	/**
	 * Non-terminal symbol representing the definition of some part of {@link
	 * Trigger a trigger}
	 */
	protected static final NonTerminal TRIGGER_ELEMENT = new NonTerminal("part of a trigger definition");
	
	/**
	 * Non-terminal symbol representing the definition of {@link
	 * Event#getPrecondition() an event's precondition}
	 */
	protected static final NonTerminal EVENT_PRECONDITION = new NonTerminal("an event precondition");
	
	/**
	 * Non-terminal symbol representing the definition of {@link
	 * Event#getEffect() an event's effect}
	 */
	protected static final NonTerminal EVENT_EFFECT = new NonTerminal("an event effect");
	
	/**
	 * Non-terminal symbol representing the definition of {@link
	 * Action#consenting an action's consenting characters}
	 */
	protected static final NonTerminal ACTION_CONSENTING = new NonTerminal("the characters who consent to an action");
	
	/**
	 * Non-terminal symbol representing a list of {@link Character characters}
	 */
	protected static final NonTerminal CHARACTER_LIST = new NonTerminal("a list of characters");
	
	/**
	 * Non-terminal symbol representing the definition of {@link
	 * Action#observing an action's observing characters}
	 */
	protected static final NonTerminal ACTION_OBSERVING = new NonTerminal("the characters who observe an action");
	
	/**
	 * Non-terminal symbol representing the definition of {@link
	 * Problem#getUtility(Character, State) a utility expression}
	 */
	protected static final NonTerminal UTILITY_DEFINITION = new NonTerminal("a utility function definition");
	
	/**
	 * Non-terminal symbol representing the definition of {@link Problem#utility
	 * the author's utility expression}
	 */
	protected static final NonTerminal AUTHOR_UTILITY_DEFINITION = new NonTerminal("the author's utility function");
	
	/**
	 * Non-terminal symbol representing the definition of {@link
	 * Problem#utilities a character's utility expression}
	 */
	protected static final NonTerminal CHARACTER_UTILITY_DEFINITION = new NonTerminal("a character's utility function");
	
	/**
	 * Non-terminal symbol representing a proposition that holds in {@link
	 * Problem#initial a problem's initial state}
	 */
	protected static final NonTerminal INITIAL_STATE_DEFINITION = new NonTerminal("a proposition describing the initial state");
	
	/** Non-terminal symbol representing an {@link Event event} */
	protected static final NonTerminal EVENT = new NonTerminal(Event.class, "an event");
	
	/** Non-terminal symbol representing an {@link Action action} */
	protected static final NonTerminal ACTION = new NonTerminal(Action.class, "an action");
	
	/** Non-terminal symbol representing a {@link Trigger trigger} */
	protected static final NonTerminal TRIGGER = new NonTerminal(Trigger.class, "a trigger");
	
	/** Non-terminal symbol representing a {@link Plan plan} */
	protected static final NonTerminal PLAN = new NonTerminal(Plan.class, "a plan");
	
	/**
	 * Non-terminal symbol representing a {@link HeadPlan plan where actions are
	 * prepended onto the beginning}
	 */
	protected static final NonTerminal HEAD_PLAN = new NonTerminal(HeadPlan.class, "a plan");
	
	/**
	 * Non-terminal symbol representing {@link HeadPlan#EMPTY the empty head
	 * plan}
	 */
	protected static final NonTerminal EMPTY_HEAD_PLAN = new NonTerminal("an empty plan");
	
	/**
	 * Non-terminal symbol representing {@link HeadPlan a head plan with one or
	 * more actions}
	 */
	protected static final NonTerminal NON_EMPTY_HEAD_PLAN = new NonTerminal("a plan with one or more actions");
	
	/** Non-terminal symbol representing a {@link Solution solution} */
	protected static final NonTerminal SOLUTION = new NonTerminal(Solution.class, "a solution");
	
	/**
	 * Non-terminal symbol representing the author's goal or a character's goal
	 * in a {@link Solution solution}
	 */
	protected static final NonTerminal SOLUTION_GOAL = new NonTerminal("a goal");
	
	/**
	 * Non-terminal symbol representing the author's goal at the end of a {@link
	 * Solution solution}
	 */
	protected static final NonTerminal SOLUTION_AUTHOR_GOAL = new NonTerminal("the author goal");
	
	/**
	 * Non-terminal symbol representing a character's goal in a {@link Solution
	 * solution}
	 */
	protected static final NonTerminal SOLUTION_CHARACTER_GOAL = new NonTerminal("a character goal");
	
	/**
	 * Non-terminal symbol representing a rest of a {@link Solution solution}
	 */
	protected static final NonTerminal SOLUTION_TAIL = new NonTerminal("the rest of a solution");
	
	/**
	 * Non-terminal symbol representing a character's goal followed by the rest
	 * of a {@link Solution solution}
	 */
	protected static final NonTerminal SOLUTION_CHARACTER_GOAL_WITH_TAIL = new NonTerminal("a character's goal");
	
	/**
	 * Non-terminal symbol representing an action in a {@link Solution solution}
	 * followed by the rest of a solution
	 */
	protected static final NonTerminal SOLUTION_PLAN = new NonTerminal("a solution plan");
	
	/**
	 * Non-terminal symbol representing a branch explaining an action in a
	 * {@link Solution solution}
	 */
	protected static final NonTerminal SOLUTION_BRANCH = new NonTerminal("a solution branch");
	
	@FunctionalInterface
	private interface TypedBuilder extends Builder {
		
		@Override
		public default Expression build(Parser parser, ParseTree tree, Definitions definitions) throws ParseException {
			Expression expression = parser.build(tree.get(0), Expression.class, definitions);
			check(expression);
			return expression;
		}
		
		public void check(Expression expression);
	}
	
	private static final class QuantifiedBuilder implements Builder {
		
		public final Quantified.Quantifier quantifier;
		public final Pattern pattern;
		
		public QuantifiedBuilder(String keyword, Quantified.Quantifier quantifier) {
			this.quantifier = quantifier;
			this.pattern = new Sentinel(keyword, PARAMETER_LIST_OPEN_BRACKET, VARIABLE_DEFINITION, PARAMETER_LIST_CLOSE_BRACKET, BOOLEAN_EXPRESSION);
		}

		@Override
		public Quantified build(Parser parser, ParseTree tree, Definitions definitions) throws ParseException {
			Variable variable = parser.build(tree.get(0), Variable.class, definitions);
			definitions.add(variable.name, variable);
			return new Quantified(quantifier, variable, parser.build(tree.get(1), Expression.class, definitions));
		}
	};
	
	private static final class ComparisonBuilder implements Builder {
		
		public final Comparison.Operator operator;
		public final Pattern comparisonPattern;
		public final Pattern preconditionPattern;
		
		public ComparisonBuilder(String keyword, Comparison.Operator operator) {
			this.operator = operator;
			this.comparisonPattern = new Sentinel(Expression.class, keyword, Expression.class);
			this.preconditionPattern = new Selection(
				new Sentinel(Variable.class, keyword, Parameter.class),
				new Sentinel(Fluent.class, keyword, Expression.class)
			);
		}

		@Override
		public Comparison build(Parser parser, ParseTree tree, Definitions definitions) throws ParseException {
			Expression left = parser.build(tree.get(0), Expression.class, definitions);
			Expression right = parser.build(tree.get(1), Expression.class, definitions);
			if(left instanceof Variable && right instanceof Parameter)
				return new Precondition(operator, (Variable) left, (Parameter) right);
			else if(left instanceof Fluent && right.isValued())
				return new Precondition(operator, (Fluent) left, right);
			else
				return new Comparison(operator, left, right);
		}
	}
	
	private static final class ArithmeticExpansionBuilder implements Builder {
		
		public final ArithmeticExpansion.Operator operator;
		public final Pattern pattern;
		
		public ArithmeticExpansionBuilder(String keyword, ArithmeticExpansion.Operator operator) {
			this.operator = operator;
			this.pattern = new Sentinel(keyword, PARAMETER_LIST_OPEN_BRACKET, VARIABLE_DEFINITION, PARAMETER_LIST_CLOSE_BRACKET, NUMBER_EXPRESSION);
		}

		@Override
		public ArithmeticExpansion build(Parser parser, ParseTree tree, Definitions definitions) throws ParseException {
			Variable variable = parser.build(tree.get(0), Variable.class, definitions);
			definitions.add(variable.name, variable);
			return new ArithmeticExpansion(operator, variable, parser.build(tree.get(1), Expression.class, definitions));
		}
	};
	
	private static final class ArithmeticBuilder implements Builder {
		
		public final Arithmetic.Operator operator;
		public final Pattern pattern;
		
		public ArithmeticBuilder(String keyword, Arithmetic.Operator operator) {
			this.operator = operator;
			this.pattern = new Sentinel(NUMBER_EXPRESSION, keyword, NUMBER_EXPRESSION);
		}

		@Override
		public Object build(Parser parser, ParseTree tree, Definitions definitions) throws ParseException {
			return new Arithmetic(operator, parser.build(tree.get(0), Expression.class, definitions), parser.build(tree.get(1), Expression.class, definitions));
		}
	}
	
	/**
	 * Constructs a new default parser.
	 */
	public DefaultParser() {
		// Object
		setRule(OBJECT, new Selection(
			ARITHMETIC_OPERATOR, QUANTIFIER, COMPARISON_OPERATOR, LOGICAL, PROBLEM
		));
		// Quantifiers
		setRule(QUANTIFIER, new Selection(
			UNIVERSAL_QUANTIFIER, EXISTENTIAL_QUANTIFIER
		));
		set(UNIVERSAL_QUANTIFIER, new Keyword(UNIVERSAL_QUANTIFICATION_KEYWORD), new ConstantBuilder(Quantified.UNIVERSAL));
		set(EXISTENTIAL_QUANTIFIER, new Keyword(EXISTENTIAL_QUANTIFICATION_KEYWORD), new ConstantBuilder(Quantified.EXISTENTIAL));
		// Comparison Operators
		setRule(COMPARISON_OPERATOR, new Selection(
			EQUAL_TO_OPERATOR, NOT_EQUAL_TO_OPERATOR, GREATER_THAN_OR_EQUAL_TO_OPERATOR, LESS_THAN_OR_EQUAL_TO_OPERATOR, GREATER_THAN_OPERATOR, LESS_THAN_OPERATOR
		));
		set(EQUAL_TO_OPERATOR, new Keyword(EQUAL_TO_KEYWORD), new ConstantBuilder(Comparison.EQUAL_TO));
		set(NOT_EQUAL_TO_OPERATOR, new Keyword(NOT_EQUAL_TO_KEYWORD), new ConstantBuilder(Comparison.NOT_EQUAL_TO));
		set(GREATER_THAN_OR_EQUAL_TO_OPERATOR, new Keyword(GREATER_THAN_OR_EQUAL_TO_KEYWORD), new ConstantBuilder(Comparison.GREATER_THAN_OR_EQUAL_TO));
		set(LESS_THAN_OR_EQUAL_TO_OPERATOR, new Keyword(LESS_THAN_OR_EQUAL_TO_KEYWORD), new ConstantBuilder(Comparison.LESS_THAN_OR_EQUAL_TO));
		set(GREATER_THAN_OPERATOR, new Keyword(GREATER_THAN_KEYWORD), new ConstantBuilder(Comparison.GREATER_THAN));
		set(LESS_THAN_OPERATOR, new Keyword(LESS_THAN_KEYWORD), new ConstantBuilder(Comparison.LESS_THAN));
		// Arithmetic Expansion Operator
		setRule(ARITHMETIC_EXPANSION_OPERATOR, new Selection(
			PRODUCT_OPERATOR, SUM_OPERATOR
		));
		set(PRODUCT_OPERATOR, new Keyword(PRODUCT_KEYWORD), new ConstantBuilder(ArithmeticExpansion.PRODUCT));
		set(SUM_OPERATOR, new Keyword(SUM_KEYWORD), new ConstantBuilder(ArithmeticExpansion.SUM));
		// Arithmetic Operator
		setRule(ARITHMETIC_OPERATOR, new Selection(
			MULTIPLY_OPERATOR, DIVIDE_OPERATOR, ADD_OPERATOR, SUBTRACT_OPERATOR
		));
		set(MULTIPLY_OPERATOR, new Keyword(MULTIPLICATION_KEYWORD), new ConstantBuilder(Arithmetic.MULTIPLY));
		set(DIVIDE_OPERATOR, new Keyword(DIVISION_KEYWORD), new ConstantBuilder(Arithmetic.DIVIDE));
		set(ADD_OPERATOR, new Keyword(ADDITION_KEYWORD), new ConstantBuilder(Arithmetic.ADD));
		set(SUBTRACT_OPERATOR, new Keyword(SUBTRACTION_KEYWORD), new ConstantBuilder(Arithmetic.SUBTRACT));
		// Logical
		setRule(LOGICAL, new Selection(
			TYPED, SIGNATURE
		));
		// Typed
		setRule(TYPED, new Selection(
			DEFINED_TYPED, EXPRESSION
		));
		setRule(DEFINED_TYPED, Terminal.TOKEN);
		setBuilder(DEFINED_TYPED, (parser, tree, definitions) -> {
			String string = parser.build(tree.get(0), String.class);
			if(string.equals(UNKNOWN_KEYWORD))
				return Unknown.UNKNOWN;
			else if(string.equals(FALSE_KEYWORD))
				return False.FALSE;
			else if(string.equals(TRUE_KEYWORD))
				return True.TRUE;
			try {
				return Number.get(new BigDecimal(string));
			}
			catch(NumberFormatException e) {}
			return definitions.get(string, Typed.class);
		});
		set(TYPE, Terminal.NAME, new DefinedObjectBuilder(Type.class));
		// Expression
		setRule(EXPRESSION, new Selection(
			new Sentinel(LOGICAL_OPEN_BRACKET, Expression.class, LOGICAL_CLOSE_BRACKET),
			PARAMETER,
			FLUENT,
			EPISTEMIC,
			BOOLEAN_PROPOSITION,
			CONDITIONAL,
			QUANTIFIED,
			ASSIGNMENT,
			COMPARISON,
			TYPE_CONSTRAINT,
			NUMERIC
		));
		set(BOOLEAN_EXPRESSION, EXPRESSION, (TypedBuilder) e -> e.mustBeBoolean());
		set(NUMBER_EXPRESSION, EXPRESSION, (TypedBuilder) e -> e.mustBeNumber());
		set(ENTITY_EXPRESSION, EXPRESSION, (TypedBuilder) e -> e.mustBeEntity());
		set(CHARACTER_EXPRESSION, EXPRESSION, (TypedBuilder) e -> e.mustBeCharacter());
		// Parameter
		setRule(PARAMETER, new Selection(
			UNKNOWN, FALSE, TRUE, NUMBER, NAMED_PARAMETER
		));
		set(CHARACTER_PARAMETER, PARAMETER, (TypedBuilder) e -> e.mustBeCharacter());
		setRule(NAMED_PARAMETER, Terminal.NAME);
		setBuilder(NAMED_PARAMETER, new DefinedObjectBuilder(Parameter.class));
		setRule(VALUE, new Selection(
			UNKNOWN, FALSE, TRUE, NUMBER, ENTITY
		));
		set(UNKNOWN, new Keyword(UNKNOWN_KEYWORD), new ConstantBuilder(Unknown.UNKNOWN));
		set(FALSE, new Keyword(FALSE_KEYWORD), new ConstantBuilder(False.FALSE));
		set(TRUE, new Keyword(TRUE_KEYWORD), new ConstantBuilder(True.TRUE));
		setRule(NUMBER, Terminal.NUMBER);
		set(ENTITY, Terminal.NAME, new DefinedObjectBuilder(Entity.class));
		set(CHARACTER, Terminal.NAME, new DefinedObjectBuilder(Character.class));
		set(VARIABLE, Terminal.NAME, new DefinedObjectBuilder(Variable.class));
		// Signature
		setRule(SIGNATURE,
			new Sentinel(Terminal.NAME, PARAMETER_LIST_OPEN_BRACKET, new List(PARAMETER, PARAMETER_SEPARATOR), PARAMETER_LIST_CLOSE_BRACKET)
		);
		setBuilder(SIGNATURE, (parser, tree, definitions) -> {
			String name = parser.build(tree.get(0), String.class);
			Parameter[] parameters = new Parameter[tree.size() - 1];
			for(int i=0; i<parameters.length; i++)
				parameters[i] = parser.build(tree.get(i + 1), Parameter.class, definitions);
			return new Signature(name, parameters);
		});
		// Fluent
		setRule(FLUENT, new Selection(
			EPISTEMIC_FLUENT, NONEPISTEMIC_FLUENT
		));
		setRule(EPISTEMIC_FLUENT,
			new Sentinel(EPISTEMIC_KEYWORD, PARAMETER_LIST_OPEN_BRACKET, CHARACTER_PARAMETER, PARAMETER_SEPARATOR, Fluent.class, PARAMETER_LIST_CLOSE_BRACKET)
		);
		setBuilder(EPISTEMIC_FLUENT, (parser, tree, definitions) -> {
			return parser.build(tree.get(1), Fluent.class, definitions).prepend(parser.build(tree.get(0), Parameter.class, definitions));
		});
		setRule(NONEPISTEMIC_FLUENT,
			SIGNATURE
		);
		setBuilder(NONEPISTEMIC_FLUENT, (parser, tree, definitions) -> {
			return definitions.require(Problem.class).getFluent(parser.build(tree.get(0), Signature.class, definitions));
		});
		// Epistemic
		setRule(EPISTEMIC,
			new Sentinel(EPISTEMIC_KEYWORD, PARAMETER_LIST_OPEN_BRACKET, CHARACTER_PARAMETER, PARAMETER_SEPARATOR, Expression.class, PARAMETER_LIST_CLOSE_BRACKET)
		);
		setBuilder(EPISTEMIC, (parser, tree, definitions) -> {
			return new Epistemic(parser.build(tree.get(0), Parameter.class, definitions), parser.build(tree.get(1), Expression.class, definitions));
		});
		// Proposition
		setRule(PROPOSITION, new Selection(
			BOOLEAN_PROPOSITION, QUANTIFIED, ASSIGNMENT, COMPARISON, TYPE_CONSTRAINT
		));
		// Boolean
		setRule(BOOLEAN_PROPOSITION, new Selection(
			FALSE, TRUE, EXPLICIT_CONJUNCTION, EXPLICIT_DISJUNCTION, NEGATION
		));
		setRule(CONJUNCTION,
			new List(BOOLEAN_EXPRESSION, CONJUNCTION_KEYWORD)
		);
		setBuilder(CONJUNCTION, (parser, tree, definitions) -> {
			Expression[] arguments = new Expression[tree.size()];
			for(int i=0; i<arguments.length; i++)
				arguments[i] = parser.build(tree.get(i), Expression.class, definitions);
			if(arguments.length == 0)
				return Clause.EMPTY;
			else
				return new Conjunction<>(arguments);
		});
		setRule(EXPLICIT_CONJUNCTION,
			new List(BOOLEAN_EXPRESSION, CONJUNCTION_KEYWORD, 2, List.NO_LIMIT)
		);
		setBuilder(EXPLICIT_CONJUNCTION, getBuilder(CONJUNCTION));
		setRule(CLAUSE,
			new List(Atom.class, CONJUNCTION_KEYWORD)
		);
		setBuilder(CLAUSE, (parser, tree, definitions) -> {
			Clause<Atom> clause = Clause.EMPTY;
			for(int i=0; i<tree.size(); i++)
				clause = clause.add(parser.build(tree.get(i), Atom.class, definitions));
			return clause;
		});
		setRule(DISJUNCTION,
			new List(BOOLEAN_EXPRESSION, DISJUNCTION_KEYWORD)
		);
		setBuilder(DISJUNCTION, (parser, tree, definitions) -> {
			Expression[] arguments = new Expression[tree.size()];
			for(int i=0; i<arguments.length; i++)
				arguments[i] = parser.build(tree.get(i), Expression.class, definitions);
			if(arguments.length == 0)
				return False.FALSE;
			else
				return new Disjunction<>(arguments);
		});
		setRule(EXPLICIT_DISJUNCTION,
			new List(BOOLEAN_EXPRESSION, DISJUNCTION_KEYWORD, 2, List.NO_LIMIT)
		);
		setBuilder(EXPLICIT_DISJUNCTION, getBuilder(DISJUNCTION));
		setRule(NEGATION,
			new Sentinel(NEGATION_KEYWORD, BOOLEAN_EXPRESSION)
		);
		setBuilder(NEGATION, (parser, tree, definitions) -> {
			return new Negation(parser.build(tree.get(0), Expression.class, definitions));
		});		
		// Conditional
		setRule(CONDITIONAL, new Lookahead() {
			@Override
			public ParseTree match(Parser parser, ImmutableList<Token> tokens) throws ParseException {
				parser.match(new Keyword(CONDITIONAL_FIRST_BRANCH_KEYWORD), tokens.size() > 0 ? Pattern.first(tokens) : tokens);
				ImmutableList<Token>[] branches = branches(tokens, 1);
				ParseTree[] children = new ParseTree[branches.length];
				children[0] = parser.match(CONDITIONAL_FIRST_BRANCH, branches[0]);
				for(int i=1; i<branches.length; i++) {
					if(i == branches.length - 1 && branches[i].first.value.equals(CONDITIONAL_LAST_BRANCH_KEYWORD))
						children[i] = parser.match(CONDITIONAL_LAST_BRANCH, branches[i]);
					else
						children[i] = parser.match(CONDITIONAL_MIDDLE_BRANCH, branches[i]);
				}
				return new ParseTree(this, tokens, children);
			}
			@SuppressWarnings("unchecked")
			private final ImmutableList<Token>[] branches(ImmutableList<Token> tokens, int size) {
				ImmutableList<Token> end = find(CONDITIONAL_MIDDLE_BRANCH_KEYWORD, tokens.rest);
				if(end == null)
					end = find(CONDITIONAL_LAST_BRANCH_KEYWORD, tokens.rest);
				if(end == null) {
					ImmutableList<Token>[] branches = new ImmutableList[size];
					branches[size - 1] = tokens;
					return branches;
				}
				else {
					ImmutableList<Token>[] branches = branches(end, size + 1);
					branches[size - 1] = Pattern.clip(tokens, tokens.size() - end.size());
					return branches;
				}
			}
		});
		setRule(CONDITIONAL_FIRST_BRANCH,
			new Sentinel(CONDITIONAL_FIRST_BRANCH_KEYWORD, PARAMETER_LIST_OPEN_BRACKET, BOOLEAN_EXPRESSION, PARAMETER_LIST_CLOSE_BRACKET, Expression.class)
		);
		setRule(CONDITIONAL_MIDDLE_BRANCH,
			new Sentinel(CONDITIONAL_MIDDLE_BRANCH_KEYWORD, PARAMETER_LIST_OPEN_BRACKET, BOOLEAN_EXPRESSION, PARAMETER_LIST_CLOSE_BRACKET, Expression.class)
		);
		setRule(CONDITIONAL_LAST_BRANCH,
			new Sentinel(CONDITIONAL_LAST_BRANCH_KEYWORD, Expression.class)
		);
		setBuilder(CONDITIONAL, (parser, tree, definitions) -> {
			Expression[] conditions;
			Expression[] branches;
			if(tree.get(tree.size() - 1).is(CONDITIONAL_LAST_BRANCH)) {
				conditions = new Expression[tree.size() - 1];
				branches = new Expression[tree.size()];
			}
			else {
				conditions = new Expression[tree.size()];
				branches = new Expression[tree.size() + 1];
				branches[branches.length - 1] = True.TRUE;
			}
			for(int i=0; i<conditions.length; i++) {
				conditions[i] = parser.build(tree.get(i).get(0), Expression.class, definitions);
				branches[i] = parser.build(tree.get(i).get(1), Expression.class, definitions);
			}
			if(branches[branches.length - 1] == null)
				branches[branches.length - 1] = parser.build(tree.get(tree.size() - 1).get(0), Expression.class, definitions);
			else if(!branches[0].isBoolean()) {
				ImmutableList<Token> last = Pattern.last(tree.get(tree.size() - 1).tokens);
				throw Exceptions.parseUnexpected(CONDITIONAL_LAST_BRANCH_KEYWORD + " branch after \"" + last.first.value + "\"", "nothing", last);
			}
			return new Conditional<>(conditions, branches);
		});
		// Quantified
		setRule(QUANTIFIED, new Selection(
			UNIVERSAL, EXISTENTIAL
		));
		QuantifiedBuilder universal = new QuantifiedBuilder(UNIVERSAL_QUANTIFICATION_KEYWORD, Quantified.UNIVERSAL);
		QuantifiedBuilder existential = new QuantifiedBuilder(EXISTENTIAL_QUANTIFICATION_KEYWORD, Quantified.EXISTENTIAL);
		set(UNIVERSAL, universal.pattern, universal);
		set(EXISTENTIAL, existential.pattern, existential);
		// Assignment
		setRule(ASSIGNMENT, new Selection(
			new Sentinel(Fluent.class, ASSIGNMENT_KEYWORD, Expression.class)
		));
		setBuilder(ASSIGNMENT, (parser, tree, definitions) -> {
			return new Assignment(parser.build(tree.get(0), Fluent.class, definitions), parser.build(tree.get(1), Expression.class, definitions));
		});
		// Comparison
		setRule(COMPARISON, new Selection(
			EQUAL_TO_COMPARISON, NOT_EQUAL_TO_COMPARISON, GREATER_THAN_OR_EQUAL_TO_COMPARISON, LESS_THAN_OR_EQUAL_TO_COMPARISON, GREATER_THAN_COMPARISON, LESS_THAN_COMPARISON
		));
		ComparisonBuilder equalTo = new ComparisonBuilder(EQUAL_TO_KEYWORD, Comparison.EQUAL_TO);
		ComparisonBuilder notEqualTo = new ComparisonBuilder(NOT_EQUAL_TO_KEYWORD, Comparison.NOT_EQUAL_TO);
		ComparisonBuilder greaterThanOrEqualTo = new ComparisonBuilder(GREATER_THAN_OR_EQUAL_TO_KEYWORD, Comparison.GREATER_THAN_OR_EQUAL_TO);
		ComparisonBuilder lessThanOrEqualTo = new ComparisonBuilder(LESS_THAN_OR_EQUAL_TO_KEYWORD, Comparison.LESS_THAN_OR_EQUAL_TO);
		ComparisonBuilder greaterThan = new ComparisonBuilder(GREATER_THAN_KEYWORD, Comparison.GREATER_THAN);
		ComparisonBuilder lessThan = new ComparisonBuilder(LESS_THAN_KEYWORD, Comparison.LESS_THAN);
		set(EQUAL_TO_COMPARISON, equalTo.comparisonPattern, equalTo);
		set(NOT_EQUAL_TO_COMPARISON, notEqualTo.comparisonPattern, notEqualTo);
		set(GREATER_THAN_OR_EQUAL_TO_COMPARISON, greaterThanOrEqualTo.comparisonPattern, greaterThanOrEqualTo);
		set(LESS_THAN_OR_EQUAL_TO_COMPARISON, lessThanOrEqualTo.comparisonPattern, lessThanOrEqualTo);
		set(GREATER_THAN_COMPARISON, greaterThan.comparisonPattern, greaterThan);
		set(LESS_THAN_COMPARISON, lessThan.comparisonPattern, lessThan);
		// Type Constraint
		setRule(TYPE_CONSTRAINT,
			new Sentinel(Expression.class, TYPE_CONSTRAINT_KEYWORD, Type.class)
		);
		setBuilder(TYPE_CONSTRAINT, (parser, tree, definitions) -> {
			return new TypeConstraint(parser.build(tree.get(0), Expression.class, definitions), parser.build(tree.get(1), Type.class, definitions));
		});
		// Numeric
		setRule(NUMERIC, new Selection(
			NUMBER, ARITHMETIC_EXPANSION, ARITHMETIC
		));
		// Arithmetic Expansion
		setRule(ARITHMETIC_EXPANSION, new Selection(
			PRODUCT, SUM
		));
		ArithmeticExpansionBuilder product = new ArithmeticExpansionBuilder(PRODUCT_KEYWORD, ArithmeticExpansion.PRODUCT);
		ArithmeticExpansionBuilder sum = new ArithmeticExpansionBuilder(SUM_KEYWORD, ArithmeticExpansion.SUM);
		set(PRODUCT, product.pattern, product);
		set(SUM, sum.pattern, sum);
		// Arithmetic
		setRule(ARITHMETIC, new Selection(
			MULTIPLICATION, DIVISION, ADDITION, SUBTRACTION
		));
		ArithmeticBuilder multiplication = new ArithmeticBuilder(MULTIPLICATION_KEYWORD, Arithmetic.MULTIPLY);
		ArithmeticBuilder division = new ArithmeticBuilder(DIVISION_KEYWORD, Arithmetic.DIVIDE);
		ArithmeticBuilder addition = new ArithmeticBuilder(ADDITION_KEYWORD, Arithmetic.ADD);
		ArithmeticBuilder subtraction = new ArithmeticBuilder(SUBTRACTION_KEYWORD, Arithmetic.SUBTRACT);
		set(MULTIPLICATION, multiplication.pattern, multiplication);
		set(DIVISION, division.pattern, division);
		set(ADDITION, addition.pattern, addition);
		set(SUBTRACTION, subtraction.pattern, subtraction);
		// Atom
		setRule(ATOM, new Selection(
			PRECONDITION, EFFECT
		));
		// Precondition
		setRule(PRECONDITION, new Selection(
			EQUAL_TO_PRECONDITION, NOT_EQUAL_TO_PRECONDITION, GREATER_THAN_OR_EQUAL_TO_PRECONDITION, LESS_THAN_OR_EQUAL_TO_PRECONDITION, GREATER_THAN_PRECONDITION, LESS_THAN_PRECONDITION
		));
		set(EQUAL_TO_PRECONDITION, equalTo.preconditionPattern, equalTo);
		set(NOT_EQUAL_TO_PRECONDITION, notEqualTo.preconditionPattern, notEqualTo);
		set(GREATER_THAN_OR_EQUAL_TO_PRECONDITION, greaterThanOrEqualTo.preconditionPattern, greaterThanOrEqualTo);
		set(LESS_THAN_OR_EQUAL_TO_PRECONDITION, lessThanOrEqualTo.preconditionPattern, lessThanOrEqualTo);
		set(GREATER_THAN_PRECONDITION, greaterThan.preconditionPattern, greaterThan);
		set(LESS_THAN_PRECONDITION, lessThan.preconditionPattern, lessThan);
		// Effect
		setRule(EFFECT, new Selection(
			CONDITIONAL_EFFECT, NONCONDITIONAL_EFFECT
		));
		setRule(CONDITIONAL_EFFECT,
			new Sentinel(CONDITIONAL_EFFECT_KEYWORD, PARAMETER_LIST_OPEN_BRACKET, BOOLEAN_EXPRESSION, PARAMETER_LIST_CLOSE_BRACKET, NONCONDITIONAL_EFFECT)
		);
		setBuilder(CONDITIONAL_EFFECT, (parser, tree, definitions) -> {
			Expression condition = parser.build(tree.get(0), Expression.class, definitions);
			Effect effect = parser.build(tree.get(1), Effect.class, definitions);
			return new Effect(condition, effect.fluent, effect.value);
		});
		setRule(NONCONDITIONAL_EFFECT, new Selection(
			new Sentinel(LOGICAL_OPEN_BRACKET, NONCONDITIONAL_EFFECT, LOGICAL_CLOSE_BRACKET), new Sentinel(Fluent.class, ASSIGNMENT_KEYWORD, Expression.class)
		));
		setBuilder(NONCONDITIONAL_EFFECT, (parser, tree, definitions) -> {
			return new Effect(parser.build(tree.get(0), Fluent.class, definitions), parser.build(tree.get(1), Expression.class, definitions));
		});
		// Problem
		setRule(PROBLEM,
			new List(DEFINITION, DEFINITION_SEPARATOR)
		);
		setBuilder(PROBLEM, (parser, tree, definitions) -> {
			// Universe
			Universe universe = new Universe();
			definitions.add(universe);
			for(ParseTree child : tree)
				if(child.is(TYPE_DEFINITION))
					definitions.add(universe = parser.build(child, Universe.class, definitions));
			for(ParseTree child : tree)
				if(child.is(ENTITY_DEFINITION))
					definitions.add(universe = parser.build(child, Universe.class, definitions));
			for(Type type : universe.types)
				definitions.add(type.name, type);
			for(Entity entity : universe.entities)
				definitions.add(entity.name, entity);
			// Properties
			ProblemBuilder builder = new ProblemBuilder("problem", universe);
			definitions.add(builder);
			for(ParseTree child : tree)
				if(child.is(PROPERTY_DEFINITION))
					parser.build(child, definitions);
			definitions.add(new Problem(builder));
			// Events
			for(ParseTree child : tree)
				if(child.is(EVENT_DEFINITION))
					parser.build(child, definitions);
			// Initial State
			for(ParseTree child : tree)
				if(child.is(INITIAL_STATE_DEFINITION))
					parser.build(child, definitions);
			// Utilities
			for(ParseTree child : tree)
				if(child.is(UTILITY_DEFINITION))
					parser.build(child, definitions);
			builder.setComment(comment(tree));
			return new Problem(builder);
		});
		setRule(DEFINITION, new Selection(
			TYPE_DEFINITION, ENTITY_DEFINITION, PROPERTY_DEFINITION, EVENT_DEFINITION, UTILITY_DEFINITION, INITIAL_STATE_DEFINITION
		));
		// Universe
		setRule(TYPE_DEFINITION, new Selection(
			new Sentinel(TYPE_DEFINITION_KEYWORD, Terminal.NAME),
			new Sentinel(TYPE_DEFINITION_KEYWORD, Terminal.NAME, INHERITANCE_KEYWORD, new List(Terminal.NAME, PARAMETER_SEPARATOR, 1, List.NO_LIMIT))
		));
		setBuilder(TYPE_DEFINITION, (parser, tree, definitions) -> {
			Universe universe = definitions.get(Universe.class);
			if(universe == null)
				universe = new Universe();
			UniverseBuilder builder = new UniverseBuilder(universe);
			String name = parser.build(tree.get(0), String.class);
			builder.defineType(name);
			for(int i=1; i<tree.size(); i++) {
				String parent = parser.build(tree.get(i), String.class);
				builder.defineType(parent);
				builder.addTypeRelationship(name, parent);
			}
			builder.setTypeComment(name, comment(tree));
			return builder.getUniverse();
		});
		setRule(ENTITY_DEFINITION,
			new Sentinel(ENTITY_DEFINITION_KEYWORD, Terminal.NAME, INHERITANCE_KEYWORD, new List(Terminal.NAME, PARAMETER_SEPARATOR, 1, List.NO_LIMIT))
		);
		setBuilder(ENTITY_DEFINITION, (parser, tree, definitions) -> {
			Universe universe = definitions.get(Universe.class);
			if(universe == null)
				universe = new Universe();
			UniverseBuilder builder = new UniverseBuilder(universe);
			String name = parser.build(tree.get(0), String.class);
			builder.defineEntity(name);
			for(int i=1; i<tree.size(); i++) {
				String type = parser.build(tree.get(i), String.class);
				builder.defineType(type);
				builder.addEntityType(name, type);
			}
			builder.setEntityComment(name, comment(tree));
			return builder.getUniverse();
		});
		// Signature
		setRule(SIGNATURE_DEFINITION,
			new Sentinel(Terminal.NAME, PARAMETER_LIST_OPEN_BRACKET, new List(PARAMETER_DEFINITION, PARAMETER_SEPARATOR), PARAMETER_LIST_CLOSE_BRACKET)
		);
		setBuilder(SIGNATURE_DEFINITION, (parser, tree, definitions) -> {
			String name = parser.build(tree.get(0), String.class);
			Parameter[] parameters = new Parameter[tree.size() - 1];
			for(int i=0; i<parameters.length; i++)
				parameters[i] = parser.build(tree.get(i + 1), Parameter.class, definitions);
			return new Signature(name, parameters);
		});
		setRule(PARAMETER_DEFINITION, new Selection(
			VARIABLE_DEFINITION, VALUE
		));
		setRule(VARIABLE_DEFINITION,
			new Sentinel(Terminal.NAME, INHERITANCE_KEYWORD, Type.class)
		);
		setBuilder(VARIABLE_DEFINITION, (parser, tree, definitions) -> {
			String name = parser.build(tree.get(0), String.class);
			Type type = parser.build(tree.get(1), Type.class, definitions);
			return new Variable(name, type);
		});
		// Property
		setRule(PROPERTY_DEFINITION,
			new Sentinel(FLUENT_DEFINITION_KEYWORD, SIGNATURE_DEFINITION, INHERITANCE_KEYWORD, Type.class)
		);
		setBuilder(PROPERTY_DEFINITION, (parser, tree, definitions) -> {
			Signature signature = parser.build(tree.get(0), Signature.class, definitions);
			Type type = parser.build(tree.get(1), Type.class, definitions);
			ProblemBuilder builder = definitions.require(ProblemBuilder.class);
			Fluent property = builder.defineFluent(signature, type, comment(tree));
			return property;
		});
		// Event
		setRule(EVENT_DEFINITION, new Selection(
			ACTION_DEFINITION, TRIGGER_DEFINITION
		));
		setRule(ACTION_DEFINITION,
			new Sentinel(ACTION_DEFINITION_KEYWORD, SIGNATURE_DEFINITION, EVENT_DEFINITION_OPEN_BRACKET, new List(ACTION_ELEMENT, DEFINITION_SEPARATOR), EVENT_DEFINITION_CLOSE_BRACKET)
		);
		setBuilder(ACTION_DEFINITION, new Builder() {
			@Override
			@SuppressWarnings("unchecked")
			public Object build(Parser parser, ParseTree tree, Definitions definitions) throws ParseException {
				ProblemBuilder builder = definitions.require(ProblemBuilder.class);
				Signature signature = parser.build(tree.get(0), Signature.class, definitions);
				for(Parameter parameter : signature.arguments)
					if(parameter instanceof Variable)
						definitions.add(((Variable) parameter).name, parameter);
				Expression precondition = True.TRUE;
				Expression effect = True.TRUE;
				ImmutableArray<Parameter> consenting = new ImmutableArray<>();
				Variable variable = new Variable(Settings.CHARACTER_TYPE_NAME, builder.getUniverse().getType(Settings.CHARACTER_TYPE_NAME));
				if(definitions.get(variable.name, Variable.class) != null)
					variable = Variable.generate(variable.name, variable.type);
				Mapping<Expression> observing = new Mapping.Function(variable, True.TRUE);
				for(int i=1; i<tree.size(); i++) {
					ParseTree part = tree.get(i);
					if(part.is(EVENT_PRECONDITION))
						precondition = parser.build(part, Expression.class, definitions);
					else if(part.is(EVENT_EFFECT))
						effect = parser.build(part, Expression.class, definitions);
					else if(part.is(ACTION_CONSENTING))
						consenting = parser.build(part, ImmutableArray.class, definitions);
					else if(part.is(ACTION_OBSERVING))
						observing = parser.build(part, Mapping.class, definitions);
				}
				return builder.defineAction(signature, precondition, effect, consenting, observing, comment(tree));
			}
		});
		setRule(ACTION_ELEMENT, new Selection(
			EVENT_PRECONDITION, EVENT_EFFECT, ACTION_CONSENTING, ACTION_OBSERVING
		));
		setRule(TRIGGER_DEFINITION,
			new Sentinel(TRIGGER_DEFINITION_KEYWORD, SIGNATURE_DEFINITION, EVENT_DEFINITION_OPEN_BRACKET, new List(TRIGGER_ELEMENT, DEFINITION_SEPARATOR), EVENT_DEFINITION_CLOSE_BRACKET)
		);
		setBuilder(TRIGGER_DEFINITION, (parser, tree, definitions) -> {
			ProblemBuilder builder = definitions.require(ProblemBuilder.class);
			Signature signature = parser.build(tree.get(0), Signature.class, definitions);
			for(Parameter parameter : signature.arguments)
				if(parameter instanceof Variable)
					definitions.add(((Variable) parameter).name, parameter);
			Expression precondition = True.TRUE;
			Expression effect = True.TRUE;
			for(int i=1; i<tree.size(); i++) {
				ParseTree part = tree.get(i);
				if(part.is(EVENT_PRECONDITION))
					precondition = parser.build(part, Expression.class, definitions);
				else if(part.is(EVENT_EFFECT))
					effect = parser.build(part, Expression.class, definitions);
			}
			return builder.defineTrigger(signature, precondition, effect, comment(tree));
		});
		setRule(TRIGGER_ELEMENT, new Selection(
			EVENT_PRECONDITION, EVENT_EFFECT
		));
		setRule(EVENT_PRECONDITION,
			new Sentinel(PRECONDITION_KEYWORD, DEFINITION_INDICATOR, BOOLEAN_EXPRESSION)
		);
		setRule(EVENT_EFFECT,
			new Sentinel(EFFECT_KEYWORD, DEFINITION_INDICATOR, BOOLEAN_EXPRESSION)
		);
		setRule(ACTION_CONSENTING,
			new Sentinel(CONSENTING_KEYWORD, DEFINITION_INDICATOR, CHARACTER_LIST)
		);
		setRule(CHARACTER_LIST, new Selection(
			new Sentinel(PARAMETER_LIST_OPEN_BRACKET, CHARACTER_LIST, PARAMETER_LIST_CLOSE_BRACKET), new List(CHARACTER_PARAMETER, PARAMETER_SEPARATOR)
		));
		setBuilder(CHARACTER_LIST, (parser, tree, definitions) -> {
			Parameter[] characters = new Parameter[tree.size()];
			for(int i=0; i<characters.length; i++)
				characters[i] = parser.build(tree.get(i), Parameter.class, definitions);
			return new ImmutableArray<>(characters);
		});
		setRule(ACTION_OBSERVING,
			new Sentinel(OBSERVING_KEYWORD, PARAMETER_LIST_OPEN_BRACKET, VARIABLE_DEFINITION, PARAMETER_LIST_CLOSE_BRACKET, DEFINITION_INDICATOR, BOOLEAN_EXPRESSION)
		);
		setBuilder(ACTION_OBSERVING, (parser, tree, definitions) -> {
			Variable variable = parser.build(tree.get(0), Variable.class, definitions);
			definitions.add(variable.name, variable);
			return new Mapping.Function(variable, parser.build(tree.get(1), Expression.class, definitions));
		});
		// Utility
		setRule(UTILITY_DEFINITION, new Selection(
			AUTHOR_UTILITY_DEFINITION, CHARACTER_UTILITY_DEFINITION
		));
		setRule(AUTHOR_UTILITY_DEFINITION,
			new Sentinel(UTILITY_DEFINITION_KEYWORD, PARAMETER_LIST_OPEN_BRACKET, PARAMETER_LIST_CLOSE_BRACKET, DEFINITION_INDICATOR, Expression.class)
		);
		setBuilder(AUTHOR_UTILITY_DEFINITION, (parser, tree, definitions) -> {
			return definitions.get(ProblemBuilder.class).setUtility(parser.build(tree.get(0), Expression.class, definitions));
		});
		setRule(CHARACTER_UTILITY_DEFINITION,
			new Sentinel(UTILITY_DEFINITION_KEYWORD, PARAMETER_LIST_OPEN_BRACKET, CHARACTER, PARAMETER_LIST_CLOSE_BRACKET, DEFINITION_INDICATOR, Expression.class)
		);
		setBuilder(CHARACTER_UTILITY_DEFINITION, (parser, tree, definitions) -> {
			return definitions.get(ProblemBuilder.class).setUtility(parser.build(tree.get(0), Character.class, definitions), parser.build(tree.get(1), Expression.class, definitions));
		});
		// Initial State
		setRule(INITIAL_STATE_DEFINITION, EXPRESSION);
		setBuilder(INITIAL_STATE_DEFINITION, (parser, tree, definitions) -> {
			return definitions.get(ProblemBuilder.class).addToInitialState(parser.build(tree.get(0), Expression.class, definitions));
		});
		// Event
		setRule(EVENT, new Selection(ACTION, TRIGGER));
		setRule(ACTION,
			SIGNATURE
		);
		setBuilder(ACTION, (parser, tree, definitions) -> {
			Signature signature = parser.build(tree.get(0), Signature.class, definitions);
			return definitions.require(Problem.class).getAction(signature);
		});
		setRule(TRIGGER,
			SIGNATURE
		);
		setBuilder(TRIGGER, (parser, tree, definitions) -> {
			Signature signature = parser.build(tree.get(0), Signature.class, definitions);
			return definitions.require(Problem.class).getTrigger(signature);
		});
		// Plan
		setRule(PLAN, new Selection(
			HEAD_PLAN
		));
		setRule(HEAD_PLAN, new Selection(
			EMPTY_HEAD_PLAN, NON_EMPTY_HEAD_PLAN
		));
		setRule(EMPTY_HEAD_PLAN,
			Terminal.NOTHING
		);
		setBuilder(EMPTY_HEAD_PLAN, (parser, tree, definitions) -> {
			return HeadPlan.EMPTY;
		});
		setRule(NON_EMPTY_HEAD_PLAN,
			new Sentinel(Terminal.NAME, PARAMETER_LIST_OPEN_BRACKET, new List(PARAMETER, PARAMETER_SEPARATOR), PARAMETER_LIST_CLOSE_BRACKET, HEAD_PLAN)
		);
		setBuilder(NON_EMPTY_HEAD_PLAN, (parser, tree, definitions) -> {
			@SuppressWarnings("unchecked")
			HeadPlan<Action> plan = parser.build(tree.get(tree.size() - 1), HeadPlan.class, definitions);
			String name = parser.build(tree.get(0), String.class, definitions);
			Parameter[] arguments = new Parameter[tree.size() - 2];
			for(int i=0; i<arguments.length; i++)
				arguments[i] = parser.build(tree.get(i + 1), Parameter.class, definitions);
			Action action = definitions.require(Problem.class).getAction(new Signature(name, arguments));
			return plan.prepend(action);
		});
		// Solution
		setRule(SOLUTION, new Selection(
			SOLUTION_PLAN
		));
		setRule(SOLUTION_GOAL, new Selection(
			SOLUTION_AUTHOR_GOAL, SOLUTION_CHARACTER_GOAL
		));
		setRule(SOLUTION_AUTHOR_GOAL,
			new Sentinel(GOAL_KEYWORD, PARAMETER_LIST_OPEN_BRACKET, BOOLEAN_EXPRESSION, PARAMETER_LIST_CLOSE_BRACKET)
		);
		setBuilder(SOLUTION_AUTHOR_GOAL, (parser, tree, definitions) -> {
			Expression goal = parser.build(tree.get(0), Expression.class, definitions);
			return new SolutionGoal<>(goal);
		});
		setRule(SOLUTION_CHARACTER_GOAL,
			new Sentinel(GOAL_KEYWORD, PARAMETER_LIST_OPEN_BRACKET, CHARACTER, PARAMETER_SEPARATOR, BOOLEAN_EXPRESSION, PARAMETER_LIST_CLOSE_BRACKET)
		);
		setBuilder(SOLUTION_CHARACTER_GOAL, (parser, tree, definitions) -> {
			Character character = parser.build(tree.get(0), Character.class, definitions);
			Expression goal = parser.build(tree.get(1), Expression.class, definitions);
			return new SolutionGoal<>(character, goal);
		});
		setRule(SOLUTION_TAIL, new Selection(
			SOLUTION_AUTHOR_GOAL, SOLUTION_CHARACTER_GOAL_WITH_TAIL, SOLUTION_PLAN, SOLUTION_BRANCH
		));
		setRule(SOLUTION_CHARACTER_GOAL_WITH_TAIL,
			new Sentinel(GOAL_KEYWORD, PARAMETER_LIST_OPEN_BRACKET, CHARACTER, PARAMETER_SEPARATOR, BOOLEAN_EXPRESSION, PARAMETER_LIST_CLOSE_BRACKET, SOLUTION_TAIL)
		);
		setRule(SOLUTION_PLAN,
			new Sentinel(Terminal.NAME, PARAMETER_LIST_OPEN_BRACKET, new List(PARAMETER, PARAMETER_SEPARATOR), PARAMETER_LIST_CLOSE_BRACKET, SOLUTION_TAIL)
		);
		setRule(SOLUTION_BRANCH,
			new Sentinel(BRANCH_INDENT_KEYWORD, SOLUTION_TAIL)
		);
		setBuilder(SOLUTION, (parser, tree, definitions) -> {
			return parseSolution(parser, tree, definitions);
		});
	}
	
	private final void set(NonTerminal symbol, Pattern pattern, Builder builder) {
		setRule(symbol, pattern);
		setBuilder(symbol, builder);
	}
	
	private final String comment(ParseTree tree) {
		if(tree.tokens.first.comment == null)
			return "";
		else
			return tree.tokens.first.comment;
	}
	
	private final Solution<Action> parseSolution(Parser parser, ParseTree tree, Definitions definitions) throws ParseException {
		return buildSolution(parser, clipSolution(tree.tokens), definitions);
	}
	
	private final ArrayList<ImmutableList<Token>> clipSolution(ImmutableList<Token> tokens) {
		ArrayList<ImmutableList<Token>> clips = new ArrayList<>();
		while(tokens.size() > 0) {
			int length = 0;
			int nesting = 0;
			while(true) {
				if(tokens.get(length).value.equals(PARAMETER_LIST_OPEN_BRACKET))
					nesting++;
				else if(tokens.get(length).value.equals(PARAMETER_LIST_CLOSE_BRACKET)) {
					nesting--;
					if(nesting == 0)
						break;
				}
				length++;
			}
			length++;
			clips.add(0, Pattern.clip(tokens, length));
			for(int i=0; i<length; i++)
				tokens = tokens.rest;
		}
		return clips;
	}
	
	@SuppressWarnings("unchecked")
	private final Solution<Action> buildSolution(Parser parser, ArrayList<ImmutableList<Token>> clips, Definitions definitions) throws ParseException {
		Solution<Action> solution = parser.build(parser.match(SOLUTION_GOAL, clips.remove(0)), Solution.class, definitions);
		ArrayList<Solution<Action>> explanations = new ArrayList<>();
		ArrayList<ImmutableList<Token>> nested = new ArrayList<>();
		for(ImmutableList<Token> clip : clips) {
			if(clip.first.value.equals(BRANCH_INDENT_KEYWORD)) {
				clip = clip.rest;
				if(clip.first.value.equals(GOAL_KEYWORD) && nested.size() > 0) {
					explanations.add(buildSolution(parser, nested, definitions));
					nested.clear();
				}
				nested.add(clip);
			}
			else {
				if(nested.size() > 0) {
					explanations.add(buildSolution(parser, nested, definitions));
					nested.clear();
				}
				Action action = parser.build(parser.match(ACTION, clip), Action.class, definitions);
				solution = solution.prepend(action);
				for(Solution<Action> explanation : explanations)
					solution = solution.setExplanation(explanation.prepend(action));
				explanations.clear();
			}
		}
		return solution;
	}
}