package edu.uky.cs.nil.sabre.logic;

import java.io.Serializable;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.Number;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.io.DefaultParser;
import edu.uky.cs.nil.sabre.util.ImmutableSet;
import edu.uky.cs.nil.sabre.util.Unique;

/**
 * An arithmetic expansion is a {@link Numeric numeric expression} which
 * represents iteratively applying an {@link Arithmetic.Operator arithmetic
 * operator} to a series of numeric expressions defined by every possible
 * value that can be substituted for a {@link #variable variable}.
 * 
 * @author Stephen G. Ware
 */
public class ArithmeticExpansion implements Numeric {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;

	/**
	 * An arithmetic expansion operator is a singleton object that specifies
	 * how an {@link Arithmetic.Operator arithmetic operator} should be applied
	 * to a series of numeric expressions.
	 * 
	 * @author Stephen G. Ware
	 */
	public static class Operator implements Comparable<Operator>, Serializable, Unique {
		
		/** Serial version ID */
		private static final long serialVersionUID = ArithmeticExpansion.serialVersionUID;
		
		/**
		 * The arithmetic operator to be applied to the series of numeric
		 * expressions
		 */
		public final Arithmetic.Operator operation;
		
		/**
		 * The number to be returned if the series contains zero numeric
		 * expressions
		 */
		public final Number identity;
		
		private Operator(Arithmetic.Operator operation, Number identity) {
			this.operation = operation;
			this.identity = identity;
		}
		
		@Override
		public int compareTo(Operator other) {
			return hashCode() - other.hashCode();
		}
		
		/**
		 * Returns a {@link Numeric numeric expression} which is logically
		 * equivalent to an {@link ArithmeticExpansion arithmetic expansion}
		 * with this operator but which does not contain this operator or
		 * variable.
		 * 
		 * @param variable a variable which appears in the expression and for
		 * which every possible value will be substituted
		 * @param argument a numeric expression which contains the variable and
		 * which will be used to create the series of numeric expressions
		 * @return a numeric expression logically equivalent to an arithmetic
		 * expansion using this operator but which does not contain this
		 * operator or the variable
		 */
		public Expression expand(Variable variable, Expression argument) {
			ImmutableSet<Value> values = variable.type.universe.getValues(variable.type);
			if(values.size() == 0)
				return identity;
			Expression expanded = (Expression) argument.substitute(variable, values.get(values.size() - 1));
			for(int i=values.size()-2; i>=0; i--)
				expanded = new Arithmetic(operation, (Expression) argument.substitute(variable, values.get(i)), expanded);
			return expanded;
		}
	}
	
	/**
	 * Iteratively applies the {@link Arithmetic#ADD addition operator} to a
	 * series of numeric expressions.
	 */
	public static final Operator SUM = new Operator(Arithmetic.ADD, Number.ZERO) {

		/** Serial version ID */
		private static final long serialVersionUID = 1;

		@Override
		public int hashCode() {
			return 0;
		}
		
		@Override
		public String toString() {
			return DefaultParser.SUM_KEYWORD;
		}
	};
	
	/**
	 * Iteratively applies the {@link Arithmetic#MULTIPLY multiplication
	 * operator} to a series of numeric expressions.
	 */
	public static final Operator PRODUCT = new Operator(Arithmetic.MULTIPLY, Number.ONE) {

		/** Serial version ID */
		private static final long serialVersionUID = 1;

		@Override
		public int hashCode() {
			return 1;
		}
		
		@Override
		public String toString() {
			return DefaultParser.PRODUCT_KEYWORD;
		}
	};
	
	/**
	 * The {@link Operator arithmetic expansion operator} to be applied to the
	 * series of numeric expressions
	 */
	public final Operator operator;
	
	/** A variable appearing in the argument */
	public final Variable variable;
	
	/**
	 * A numeric expression which will be used to create the series of numeric
	 * expressions
	 */
	public final Expression argument;
	
	/**
	 * Constructs a new arithmetic expansion.
	 * 
	 * @param operator the arithmetic expansion operator to be applied to the
	 * series of numeric expressions
	 * @param variable a variable appearing in the argument
	 * @param argument a numeric expression which will be used to generate the
	 * series of numeric expressions
	 */
	public ArithmeticExpansion(Operator operator, Variable variable, Expression argument) {
		this.operator = operator;
		this.variable = variable;
		argument.mustBeNumber();
		this.argument = argument;
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			ArithmeticExpansion otherExpanation = (ArithmeticExpansion) other;
			return operator.equals(otherExpanation.operator) && variable.equals(otherExpanation.variable) && argument.equals(otherExpanation.argument);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), operator, variable, argument);
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(other.getClass())) {
			ArithmeticExpansion otherExpanation = (ArithmeticExpansion) other;
			return Utilities.compare(operator, otherExpanation.operator, variable, otherExpanation.variable, argument, otherExpanation.argument);
		}
		else
			return Numeric.super.compareTo(other);
	}
	
	@Override
	public boolean isGround() {
		return false;
	}

	@Override
	public ArithmeticExpansion apply(Function<Object, Object> function) {
		Variable variable = (Variable) function.apply(this.variable);
		Expression argument = (Expression) function.apply(this.argument);
		if(variable != this.variable || argument != this.argument)
			return new ArithmeticExpansion(operator, variable, argument);
		else
			return this;
	}
	
	@Override
	public ArithmeticExpansion simplify() {
		return (ArithmeticExpansion) Numeric.super.simplify();
	}

	/**
	 * First generates a series of numeric expressions by substituting every
	 * possible value for this expansion's {@link #variable variable} into this
	 * expansion's {@link #argument argument} and then applies this expansion's
	 * {@link #operator operator's} {@link Operator#operation arithmetic
	 * operator} to the series of expressions, returning the resulting value.
	 * 
	 * @param state the state in which this expression will be evaluated
	 * @return the numeric value of this expression
	 */
	@Override
	public Value evaluate(State state) {
		return expand().evaluate(state);
	}
	
	@Override
	public ArithmeticExpansion prepend(Parameter character) {
		return new ArithmeticExpansion(operator, variable, new Epistemic(character, argument));
	}
	
	@Override
	public Conditional<Disjunction<Clause<Precondition>>> toValued() {
		return expand().toValued();
	}
	
	/**
	 * Returns a {@link Numeric numeric expression} which is logically
	 * equivalent to this expansion but which does not contain this expansion's
	 * operator or variable.
	 * 
	 * @return a logically equivalent numeric expression
	 */
	public Expression expand() {
		return operator.expand(variable, argument);
	}
}