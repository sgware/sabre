package edu.uky.cs.nil.sabre.logic;

import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Number;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.io.DefaultParser;
import edu.uky.cs.nil.sabre.util.Unique;

/**
 * An arithmetic expression is a {@link Numeric numeric expression} whose value
 * if the results of applying {@link Operator an arithmetic operation} to two
 * numeric values.
 * 
 * @author Stephen G. Ware
 */
public class Arithmetic implements Numeric {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * An arithmetic operator is a singleton object that specifies how to
	 * combine two numeric input values into a numeric output value.
	 * 
	 * @author Stephen G. Ware
	 */
	public static abstract class Operator implements Comparable<Operator>, Serializable, Unique {
		
		/** Serial version ID */
		private static final long serialVersionUID = Arithmetic.serialVersionUID;
		
		private Operator() {}
		
		@Override
		public int compareTo(Operator other) {
			return hashCode() - other.hashCode();
		}
		
		/**
		 * Returns the result of applying this operator to two input values.
		 * If both values are numbers, this method calls {@link
		 * #calculate(Number, Number)}, otherwise this method returns {@link
		 * Unknown unknown}.
		 * 
		 * @param left the value on the left of the arithmetic operator
		 * @param right the value on the right of the arithmetic operator
		 * @return the result of applying the arithmetic operation to the
		 * values
		 */
		public Value calculate(Value left, Value right) {
			if(left instanceof Number && right instanceof Number)
				return calculate((Number) left, (Number) right);
			else
				return Unknown.UNKNOWN;
		}
		
		/**
		 * This method is called from {@link #calculate(Value, Value)} when
		 * both values are {@link Number numbers} and returns the result of
		 * applying this operator to two numbers.
		 * 
		 * @param left the number on the left of the arithmetic operator
		 * @param right the number on the right of the arithmetic operator
		 * @return the result of applying the arithmetic operation to the
		 * numbers
		 */
		protected abstract Number calculate(Number left, Number right);
		
		/**
		 * If an {@link Arithmetic arithmetic expression} using this operator
		 * and the given left and right values can be simplified (for example,
		 * if the left and right values are both {@link Value values}), this
		 * method returns the simplified expression. Otherwise, this method
		 * returns null.
		 * 
		 * @param left the {@link Arithmetic#left left side} of an 
		 * arithmetic expression using this operator
		 * @param right the {@link Arithmetic#right right side} of an
		 * arithmetic expression using this operator
		 * @return a simplified logical expression or null if the expression
		 * cannot be simplified
		 */
		public Expression simplify(Expression left, Expression right) {
			if(left instanceof Value && right instanceof Value)
				return calculate((Value) left, (Value) right);
			else
				return null;
		}
		
		/**
		 * Isolates a query {@link Fluent fluent} on the left side of a {@link
		 * Comparison comparison} which has an arithmetic expression using this
		 * operator {@link Comparison#left on its left}.
		 * 
		 * @param query the fluent that should be isolated
		 * @param left the {@link Arithmetic#left left side of the arithmetic
		 * expression} which appears on the left side of the comparison
		 * @param right the {@link Arithmetic#right right side of the
		 * arithmetic expression} which appear on the left side of the
		 * comparison
		 * @param inequality the {@link Comparison.Operator comparison
		 * operator} used in the {@link Comparison comparison}
		 * @param rhs the {@link Comparison#right right side of the comparison}
		 * @return a comparison with the flight as its left side
		 */
		protected Expression isolate(Fluent query, Expression left, Expression right, Comparison.Operator inequality, Expression rhs) {
			throw Exceptions.cannotIsolate(query, new Comparison(inequality, new Arithmetic(this, left, right), rhs));
		}
	}
	
	/**
	 * An {@link Operator arithmetic operator} for adding the left and right
	 * sides of an {@link Arithmetic arithmetic expression}.
	 */
	public static final Operator ADD = new Operator() {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1;
		
		@Override
		public int hashCode() {
			return 0;
		}
		
		@Override
		public String toString() {
			return DefaultParser.ADDITION_KEYWORD;
		}

		/**
		 * Adds two numbers.
		 * 
		 * @param left the number on the left
		 * @param right the number on the right
		 * @return the result of adding the numbers
		 */
		@Override
		protected Number calculate(Number left, Number right) {
			return left.add(right);
		}

		@Override
		public Expression simplify(Expression left, Expression right) {
			if(left.equals(Number.ZERO))
				return right;
			else if(right.equals(Number.ZERO))
				return left;
			else
				return super.simplify(left, right);
		}
		
		@Override
		protected Expression isolate(Fluent query, Expression left, Expression right, Comparison.Operator inequality, Expression rhs) {
			if(left.occurs(query))
				return new Comparison(inequality, left, new Arithmetic(SUBTRACT, rhs, right)).isolate(query);
			else if(right.occurs(query))
				return new Comparison(inequality, new Arithmetic(this, right, left), rhs).isolate(query);
			else
				return super.isolate(query, left, right, inequality, rhs);
		}
		
		private Object readResolve() throws ObjectStreamException {
			return ADD;
		}
	};
	
	/**
	 * An {@link Operator arithmetic operator} for subtracting the right side
	 * of an {@link Arithmetic arithmetic expression} from the left.
	 */
	public static final Operator SUBTRACT = new Operator() {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1;
		
		@Override
		public int hashCode() {
			return 1;
		}
		
		@Override
		public String toString() {
			return DefaultParser.SUBTRACTION_KEYWORD;
		}

		/**
		 * Subtracts the right number from the left.
		 * 
		 * @param left the number on the left
		 * @param right the number on the right
		 * @return the result of subtracting the right from the left
		 */
		@Override
		public Number calculate(Number left, Number right) {
			return left.subtract(right);
		}
		
		@Override
		public Expression simplify(Expression left, Expression right) {
			if(right.equals(Number.ZERO))
				return left;
			else
				return super.simplify(left, right);
		}
		
		@Override
		protected Expression isolate(Fluent query, Expression left, Expression right, Comparison.Operator inequality, Expression rhs) {
			if(left.occurs(query))
				return new Comparison(inequality, left, new Arithmetic(ADD, rhs, right)).isolate(query);
			else if(right.occurs(query))
				return new Comparison(inequality, left, new Arithmetic(ADD, right, rhs)).isolate(query);
			else
				return super.isolate(query, left, right, inequality, rhs);
		}
		
		private Object readResolve() throws ObjectStreamException {
			return SUBTRACT;
		}
	};
	
	/**
	 * An {@link Operator arithmetic operator} for multiplying the left and
	 * right sides of an {@link Arithmetic arithmetic expression}.
	 */
	public static final Operator MULTIPLY = new Operator() {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1;
		
		@Override
		public int hashCode() {
			return 2;
		}
		
		@Override
		public String toString() {
			return DefaultParser.MULTIPLICATION_KEYWORD;
		}

		/**
		 * Multiplies two numbers.
		 * 
		 * @param left the number on the left
		 * @param right the number on the right
		 * @return the result of multiplying the numbers
		 */
		@Override
		public Number calculate(Number left, Number right) {
			return left.multiply(right);
		}
		
		@Override
		public Expression simplify(Expression left, Expression right) {
			if(left.equals(Number.ZERO) || right.equals(Number.ZERO))
				return Number.ZERO;
			else if(left.equals(Number.ONE))
				return right;
			else if(right.equals(Number.ONE))
				return left;
			else
				return super.simplify(left, right);
		}
		
		@Override
		protected Expression isolate(Fluent query, Expression left, Expression right, Comparison.Operator inequality, Expression rhs) {
			if(left.occurs(query)) {
				return new Disjunction<>(
					new Conjunction<>(
						new Comparison(inequality, left, new Arithmetic(DIVIDE, rhs, right)).isolate(query),
						new Comparison(Comparison.GREATER_THAN, right, Number.ZERO)
					),
					new Conjunction<>(
						new Comparison(inequality.flip(), left, new Arithmetic(DIVIDE, rhs, right)).isolate(query),
						new Comparison(Comparison.LESS_THAN, right, Number.ZERO)
					)
				);
			}
			else if(right.occurs(query))
				return new Comparison(inequality, new Arithmetic(this, right, left), rhs).isolate(query);
			else
				return super.isolate(query, left, right, inequality, rhs);
		}
		
		private Object readResolve() throws ObjectStreamException {
			return MULTIPLY;
		}
	};
	
	/**
	 * An {@link Operator arithmetic operator} for dividing the left side of
	 * an {@link Arithmetic arithmetic expression} by the right.
	 */
	public static final Operator DIVIDE = new Operator() {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1;
		
		@Override
		public int hashCode() {
			return 3;
		}
		
		@Override
		public String toString() {
			return DefaultParser.DIVISION_KEYWORD;
		}

		/**
		 * Divides the left number by the right.
		 * 
		 * @param left the number on the left
		 * @param right the number on the right
		 * @return the result of dividing the numbers
		 */
		@Override
		public Number calculate(Number left, Number right) {
			return left.divide(right);
		}
		
		@Override
		public Expression simplify(Expression left, Expression right) {
			if(left.equals(Number.ZERO))
				return Number.ZERO;
			if(right.equals(Number.ONE))
				return left;
			else
				return super.simplify(left, right);
		}
		
		@Override
		protected Expression isolate(Fluent query, Expression left, Expression right, Comparison.Operator inequality, Expression rhs) {
			if(left.occurs(query)) {
				return new Disjunction<>(
					new Conjunction<>(
						new Comparison(inequality, left, new Arithmetic(MULTIPLY, rhs, right)).isolate(query),
						new Comparison(Comparison.GREATER_THAN, right, Number.ZERO)
					),
					new Conjunction<>(
						new Comparison(inequality.flip(), left, new Arithmetic(MULTIPLY, rhs, right)).isolate(query),
						new Comparison(Comparison.LESS_THAN, right, Number.ZERO)
					)
				);
			}
			else if(right.occurs(query)) {
				return new Disjunction<>(
					new Conjunction<>(
						new Comparison(inequality, left, new Arithmetic(MULTIPLY, right, rhs)).isolate(query),
						new Comparison(Comparison.GREATER_THAN, right, Number.ZERO)
					),
					new Conjunction<>(
						new Comparison(inequality.flip(), left, new Arithmetic(MULTIPLY, right, rhs)).isolate(query),
						new Comparison(Comparison.LESS_THAN, right, Number.ZERO)
					)
				);
			}
			else
				return super.isolate(query, left, right, inequality, rhs);
		}
		
		private Object readResolve() throws ObjectStreamException {
			return DIVIDE;
		}
	};
	
	/**
	 * The {@link Operator arithmetic operator} to apply to the left and right
	 * sides
	 */
	public final Operator operator;
	
	/** The left numeric expression */
	public final Expression left;
	
	/** The right numeric expression */
	public final Expression right;
	
	/**
	 * Constructs a new arithmetic expression.
	 * 
	 * @param operator the arithmetic operator to apply to the left and right
	 * sides
	 * @param left the numeric expression on the left
	 * @param right the numeric expression on the right
	 * @throws edu.uky.cs.nil.sabre.FormatException if either side is not of
	 * type {@code number}
	 */
	public Arithmetic(Operator operator, Expression left, Expression right) {
		this.operator = operator;
		left.mustBeNumber();
		this.left = left;
		right.mustBeNumber();
		this.right = right;
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			Arithmetic otherArithmetic = (Arithmetic) other;
			return operator.equals(otherArithmetic.operator) && left.equals(otherArithmetic.left) && right.equals(otherArithmetic.right);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), operator, left, right);
	}
	
	@Override
	public String toString() {
		return "(" + left + " " + operator + " " + right + ")";
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(other.getClass())) {
			Arithmetic otherArithmetic = (Arithmetic) other;
			return Utilities.compare(operator, otherArithmetic.operator, left, otherArithmetic.left, right, otherArithmetic.right);
		}
		else
			return Numeric.super.compareTo(other);
	}

	@Override
	public boolean isGround() {
		return left.isGround() && right.isGround();
	}

	@Override
	public Arithmetic apply(Function<Object, Object> function) {
		Expression left = (Expression) function.apply(this.left);
		Expression right = (Expression) function.apply(this.right);
		if(left != this.left || right != this.right)
			return new Arithmetic(operator, left, right);
		else
			return this;
	}
	
	@Override
	public Expression simplify() {
		Arithmetic simplified = (Arithmetic) Numeric.super.simplify();
		Expression further = operator.simplify(simplified.left, simplified.right);
		if(further == null)
			return simplified;
		else
			return further;
	}
	
	@Override
	public boolean isValued() {
		return left.isValued() && right.isValued();
	}

	/**
	 * First {@link Expression#evaluate(State) evaluates} the left and right
	 * side of this expression and then applies this expression's {@link
	 * #operator operator} to the resulting numbers.
	 * 
	 * @param state the state in which this arithmetic expression will be
	 * evaluated
	 * @return a {@link Number number} or {@link Unknown unknown}
	 */
	@Override
	public Value evaluate(State state) {
		return operator.calculate(left.evaluate(state), right.evaluate(state));
	}
	
	@Override
	public Arithmetic prepend(Parameter character) {
		return (Arithmetic) Numeric.super.prepend(character);
	}
	
	/**
	 * Isolates a query {@link Fluent fluent} on the left side of a comparison
	 * which has this arithmetic expression on its left.
	 * 
	 * @param query the fluent that should be isolated
	 * @param inequality the {@link Comparison.Operator comparison operator} of
	 * the comparison which has this arithmetic expression on its left
	 * @param rhs the {@link Comparison#right right side} of the comparison
	 * @return a comparison with the fluent as its left side
	 */
	final Expression isolate(Fluent query, Comparison.Operator inequality, Expression rhs) {
		return operator.isolate(query, left, right, inequality, rhs);
	}
}