package edu.uky.cs.nil.sabre.logic;

import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Number;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.io.DefaultParser;
import edu.uky.cs.nil.sabre.util.ImmutableArray;
import edu.uky.cs.nil.sabre.util.Unique;

/**
 * A comparison is a {@link Proposition proposition} which is true if and only
 * if a specific {@link Operator relationship} exists between a {@link #left
 * left expression} and a {@link #right right expression}.
 * 
 * @author Stephen G. Ware
 */
public class Comparison implements Proposition {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * An operator is a singleton object that represents one of the possible
	 * binary relationship between the left and right expressions of a {@link
	 * Comparison comparison}.
	 * 
	 * @author Stephen G. Ware
	 */
	public static abstract class Operator implements Comparable<Operator>, Serializable, Unique {
		
		/** Serial version ID */
		private static final long serialVersionUID = Comparison.serialVersionUID;
		
		private Operator() {}
		
		@Override
		public int compareTo(Operator other) {
			return hashCode() - other.hashCode();
		}
		
		/**
		 * Tests whether the relationship represented by this operator holds
		 * between two {@link Value values} which appear on the left and right
		 * of a {@link Comparison comparison}.
		 * 
		 * @param left the value on the left of the comparison
		 * @param right the value on the right of the comparison
		 * @return true if the relationship holds, false otherwise
		 */
		public abstract boolean test(Value left, Value right);
		
		/**
		 * Returns the operator this operator would become when {@link
		 * Comparison#flip() flipping} a comparison in which it appears.
		 * 
		 * @return the flipped operator
		 */
		public abstract Operator flip();
		
		/**
		 * Returns the operator this operator would become when {@link
		 * Comparison#negate() negating} a comparison in which it appears.
		 * 
		 * @return the negated operator
		 */
		public abstract Operator negate();
		
		/**
		 * If a {@link Comparison comparison} using this operator with the
		 * given left and right expressions can be simplified, this method
		 * returns the simplified expression; otherwise it returns null. The
		 * most common case in which a comparison can be simplified is when
		 * its left and right expressions are both {@link Value values}, in
		 * which case this method returns {@link True true} if {@link
		 * #test(Value, Value) this operator's test method} returns true, and
		 * {@link False false} if this operator's test method returns false.
		 * 
		 * @param left the left expression of a comparison
		 * @param right the right expression of a comparison
		 * @return a simplified expression or null if the comparison cannot be
		 * simplified
		 */
		public Expression simplify(Expression left, Expression right) {
			if(left instanceof Value && right instanceof Value) {
				if(test((Value) left, (Value) right))
					return True.TRUE;
				else
					return False.FALSE;
			}
			else
				return null;
		}
		
		/**
		 * If a {@link Precondition} (that is, an {@link Atom atomic} {@link
		 * Comparison comparison}) using this operator can be {@link
		 * Atom#combine(Atom) combined} with a second precondition, this method
		 * returns the resulting precondition, or null if they cannot be
		 * combined. This method assumes the {@link Comparison#left left sides}
		 * of both preconditions are the same. If the operators of the two
		 * preconditions are different, this method should be called twice,
		 * once for the first precondition's operator and once for the second
		 * precondition's operator, and only one of those methods needs to
		 * return the combined precondition if it exists.
		 * 
		 * @param self the first precondition
		 * @param other the second precondition with matching left side
		 * @return a combined precondition which represents both, if one
		 * exists, or null if one does not exist
		 */
		protected Precondition combine(Precondition self, Precondition other) {
			if(self.equals(other))
				return self;
			else
				return null;
		}
		
		/**
		 * Tests whether a {@link Precondition} (that is, an {@link Atom
		 * atomic} {@link Comparison comparison}) using this operator {@link
		 * Atom#negates(Atom) negates} a second precondition. This method
		 * assumes the {@link Comparison#left left sides} of both preconditions
		 * are the same. If the operators of the two preconditions are
		 * different, this method should be called twice, once for the first
		 * precondition's operator and once for the second precondition's
		 * operator, and only one of those methods needs to return true if the
		 * preconditions negate one another.
		 * 
		 * @param self the first precondition
		 * @param other the second precondition with matching left side
		 * @return true if the preconditions negate one another, false
		 * otherwise
		 */
		protected boolean negates(Precondition self, Precondition other) {
			return false;
		}
	}
	
	/**
	 * Represents a {@link Comparison} where the left and right are the same.
	 */
	public static final Operator EQUAL_TO = new Operator() {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1;

		@Override
		public int hashCode() {
			return 0;
		}
		
		@Override
		public String toString() {
			return DefaultParser.EQUAL_TO_KEYWORD;
		}

		/**
		 * Returns true if and only if the left and right values are the same.
		 * 
		 * @param left the value on the left of the comparison
		 * @param right the value on the right of the comparison
		 * @return true if the values are the same, false otherwise
		 */
		@Override
		public boolean test(Value left, Value right) {
			return left.equals(right);
		}
		
		/**
		 * Returns {@link Comparison#EQUAL_TO}.
		 * 
		 * @returns {@link Comparison#EQUAL_TO}
		 */
		@Override
		public Operator flip() {
			return EQUAL_TO;
		}
		
		/**
		 * Returns {@link Comparison#NOT_EQUAL_TO}.
		 * 
		 * @returns {@link Comparison#NOT_EQUAL_TO}
		 */
		@Override
		public Operator negate() {
			return NOT_EQUAL_TO;
		}
		
		/**
		 * If the left and right expressions are {@link Object#equals(Object)
		 * equal to} each other, this method returns {@link True true}. If the
		 * left and right expressions are different {@link Value values}, this
		 * method returns {@link False false}. Otherwise, this method returns
		 * null.
		 * 
		 * @param left the left expression of an equal to comparison
		 * @param right the right expression of an equal to comparison
		 * @return true if the left and right must be the same, false if they
		 * cannot be the same, or null if the comparison cannot be simplified
		 */
		@Override
		public Expression simplify(Expression left, Expression right) {
			if(left.equals(right))
				return True.TRUE;
			else
				return super.simplify(left, right);
		}
		
		@Override
		protected Precondition combine(Precondition self, Precondition other) {
			if(other.operator.equals(NOT_EQUAL_TO) && self.right instanceof Value && other.right instanceof Value && !self.right.equals(other.right))
				return self;
			else if(other.operator.equals(GREATER_THAN_OR_EQUAL_TO) && self.right.equals(other.right))
				return self;
			else if(other.operator.equals(LESS_THAN_OR_EQUAL_TO) && self.right.equals(other.right))
				return self;
			else
				return super.combine(self, other);
		}
		
		@Override
		protected boolean negates(Precondition self, Precondition other) {
			if(other.operator.equals(EQUAL_TO))
				return self.right instanceof Value && other.right instanceof Value && !self.right.equals(other.right);
			else if(other.operator.equals(NOT_EQUAL_TO))
				return self.right.equals(other.right);
			else
				return super.negates(self, other);
		}
		
		private Object readResolve() throws ObjectStreamException {
			return EQUAL_TO;
		}
	};
	
	/**
	 * Represents a {@link Comparison} where the left and right are not the
	 * same.
	 */
	public static final Operator NOT_EQUAL_TO = new Operator() {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1;
		
		@Override
		public int hashCode() {
			return 1;
		}
		
		@Override
		public String toString() {
			return DefaultParser.NOT_EQUAL_TO_KEYWORD;
		}

		/**
		 * Returns true if and only if the left and right values are different.
		 * 
		 * @param left the value on the left of the comparison
		 * @param right the value on the right of the comparison
		 * @return true if the values are different, false otherwise
		 */
		@Override
		public boolean test(Value left, Value right) {
			return !left.equals(right);
		}
		
		/**
		 * Returns {@link Comparison#NOT_EQUAL_TO}.
		 * 
		 * @returns {@link Comparison#NOT_EQUAL_TO}
		 */
		@Override
		public Operator flip() {
			return NOT_EQUAL_TO;
		}
		
		/**
		 * Returns {@link Comparison#EQUAL_TO}.
		 * 
		 * @returns {@link Comparison#EQUAL_TO}
		 */
		@Override
		public Operator negate() {
			return EQUAL_TO;
		}
		
		@Override
		protected Precondition combine(Precondition self, Precondition other) {
			if(other.operator.equals(GREATER_THAN_OR_EQUAL_TO) && self.right.equals(other.right))
				return new Precondition(GREATER_THAN, self.left, self.right);
			else if(other.operator.equals(LESS_THAN_OR_EQUAL_TO) && self.right.equals(other.right))
				return new Precondition(LESS_THAN, self.left, self.right);
			else
				return super.combine(self, other);
		}
		
		@Override
		protected boolean negates(Precondition self, Precondition other) {
			if(other.operator.equals(EQUAL_TO))
				return self.right.equals(other.right);
			else
				return super.negates(self, other);
		}
		
		private Object readResolve() throws ObjectStreamException {
			return NOT_EQUAL_TO;
		}
	};
	
	/**
	 * An numeric operator is an {@link Operator operator} which compares
	 * expressions of type {@code number}.
	 * 
	 * @author Stephen G. Ware
	 */
	public static abstract class NumericOperator extends Operator {
		
		/** Serial version ID */
		private static final long serialVersionUID = Comparison.serialVersionUID;
		
		private NumericOperator() {}
		
		@Override
		public boolean test(Value left, Value right) {
			if(left instanceof Number && right instanceof Number)
				return test((Number) left, (Number) right);
			else
				return false;
		}
		
		/**
		 * Tests whether the relationship represented by this numeric operator
		 * holds between two {@link Number numbers} which appear on the left
		 * and right of a {@link Comparison comparison}.
		 * 
		 * @param left the number on the left of the comparison
		 * @param right the number on the right of the comparison
		 * @return true if the relationship holds, false otherwise
		 */
		public abstract boolean test(Number left, Number right);
		
		@Override
		public Expression simplify(Expression left, Expression right) {
			if(left.equals(Unknown.UNKNOWN) || right.equals(Unknown.UNKNOWN))
				return new Comparison(EQUAL_TO, left, right).simplify();
			else
				return super.simplify(left, right);
		}
		
		@Override
		protected Precondition combine(Precondition self, Precondition other) {
			if(self.right instanceof Number && other.right instanceof Number)
				return combine(self, other, (Number) self.right, (Number) other.right);
			else
				return super.combine(self, other);
		}
		
		/**
		 * This convenience method behaves exactly like {@link
		 * Operator#simplify(Expression, Expression)} and makes the same
		 * assumptions, except that it only handles cases where the right sides
		 * of both preconditions are {@link Number numbers}.
		 * 
		 * @param self the first precondition
		 * @param other the second precondition with matching left side
		 * @param sr the right side of the first precondition
		 * @param or the right side of the second precondition
		 * @return a combined precondition which represents both, if one
		 * exists, or null if one does not exist
		 */
		protected Precondition combine(Precondition self, Precondition other, Number sr, Number or) {
			return super.combine(self, other);
		}
		
		@Override
		protected boolean negates(Precondition self, Precondition other) {
			if(self.right instanceof Number && other.right instanceof Number)
				return negates(self, other, (Number) self.right, (Number) other.right);
			else
				return super.negates(self, other);
		}
		
		/**
		 * This convenience method behaves exactly like {@link
		 * Operator#negates(Precondition, Precondition)} and makes the same
		 * assumptions, except that it only handles cases where the right sides
		 * of both preconditions are {@link Number numbers}.
		 * 
		 * @param self the first precondition
		 * @param other the second precondition with matching left side
		 * @param sr the right side of the first precondition
		 * @param or the right side of the second precondition
		 * @return true if the preconditions negate one another, false
		 * otherwise
		 */
		protected boolean negates(Precondition self, Precondition other, Number sr, Number or) {
			return super.negates(self, other);
		}
	}
	
	/**
	 * Represents a {@link Comparison} where the left is greater than the
	 * right.
	 */
	public static final Operator GREATER_THAN = new NumericOperator() {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1;
		
		@Override
		public int hashCode() {
			return 2;
		}
		
		@Override
		public String toString() {
			return DefaultParser.GREATER_THAN_KEYWORD;
		}

		/**
		 * Returns true if and only if the left {@link Number number} {@link
		 * Number#isGreaterThan(Number) is greater than} the right.
		 * 
		 * @param left the number on the left of the comparison
		 * @param right the number on the right of the comparison
		 * @return true if the left is greater than the right
		 */
		@Override
		public boolean test(Number left, Number right) {
			return left.isGreaterThan(right);
		}
		
		/**
		 * Returns {@link Comparison#LESS_THAN}.
		 * 
		 * @returns {@link Comparison#LESS_THAN}
		 */
		@Override
		public Operator flip() {
			return LESS_THAN;
		}
		
		/**
		 * Returns {@link Comparison#LESS_THAN_OR_EQUAL_TO}.
		 * 
		 * @returns {@link Comparison#LESS_THAN_OR_EQUAL_TO}
		 */
		@Override
		public Operator negate() {
			return LESS_THAN_OR_EQUAL_TO;
		}
		
		@Override
		protected Precondition combine(Precondition self, Precondition other, Number sr, Number or) {
			if(other.operator.equals(EQUAL_TO) && sr.isLessThan(or))
				return other;
			else if(other.operator.equals(GREATER_THAN) || other.operator.equals(GREATER_THAN_OR_EQUAL_TO)) {
				if(sr.isLessThan(or))
					return other;
				else
					return self;
			}
			else
				return super.combine(self, other);
		}
		
		@Override
		protected boolean negates(Precondition self, Precondition other, Number sr, Number or) {
			if(other.operator.equals(EQUAL_TO) || other.operator.equals(LESS_THAN) || other.operator.equals(LESS_THAN_OR_EQUAL_TO))
				return sr.isGreaterThanOrEqualTo(or);
			else
				return super.negates(self, other, sr, or);
		}
		
		private Object readResolve() throws ObjectStreamException {
			return GREATER_THAN;
		}
	};
	
	/**
	 * Represents a {@link Comparison} where the left is greater than or equal
	 * to the right.
	 */
	public static final Operator GREATER_THAN_OR_EQUAL_TO = new NumericOperator() {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1;
		
		@Override
		public int hashCode() {
			return 3;
		}
		
		@Override
		public String toString() {
			return DefaultParser.GREATER_THAN_OR_EQUAL_TO_KEYWORD;
		}
		
		@Override
		public boolean test(Value left, Value right) {
			return EQUAL_TO.test(left, right) || GREATER_THAN.test(left, right);
		}

		/**
		 * Returns true if and only if the left {@link Number number} {@link
		 * Number#isGreaterThanOrEqualTo(Number) is greater than or equal to}
		 * the right.
		 * 
		 * @param left the number on the left of the comparison
		 * @param right the number on the right of the comparison
		 * @return true if the left is greater than or equal to the right
		 */
		@Override
		public boolean test(Number left, Number right) {
			return left.isGreaterThanOrEqualTo(right);
		}
		
		/**
		 * Returns {@link Comparison#LESS_THAN_OR_EQUAL_TO}.
		 * 
		 * @returns {@link Comparison#LESS_THAN_OR_EQUAL_TO}
		 */
		@Override
		public Operator flip() {
			return LESS_THAN_OR_EQUAL_TO;
		}
		
		/**
		 * Returns {@link Comparison#LESS_THAN}.
		 * 
		 * @returns {@link Comparison#LESS_THAN}
		 */
		@Override
		public Operator negate() {
			return LESS_THAN;
		}
		
		@Override
		public Expression simplify(Expression left, Expression right) {
			if(!left.isNumber() || !right.isNumber())
				return new Comparison(Comparison.EQUAL_TO, left, right);
			else
				return super.simplify(left, right);
		}
		
		@Override
		protected Precondition combine(Precondition self, Precondition other, Number sr, Number or) {
			if(other.operator.equals(EQUAL_TO) && sr.isLessThanOrEqualTo(or))
				return other;
			else if(other.operator.equals(GREATER_THAN) || other.operator.equals(GREATER_THAN_OR_EQUAL_TO)) {
				if(sr.isLessThanOrEqualTo(or))
					return other;
				else
					return self;
			}
			else if(other.operator.equals(LESS_THAN_OR_EQUAL_TO) && sr.equals(or))
				return new Precondition(EQUAL_TO, self.left, sr);				
			else
				return super.combine(self, other);
		}
		
		@Override
		protected boolean negates(Precondition self, Precondition other, Number sr, Number or) {
			if(other.operator.equals(EQUAL_TO) || other.operator.equals(LESS_THAN_OR_EQUAL_TO))
				return sr.isGreaterThan(or);
			else if(other.operator.equals(LESS_THAN))
				return sr.isGreaterThanOrEqualTo(or);
			else
				return super.negates(self, other, sr, or);
		}
		
		private Object readResolve() throws ObjectStreamException {
			return GREATER_THAN_OR_EQUAL_TO;
		}
	};
	
	/**
	 * Represents a {@link Comparison} where the left is less than the right.
	 */
	public static final Operator LESS_THAN = new NumericOperator() {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1;
		
		@Override
		public int hashCode() {
			return 4;
		}
		
		@Override
		public String toString() {
			return DefaultParser.LESS_THAN_KEYWORD;
		}

		/**
		 * Returns true if and only if the left {@link Number number} {@link
		 * Number#isLessThan(Number) is less than} the right.
		 * 
		 * @param left the number on the left of the comparison
		 * @param right the number on the right of the comparison
		 * @return true if the left is less than the right
		 */
		@Override
		public boolean test(Number left, Number right) {
			return left.isLessThan(right);
		}
		
		/**
		 * Returns {@link Comparison#GREATER_THAN}.
		 * 
		 * @returns {@link Comparison#GREATER_THAN}
		 */
		@Override
		public Operator flip() {
			return GREATER_THAN;
		}
		
		/**
		 * Returns {@link Comparison#GREATER_THAN_OR_EQUAL_TO}.
		 * 
		 * @returns {@link Comparison#GREATER_THAN_OR_EQUAL_TO}
		 */
		@Override
		public Operator negate() {
			return GREATER_THAN_OR_EQUAL_TO;
		}
		
		@Override
		protected Precondition combine(Precondition self, Precondition other, Number sr, Number or) {
			if(other.operator.equals(EQUAL_TO) && sr.isGreaterThan(or))
				return other;
			else if(other.operator.equals(LESS_THAN) || other.operator.equals(LESS_THAN_OR_EQUAL_TO)) {
				if(sr.isGreaterThan(or))
					return other;
				else
					return self;
			}
			else
				return super.combine(self, other);
		}
		
		@Override
		protected boolean negates(Precondition self, Precondition other, Number sr, Number or) {
			if(other.operator.equals(EQUAL_TO) || other.operator.equals(GREATER_THAN) || other.operator.equals(GREATER_THAN_OR_EQUAL_TO))
				return sr.isLessThanOrEqualTo(or);
			else
				return super.negates(self, other, sr, or);
		}
		
		private Object readResolve() throws ObjectStreamException {
			return LESS_THAN;
		}
	};
	
	/**
	 * Represents a {@link Comparison} where the left is less than or equal to
	 * the right.
	 */
	public static final Operator LESS_THAN_OR_EQUAL_TO = new NumericOperator() {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1;
		
		@Override
		public int hashCode() {
			return 5;
		}
		
		@Override
		public String toString() {
			return DefaultParser.LESS_THAN_OR_EQUAL_TO_KEYWORD;
		}
		
		@Override
		public boolean test(Value left, Value right) {
			return EQUAL_TO.test(left, right) || LESS_THAN.test(left, right);
		}

		/**
		 * Returns true if and only if the left {@link Number number} {@link
		 * Number#isLessThanOrEqualTo(Number) is less than or equal to} the
		 * right.
		 * 
		 * @param left the number on the left of the comparison
		 * @param right the number on the right of the comparison
		 * @return true if the left is less than or equal to the right
		 */
		@Override
		public boolean test(Number left, Number right) {
			return left.isLessThanOrEqualTo(right);
		}
		
		/**
		 * Returns {@link Comparison#GREATER_THAN_OR_EQUAL_TO}.
		 * 
		 * @returns {@link Comparison#GREATER_THAN_OR_EQUAL_TO}
		 */
		@Override
		public Operator flip() {
			return GREATER_THAN_OR_EQUAL_TO;
		}
		
		/**
		 * Returns {@link Comparison#GREATER_THAN}.
		 * 
		 * @returns {@link Comparison#GREATER_THAN}
		 */
		@Override
		public Operator negate() {
			return GREATER_THAN;
		}
		
		@Override
		public Expression simplify(Expression left, Expression right) {
			if(!left.isNumber() || !right.isNumber())
				return new Comparison(Comparison.EQUAL_TO, left, right);
			else
				return super.simplify(left, right);
		}
		
		@Override
		protected Precondition combine(Precondition self, Precondition other, Number sr, Number or) {
			if(other.operator.equals(EQUAL_TO) && sr.isGreaterThanOrEqualTo(or))
				return other;
			else if(other.operator.equals(LESS_THAN) || other.operator.equals(LESS_THAN_OR_EQUAL_TO)) {
				if(sr.isGreaterThanOrEqualTo(or))
					return other;
				else
					return self;
			}
			else if(other.operator.equals(GREATER_THAN_OR_EQUAL_TO) && sr.equals(or))
				return new Precondition(EQUAL_TO, self.left, sr);				
			else
				return super.combine(self, other);
		}
		
		@Override
		protected boolean negates(Precondition self, Precondition other, Number sr, Number or) {
			if(other.operator.equals(EQUAL_TO) || other.operator.equals(GREATER_THAN_OR_EQUAL_TO))
				return sr.isLessThan(or);
			else if(other.operator.equals(GREATER_THAN))
				return sr.isLessThanOrEqualTo(or);
			else
				return super.negates(self, other, sr, or);
		}
		
		private Object readResolve() throws ObjectStreamException {
			return LESS_THAN_OR_EQUAL_TO;
		}
	};

	/** The relationship between the left and right sides */
	public final Operator operator;
	
	/** The left side expression */
	public final Expression left;
	
	/** The right side expression */
	public final Expression right;
	
	/**
	 * Constructs a new comparison with a given {@link Operator relationship},
	 * left side, and right side.
	 * 
	 * @param operator the relationship between the sides
	 * @param left the left side
	 * @param right the right side
	 */
	public Comparison(Operator operator, Expression left, Expression right) {
		this.operator = operator;
		this.left = left;
		this.right = right;
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			Comparison otherComparison = (Comparison) other;
			return operator.equals(otherComparison.operator) && left.equals(otherComparison.left) && right.equals(otherComparison.right);
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
			Comparison otherComparison = (Comparison) other;
			return Utilities.compare(left, otherComparison.left, operator, otherComparison.operator, right, otherComparison.right);
		}
		else
			return Proposition.super.compareTo(other);
	}

	@Override
	public boolean isGround() {
		return left.isGround() && right.isGround();
	}

	@Override
	public Comparison apply(Function<Object, Object> function) {
		Expression left = (Expression) function.apply(this.left);
		Expression right = (Expression) function.apply(this.right);
		if(left != this.left || right != this.right)
			return new Comparison(operator, left, right);
		else
			return this;
	}
	
	@Override
	public Expression simplify() {
		Comparison simplified = (Comparison) Proposition.super.simplify();
		Expression further = operator.simplify(simplified.left, simplified.right);
		if(further == null)
			return simplified;
		else
			return further;
	}
	
	/**
	 * Returns {@link True true} if the relationship represented by this
	 * comparison's {@link #operator operator} holds between the left and right
	 * sides, or {@link False false} otherwise.
	 * 
	 * @param state the state in which this comparison will be evaluated
	 * @return the Boolean value of this expression
	 */
	@Override
	public Value evaluate(State state) {
		if(operator.test(left.evaluate(state), right.evaluate(state)))
			return True.TRUE;
		else
			return False.FALSE;
	}
	
	@Override
	public Comparison prepend(Parameter character) {
		return (Comparison) Proposition.super.prepend(character);
	}
	
	/**
	 * Returns a new comparison with the same left and right sides, but with a
	 * {@link Operator#negate() negated operator}.
	 * 
	 * @return a comparison with the opposite value of this comparison
	 */
	@Override
	public Comparison negate() {
		return new Comparison(operator.negate(), left, right);
	}
	
	@Override
	public Disjunction<Clause<Precondition>> toPrecondition() {
		// Case 1: Two values. Simplify.
		if(left instanceof Value && right instanceof Value)
			return simplify().toPrecondition();
		// Case 2: Numeric. Make constraints on all variables and fluents explicit.
		else if(left.isNumber() && left.isValued() && right.isNumber() && right.isValued()) {
			ArrayList<Expression> constraints = new ArrayList<>();
			for(Fluent fluent : collect(Fluent.class))
				constraints.add(isolate(fluent));
			return new Conjunction<>(constraints).toPrecondition();
		}
		// Case 3: Valid left side and valid right side.
		else if((left instanceof Variable || left instanceof Fluent) && !left.isNumber() && right.isValued()) {
			if(operator.equals(NOT_EQUAL_TO) && right.isBoolean())
				return new Comparison(operator.negate(), left, right.negate()).toPrecondition();
			else
				return new Precondition(operator, left, right).toPrecondition();
		}
		else if((right instanceof Variable || right instanceof Fluent) && !right.isNumber() && left.isValued())
			return flip().toPrecondition();
		// Case 4: Need to factor out something that is not valued.
		else if(!right.isValued()) {
			Conditional<Disjunction<Clause<Precondition>>> valued = right.toValued();
			Expression[] branches = new Expression[valued.branches.size()];
			for(int i=0; i<branches.length; i++)
				branches[i] = new Comparison(operator, left, valued.branches.get(i));
			return new Conditional<>(valued.conditions, new ImmutableArray<>(branches)).toPrecondition();
		}
		// Case 5: Flip
		else
			return flip().toPrecondition();
	}
	
	@Override
	public Clause<Effect> toEffect() {
		if(left instanceof Fluent && operator.equals(EQUAL_TO))
			return new Assignment((Fluent) left, right).toEffect();
		else if(left instanceof Fluent && left.isBoolean() && operator.equals(NOT_EQUAL_TO))
			return new Assignment((Fluent) left, right.negate()).toEffect();
		else
			return Proposition.super.toEffect();
	}
	
	/**
	 * Attempts to isolate a given {@link Fluent fluent} which might appear
	 * anywhere in this comparison on the left side of this comparison. If the
	 * fluent is numeric, {@link
	 * Arithmetic#isolate(Fluent, Operator, Expression) some arithmetic may
	 * need to be done to isolate the fluent}, but the comparison returned
	 * should be logically equivalent to this comparison.
	 * 
	 * @param query the query which should be isolated on the left side
	 * @return an equivalent comparison with the query on the left side
	 * @throws FormatException if the query cannot be isolated
	 */
	Expression isolate(Fluent query) {
		if(left.equals(query))
			return new Precondition(operator, query, right.simplify());
		else if(right.equals(query))
			return flip().isolate(query);
		else if(left.occurs(query) && left instanceof Arithmetic)
			return ((Arithmetic) left).isolate(query, operator, right);
		else if(right.occurs(query) && right instanceof Arithmetic)
			return flip().isolate(query);
		else
			throw Exceptions.cannotIsolate(query, this);
	}
	
	/**
	 * Returns a new comparison where the left is now on the right, the right
	 * is now on the left, and the operator has been {@link Operator#flip()
	 * flipped}.
	 * 
	 * @return an equivalent comparison, but with its sides flipped
	 */
	public Comparison flip() {
		return new Comparison(operator.flip(), right, left);
	}
}