package edu.uky.cs.nil.sabre;

import java.io.ObjectStreamException;
import java.util.HashMap;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.Logical;
import edu.uky.cs.nil.sabre.logic.Numeric;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Unknown;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * Represents numbers (using the Java {@code double} data type) as {@link
 * Value values}. Number objects are singletons, meaning there can only ever
 * be one number object with a given value. This allows numbers to be compared
 * using {@code ==}.
 * 
 * @author Stephen G. Ware
 */
public class Number implements Numeric, Value {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** A mapping of Java doubles to their unique corresponding number objects */
	private static final HashMap<java.lang.Double, Number> numbers = new HashMap<>();
	
	/**
	 * Converts a {@link java.lang.Number Java Number} to a {@link
	 * Value value}, interning it to ensure that there is only ever one number
	 * object with this value. If this method is called multiple times with the
	 * same input value, it will return the same number object every time.
	 * 
	 * @param value the value of the number
	 * @return the number object
	 */
	public static final Number get(java.lang.Number value) {
		value = Double.valueOf(value.doubleValue());
		if(value.equals(Double.NaN) || value.equals(-0.0))
			throw Exceptions.notANumber(value);
		Number number = numbers.get(value);
		if(number == null)
			number = new Number((Double) value);
		return number;
	}
	
	/** A constant representing zero */
	public static final Number ZERO = get(0);
	
	/** A constant representing one */
	public static final Number ONE = get(1);
	
	/** A constant representing positive infinity */
	public static final Number POSITIVE_INFINITY = get(Double.POSITIVE_INFINITY);
	
	/** A constant representing negative infinity */
	public static final Number NEGATIVE_INFINITY = get(Double.NEGATIVE_INFINITY);
	
	/**
	 * Returns the larger of two numbers.
	 * 
	 * @param n1 the first number
	 * @param n2 the second number
	 * @return n2 if it is larger, n1 otherwise
	 */
	public static final Number max(Number n1, Number n2) {
		if(n2.isGreaterThan(n1))
			return n2;
		else
			return n1;
	}
	
	/**
	 * Returns the smaller of two numbers.
	 * 
	 * @param n1 the first number
	 * @param n2 the second number
	 * @return n2 if it is smaller, n1 otherwise
	 */
	public static final Number min(Number n1, Number n2) {
		if(n2.isLessThan(n1))
			return n2;
		else
			return n1;
	}
	
	/** The value of this number as a Java {@code double} */
	public final java.lang.Double value;
	
	/**
	 * Constructs a new number.
	 * 
	 * @param value the value
	 */
	private Number(java.lang.Double value) {
		this.value = value;
		numbers.put(value, this);
	}
	
	@Override
	public int hashCode() {
		return value.hashCode();
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public int compareTo(Logical other) {
		if(other.equals(False.FALSE) || other.equals(True.TRUE) || other.equals(Unknown.UNKNOWN))
			return 1;
		else if(other instanceof Number)
			return Double.compare(value, ((Number) other).value);
		else
			return -1;
	}
	
	@Override
	public boolean isGround() {
		return true;
	}

	@Override
	public Number apply(Function<Object, Object> function) {
		return this;
	}
	
	@Override
	public Number simplify() {
		return this;
	}

	@Override
	public Number evaluate(State state) {
		return this;
	}
	
	@Override
	public Number prepend(Parameter character) {
		return this;
	}
	
	/**
	 * Tests whether this number is larger than a given second number.
	 * 
	 * @param other the second number
	 * @return true if this number is larger than the second, false otherwise
	 */
	public boolean isGreaterThan(Number other) {
		return compareTo(other) > 0;
	}

	/**
	 * Tests whether this number is larger than or the same as a given second
	 * number.
	 * 
	 * @param other the second number
	 * @return true if this number is larger than or the same as the second,
	 * false otherwise
	 */
	public boolean isGreaterThanOrEqualTo(Number other) {
		return isGreaterThan(other) || equals(other);
	}

	/**
	 * Tests whether this number is smaller than a given second number.
	 * 
	 * @param other the second number
	 * @return true if this number is smaller than the second, false otherwise
	 */
	public boolean isLessThan(Number other) {
		return compareTo(other) < 0;
	}

	/**
	 * Tests whether this number is smaller than or the same as a given second
	 * number.
	 * 
	 * @param other the second number
	 * @return true if this number is smaller than or the same as the second,
	 * false otherwise
	 */
	public boolean isLessThanOrEqualTo(Number other) {
		return isLessThan(other) || equals(other);
	}

	/**
	 * Returns a new number which is the sum of this number and the given
	 * second number.
	 * 
	 * @param number the second number
	 * @return the sum of the two numbers
	 */
	public Number add(Number number) {
		if(this.equals(POSITIVE_INFINITY) || number.equals(POSITIVE_INFINITY))
			return POSITIVE_INFINITY;
		else
			return get(value.doubleValue() + number.value.doubleValue());
	}

	/**
	 * Returns a new number which is the difference of this number and the
	 * given second number.
	 * 
	 * @param number the second number
	 * @return the difference of the two numbers
	 */
	public Number subtract(Number number) {
		if(this.equals(POSITIVE_INFINITY) || number.equals(NEGATIVE_INFINITY))
			return POSITIVE_INFINITY;
		else if(number.equals(POSITIVE_INFINITY))
			return NEGATIVE_INFINITY;
		else
			return get(value.doubleValue() - number.value.doubleValue());
	}

	/**
	 * Returns a new number which is the product of this number and the given
	 * second number.
	 * 
	 * @param number the second number
	 * @return the product of the two numbers
	 */
	public Number multiply(Number number) {
		if(this.equals(ZERO) || number.equals(ZERO))
			return ZERO;
		else
			return get(value.doubleValue() * number.value.doubleValue());
	}

	/**
	 * Returns a new number which is the quotient of this number and the given
	 * second number.
	 * 
	 * @param number the second number
	 * @return the quotient of the two numbers
	 */
	public Number divide(Number number) {
		if(number.equals(ZERO))
			throw Exceptions.divideByZero(this);
		else if(this.equals(ZERO) || number.equals(POSITIVE_INFINITY) || number.equals(NEGATIVE_INFINITY))
			return ZERO;
		else
			return get(value.doubleValue() / number.value.doubleValue());
	}
	
	/**
	 * Ensures there is at most one object for each unique value.
	 * 
	 * @return the same object used for all numbers of this value
	 * @throws ObjectStreamException if deserialization failed
	 */
	private Object readResolve() throws ObjectStreamException {
		return get(value);
	}
}