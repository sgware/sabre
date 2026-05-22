package edu.uky.cs.nil.sabre.logic;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Type;
import edu.uky.cs.nil.sabre.io.DefaultParser;

/**
 * Unknown is a {@link Value value} that represents a missing or empty value.
 * {@link edu.uky.cs.nil.sabre.Fluent Fluents} of every type can be assigned
 * this value, except fluents of type {@code boolean}, which must always be
 * {@link True true} or {@link False false}. Unknown always evaluates to
 * itself. Unknown is of every type except {@code boolean}.
 * <p>
 * Unknown should be treated similarly to the concept of {@code null}, and does
 * not really enable true reasoning about uncertainty. For example, when we say
 * a character believes a fluent's value to be unknown, this means the character
 * believes the fluent has no value, not that the fluent can have any value.
 * Unknown does not provide a way to circumvent the requirement that {@link
 * Expression#toEffect() effects by deterministic}.
 * 
 * @author Stephen G. Ware
 */
public class Unknown implements Value {
	
	/** The singleton unknown object */
	public static final Unknown UNKNOWN = new Unknown();
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	private Unknown() {}
	
	@Override
	public int hashCode() {
		return -1;
	}
	
	@Override
	public String toString() {
		return DefaultParser.UNKNOWN_KEYWORD;
	}
	
	@Override
	public int compareTo(Logical other) {
		if(other.equals(False.FALSE) || other.equals(True.TRUE))
			return 1;
		else if(other.equals(this))
			return 0;
		else
			return -1;
	}

	@Override
	public Unknown apply(Function<Object, Object> function) {
		return this;
	}
	
	@Override
	public boolean isBoolean() {
		return false;
	}
	
	@Override
	public boolean isNumber() {
		return true;
	}
	
	@Override
	public boolean isEntity() {
		return true;
	}
	
	@Override
	public boolean isCharacter() {
		return true;
	}

	@Override
	public boolean is(Type type) {
		return type.isNumber() || type.isEntity();
	}
	
	@Override
	public Unknown simplify() {
		return this;
	}
	
	@Override
	public Unknown evaluate(State state) {
		return this;
	}
	
	@Override
	public Unknown prepend(Parameter character) {
		return this;
	}
	
	/**
	 * Ensures the same unknown constant is used by deserialized objects.
	 * 
	 * @return the unknown constant
	 */
	private Object readResolve() {
		return UNKNOWN;
	}
}