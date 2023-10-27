package edu.uky.cs.nil.sabre.io;

/**
 * A keyword is a {@link Terminal terminal symbol} which matches {@link Single
 * exactly one} {@link Token token} that must have a specified {@link
 * Token#value string value}.
 * 
 * @author Stephen G. Ware
 */
public class Keyword extends Single {
	
	/** The value the single token must have */
	public final String value;
	
	/**
	 * Constructs a new keyword symbol that will only match a single token
	 * whose string value is this value.
	 * 
	 * @param value the value to match
	 */
	public Keyword(String value) {
		this.value = value;
	}
	
	@Override
	public boolean equals(Object other) {
		return getClass().equals(other.getClass()) && value.equals(((Keyword) other).value);
	}
	
	@Override
	public int hashCode() {
		return value.hashCode();
	}
	
	@Override
	public String toString() {
		return "\"" + value + "\"";
	}

	@Override
	protected boolean match(String token) {
		return value.equals(token);
	}
}