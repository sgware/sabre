package edu.uky.cs.nil.sabre.io;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.util.ImmutableList;

/**
 * A non-terminal {@link Symbol symbol} is a complex element in a grammar used
 * by a {@link Parser parser} which can be further broken down into smaller
 * parts made up of terminal symbols and other non-terminal symbols.
 * This class is a wrapper around any arbitrary object that serves as the
 * unique identifier for the non-terminal symbol. This wrapper allows the
 * object to be used as a {@link Pattern pattern} and associates {@link
 * #description a description} with the object. Two non-terminal symbols are
 * considered the same if their {@link #key keys} are equal.
 * 
 * @author Stephen G. Ware
 */
public final class NonTerminal extends Symbol {
	
	/** A unique object that identifies this non-terminal symbol */
	public final Object key;
	
	/** A description of the thing this non-terminal symbol represents */
	public final String description;
	
	/**
	 * Constructs a new non-terminal symbol with a given unique identifier and
	 * a given description.
	 * 
	 * @param key a unique object that identifies this non-terminal symbol
	 * @param description a description of what this symbol represents
	 */
	public NonTerminal(Object key, String description) {
		this.key = key;
		this.description = description;
	}
	
	/**
	 * Constructs a new non-terminal symbol with a given unique identifier.
	 * 
	 * @param key a unique object that identifies this non-terminal symbol
	 */
	public NonTerminal(Object key) {
		this(key, key.toString());
	}
	
	@Override
	public boolean equals(Object other) {
		return getClass().equals(other.getClass()) && key.equals(((NonTerminal) other).key);
	}
	
	@Override
	public int hashCode() {
		return key.hashCode();
	}
	
	@Override
	public String toString() {
		return description;
	}

	@Override
	public ParseTree match(Parser parser, ImmutableList<Token> tokens) throws ParseException {
		if(parser.getSymbol(key) != this)
			return parser.getSymbol(key).match(parser, tokens);
		Pattern rule = parser.getRule(this);
		if(rule == null)
			throw Exceptions.noPattern(description, tokens);
		else
			return new ParseTree(this, tokens, parser.match(rule, tokens));
	}
}