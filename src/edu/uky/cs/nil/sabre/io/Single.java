package edu.uky.cs.nil.sabre.io;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.util.ImmutableList;

/**
 * A single is a {@link Terminal terminal symbol} that matches exactly one
 * {@link Token token}.
 * 
 * @author Stephen G. Ware
 */
public abstract class Single extends Terminal {

	@Override
	public ParseTree match(Parser parser, ImmutableList<Token> tokens) throws ParseException {
		if(tokens.size() == 0)
			throw Exceptions.parseUnexpected(toString(), "nothing", tokens);
		else if(tokens.size() == 1) {
			if(match(tokens.first.value))
				return new ParseTree(this, tokens);
			else
				throw Exceptions.parseUnexpected(toString(), "\"" + tokens.first.value + "\"", tokens);
		}
		else
			throw Exceptions.parseUnexpected("\"" + tokens.rest.first.value + "\"", Pattern.first(tokens.rest));
	}
	
	/**
	 * Given the string representation of a single {@link Token token}, this
	 * method returns true if that string matches the correct format or false
	 * otherwise.
	 * 
	 * @param token a string representation of a single token
	 * @return true if the token matches the correct format, false otherwise
	 */
	protected abstract boolean match(String token);
}