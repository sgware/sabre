package edu.uky.cs.nil.sabre.io;

import edu.uky.cs.nil.sabre.util.ImmutableList;

/**
 * A pattern determines whether a sequence of {@link Token tokens} matches the
 * expected format to be parsed as an object. A pattern's primary method is
 * {@link #match(Parser, ImmutableList)}, which returns a {@link ParseTree
 * parse tree} if the tokens are formatted correctly or throws a {@link
 * ParseException parse exception} if they are not. 
 * 
 * @author Stephen G. Ware
 */
@FunctionalInterface
public interface Pattern {
	
	/**
	 * A convenience method for easily creating common patters from single
	 * objects. If the given object is null, this method returns {@link
	 * Terminal#NOTHING the nothing terminal symbol}. If the object is
	 * already a pattern, this method casts and returns it. If the object
	 * is a String, this method returns a {@link Keyword keyword}. Otherwise,
	 * this method returns the object as a {@link NonTerminal non-terminal
	 * symbol}.
	 * 
	 * @param object the object to be converted into a pattern
	 * @return a pattern
	 */
	public static Pattern toPattern(Object object) {
		if(object == null)
			return Terminal.NOTHING;
		else if(object instanceof Pattern)
			return (Pattern) object;
		else if(object instanceof String)
			return new Keyword((String) object);
		else
			return new NonTerminal(object);
	}
	
	/**
	 * Returns the first {@link Token token} in a list as a new list containing
	 * exactly that one token.
	 * 
	 * @param tokens the list of tokens from which the first token is desired
	 * @return a new list containing only the first token from the list, or 
	 * null if the list was empty
	 */
	public static ImmutableList<Token> first(ImmutableList<Token> tokens) {
		if(tokens.size() == 0)
			return null;
		else
			return ImmutableList.EMPTY.cast(Token.class).add(tokens.first);
	}
	
	/**
	 * Returns the last {@link Token token} in a list as a new list containing
	 * exactly that one token.
	 * 
	 * @param tokens the list of tokens from which the last token is desired
	 * @return a new list containing only the last token from the list, or 
	 * null if the list was empty
	 */
	public static ImmutableList<Token> last(ImmutableList<Token> tokens) {
		while(tokens.size() > 1)
			tokens = tokens.rest;
		if(tokens.size() == 0)
			return null;
		else
			return tokens;
	}
	
	/**
	 * Returns the first {@link Token tokens} from a list as a new list
	 * containing exactly those tokens.
	 * 
	 * @param tokens the list of tokens from which the first tokens are desired
	 * @param length the number of first tokens desired
	 * @return a new list containing only the given number of first tokens, or
	 * as many first tokens as were available, or an empty list if the given
	 * list was empty
	 */
	public static ImmutableList<Token> clip(ImmutableList<Token> tokens, int length) {
		if(tokens.size() == 0 || length <= 0)
			return ImmutableList.EMPTY.cast(Token.class);
		else
			return clip(tokens.rest, length - 1).add(tokens.first);
	}

	/**
	 * If a list of {@link Token tokens} is formatted correctly according to
	 * this pattern, this method returns a {@link ParseTree parse tree} that
	 * can then be given to a {@link Builder builder} to create an object; if
	 * the tokens do not match this pattern, a {@link ParseException parse
	 * exception} is thrown. The {@link Parser parser} which called this
	 * method is given as an argument so that it can be used to parse other
	 * patterns inside this pattern.
	 * 
	 * @param parser the parse which called this method
	 * @param tokens the tokens to be checked
	 * @return a {@link ParseTree parse tree} if the tokens match this pattern
	 * @throws ParseException if the tokens do not match this pattern
	 */
	public ParseTree match(Parser parser, ImmutableList<Token> tokens) throws ParseException;
}