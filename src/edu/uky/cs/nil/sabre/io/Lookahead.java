package edu.uky.cs.nil.sabre.io;

import edu.uky.cs.nil.sabre.util.ImmutableList;

/**
 * A lookahead {@link Pattern pattern} is one that can look arbitrarily far
 * into a sequence of {@link Token tokens} to determine if it matches.
 * 
 * @author Stephen G. Ware
 */
public abstract class Lookahead implements Pattern {

	/**
	 * Searches a list of tokens for the first occurrence of a {@link Keyword
	 * keyword}, and if it is found, returns the list starting at that token.
	 * This search will skip over tokens inside paired brackets, including
	 * parentheses, square brackets, and curly brackets. For example, if the
	 * list contains six tokens <code>"a ( a b ) b"</code> and the keyword to
	 * find is <code>"b"</code>, the first occurrence of <code>"b"</code> would
	 * be skipped because it appears inside paired parentheses and the last
	 * token would be the one found.
	 * 
	 * @param keyword the keyword to search for
	 * @param tokens the list of tokens to search
	 * @return the list of tokens starting at the first occurrence of the
	 * keyword, or null if the keyword was not found
	 */
	protected static ImmutableList<Token> find(Keyword keyword, ImmutableList<Token> tokens) {
		return find(keyword.value, tokens);
	}
	
	/**
	 * Searches a list of tokens for the first occurrence of a token whose
	 * {@link Token#value string value} matches a query, and if it is found,
	 * returns the list starting at that token.
	 * This search will skip over tokens inside paired brackets, including
	 * parentheses, square brackets, and curly brackets. For example, if the
	 * list contains six tokens <code>"a ( a b ) b"</code> and the query to
	 * find is <code>"b"</code>, the first occurrence of <code>"b"</code> would
	 * be skipped because it appears inside paired parentheses and the last
	 * token would be the one found.
	 * 
	 * @param query the token value to search for
	 * @param tokens the list of tokens to search
	 * @return the list of tokens starting at the first occurrence of the
	 * query, or null if the query was not found
	 */
	protected static final ImmutableList<Token> find(String query, ImmutableList<Token> tokens) {
		while(tokens.size() > 0) {
			if(tokens.first.value.equals(query))
				return tokens;
			else if(tokens.first.value.equals("(")) {
				tokens = find(")", tokens.rest);
				if(tokens == null)
					return null;
				else
					tokens = tokens.rest;
			}
			else if(tokens.first.value.equals("[")) {
				tokens = find("]", tokens.rest);
				if(tokens == null)
					return null;
				else
					tokens = tokens.rest;
			}
			else if(tokens.first.value.equals("{")) {
				tokens = find("}", tokens.rest);
				if(tokens == null)
					return null;
				else
					tokens = tokens.rest;
			}
			else
				tokens = tokens.rest;
		}
		return null;
	}
}