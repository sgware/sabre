package edu.uky.cs.nil.sabre.io;

import edu.uky.cs.nil.sabre.util.ImmutableArray;
import edu.uky.cs.nil.sabre.util.ImmutableList;

/**
 * A parsing {@link Pattern pattern} that attempts to match a sequence of
 * {@link Token tokens} to any pattern in an ordered {@link #patterns list of
 * patterns}. If any pattern matches, its {@link ParseTree parse tree} is
 * returned, otherwise an exception is thrown.
 * 
 * @author Stephen G. Ware
 */
public class Selection implements Pattern {
	
	/**
	 * The list of patterns to attempt to match in the order they should be
	 * tried
	 */
	public final ImmutableArray<Pattern> patterns;
	
	/**
	 * Constructs a new selection pattern.
	 * 
	 * @param patterns the patterns to attempt to match, in the order they
	 * should be tried
	 */
	public Selection(Pattern...patterns) {
		this.patterns = new ImmutableArray<>(patterns);
	}
	
	@Override
	public String toString() {
		String string = "";
		for(int i=0; i<patterns.size(); i++) {
			if(i != 0)
				string += " | ";
			string += patterns.get(i);
		}
		return string;
	}

	@Override
	public ParseTree match(Parser parser, ImmutableList<Token> tokens) throws ParseException {
		ParseException exception = null;
		for(Pattern pattern : patterns) {
			try {
				return new ParseTree(this, tokens, parser.match(pattern, tokens));
			}
			catch(ParseException e) {
				if(exception == null || exception.tokens.size() == 0 || (e.tokens.size() > 0 && e.tokens.first.position > exception.tokens.first.position))
					exception = e;
			}
		}
		throw exception;
	}
	
	/**
	 * Add a new pattern to the end of the {@link #patterns list of patterns}
	 * this selection will try to match.
	 * 
	 * @param pattern the pattern to add to the end of the list
	 * @return a new selection pattern with the given pattern added to the end
	 * of the list
	 */
	public Selection add(Pattern pattern) {
		Pattern[] patterns = new Pattern[this.patterns.size() + 1];
		for(int i=0; i<this.patterns.size(); i++)
			patterns[i] = this.patterns.get(i);
		patterns[patterns.length - 1] = pattern;
		return new Selection(patterns);
	}
}