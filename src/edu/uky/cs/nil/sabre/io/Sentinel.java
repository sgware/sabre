package edu.uky.cs.nil.sabre.io;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.util.ImmutableArray;
import edu.uky.cs.nil.sabre.util.ImmutableList;

/**
 * A sentinel is a parsing {@link Pattern pattern} composed of {@link #patterns
 * a sequence of patterns} such that two non-{@link Keyword keyword} patterns
 * never appear next to one another. In other words, every pattern in the
 * sequence that is not a keyword must have at least one keyword between it
 * and the next element in the sequence which is not a keyword. To {@link
 * Pattern#match(Parser, ImmutableList) match} a list of tokens, this pattern
 * first {@link Lookahead#find(Keyword, ImmutableList) finds} the sentinel
 * keywords to break down the list of tokens into smaller lists between
 * keywords and then tries to match those smaller lists to its non-keyword
 * patterns between the keywords.
 * 
 * @author Stephen G. Ware
 */
public class Sentinel extends Lookahead {
	
	/** A sequence of patterns where every other one must be a keyword */
	public final ImmutableArray<Pattern> patterns;
	
	/** The number of non-keyword patterns in the sequence */
	private final int children;
	
	/**
	 * Constructs a new sentinel pattern from a sequence of objects, each of
	 * which will be {@link Pattern#toPattern(Object) converted into a
	 * pattern}.
	 * 
	 * @param objects the list of objects to be converted into patterns
	 * @throws IllegalArgumentException if two non-keyword patterns appear next
	 * to each other
	 */
	public Sentinel(Object...objects) {
		Pattern[] patterns = new Pattern[objects.length];
		int children = 0;
		for(int i=0; i<patterns.length; i++) {
			patterns[i] = Pattern.toPattern(objects[i]);
			if(!(patterns[i] instanceof Keyword))
				children++;
		}
		for(int i=0; i<patterns.length-1; i++)
			if(!(patterns[i] instanceof Keyword) && !(patterns[i+1] instanceof Keyword))
				throw Exceptions.noSentinel(patterns[i]);
		this.patterns = new ImmutableArray<>(patterns);
		this.children = children;
	}
	
	@Override
	public String toString() {
		String string = "";
		for(Pattern pattern : patterns)
			string += pattern + " ";
		return string;
	}

	@Override
	public ParseTree match(Parser parser, ImmutableList<Token> tokens) throws ParseException {
		if(patterns.size() == 0) {
			ParseTree result = parser.match(Terminal.NOTHING, tokens);
			return new ParseTree(this, result.tokens);
		}
		if(tokens.size() == 0)
			return parser.match(Terminal.TOKEN, tokens);
		ImmutableList<Token> remainder = tokens;
		ParseTree[] children = new ParseTree[this.children];
		int child = 0;
		for(int i=0; i<patterns.size(); i++) {
			Pattern pattern = patterns.get(i);
			ImmutableList<Token> clip;
			if(i == patterns.size() - 1)
				clip = remainder;
			else if(pattern instanceof Keyword) {
				clip = Pattern.clip(remainder, 1);
				remainder = remainder.rest;
			}
			else {
				Keyword sentinel = (Keyword) patterns.get(i + 1);
				ImmutableList<Token> end = find(sentinel, remainder);
				if(end == null)
					throw Exceptions.parseUnexpected(sentinel + " after \"" + Pattern.last(remainder).first.value + "\"", "nothing", remainder);
				clip = Pattern.clip(remainder, remainder.size() - end.size());
				remainder = end;
			}
			ParseTree result;
			try {
				result = parser.match(pattern, clip);
			}
			catch(ParseException e) {
				if(clip.size() == 0)
					throw Exceptions.parseUnexpected("something after \"" + Pattern.last(tokens).first.value + "\"", "nothing", Pattern.last(tokens));
				throw e;
			}
			if(!(pattern instanceof Keyword))
				children[child++] = result;
		}
		return new ParseTree(this, tokens, children);
	}
}