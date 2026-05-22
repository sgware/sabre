package edu.uky.cs.nil.sabre.io;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.util.ImmutableList;

/**
 * A list is a parsing {@link Pattern pattern} where a given {@link #element
 * pattern} is repeated some number of times with a {@link #separator
 * separator} between occurrences. To {@link #match(Parser, ImmutableList)
 * match} a list of tokens, a list first {@link Lookahead#find(Keyword,
 * ImmutableList) finds} all separators and then tries to match the tokens
 * between each separator using its {@link #element element pattern}.
 * 
 * @author Stephen G. Ware
 */
public class List extends Lookahead {

	/**
	 * A constant representing no limit on the minimum or maximum number of
	 * elements in a list
	 */
	public static final int NO_LIMIT = -1;

	/** A pattern that every element in the list should match */
	public final Pattern element;
	
	/** A keyword that separates the elements in the list */
	public final Keyword separator;
	
	/**
	 * The minimum number of elements the list can have (or {@link #NO_LIMIT})
	 */
	public final int min;
	
	/**
	 * The maximum number of elements the list can have (or {@link #NO_LIMIT})
	 */
	public final int max;
	
	/**
	 * Constructs a new list with a given element pattern, separator keyword,
	 * and min and max element counts.
	 * 
	 * @param element a pattern that should match every element in the list
	 * @param separator a keyword that separates elements in the list
	 * @param min the minimum number of elements the list can have
	 * @param max the maximum number of elements the list can have
	 */
	public List(Pattern element, Keyword separator, int min, int max) {
		this.element = element;
		this.separator = separator;
		this.min = min;
		this.max = max;
	}
	
	/**
	 * Constructs a new list with a given element object (that will be {@link
	 * Pattern#toPattern(Object) converted to a pattern}), separator keyword,
	 * and min and max element counts.
	 * 
	 * @param element an object that will be converted to a pattern that
	 * should match every element in the list
	 * @param separator a keyword that separates elements in the list
	 * @param min the minimum number of elements the list can have
	 * @param max the maximum number of elements the list can have
	 */
	public List(Object element, Keyword separator, int min, int max) {
		this(Pattern.toPattern(element), separator, min, max);
	}
	
	/**
	 * Constructs a new list with a given element pattern, separator string
	 * (that will be converted into a {@link Keyword keyword}), and min and max
	 * element counts.
	 * 
	 * @param element a pattern that should match every element in the list
	 * @param separator a string that separates elements in the list
	 * @param min the minimum number of elements the list can have
	 * @param max the maximum number of elements the list can have
	 */
	public List(Pattern element, String separator, int min, int max) {
		this(element, new Keyword(separator), min, max);
	}
	
	/**
	 * Constructs a new list with a given element object (that will be {@link
	 * Pattern#toPattern(Object) converted to a pattern}), separator string
	 * (thst will be converted to a {@link Keyword keyword}), and min and max
	 * element counts.
	 * 
	 * @param element an object that will be converted to a pattern that
	 * should match every element in the list
	 * @param separator a string that separates elements in the list
	 * @param min the minimum number of elements the list can have
	 * @param max the maximum number of elements the list can have
	 */
	public List(Object element, String separator, int min, int max) {
		this(Pattern.toPattern(element), separator, min, max);
	}
	
	/**
	 * Constructs a new list with a given element pattern, separator keyword,
	 * and unlimited min and max elements.
	 * 
	 * @param element a pattern that should match every element in the list
	 * @param separator a keyword that separates elements in the list
	 */
	public List(Pattern element, Keyword separator) {
		this(element, separator, NO_LIMIT, NO_LIMIT);
	}
	
	/**
	 * Constructs a new list with a given element object (that will be {@link
	 * Pattern#toPattern(Object) converted to a pattern}), separator keyword,
	 * and unlimited min and max elements.
	 * 
	 * @param element an object that will be converted to a pattern that
	 * should match every element in the list
	 * @param separator a keyword that separates elements in the list
	 */
	public List(Object element, Keyword separator) {
		this(Pattern.toPattern(element), separator);
	}
	
	/**
	 * Constructs a new list with a given element pattern, separator string
	 * (that will be converted into a {@link Keyword keyword}), and unlimited
	 * min and max elements.
	 * 
	 * @param element a pattern that should match every element in the list
	 * @param separator a string that separates elements in the list
	 */
	public List(Pattern element, String separator) {
		this(element, new Keyword(separator));
	}
	
	/**
	 * Constructs a new list with a given element object (that will be {@link
	 * Pattern#toPattern(Object) converted to a pattern}), separator string
	 * (that will be converted to a {@link Keyword keyword}), and unlimited
	 * min and max elements.
	 * 
	 * @param element an object that will be converted to a pattern that
	 * should match every element in the list
	 * @param separator a string that separates elements in the list
	 */
	public List(Object element, String separator) {
		this(Pattern.toPattern(element), separator);
	}
	
	@Override
	public String toString() {
		return element + " " + separator + " ...";
	}

	@Override
	public ParseTree match(Parser parser, ImmutableList<Token> tokens) throws ParseException {
		ImmutableList<Token>[] clips = clip(tokens, find(separator, tokens), 0);
		if((min != NO_LIMIT && clips.length < min) || (max != NO_LIMIT && clips.length > max))
			throw Exceptions.parseListCount(clips.length, min, max, tokens);
		ParseTree[] children = new ParseTree[clips.length];
		for(int i=0; i<children.length; i++)
			children[i] = parser.match(element, clips[i]);
		if(element instanceof Keyword)
			return new ParseTree(this, tokens);
		else
			return new ParseTree(this, tokens, children);
	}
	
	@SuppressWarnings("unchecked")
	private final ImmutableList<Token>[] clip(ImmutableList<Token> start, ImmutableList<Token> end, int size) {
		if(end == null || end.size() == 0) {
			if(start.size() == 0)
				return new ImmutableList[size];
			else {
				ImmutableList<Token>[] clips = new ImmutableList[size + 1];
				clips[clips.length - 1] = start;
				return clips;
			}
		}
		else {
			ImmutableList<Token>[] clips = clip(end.rest, find(separator, end.rest), size + 1);
			clips[size] = Pattern.clip(start, start.size() - end.size());
			return clips;
		}
	}
}