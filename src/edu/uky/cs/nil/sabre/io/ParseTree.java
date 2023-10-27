package edu.uky.cs.nil.sabre.io;

import java.util.Collection;
import java.util.Iterator;

import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.util.ImmutableArray;
import edu.uky.cs.nil.sabre.util.ImmutableList;

/**
 * A parse tree is the result of successfully {@link Pattern#match(Parser,
 * ImmutableList) matching} a sequence of {@link Token tokens} to a {@link
 * Pattern pattern}. Parse trees are an intermediate step in parsing an object
 * and will usually be given to a {@link Builder builder} to create an object.
 * 
 * @author Stephen G. Ware
 */
public class ParseTree implements Iterable<ParseTree> {
	
	/** The pattern which matches the sequence of tokens */
	public final Pattern pattern;
	
	/** The sequence of tokens that matches the pattern */
	public final ImmutableList<Token> tokens;
	
	/**
	 * The results of matching any smaller patterns that appear inside this
	 * tree's pattern
	 */
	public final ImmutableArray<ParseTree> children;
	
	/**
	 * Constructs a new parse tree. Note that child trees that match non-{@link
	 * Symbol symbols} will be flattened one level. For example, if one of the
	 * children given is a parse tree matching a {@link List list}, rather than
	 * adding that tree as a child, that tree's children will be added as
	 * children of this tree.
	 * 
	 * @param pattern the pattern that matches the sequence of tokens
	 * @param tokens the sequence of tokens
	 * @param children the results of matching any smaller patterns that appear
	 * inside this tree's pattern
	 */
	public ParseTree(Pattern pattern, ImmutableList<Token> tokens, ParseTree...children) {
		this.pattern = pattern;
		this.tokens = tokens;
		ParseTree[] array = children(pattern, children, 0, 0);
		for(int i=0; i<array.length; i++)
			if(array[i].pattern.equals(pattern))
				array[i] = array[i].get(0);
		this.children = new ImmutableArray<>(array);
	}
	
	private static final ParseTree[] children(Pattern pattern, ParseTree[] array, int index, int size) {
		if(index == array.length)
			return new ParseTree[size];
		else if(array[index].pattern instanceof Symbol) {
			ParseTree[] children = children(pattern, array, index + 1, size + 1);
			children[size] = array[index];
			return children;
		}
		else {
			ParseTree[] children = children(pattern, array, index + 1, size + array[index].size());
			for(int i=0; i<array[index].size(); i++)
				children[size + i] = array[index].get(i);
			return children;
		}
	}
	
	/**
	 * Constructs a new parse tree. Note that child trees that match non-{@link
	 * Symbol symbols} will be flattened one level. For example, if one of the
	 * children given is a parse tree matching a {@link List list}, rather than
	 * adding that tree as a child, that tree's children will be added as
	 * children of this tree.
	 * 
	 * @param pattern the pattern that matches the sequence of tokens
	 * @param tokens the sequence of tokens
	 * @param children the results of matching any smaller patterns that appear
	 * inside this tree's pattern
	 */
	public ParseTree(Pattern pattern, ImmutableList<Token> tokens, Collection<ParseTree> children) {
		this(pattern, tokens, Utilities.toArray(children, ParseTree.class));
	}
	
	@Override
	public String toString() {
		return toString(this, 0);
	}
	
	private static final String toString(ParseTree tree, int indent) {
		String string = "";
		for(int i=0; i<indent; i++)
			string += "| ";
		string += tree.pattern;
		if(tree.pattern instanceof Terminal && tree.tokens.size() > 0 && !(tree.pattern instanceof Keyword)) {
			string += ":";
			ImmutableList<Token> tokens = tree.tokens;
			while(tokens.size() > 0) {
				string += " " + tokens.first;
				tokens = tokens.rest;
			}
		}
		for(ParseTree child : tree)
			string += "\n" + toString(child, indent + 1);
		return string;
	}

	@Override
	public Iterator<ParseTree> iterator() {
		return children.iterator();
	}
	
	/**
	 * Returns the number of {@link #children children} this tree has.
	 * 
	 * @return the number of children
	 */
	public int size() {
		return children.size();
	}
	
	/**
	 * Returns the child parse tree at a given index.
	 * 
	 * @param index the index of the desired child
	 * @return the child tree
	 */
	public ParseTree get(int index) {
		return children.get(index);
	}
	
	/**
	 * Returns true if this tree represents parsing an object described by a
	 * given {@link Symbol symbol}.
	 * 
	 * @param symbol a symbol
	 * @return true if this tree's pattern is that symbol or if this tree has
	 * exactly 1 child whose pattern is that symbol, false otherwise
	 */
	public boolean is(Symbol symbol) {
		return pattern.equals(symbol) || (size() == 1 && get(0).is(symbol));
	}
}