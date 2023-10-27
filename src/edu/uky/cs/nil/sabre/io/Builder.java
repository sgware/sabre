package edu.uky.cs.nil.sabre.io;

/**
 * A builder converts a {@link ParseTree parse tree} into an object. Building
 * is the last stage of {@link Parser parsing} on object from text.
 * 
 * @author Stephen G. Ware
 */
@FunctionalInterface
public interface Builder {

	/**
	 * Convert a {@link ParseTree parse tree} into an object.
	 * 
	 * @param parser the parser which called this method, in case it is needed
	 * to {@link Parser#build(ParseTree, Class, Definitions) build} and of the
	 * {@link ParseTree#children parse tree's children}
	 * @param tree the parse tree to be converted into an object
	 * @param definitions a set of other objects which have already been
	 * defined in the same context and which may be referenced by this parse
	 * tree
	 * @return an object created according to the parse tree
	 * @throws ParseException if the parse tree does not define a valid object
	 */
	public Object build(Parser parser, ParseTree tree, Definitions definitions) throws ParseException;
}