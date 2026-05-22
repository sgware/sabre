package edu.uky.cs.nil.sabre.io;

/**
 * A {@link Builder builder} which returns a {@link Definitions defined
 * object}. This builder assumes the {@link ParseTree parse tree} given to its
 * {@link #build(Parser, ParseTree, Definitions) build} method has exactly one
 * {@link ParseTree#children child} and that the child can be {@link
 * Parser#build(ParseTree, Class) built} as a String. That string and the
 * {@link #type provided type} will be used to {@link
 * Definitions#require(String, Class) find a defined object}. Defined object
 * builders are typically used for referencing named things defined in a {@link
 * edu.uky.cs.nil.sabre.Universe universe}, such as {@link
 * edu.uky.cs.nil.sabre.Entity entities}, or {@link
 * edu.uky.cs.nil.sabre.logic.Variable variables} defined in the current
 * context.
 * 
 * @author Stephen G. Ware
 */
public class DefinedObjectBuilder implements Builder {
	
	/** The type of the defined object */
	public final Class<?> type;
	
	/**
	 * Constructs a new defined object builder.
	 * 
	 * @param type the type of the defined object
	 */
	public DefinedObjectBuilder(Class<?> type) {
		this.type = type;
	}

	@Override
	public Object build(Parser parser, ParseTree tree, Definitions definitions) throws ParseException {
		return definitions.require(parser.build(tree.get(0), String.class), type);
	}
}