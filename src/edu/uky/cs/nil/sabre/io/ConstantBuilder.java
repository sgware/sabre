package edu.uky.cs.nil.sabre.io;

/**
 * A {@link Builder builder} which always returns the same object regardless of
 * what input it receives. Constant builders are typically used for parsing
 * universally-defined objects that can be identified by keywords, such as the
 * logical constants {@link edu.uky.cs.nil.sabre.logic.True#TRUE true} and
 * {@link edu.uky.cs.nil.sabre.logic.False#FALSE false}.
 * 
 * @author Stephen G. Ware
 */
public class ConstantBuilder implements Builder {
	
	/**
	 * The object that will always be returned by {@link
	 * #build(Parser, ParseTree, Definitions)}
	 */
	public final Object constant;
	
	/**
	 * Constructs a new constant builder whose {@link #build(Parser, ParseTree,
	 * Definitions)} method will always return the given object.
	 * 
	 * @param constant the object to always return
	 */
	public ConstantBuilder(Object constant) {
		this.constant = constant;
	}

	@Override
	public Object build(Parser parser, ParseTree tree, Definitions definitions) throws ParseException {
		return constant;
	}
}