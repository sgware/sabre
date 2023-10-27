package edu.uky.cs.nil.sabre.comp;

import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.util.ImmutableArray;

/**
 * A special {@link Fluent fluent} object that may be created when reasoning
 * about beliefs and {@link CompiledFluent compiled fluents}.
 * <p>
 * Because compiled fluents have {@link CompiledFluent#id ID numbers} which
 * indicate their index in a {@link CompiledProblem#fluents compiled problem's
 * set of fluents}, new compiled fluents cannot be added to a compiled problem
 * after it is created. However, the {@link CompiledFluent#prepend(Parameter)}
 * method still needs to return something, so a prefix fluent is a special
 * object representing a compiled fluent (or another prefix fluent) to which an
 * {@link edu.uky.cs.nil.sabre.Character character} has been {@link
 * edu.uky.cs.nil.sabre.logic.Expression#prepend(Parameter) prepended} when
 * such a fluent does not exist in the compiled problem.
 * 
 * @author Stephen G. Ware
 */
public class PrefixFluent extends Fluent {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * The fluent whose signature is identical but whose list of characters is
	 * the same except that the first character has been removed
	 */
	protected final Fluent parent;
	
	private PrefixFluent(Parameter character, Fluent parent) {
		super(prepend(character, parent.characters), parent.signature, parent.type, parent.comment);
		this.parent = parent;
	}
	
	private static final ImmutableArray<Parameter> prepend(Parameter character, ImmutableArray<Parameter> characters) {
		Parameter[] parameters = new Parameter[characters.size() + 1];
		parameters[0] = character;
		for(int i=0; i<characters.size(); i++)
			parameters[i + 1] = characters.get(i);
		return new ImmutableArray<>(parameters);
	}
	
	/**
	 * Constructs a new prefix fluent where a given character is {@link
	 * CompiledFluent#prepend(Parameter) prepended} to the start of a compiled
	 * fluent's {@link CompiledFluent#characters characters}.
	 * 
	 * @param character the character to prepend to the fluent
	 * @param parent the compiled fluent to which the character will be
	 * prepended
	 */
	protected PrefixFluent(Parameter character, CompiledFluent parent) {
		this(character, (Fluent) parent);
	}
	
	/**
	 * Constructs a new prefix fluent where a given character is {@link
	 * CompiledFluent#prepend(Parameter) prepended} to the start of a prefix
	 * fluent's {@link CompiledFluent#characters characters}.
	 * 
	 * @param character the character to prepend to the fluent
	 * @param parent the prefix fluent to which the character will be prepended
	 */
	protected PrefixFluent(Parameter character, PrefixFluent parent) {
		this(character, (Fluent) parent);
	}
	
	@Override
	public PrefixFluent prepend(Parameter character) {
		return new PrefixFluent(character, this);
	}
	
	@Override
	public Fluent removeFirstCharacter() {
		return parent;
	}
}