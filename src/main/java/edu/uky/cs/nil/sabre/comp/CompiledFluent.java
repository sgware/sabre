package edu.uky.cs.nil.sabre.comp;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.FormatException;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Signature;
import edu.uky.cs.nil.sabre.Type;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.util.ImmutableArray;
import edu.uky.cs.nil.sabre.util.Unique;

/**
 * A compiled fluent is a {@link Fluent fluent} whose {@link #characters
 * characters} and {@link Fluent#signature signature} are {@link
 * edu.uky.cs.nil.sabre.logic.Logical#isGround() ground} and which defines a
 * {@link Unique unique} {@link #id ID number} which corresponds to the
 * fluent's index in its {@link CompiledProblem#fluents compiled problem's set
 * of fluents}.
 * 
 * @author Stephen G. Ware
 */
public class CompiledFluent extends Fluent implements Unique {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * A unique ID number that corresponds to this fluent's index in {@link
	 * CompiledProblem#fluents its compiled problem's set of fluents}
	 */
	public final int id;
	
	/**
	 * An ordered list of {@link Character characters} specifying whose beliefs
	 * this fluent represents
	 */
	public final ImmutableArray<Character> characters;
	
	/**
	 * The compiled fluent whose signature is identical but whose list of
	 * characters is the same except that the first character has been removed
	 */
	private final CompiledFluent parent;
	
	/**
	 * Compiled fluents whose signatures are identical but with one additional
	 * character added to the start of the list of characters
	 */
	private final CompiledFluent[] children;
	
	/**
	 * Constructs a new compiled fluent by prepending a character onto a parent.
	 * 
	 * @param id a unique ID number
	 * @param character the first character in this fluent's list of characters
	 * @param parent the fluent's parent; its signature will be copied and the
	 * character will be prepended to the start of its parent's list of
	 * characters
	 */
	protected CompiledFluent(int id, Character character, CompiledFluent parent) {
		super(prepend(character, parent.characters), parent.signature, parent.type, parent.comment);
		this.id = id;
		this.characters = super.characters.cast(Character.class);
		this.parent = parent;
		this.children = new CompiledFluent[type.universe.characters.size()];
		parent.children[character.id] = this;
	}
	
	private static final ImmutableArray<Parameter> prepend(Character character, ImmutableArray<Character> characters) {
		Character[] array = new Character[characters.size() + 1];
		array[0] = character;
		for(int i=0; i<characters.size(); i++)
			array[i + 1] = characters.get(i);
		return new ImmutableArray<Parameter>(array);
	}
	
	/**
	 * Constructs a new compiled fluent with an empty list of characters.
	 * 
	 * @param id a unique ID number
	 * @param signature the fluent's signature, which must be ground
	 * @param type the fluent's type
	 * @param comment the comment
	 * @throws FormatException if the signature is not ground
	 */
	protected CompiledFluent(int id, Signature signature, Type type, String comment) {
		super(signature, type, comment);
		signature.mustBeGround();
		this.id = id;
		this.characters = super.characters.cast(Character.class);
		this.parent = this;
		this.children = new CompiledFluent[type.universe.characters.size()];
	}
	
	@Override
	public boolean equals(Object other) {
		return this == other;
	}
	
	@Override
	public int hashCode() {
		return id;
	}
	
	@Override
	public boolean isGround() {
		return true;
	}
	
	@Override
	public CompiledFluent apply(Function<Object, Object> function) {
		return this;
	}
	
	@Override
	public CompiledFluent simplify() {
		return this;
	}
	
	@Override
	public Fluent prepend(Parameter character) {
		if(character instanceof Character) {
			CompiledFluent child = children[((Character) character).id];
			if(child != null)
				return child;
		}
		return new PrefixFluent(character, this);
	}
	
	@Override
	public CompiledFluent removeFirstCharacter() {
		return parent;
	}
}