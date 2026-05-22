package edu.uky.cs.nil.sabre;

import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * A character is a special {@link Entity entity} that represents an intelligent
 * being with its own (possibly wrong) beliefs about the world state and its own
 * {@link Problem#utilities utility function} that it will attempt to maximize.
 * The term 'character' is used instead of 'agent' to emphasize that, while
 * a character's actions need to make sense based on their beliefs and
 * intentions, they are not actually independent decision-makers. The {@link
 * edu.uky.cs.nil.sabre.search.Planner planner} is the only decision-maker, but
 * it should ensure that characters appear to be acting like independent agents.
 * <p>
 * Characters are always listed first in {@link Universe#entities} so that their
 * ID numbers correspond to their index in both {@link Universe#entities} and
 * {@link Universe#characters}.
 * 
 * @author Stephen G. Ware
 */
public class Character extends Entity {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;

	/**
	 * Constructs a new character.
	 * 
	 * @param universe the universe to which this character belongs
	 * @param id the character's unique sequential ID number
	 * @param name the character's name
	 * @param types a set of types that apply to this character
	 * @param comment the comment
	 */
	Character(Universe universe, int id, String name, ImmutableSet<Type> types, String comment) {
		super(universe, id, name, types, comment);
	}
	
	/**
	 * Constructs a character which is a copy of an entity from a previous
	 * universe.
	 * 
	 * @param original the entity form the original universe to copy
	 * @param constructor the universe constructor
	 */
	Character(Entity original, UniverseConstructor constructor) {
		super(original, constructor);
	}
	
	@Override
	public Character apply(java.util.function.Function<Object, Object> function) {
		return this;
	}
	
	@Override
	public boolean isCharacter() {
		return true;
	}
	
	@Override
	public Character simplify() {
		return this;
	}
	
	@Override
	public Character evaluate(State state) {
		return this;
	}
	
	@Override
	public Character prepend(Parameter character) {
		return this;
	}
}