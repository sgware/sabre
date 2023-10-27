package edu.uky.cs.nil.sabre;

import java.io.Serializable;

import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A wrapper around any {@link State state} which represents an {@link Character
 * character}'s beliefs in that state. This wrapper simply {@link
 * edu.uky.cs.nil.sabre.logic.Expression#prepend(edu.uky.cs.nil.sabre.logic.Parameter)
 * prepends} the character onto every expression it evaluates and then evaluates
 * the expression in the wrapped state.
 * 
 * @author Stephen G. Ware
 */
public class Beliefs implements Serializable, State {

	/** Serial version ID */
	private static final long serialVersionUID = 1L;
	
	/** The character who has beliefs */
	public final Character character;
	
	/** The state in which the character has beliefs */
	public final State state;
	
	/**
	 * Constructs a new set of beliefs.
	 * 
	 * @param character the character who has beliefs
	 * @param state the state in which the character has beliefs
	 */
	public Beliefs(Character character, State state) {
		this.character = character;
		this.state = state;
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			Beliefs otherState = (Beliefs) other;
			return character.equals(otherState.character) && state.equals(otherState.state);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), character, state);
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public Value getValue(Fluent fluent) {
		return state.getValue(fluent.prepend(character));
	}
}