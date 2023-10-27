package edu.uky.cs.nil.sabre.comp;

import java.util.HashMap;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.FiniteState;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A wrapper for a {@link FiniteState finite state} used when mapping {@link
 * Fluent fluents} from one version of a problem to another, such as when
 * compiling a {@link edu.uky.cs.nil.sabre.Problem problem}.
 * 
 * @author Stephen G. Ware
 */
final class MappedState implements FiniteState {
	
	/** A state from the old problem */
	public final FiniteState original;
	
	/** A function which maps fluents from the new problem to the old */
	public final Function<Fluent, Fluent> mapping;
	
	/** A table of states representing the beliefs of each character */
	private final HashMap<Character, MappedState> beliefs = new HashMap<>();
	
	/**
	 * Constructs a new mapped state.
	 * 
	 * @param original a state from the old problem
	 * @param mapping a function which maps fluents from the new problem to
	 * their original versions in the old problem
	 */
	public MappedState(FiniteState original, Function<Fluent, Fluent> mapping) {
		this.original = original;
		this.mapping = mapping;
	}

	@Override
	public Value getValue(Fluent fluent) {
		return original.getValue(mapping.apply(fluent));
	}

	@Override
	public FiniteState getBeliefs(Character character) {
		MappedState beliefs = this.beliefs.get(character);
		if(beliefs == null) {
			FiniteState child = original.getBeliefs(character);
			if(child.equals(original))
				beliefs = this;
			else
				beliefs = new MappedState(child, mapping);
		}
		return beliefs;
	}
}