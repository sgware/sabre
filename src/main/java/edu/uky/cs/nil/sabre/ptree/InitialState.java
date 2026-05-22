package edu.uky.cs.nil.sabre.ptree;

import java.io.Serializable;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Beliefs;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.comp.CompiledFluent;
import edu.uky.cs.nil.sabre.comp.CompiledProblem;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * A special object for representing the initial state that a {@link
 * ProgressionTree progression tree} {@link ProgressionTree#initialize(State)
 * starts in}.
 * 
 * @author Stephen G. Ware
 */
final class InitialState implements Serializable {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The problem whose state is being modeled */
	public final CompiledProblem problem;
	
	/** The state a progression problem will start in */
	public final State state;
	
	/**
	 * The values of each fluent in that state, where the value for each fluent
	 * is stored in the index of that fluent's ID
	 */
	private final Value[] values;
	
	/**
	 * Constructs a new initial state object.
	 * 
	 * @param problem the problem whose state is being modeled
	 * @param state a state for that problem
	 */
	public InitialState(CompiledProblem problem, State state) {
		this.problem = problem;
		this.state = state;
		this.values = new Value[problem.fluents.size()];
		for(int i=0; i<problem.fluents.size(); i++) {
			CompiledFluent fluent = problem.fluents.get(i);
			this.values[fluent.id] = state.getValue(fluent);
		}
	}
	
	/**
	 * Returns the value of a {@link CompiledFluent compiled fluent} in the
	 * initial state represented by this object.
	 * 
	 * @param fluent the fluent whose value is desired
	 * @return the value of that fluent
	 */
	public Value getValue(CompiledFluent fluent) {
		return values[fluent.id];
	}
	
	/**
	 * Returns a state reflecting the beliefs of a character in this state.
	 * 
	 * @param character the character whose beliefs are desired
	 * @return a state representing that character's beliefs about this state
	 */
	public InitialState getBeliefs(Character character) {
		return new InitialState(problem, new Beliefs(character, state));
	}
}