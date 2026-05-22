package edu.uky.cs.nil.sabre;

/**
 * A finite state is a {@link State state} which can {@link
 * #getBeliefs(Character) return the beliefs} of any {@link Character character}
 * as a state object, such that the graph of all states is finite. In other
 * words, it is possible to generate the current state, every character's
 * beliefs about the current state, their beliefs about beliefs, and so on, and
 * this will not lead to an infinite number of state objects being created.
 * 
 * @author Stephen G. Ware
 */
public interface FiniteState extends State {

	/**
	 * Returns an character's beliefs as a {@link FiniteState}.
	 * 
	 * @param character the character
	 * @return the character's beliefs as a finite state object
	 */
	public FiniteState getBeliefs(Character character);
}