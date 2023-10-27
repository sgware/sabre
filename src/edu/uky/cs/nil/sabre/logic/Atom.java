package edu.uky.cs.nil.sabre.logic;

/**
 * An atom is a {@link Proposition} which cannot be further simplified (meaning
 * {@link #simplify()} always returns this atom itself).
 * 
 * @author Stephen G. Ware
 */
public interface Atom extends Proposition {
	
	@Override
	public default Atom simplify() {
		return this;
	}
	
	@Override
	public Atom prepend(Parameter character);
	
	/**
	 * If a {@link Conjunction conjunction} of this atom and a given atom can
	 * be simplified to a single atom, this method returns that atom; if not,
	 * null is returned. If this method returns null, it does not mean the
	 * atoms are contradictory (see {@link #negates(Atom)}), only that they
	 * cannot be combined into a single atom.
	 * 
	 * @param other the other atom which can possibly be combined with this one
	 * @return a single atom which expresses both this atom and the given atom,
	 * or null if no such atom exists
	 */
	public Atom combine(Atom other);
	
	/**
	 * Tests whether a {@link Conjunction conjunction} of this atom and a given
	 * atom would be a contradiction.
	 * 
	 * @param other the other atom which might contradict this one
	 * @return true if the atoms contradict one another, false otherwise
	 */
	public boolean negates(Atom other);
}