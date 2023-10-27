package edu.uky.cs.nil.sabre.logic;

import java.util.function.Function;

/**
 * A simplifiable {@link Logical logical formula} is one which can possibly be
 * converted into a logically equivalent but smaller, simpler, or more
 * convenient form via the {@link #simplify()} method.
 * 
 * @author Stephen G. Ware
 */
public interface Simplifiable extends Logical {
	
	@Override
	public Simplifiable apply(Function<Object, Object> function);

	/**
	 * Returns a logically equivalent but smaller, simpler, or more convenient
	 * form of this logical formula.
	 * 
	 * @return a smaller, simpler, or more convenient logical formula which is
	 * equivalent to this formula
	 */
	public default Simplifiable simplify() {
		return (Simplifiable) apply(object -> object instanceof Simplifiable ? ((Simplifiable) object).simplify() : object);
	}
}
