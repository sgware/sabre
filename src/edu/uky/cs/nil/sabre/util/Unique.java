package edu.uky.cs.nil.sabre.util;

/**
 * A unique object is one of a collection whose {@link #hashCode() hash codes}
 * are unique, sequential integers starting at 0. They are especially suited
 * as keys for {@link UniqueMap unique maps}.
 *
 * @author Stephen G. Ware
 */
public interface Unique {

	@Override
	public int hashCode();
}