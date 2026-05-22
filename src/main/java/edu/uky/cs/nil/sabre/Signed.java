package edu.uky.cs.nil.sabre;

import edu.uky.cs.nil.sabre.logic.Logical;

/**
 * An interface for anything which has a {@link Signature}. This class
 * overrides {@link Logical#compareTo(Logical)} so that two signed objects will
 * be compared using their signatures.
 * 
 * @author Stephen G. Ware
 */
public interface Signed extends Logical {
	
	@Override
	public default int compareTo(Logical other) {
		if(getClass().equals(other.getClass()))
			return getSignature().compareTo(((Signed) other).getSignature());
		else
			return Logical.super.compareTo(other);
	}

	/**
	 * Returns the object's signature.
	 * 
	 * @return the signature
	 */
	public Signature getSignature();
}