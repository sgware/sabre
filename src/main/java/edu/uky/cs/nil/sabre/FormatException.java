package edu.uky.cs.nil.sabre;

/**
 * A format exception is thrown when an object defined in a {@link Problem
 * planning problem} is constructed in an illegal way.
 * 
 * @author Stephen G. Ware
 */
public class FormatException extends RuntimeException {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;

	/**
	 * Constructs a new format exception with a given message.
	 * 
	 * @param message the message
	 */
	public FormatException(String message) {
		super(message);
	}
}