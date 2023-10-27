package edu.uky.cs.nil.sabre.search;

import edu.uky.cs.nil.sabre.Settings;

/**
 * A search exception is a {@link RuntimeException} that will be caught if it
 * is thrown while a search {@link
 * Search#get(edu.uky.cs.nil.sabre.util.Worker.Status) is running}. The
 * {@link Exception#getMessage() exception's message} will be used as the
 * {@link Result#message result message} when the result of the search is
 * returned.
 * 
 * @author Stephen G. Ware
 */
public class SearchException extends RuntimeException {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * Constructs a new search exception with a given message and cause.
	 * 
	 * @param message the exception's message
	 * @param cause another {@link Throwable throwable} that led to this
	 * exception
	 */
	public SearchException(String message, Throwable cause) {
		super(message, cause);
	}
	
	/**
	 * Constructs a new search exception with a given message.
	 * 
	 * @param message the exception's message
	 */
	public SearchException(String message) {
		super(message);
	}
}