package edu.uky.cs.nil.sabre.io;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.util.ImmutableList;

/**
 * A parse exception is thrown when some failure occurs while {@link Parser
 * parsing} an object.
 * 
 * @author Stephen G. Ware
 */
public class ParseException extends Exception {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The tokens being parsed when the exception occurred */
	public final ImmutableList<Token> tokens;
	
	/**
	 * Constructs a new parse exception that arose due to another exception.
	 * 
	 * @param message the parse exception message
	 * @param tokens the tokens being parsed when the exception occurred
	 * @param cause another exception that caused the parse exception
	 */
	public ParseException(String message, ImmutableList<Token> tokens, Throwable cause) {
		super(message, cause);
		this.tokens = tokens;
	}
	
	/**
	 * Constructs a new parse exception.
	 * 
	 * @param message the parse exception message
	 * @param tokens the tokens being parsed when the exception occurred
	 */
	public ParseException(String message, ImmutableList<Token> tokens) {
		super(message);
		this.tokens = tokens;
	}
	
	@Override
	public String getMessage() {
		String message = super.getMessage();
		if(!tokens.equals(ImmutableList.EMPTY))
			message += " (at line " + tokens.first.line + ", character " + tokens.first.character + ")";
		return message;
	}
}