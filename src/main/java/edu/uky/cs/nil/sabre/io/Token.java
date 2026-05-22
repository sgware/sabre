package edu.uky.cs.nil.sabre.io;

/**
 * A token is a single unit of input text, such as a keyword, symbol, bracket,
 * string literal, number, or operator.
 * 
 * @author Stephen G. Ware
 */
public class Token {
	
	/** The text of the token */
	public final String value;
	
	/** The comment associated with the token or null */
	public final String comment;
	
	/** The position in the input stream where this token began */
	public final int position;
	
	/** The line number in the input stream where this token began */
	public final int line;
	
	/** The character number in the line where this token began */
	public final int character;
	
	/**
	 * Constructs a new token.
	 * 
	 * @param value the text value of the token
	 * @param comment the comment associated with the token, or null
	 * @param position the position in the input stream where this token began
	 * @param line the line number in the input stream where this token began
	 * @param character the character number in the line where this token began
	 */
	Token(String value, String comment, int position, int line, int character) {
		this.value = value;
		this.comment = comment;
		this.position = position;
		this.line = line;
		this.character = character;
	}
	
	@Override
	public String toString() {
		return value;
	}
}