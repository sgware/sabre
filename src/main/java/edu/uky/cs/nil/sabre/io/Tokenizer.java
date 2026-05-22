package edu.uky.cs.nil.sabre.io;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.LinkedHashSet;

import edu.uky.cs.nil.sabre.util.ImmutableArray;
import edu.uky.cs.nil.sabre.util.ImmutableList;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * A tokenizer converts an input stream of text into a {@link ImmutableList
 * list} of {@link Token tokens}, which are meaningful units of text annotated
 * with where they occurred in the stream and any comments associated with
 * them.
 * <p>
 * Tokens are usually delineated by whitespace, but a tokenizer can be
 * initialized with a list of special symbols that will always be their own
 * tokens regardless of whitespace. For example, if the open and close
 * parenthesis characters are special symbols, the text <code>"(word)"</code>
 * will be split into three tokens: <code>"("</code>, <code>"word"</code>, and
 * <code>")"</code> despite their being no whitespace in the text.
 * <p>
 * A tokenizer removes comments from the input text, but the text of those
 * comments is preserved in the {@link Token#comment} field. Comments are
 * associated with the token immedately after the end of the comment. By
 * default, a tokenizer recognizes C-style block and line comments, but this
 * behavior can be changed by overriding the {@link #isComment(String)}
 * method.
 * 
 * @author Stephen G. Ware
 */
public class Tokenizer {
	
	/** A default list of symbols */
	public static final ImmutableSet<String> DEFAULT_SYMBOLS = new ImmutableSet<>(new String[] {
		"//", "/*",
		"(", ")", "{", "}", "[", "]",
		",", DefaultParser.DEFINITION_SEPARATOR, DefaultParser.INHERITANCE_KEYWORD,
		DefaultParser.MULTIPLICATION_KEYWORD, DefaultParser.DIVISION_KEYWORD, DefaultParser.ADDITION_KEYWORD, DefaultParser.SUBTRACTION_KEYWORD,
		DefaultParser.CONJUNCTION_KEYWORD, DefaultParser.DISJUNCTION_KEYWORD, DefaultParser.NEGATION_KEYWORD,
		DefaultParser.EQUAL_TO_KEYWORD, DefaultParser.NOT_EQUAL_TO_KEYWORD, DefaultParser.GREATER_THAN_OR_EQUAL_TO_KEYWORD, DefaultParser.LESS_THAN_OR_EQUAL_TO_KEYWORD, DefaultParser.GREATER_THAN_KEYWORD, DefaultParser.LESS_THAN_KEYWORD,
		DefaultParser.ASSIGNMENT_KEYWORD,
	});
	
	/**
	 * Represents the three basic operations the tokenizer can perform while
	 * processing input text.
	 * 
	 * @author Stephen G. Ware
	 */
	protected enum Instruction {
		
		/**
		 * Remove the first character from the buffer and add it to the end of
		 * the current token
		 */
		READ,
		
		/**
		 * Read a new character from the input stream and add it to the end of
		 * the buffer
		 */
		BUFFER,
		
		/** Mark the current token as complete and start a new one */
		CUT
	}
	
	/**
	 * A set of special symbols which are always their own token regardless of
	 * whitespace
	 */
	protected final LinkedHashSet<String> symbols = new LinkedHashSet<>();
	
	/** A set of substrings of every symbol */
	protected final LinkedHashSet<String> prefixes = new LinkedHashSet<>();
	
	/**
	 * Constructs a new tokenizer with the given list of symbols.
	 * 
	 * @param symbols a set of symbols which will always be their own tokens
	 * regardless of whitespace
	 */
	protected Tokenizer(Iterable<String> symbols) {
		for(String symbol : symbols) {
			this.symbols.add(symbol);
			while(!symbol.isEmpty()) {
				prefixes.add(symbol);
				symbol = symbol.substring(0, symbol.length() - 1);
			}
		}
	}
	
	/**
	 * Constructs a new tokenizer with the given array of symbols.
	 * 
	 * @param symbols a set of symbols which will always be their own tokens
	 * regardless of whitespace
	 */
	protected Tokenizer(String...symbols) {
		this(new ImmutableArray<>(symbols));
	}
	
	/**
	 * Constructs a new tokenizer with the default set of symbols.
	 */
	public Tokenizer() {
		this(DEFAULT_SYMBOLS);
	}
	
	/**
	 * Reads every character of input from a {@link Reader reader} and converts
	 * them into a {@link ImmutableList list} of {@link Token tokens}.
	 * 
	 * @param reader the text to be read
	 * @return a list of tokens
	 * @throws IOException if an exception occurs while reading from the reader
	 */
	public ImmutableList<Token> tokenize(Reader reader) throws IOException {
		ArrayList<Token> tokens = new ArrayList<>(512);
		String token = "";
		String comment = null;
		int position = 0;
		int line = 1;
		int character = 1;
		int read = reader.read();
		String buffer = read == -1 ? "" : Character.toString(read);
		while(!(read == -1 && token.isEmpty() && buffer.isEmpty())) {
			Instruction instruction = getInstruction(token, buffer);
			// Can't cut an empty token.
			if(instruction == Instruction.CUT && token.isEmpty())
				instruction = Instruction.READ;
			// Can't read from an empty buffer.
			if(instruction == Instruction.READ && buffer.isEmpty())
				instruction = Instruction.BUFFER;
			// Can't read from a finished stream.
			if(instruction == Instruction.BUFFER && read == -1) {
				if(buffer.isEmpty())
					instruction = Instruction.CUT;
				else
					instruction = Instruction.READ;
			}
			// Process instruction
			switch(instruction) {
			// Move one character from the buffer to the token.
			case READ:
				token += buffer.substring(0, 1);
				buffer = buffer.substring(1);
				break;
			// Read another character into the buffer.
			case BUFFER:
				read = reader.read();
				if(read != -1)
					buffer += Character.toString(read);
				break;
			// The current token is complete.
			case CUT:
				if(isComment(token))
					comment = trimComment(token);
				else if(!ignore(token)) {
					tokens.add(new Token(token, comment, position, line, character));
					comment = null;
				}
				for(int i=0; i<token.length(); i++) {
					position++;
					if(token.charAt(i) == '\n') {
						line++;
						character = 1;
					}
					else
						character++;
				}
				token = "";
			}
		}
		ImmutableList<Token> result = ImmutableList.EMPTY.cast(Token.class);
		for(int i=tokens.size()-1; i>=0; i--)
			result = result.add(tokens.get(i));
		return result;
	}
	
	/**
	 * A tokenizer maintains two strings: the current token being built and
	 * a buffer. The tokenizer repeatedly calls this method which returns one
	 * of three {@link Instruction instructions}. Read means the tokenizer
	 * should remove the first token of the buffer and add it to the end of the
	 * token. Buffer means the tokenizer should read a new character of text
	 * onto the end of the buffer. Cut means the current token is complete, so
	 * the tokenizer will add it to the list and begin a new empty token.
	 * 
	 * @param token the current token being build
	 * @param buffer a buffer of characters read from the input stream
	 * @return the instruction the tokenizer should follow next
	 */
	protected Instruction getInstruction(String token, String buffer) {
		// Comments
		if(token.startsWith("//")) {
			if(buffer.startsWith("\n") || buffer.startsWith("\r"))
				return Instruction.CUT;
		}
		else if(token.startsWith("/*")) {
			if(token.endsWith("*/"))
				return Instruction.CUT;
		}
		// String literals
		else if(token.startsWith("\"")) {
			if(token.length() > 1 && !token.endsWith("\\\"") && token.endsWith("\""))
				return Instruction.CUT;
		}
		// Numbers
		else if(isNumber(token)) {
			if(buffer.isEmpty())
				return Instruction.BUFFER;
			else if(!isNumber(token + buffer.substring(0, 1)))
				return Instruction.CUT;
		}
		// Symbols
		else if(symbols.contains(token)) {
			if(buffer.isEmpty())
				return Instruction.BUFFER;
			else if(!symbols.contains(token + buffer.substring(0, 1)))
				return Instruction.CUT;
		}
		else if(symbols.contains(buffer))
			return Instruction.CUT;
		else if(prefixes.contains(buffer))
			return Instruction.BUFFER;
		// No whitespace at beginning of token
		else if(!token.isEmpty() && token.isBlank())
			return Instruction.CUT;
		// Cut token before whitespace
		else if(!token.isEmpty() && !buffer.isEmpty() && Character.isWhitespace(buffer.charAt(0)))
			return Instruction.CUT;
		// Default case
		return Instruction.READ;
	}
	
	/**
	 * Checks whether a string is formatted as a numeric literal.
	 * 
	 * @param string the string to check
	 * @return true if the string is a numeric literal, false otherwise
	 */
	private boolean isNumber(String string) {
		int position = string.startsWith("-") ? 1 : 0;
		boolean decimal = false;
		for(; position<string.length(); position++) {
			if(string.charAt(position) == '.' && !decimal)
				decimal = true;
			else if(!Character.isDigit(string.charAt(position)))
				return false;
		}
		return true;
	}
	
	/**
	 * Given a string that {@link #isComment(String) is a comment}, this method
	 * removes the characters to signal this string is a comment, preserving
	 * only the content of the comment.
	 * 
	 * @param string the comment string
	 * @return the content of the comment
	 */
	protected String trimComment(String string) {
		if(string.startsWith("//"))
			string = string.substring(2);
		else if(string.startsWith("/*")) {
			string = string.substring(2);
			if(string.endsWith("*/"))
				string = string.substring(0, string.length() - 2);
			String[] lines = string.split("\n");
			for(int i=0; i<lines.length; i++)
				lines[i] = lines[i].trim();
			string = lines[0];
			for(int i=1; i<lines.length; i++)
				string += "\n" + lines[i];
		}
		return string.trim();
	}
	
	/**
	 * Checks whether a string is formatted as a comment.
	 * 
	 * @param string the string to check
	 * @return true if the string is a comment, false otherwise
	 */
	protected boolean isComment(String string) {
		return string.startsWith("//") || (string.startsWith("/*"));
	}
	
	/**
	 * Checks whether a token should be ignored. By default, tokens
	 * composed entirely of whitespace are ignored.
	 * 
	 * @param string the token string to check
	 * @return true if the token should be ignored, false otherwise
	 */
	protected boolean ignore(String string) {
		return string.isBlank();
	}
}