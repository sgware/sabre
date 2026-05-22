package edu.uky.cs.nil.sabre.io;

import java.math.BigDecimal;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.util.ImmutableList;

/**
 * A terminal {@link Symbol symbol} is a primitive element in a grammar used by
 * a {@link Parser parser} which cannot be broken down into smaller parts.
 * 
 * @author Stephen G. Ware
 */
public abstract class Terminal extends Symbol {

	Terminal() {}
	
	/**
	 * A terminal symbol representing no tokens. This patterns only matches an
	 * empty list of {@link Token tokens}.
	 */
	public static final Terminal NOTHING = new Terminal() {
		
		@Override
		public String toString() {
			return "nothing";
		}

		@Override
		public ParseTree match(Parser parser, ImmutableList<Token> tokens) throws ParseException {
			if(tokens.size() == 0)
				return new ParseTree(this, tokens);
			else
				throw Exceptions.parseUnexpected(toString(), "\"" + tokens.first.value + "\"", tokens);
		}
	};
	
	/**
	 * A terminal symbol that matches any single token.
	 */
	public static final Terminal TOKEN = new Single() {
		
		@Override
		public String toString() {
			return "token";
		}

		@Override
		protected boolean match(String value) {
			return true;
		}
	};
	
	/**
	 * A terminal symbol that matches a name, which is any sequence that starts
	 * with {@link Character#isAlphabetic(int) a letter of the alphabet}.
	 */
	public static final Terminal NAME = new Single() {
		
		@Override
		public String toString() {
			return "name";
		}

		@Override
		protected boolean match(String value) {
			return value.length() > 0 && Character.isAlphabetic(value.charAt(0));
		}
	};
	
	/**
	 * A terminal symbol that matches a numeric literal, which is any string
	 * that can be used to create a {@link BigDecimal}.
	 */
	public static final Terminal NUMBER = new Single() {
		
		@Override
		public String toString() {
			return "number";
		}

		@Override
		protected boolean match(String value) {
			try {
				new BigDecimal(value);
				return true;
			}
			catch(NumberFormatException e) {
				return false;
			}
		}
	};
	
	/**
	 * A terminal symbol that matches a string literal, which must begin and end
	 * with a double quotation mark character.
	 */
	public static final Terminal STRING = new Single() {
		
		@Override
		public String toString() {
			return "string";
		}

		@Override
		protected boolean match(String value) {
			return value.startsWith("\"") && value.endsWith("\"");
		}
	};
	
	/**
	 * A terminal symbol that matches any token which is not a {@link
	 * Terminal#NAME name}, not a {@link Terminal#NUMBER number}, and not a
	 * {@link Terminal#STRING string literal}.
	 */
	public static final Terminal SYMBOL = new Single() {
		
		@Override
		public String toString() {
			return "symbol";
		}

		@Override
		protected boolean match(String value) {
			return !((Single) NAME).match(value) && !((Single) NUMBER).match(value) && !((Single) STRING).match(value);
		}
	};
}