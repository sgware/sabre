package edu.uky.cs.nil.sabre.io;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.math.BigDecimal;
import java.util.HashMap;

import edu.uky.cs.nil.sabre.Entity;
import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.FormatException;
import edu.uky.cs.nil.sabre.Number;
import edu.uky.cs.nil.sabre.Problem;
import edu.uky.cs.nil.sabre.ProblemBuilder;
import edu.uky.cs.nil.sabre.Type;
import edu.uky.cs.nil.sabre.Universe;
import edu.uky.cs.nil.sabre.util.ImmutableList;

/**
 * A parser converts text into objects according to rules. A parser works in
 * three stages. First, text is {@link Tokenizer tokenized}. Then the sequence
 * of tokens is matched against {@link Pattern patterns} to produce a {@link
 * ParseTree parse tree}. Finally, the parse tree is given to a {@link Builder
 * builder} to create the object.
 * <p>
 * A grammar, or list of parsing rules, is defined in terms of two kinds of
 * {@link Symbol symbols}: {@link NonTerminal non-terminal symbols} that
 * represent complex things and {@link Terminal terminal symbols} that
 * represent basic building blocks like keywords, names, numbers, and strings.
 * A parsing rule has a left-hand side which must always be a single
 * non-terminal symbol and a right-hand side which is a {@link Pattern
 * pattern}, or a way to check whether a list of tokens is formatted correctly
 * to represent an object. If there is more than one way to parse a
 * non-terminal symbol, the pattern may be a {@link Selection selection}, which
 * is a list of patterns that will be tried in order.
 * 
 * @author Stephen G. Ware
 */
public class Parser {
	
	private Tokenizer tokenizer = new Tokenizer();
	private final HashMap<Object, NonTerminal> nonTerminals = new HashMap<>();
	private final HashMap<NonTerminal, Pattern> rules = new HashMap<>();
	private final HashMap<Symbol, Builder> builders = new HashMap<>();
	private final Definitions definitions = new Definitions();
	
	/**
	 * Constructs a new parser with a {@link Tokenizer#Tokenizer() default
	 * tokenizer}, no parsing rules, and only simple {@link Builder builders}
	 * for for {@link Terminal terminal symbols}.
	 */
	public Parser() {
		setBuilder(Terminal.NOTHING, (p, t, d) -> null);
		setBuilder(Terminal.TOKEN, (p, t, d) -> t.tokens.first.value);
		setBuilder(Terminal.NAME, (p, t, d) -> t.tokens.first.value);
		setBuilder(Terminal.NUMBER, (p, t, d) -> Number.get(new BigDecimal(t.tokens.first.value)));
		setBuilder(Terminal.STRING, (p, t, d) -> t.tokens.first.value.substring(1, t.tokens.first.value.length() - 1));
		setBuilder(Terminal.SYMBOL, (p, t, d) -> t.tokens.first.value);
	}
	
	/**
	 * Returns this parser's tokenizer.
	 * 
	 * @return the tokenizer
	 */
	public Tokenizer getTokenizer() {
		return tokenizer;
	}
	
	/**
	 * Sets this parser's tokenizer.
	 * 
	 * @param tokenizer the new tokenizer to use
	 */
	public void setTokenizer(Tokenizer tokenizer) {
		this.tokenizer = tokenizer;
	}
	
	/**
	 * Reads all text from a {@link java.io.Reader reader} and converts it to
	 * a {@link ImmutableList list} of {@link Token tokens} using {@link
	 * #getTokenizer() this parser's tokenizer}.
	 * 
	 * @param reader the source of text
	 * @return the tokenized text
	 * @throws IOException if an exception occurs while reading from the reader
	 */
	protected ImmutableList<Token> tokenize(Reader reader) throws IOException {
		return getTokenizer().tokenize(reader);
	}
	
	/**
	 * Converts an object into a {@link Symbol symbol} that can be used to
	 * define parsing rules. If the object is a {@link Symbol symbol}, it is
	 * cast and returned, otherwise a {@link NonTerminal non-terminal symbol}
	 * is created with that object as the {@link NonTerminal#key key}.
	 * 
	 * @param key a symbol or an object that will be the key of a non-terminal
	 * symbol
	 * @return a symbol that can be used to define parsing rules
	 */
	protected Symbol getSymbol(Object key) {
		NonTerminal symbol;
		if(key instanceof NonTerminal) {
			symbol = nonTerminals.get(((NonTerminal) key).key);
			if(symbol == null) {
				symbol = (NonTerminal) key;
				nonTerminals.put(((NonTerminal) key).key, symbol);
			}
		}
		else if(key instanceof Symbol)
			return (Symbol) key;
		else {
			symbol = nonTerminals.get(key);
			if(symbol == null) {
				symbol = new NonTerminal(key);
				nonTerminals.put(key, symbol);
			}
		}
		return symbol;
	}
	
	/**
	 * Returns the {@link Pattern pattern} the parser will try to match tokens
	 * against when parsing text as a given non-terminal symbol. If there are
	 * multiple ways to parse the non-terminal symbol, the pattern returned
	 * may be a {@link Selection selection}.
	 * 
	 * @param symbol the non-terminal symbol whose parsing rule is desired
	 * @return the pattern this parser uses to parse that non-terminal symbol
	 */
	public Pattern getRule(NonTerminal symbol) {
		return rules.get(symbol);
	}
	
	/**
	 * Sets the {@link Pattern pattern} this parser will try to match tokens
	 * against when parsing text as the given non-terminal symbol.
	 * 
	 * @param symbol the non-terminal symbol whose parsing rule is to be set
	 * @param pattern the pattern this parser should use to parse the
	 * non-terminal symbol
	 */
	public void setRule(NonTerminal symbol, Pattern pattern) {
		rules.put((NonTerminal) getSymbol(symbol), pattern);
	}
	
	/**
	 * Adds an additional {@link Pattern pattern} to the end of the list of
	 * patterns that this parser will try to match tokens against when parsing
	 * text as the given non-terminal symbol. If the current pattern is a
	 * {@link Selection selection}, the given pattern is added to the end of
	 * {@link Selection#patterns its list}; otherwise, the pattern used for this
	 * non-terminal will be set to a selection of the current pattern and the
	 * given pattern.
	 * 
	 * @param symbol the non-terminal symbol to which a parsing rule will be
	 * added
	 * @param pattern an additional pattern this parser can use when trying
	 * to parse the non-terminal symbol
	 */
	public void addRule(NonTerminal symbol, Pattern pattern) {
		Pattern rule = getRule((NonTerminal) getSymbol(symbol));
		if(rule == null)
			setRule(symbol, pattern);
		else if(rule instanceof Selection)
			setRule(symbol, ((Selection) rule).add(pattern));
		else
			setRule(symbol, new Selection(rule, pattern));
	}
	
	/**
	 * Attempts to {@link Pattern#match(Parser, ImmutableList) match} a list of
	 * tokens to a {@link Pattern pattern}, returning a {@link ParseTree parse
	 * tree} on success or throwing a {@link ParseException parse exception} on
	 * failure. This method catches {@link FormatException format exceptions}
	 * that arise during matching and converts them to {@link ParseException
	 * parse exceptions}.
	 * 
	 * @param pattern the pattern to match tokens against
	 * @param tokens the tokens to match against the pattern
	 * @return a parse tree
	 * @throws ParseException if a parse exception or format exception was
	 * thrown during matching
	 */
	protected ParseTree match(Pattern pattern, ImmutableList<Token> tokens) throws ParseException {
		try {
			ParseTree result = pattern.match(this, tokens);
			return result;
		}
		catch(FormatException e) {
			throw new ParseException(e.getMessage(), tokens, e);
		}
	}
	
	/**
	 * Returns the {@link Builder builder} that will be used to create objects
	 * from {@link ParseTree parse trees} when parsing text as a given {@link
	 * Symbol symbol}.
	 * 
	 * @param symbol the symbol whose associated builder is desired
	 * @return the builder used to create objects when parsing text as this
	 * symbol
	 */
	public Builder getBuilder(Symbol symbol) {
		Builder builder = builders.get(symbol);
		if(builder == null && symbol instanceof Keyword) {
			builder = (p, t, d) -> t.tokens.first.value;
			setBuilder(symbol, builder);
		}
		return builder;
	}
	
	/**
	 * Sets the {@link Builder builder} that will be used to create objects
	 * from {@link ParseTree parse trees} when parsing text as a given {@link
	 * Symbol symbol}.
	 * 
	 * @param symbol the symbol whose associated builder will be changed
	 * @param builder the new builder to use when parsing text as that symbol
	 * @return the builder previously associated with the symbol, or null if
	 * no builder was previous associated with the symbol
	 */
	public Builder setBuilder(Symbol symbol, Builder builder) {
		return builders.put(symbol, builder);
	}
	
	/**
	 * Permanently {@link Definitions#add(String, Object) defines} an object
	 * by name; the object will now be defined in all contexts when {@link
	 * Builder#build(Parser, ParseTree, Definitions) building}.
	 * 
	 * @param name the object's name
	 * @param object the object
	 */
	public void define(String name, Object object) {
		definitions.add(name, object);
	}
	
	/**
	 * Permanently {@link Definitions#add(Object) defines} an object; the
	 * object will now be defined in all contexts when {@link
	 * Builder#build(Parser, ParseTree, Definitions) building}.
	 * 
	 * @param object the object
	 */
	public void define(Object object) {
		definitions.add(object);
	}
	
	/**
	 * Permanently {@link #define(String, Object) defines} a {@link Problem
	 * problem} and {@link Problem#universe its associated universe}.
	 * 
	 * @param problem the problem which will be permanently defined along
	 * with its universe
	 */
	public void define(Problem problem) {
		define((Object) problem);
		define(problem.universe);
	}
	
	/**
	 * Permanently {@link #define(String, Object) defines} a {@link
	 * edu.uky.cs.nil.sabre.Universe universe} and all of its {@link
	 * edu.uky.cs.nil.sabre.Type types} and {@link Entity entities}.
	 * 
	 * @param universe the universe which will be permanently defined along
	 * with all its types and entities
	 */
	public void define(Universe universe) {
		define((Object) universe);
		for(Type type : universe.types)
			define(type);
		for(Entity entity : universe.entities)
			define(entity);
	}
	
	/**
	 * Permanently {@link #define(String, Object) defines} a {@link
	 * edu.uky.cs.nil.sabre.Type type}.
	 * 
	 * @param type the type to be permanently defined
	 */
	public void define(Type type) {
		define(type.name, type);
	}
	
	/**
	 * Permanently {@link #define(String, Object) defines} an {@link
	 * edu.uky.cs.nil.sabre.Entity entity}.
	 * 
	 * @param entity the entity to be permanently defined
	 */
	public void define(Entity entity) {
		define(entity.name, entity);
	}
	
	/**
	 * Converts a {@link ParseTree parse tree} into an object of a given type
	 * using the {@link #getBuilder(Symbol) builder defined} for the {@link
	 * ParseTree#pattern parse tree's pattern} (which should be a {@link Symbol
	 * symbol}). If a {@link FormatException format exception} is thrown during
	 * building, it will be converted into {@link ParseException parse
	 * exception}.
	 * 
	 * @param <T> the type of object to be created
	 * @param tree the parse tree to be given to the builder
	 * @param type the Java class of the object to be created
	 * @param definitions a list of objects defined in the current context
	 * @return the object built from the parse tree
	 * @throws ParseException if a parse exception or format exception was
	 * thrown during building
	 * @throws ClassCastException if the object created cannot be cast to the
	 * given type
	 */
	protected <T> T build(ParseTree tree, Class<T> type, Definitions definitions) throws ParseException {
		Builder builder = null;
		if(tree.pattern instanceof Symbol)
			builder = getBuilder((Symbol) tree.pattern);
		if(builder == null) {
			if(tree.children.size() == 1 && tree.get(0).pattern instanceof Symbol)
				return build(tree.children.get(0), type, definitions);
			else
				throw Exceptions.noBuilder(tree.pattern.toString(), tree.tokens);
		}
		try {
			return type.cast(builder.build(this, tree, definitions.clone()));
		}
		catch(FormatException e) {
			throw new ParseException(e.getMessage(), tree.tokens, e);
		}
	}
	
	/**
	 * {@link #build(ParseTree, Class, Definitions) Builds} an object of a
	 * given type using the parser's {@link #define(String, Object) permanently
	 * defined objects}.
	 * 
	 * @param <T> the type of object to be created
	 * @param tree the parse tree to be built
	 * @param type the Java class of the object to be created
	 * @return the object built from the parse tree
	 * @throws ParseException if a parse exception or format exception was
	 * thrown during building
	 */
	protected <T> T build(ParseTree tree, Class<T> type) throws ParseException {
		return build(tree, type, definitions);
	}
	
	/**
	 * {@link #build(ParseTree, Class, Definitions) Builds} an object of type
	 * {@link Object} using a given set of {@link Definitions defined objects}.
	 * 
	 * @param tree the parse tree to built
	 * @param definitions a list of objects defined in the current context
	 * @return the object built from the parse tree
	 * @throws ParseException if a parse exception or format exception was
	 * thrown during building
	 */
	protected Object build(ParseTree tree, Definitions definitions) throws ParseException {
		return build(tree, Object.class, definitions);
	}
	
	/**
	 * {@link #build(ParseTree, Class, Definitions) Builds} an object of type
	 * {@link Object} using the parser's {@link #define(String, Object)
	 * permanently defined objects}.
	 * 
	 * @param tree the parse tree to built
	 * @return the object built from the parse tree
	 * @throws ParseException if a parse exception or format exception was
	 * thrown during building
	 */
	protected Object build(ParseTree tree) throws ParseException {
		return build(tree, definitions);
	}
	
	/**
	 * Reads all text from a {@link Reader reader} and converts it to an object
	 * according to this parser's {@link #getTokenizer() tokenizer}, {@link
	 * #setRule(NonTerminal, Pattern) parsing rules}, and {@link
	 * #getBuilder(Symbol) builders}.
	 * 
	 * @param <T> the type of object to be parsed
	 * @param reader the source of text to be parsed
	 * @param type the Java class of the object to be parsed
	 * @return the parsed object
	 * @throws IOException if an exception occurred while reading from the
	 * reader
	 * @throws ParseException if the text cannot be parsed as that kind of
	 * objects
	 * @throws ClassCastException if the object created cannot be cast to the
	 * given type
	 */
	public <T> T parse(Reader reader, Class<T> type) throws IOException, ParseException {
		return build(match(getSymbol(type), tokenize(reader)), type);
	}
	
	/**
	 * {@link #parse(Reader, Class) Parses} a string of text and converts it to
	 * an object.
	 * 
	 * @param <T> the type of object to be parsed
	 * @param string the string of text to be parsed
	 * @param type the Java class of the object to be parsed
	 * @return the parsed object
	 * @throws ParseException if the text cannot be parsed as that kind of
	 * objects
	 * @throws ClassCastException if the object created cannot be cast to the
	 * given type
	 */
	public <T> T parse(String string, Class<T> type) throws ParseException {
		try {
			return parse(new StringReader(string), type);
		}
		catch(IOException e) {
			// will not happen
			return null;
		}
	}
	
	/**
	 * {@link #parse(Reader, Class) Parses} a text {@link java.io.File} and
	 * converts it to an object. If the object bring parsed is a {@link
	 * Problem problem}, the {@link Problem#name problem's name} will be set to
	 * the {@link File#getName() file's name}.
	 * 
	 * @param <T> the type of object to be parsed
	 * @param file the text file to be parsed
	 * @param type the Java class of the object to be parsed
	 * @return the parsed object
	 * @throws IOException if an exception occurred while reading from the
	 * file
	 * @throws ParseException if the text cannot be parsed as that kind of
	 * objects
	 * @throws ClassCastException if the object created cannot be cast to the
	 * given type
	 */
	@SuppressWarnings("unchecked")
	public <T> T parse(File file, Class<T> type) throws IOException, ParseException {
		try(BufferedReader reader = new BufferedReader(new FileReader(file))) {
			T result = parse(reader, type);
			if(result.getClass() == Problem.class) {
				String name = file.getName();
				int last = name.lastIndexOf(".");
				if(last > 0)
					name = name.substring(0, last);
				if(name.length() != 0) {
					ProblemBuilder builder = new ProblemBuilder((Problem) result);
					builder.setName(name);
					result = (T) new Problem(builder);
				}
			}
			return result;
		}
	}
}