package edu.uky.cs.nil.sabre.io;

/**
 * There are two kinds of symbols that can be used to define a grammar for a
 * {@link Parser parser} to follow: {@link Terminal terminal symbols} that
 * represent primitive elements that cannot be broken down any further and
 * {@link NonTerminal non-terminal symbols} that represent complex patterns
 * that can be broken down further.
 * 
 * @author Stephen G. Ware
 */
public abstract class Symbol implements Pattern {

	Symbol() {}
}