package edu.uky.cs.nil.sabre.util;

import java.io.File;

import edu.uky.cs.nil.sabre.Exceptions;

/**
 * A collection of command line arguments passed to the library and utility
 * methods for pasring them.
 * 
 * @author Stephen G. Ware
 */
public class CommandLineArguments {
	
	private final String[] arguments;
	private final boolean[] used;

	/**
	 * Constructs a new collection of command line arguments.
	 * 
	 * @param args the arguments array passed to the {@code main} method
	 */
	public CommandLineArguments(String[] args) {
		this.arguments = args;
		this.used = new boolean[args.length];
	}
	
	/**
	 * Tests whether a given argument is in the collection. If so, the argument
	 * is marked as used.
	 * 
	 * @param argument the argument to search for
	 * @return true if the argument is included, false otherwise
	 */
	public boolean contains(String argument) {
		for(int i=0; i<arguments.length; i++) {
			if(arguments[i].equalsIgnoreCase(argument)) {
				used[i] = true;
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Returns the value immediately after a given key, or a default value if
	 * the key is not found. The key and value are marked as used if they are
	 * found. Case is ignored.
	 * 
	 * @param key the key to search for
	 * @param defaultValue a value to return if the key is not found
	 * @return the value immediately after the key in the collection of
	 * arguments
	 */
	public String get(String key, String defaultValue) {
		for(int i=0; i<arguments.length-1; i++) {
			if(arguments[i].equalsIgnoreCase(key)) {
				used[i] = true;
				used[i + 1] = true;
				return arguments[i + 1];
			}
		}
		return defaultValue;
	}
	
	/**
	 * Returns the value immediately after a given key, or null if the key is
	 * not found. The key and value are marked as used if they are found. Case
	 * is ignored.
	 * 
	 * @param key the key to search for
	 * @return the value immediately after the key in the collection of
	 * arguments
	 */
	public String get(String key) {
		return get(key, (String) null);
	}
	
	/**
	 * Returns the value immediately after a given key, or throws an exception
	 * if the key is not found. The key and value are marked as used if they
	 * are found. Case is ignored.
	 * 
	 * @param key the key to search for
	 * @return the value immediately after the key in the collection of
	 * arguments
	 * @throws IllegalArgumentException if the key is not found or if there is
	 * no value after the key
	 */
	public String require(String key) {
		String value = get(key);
		if(value == null)
			throw Exceptions.missingCommandLineArgument(key);
		return value;
	}
	
	/**
	 * Returns the value immediately after a given key, provided that value is
	 * one of a pre-defined list of possible options. If the key is not found,
	 * the first option is returned. If the value is no in the pre-defined
	 * list, an exceptioon is thrown. The key and value are marked as used if
	 * they are found. Case is ignored.
	 * 
	 * @param key the key to search for
	 * @param options a pre-defined list of options that may be the value
	 * @return the value immediately after the key if it was in the list of
	 * options, or the first value if the key was not found or if there was no
	 * value after the key
	 * @throws IllegalArgumentException if the value found was not one of the
	 * pre-defined options
	 */
	public String getOption(String key, String...options) {
		String value = get(key);
		if(value == null)
			return options.length > 0 ? options[0] : null;
		for(String option : options)
			if(option.equalsIgnoreCase(value))
				return value;
		throw Exceptions.failedToParseCommandLineArgument(key, value);
	}
	
	/** Strings that mean true */
	private static final String[] TRUE = new String[] { "true", "t", "yes", "y", "1" };
	
	/** Strings that mean false */
	private static final String[] FALSE = new String[] { "false", "f", "no", "n", "0" };
	
	/**
	 * Like {@link #get(String, String)}, but the value returned will be parsed
	 * as an {@code boolean}.
	 * 
	 * @param key the key to search for
	 * @param defaultValue a value to return if the key is not found
	 * @return the value, as a {@code boolean}
	 * @throws IllegalArgumentException if the value cannot be parsed as a
	 * {@code boolean}
	 */
	public boolean getBoolean(String key, boolean defaultValue) {
		String value = get(key);
		if(value == null)
			return defaultValue;
		for(String string : TRUE)
			if(value.equalsIgnoreCase(string))
				return true;
		for(String string : FALSE)
			if(value.equalsIgnoreCase(string))
				return false;
		throw Exceptions.failedToParseCommandLineArgument(key, value);
	}
	
	/**
	 * Like {@link #get(String, String)}, but the value returned will be parsed
	 * as an {@code int}.
	 * 
	 * @param key the key to search for
	 * @param defaultValue a value to return if the key is not found
	 * @return the value, as an {@code int}
	 * @throws IllegalArgumentException if the value cannot be parsed as an
	 * {@code int}
	 */
	public int getInt(String key, int defaultValue) {
		String value = get(key);
		if(value == null)
			return defaultValue;
		try {
			return Integer.parseInt(value);
		}
		catch(NumberFormatException e) {
			throw Exceptions.failedToParseCommandLineArgument(key, value);
		}
	}
	
	/**
	 * Like {@link #get(String, String)}, but the value returned will be parsed
	 * as a {@code long}.
	 * 
	 * @param key the key to search for
	 * @param defaultValue a value to return if the key is not found
	 * @return the value, as an {@code long}
	 * @throws IllegalArgumentException if the value cannot be parsed as a
	 * {@code long}
	 */
	public long getLong(String key, long defaultValue) {
		String value = get(key);
		if(value == null)
			return defaultValue;
		try {
			return Long.parseLong(value);
		}
		catch(NumberFormatException e) {
			throw Exceptions.failedToParseCommandLineArgument(key, value);
		}
	}
	
	/**
	 * Like {@link #get(String, String)}, but the value returned will be parsed
	 * as a {@code double}.
	 * 
	 * @param key the key to search for
	 * @param defaultValue a value to return if the key is not found
	 * @return the value, as an {@code double}
	 * @throws IllegalArgumentException if the value cannot be parsed as a
	 * {@code double}
	 */
	public double getDouble(String key, double defaultValue) {
		String value = get(key);
		if(value == null)
			return defaultValue;
		try {
			return Double.parseDouble(value);
		}
		catch(NumberFormatException e) {
			throw Exceptions.failedToParseCommandLineArgument(key, value);
		}
	}
	
	/**
	 * Like {@link #get(String, String)}, but the value returned will be parsed
	 * as a {@link java.io.File}.
	 * 
	 * @param key the key to search for
	 * @return the value, as a {@link java.io.File}
	 * @throws IllegalArgumentException if the file is not found
	 */
	public File getFile(String key) {
		String path = require(key);
		File file = new File(path);
		if(file.exists())
			return file;
		else
			throw Exceptions.fileNotFound(path);
	}
	
	/**
	 * Throws an exception if any arguments remain that have not been marked as
	 * used.
	 * 
	 * @throws IllegalArgumentException if any arguments are unused
	 */
	public void checkUnused() {
		for(int i=0; i<arguments.length; i++)
			if(!used[i])
				throw Exceptions.unusedCommandLineArgument(arguments[i]);
	}
}