package edu.uky.cs.nil.sabre.util;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.text.DecimalFormat;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.Settings;

/**
 * A memory budget is used to track how much memory a data structure has used
 * and prevent it from going over a defined limit. This object simply a counter
 * that will throw an {@link OutOfMemoryError} when it is asked for memory
 * beyond its limit. A memory budget can be {@link Serializable serialized},
 * but it will not retain information about how much memory has been used; when
 * deserialized, it will assume no memory has yet been used.
 * 
 * @author Stephen G. Ware
 */
public class MemoryBudget implements Serializable {
	
	/** A constant representing an unlimited amount of memory */
	public static final long UNLIMITED = 0;
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The size of the budget in bytes */
	public final long size;
	
	/** The amount of bytes that remain unused */
	private transient long remaining;
	
	/**
	 * Constructs a new memory budget of a given number of bytes.
	 * 
	 * @param bytes the number of bytes in the budget
	 */
	public MemoryBudget(long bytes) {
		if(bytes <= 0)
			this.size = UNLIMITED;
		else
			this.size = bytes;
		initialize();
	}
	
	/**
	 * Constructs a new memory budget from a string representing a number of
	 * bytes. See {@link #toString(long)} for how the string should be
	 * formatted.
	 * 
	 * @param string a string representing a number of bytes
	 */
	public MemoryBudget(String string) {
		this(parse(string));
	}
	
	/**
	 * Constructs an unlimited memory budget.
	 */
	public MemoryBudget() {
		this(UNLIMITED);
	}
	
	/**
	 * Ensures that, when a memory budget is deserialized, it always has all of
	 * its memory remaining.
	 * 
	 * @param in the object input stream from which the memory budget will be
	 * deserialized
	 * @throws IOException if an error occurs while reading from the input
	 * stream
	 * @throws ClassNotFoundException if a class needed to deserialize the
	 * memory budget is not found
	 */
	private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
		in.defaultReadObject();
		initialize();
	}
	
	private final void initialize() {
		this.remaining = this.size;
	}
	
	@Override
	public String toString() {
		return "[Memory Budget: " + toString(size - remaining) + " used of " + toString(size) + "]";
	}
	
	/**
	 * Returns the number of bytes remaining in this budget, or {@link
	 * java.lang.Long#MAX_VALUE} if this budget is unlimited.
	 * 
	 * @return the bytes remaining or {@link java.lang.Long#MAX_VALUE}
	 */
	public long getRemaining() {
		if(size == UNLIMITED)
			return Long.MAX_VALUE;
		else
			return remaining;
	}
	
	/**
	 * Reduces the budget by the given amount (or as much as it can without
	 * going below 0) or throws an exception if the budget is completely
	 * exhausted.
	 * 
	 * @param bytes the number of bytes to reduce the budget by
	 * @return the number of bytes the budget was reduced by, which may be less
	 * than the requested amount if the requested amount was higher than the
	 * remaining budget
	 * @throws OutOfMemoryError if the budget was already exhausted when this
	 * method was called
	 */
	public long reduce(long bytes) {
		if(size == UNLIMITED)
			return bytes;
		else if(bytes < 0)
			return 0;
		else if(remaining == 0)
			throw Exceptions.memoryBudgetExceeded(size);
		long reducation = Math.max(Math.min(bytes, remaining), 0);
		remaining -= reducation;
		return reducation;
	}
	
	private static final String[] SUFFIX = new String[] { "k",    "M",    "G",    "T",    "P",   "E" };
	private static final long[] MULTIPLIER = new long[] { pow(1), pow(2), pow(3), pow(4), pow(5), pow(6) };
	private static final DecimalFormat FORMAT = new DecimalFormat("#.#");
	private static final String UNLIMITED_STRING = "unlimited";
	
	private static final long pow(int times) {
		long value = 1;
		for(int i=0; i<times; i++)
			value *= 1000;
		return value;
	}
	
	/**
	 * Returns a string representation of a number of bytes using abbreviations
	 * to approximate large amounts:
	 * <ul>
	 * <li>B for 1 byte</li>
	 * <li>k for 1000 bytes</li>
	 * <li>M for 1000 k</li>
	 * <li>G for 1000 M</li>
	 * <li>T for 1000 G</li>
	 * <li>P for 1000 T</li>
	 * <li>E for 1000 P</li>
	 * </ul>
	 * The string returned may not be an exact representation of the number.
	 * For example, 5002000 will be represented as {@code "5M"} even though it
	 * is actually 2k more than that.
	 * <p>
	 * This formatting can also used by {@link #MemoryBudget(String) this
	 * constructor}.
	 * 
	 * @param bytes the number of bytes to convert to a string
	 * @return the string or {@code "unlimited"} if the number given is less
	 * than or equal to 0
	 */
	public static final String toString(long bytes) {
		if(bytes <= 0)
			return UNLIMITED_STRING;
		if(bytes < 1000)
			return bytes + "B";
		int suffix = 0;
		while(bytes <= -999950 || bytes >= 999950) {
			bytes /= 1000;
			suffix++;
		}
		return FORMAT.format(bytes / 1000.0) + SUFFIX[suffix];
	}
	
	/**
	 * Converts a string representation of an amount of memory into a number of
	 * bytes. This method uses the same formating as {@link #toString(long)}.
	 * 
	 * @param string the number of bytes as a string
	 * @return the number of bytes as a number
	 * @throws IllegalArgumentException if the string cannot be parsed
	 */
	public static final long parse(String string) {
		if(string.length() == 0)
			return 0;
		else if(string.equalsIgnoreCase(UNLIMITED_STRING))
			return UNLIMITED;
		String prefix = string.substring(0, string.length() - 1);
		String suffix = string.substring(string.length() - 1);
		try {
			if(suffix.equals("B"))
				return Long.parseLong(prefix);
			for(int i=0; i<SUFFIX.length; i++)
				if(SUFFIX[i].endsWith(suffix))
					return Long.parseLong(prefix) * MULTIPLIER[i];
			return Long.parseLong(string);
		}
		catch(NumberFormatException e) {
			throw Exceptions.cannotParseMemory(string);
		}
	}
}