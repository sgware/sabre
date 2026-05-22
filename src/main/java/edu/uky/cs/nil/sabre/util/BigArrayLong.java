package edu.uky.cs.nil.sabre.util;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Arrays;

import edu.uky.cs.nil.sabre.Settings;

/**
 * An object for managing a large table of Java {@code long} values, similar to
 * an array, but with larger capacity and able to expand dynamically. The table
 * is composed of many arrays of {@code long}s called chunks which are
 * allocated as needed, subtracting from a bounded {@link MemoryBudget memory
 * budget} if one is provided. Unlike arrays, which are indexed by {@code
 * int}s, this table is indexed by {@code long}s.
 * 
 * @author Stephen G. Ware
 */
public final class BigArrayLong implements Serializable {
	
	/** The smallest allowed size for a chunk */
	public static final long MIN_CHUNK_SIZE = Long.BYTES;
	
	/** The largest allowed size for a chunk */
	public static final long MAX_CHUNK_SIZE = ((long) Integer.MAX_VALUE) * Long.BYTES;
	
	/** The default chunk size */
	public static final long DEFAULT_CHUNK_SIZE = 1048576L * Long.BYTES; // 8 MiB
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** Chunk size (in bytes) */
	public final long chunk;
	
	/** Each newly allocated chunk reduces this memory budget */
	public final MemoryBudget budget;
	
	/** The number of array indices each chunk will have */
	private final int slotsPerChunk;
	
	/** The array of chunks */
	private transient long[][] chunks;
	
	/** The total number of indices that can be stored in the big array */
	private transient long capacity;

	/**
	 * Constructs a table of {@code long} values with a given chunk size and
	 * limited by a given memory budget.
	 * 
	 * @param chunk the chunk size in bytes
	 * @param budget the memory budget
	 */
	public BigArrayLong(long chunk, MemoryBudget budget) {
		chunk = Math.min(chunk, MAX_CHUNK_SIZE);
		chunk -= chunk % Long.BYTES;
		chunk = Math.max(chunk, MIN_CHUNK_SIZE);
		this.chunk = chunk;
		this.budget = budget;
		this.slotsPerChunk = (int) chunk / Long.BYTES;
		initialize();
	}
	
	/**
	 * Constructs a table of {@code long} values with a given chunk size and an
	 * unlimited memory budget.
	 * 
	 * @param chunk the chunk size in bytes
	 */
	public BigArrayLong(long chunk) {
		this(chunk, new MemoryBudget(MemoryBudget.UNLIMITED));
	}
	
	/**
	 * Constructs a table of {@code long} values with the default chunk size
	 * using the provided memory budget.
	 * 
	 * @param budget the memory budget
	 */
	public BigArrayLong(MemoryBudget budget) {
		this(DEFAULT_CHUNK_SIZE, budget);
	}
	
	/**
	 * Constructs a table of {@code long} values using the default chunk size
	 * and an unlimited memory budget.
	 */
	public BigArrayLong() {
		this(DEFAULT_CHUNK_SIZE);
	}
	
	/**
	 * Ensures that, when a big array is deserialized, it is always empty.
	 * 
	 * @param in the object input stream from which the big array will be
	 * deserialized
	 * @throws IOException if an error occurs while reading from the input
	 * stream
	 * @throws ClassNotFoundException if a class needed to deserialize the big
	 * array is not found
	 */
	private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
		in.defaultReadObject();
		initialize();
	}
	
	private final void initialize() {
		chunks = new long[0][];
		capacity = 0;
		allocate();
	}
	
	@Override
	public String toString() {
		return "[Big Array: capacity " + capacity + "; chunk size " + chunk + "]";
	}
	
	/**
	 * Returns the number of {@code long} values that can currently be stored
	 * in this table.
	 * 
	 * @return the number of {@code long} values that can be stored
	 */
	public final long capacity() {
		return capacity;
	}
	
	/**
	 * Returns the {@code long} value stored at this index in the table.
	 * 
	 * @param index the index
	 * @return the value at that index
	 */
	public final long get(long index) {
		return chunks[(int) (index / slotsPerChunk)][(int) (index % slotsPerChunk)];
	}
	
	/**
	 * Sets the {@code long} value at the given index to the given value.
	 * 
	 * @param index the index
	 * @param value the value to store at that index
	 */
	public final void set(long index, long value) {
		while(index >= capacity)
			allocate();
		chunks[(int) (index / slotsPerChunk)][(int) (index % slotsPerChunk)] = value;
	}
	
	private final void allocate() {
		long bytes = Math.min(this.chunk, budget.getRemaining());
		bytes -= bytes % Long.BYTES;
		bytes = Math.max(bytes, MIN_CHUNK_SIZE);
		budget.reduce(bytes);
		int slots = (int) (bytes / Long.BYTES);
		chunks = Arrays.copyOf(chunks, chunks.length + 1);
		chunks[chunks.length - 1] = new long[slots];
		capacity += slots;
	}
}