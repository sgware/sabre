package edu.uky.cs.nil.sabre;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.logic.Logical;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.logic.Substitution;
import edu.uky.cs.nil.sabre.logic.Unknown;
import edu.uky.cs.nil.sabre.util.ImmutableArray;

/**
 * A signature is a name followed by an ordered list of zero to many {@link
 * Parameter arguments}. Signatures provide a compact way to refer to
 * parameterized things like {@link Fluent fluents} and {@link Event events}.
 * 
 * @author Stephen G. Ware
 */
public final class Signature implements Logical {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** A name */
	public final String name;
	
	/** A list of zero to many arguments */
	public final ImmutableArray<Parameter> arguments;
	
	/**
	 * Constructs a new signature.
	 * 
	 * @param name a name
	 * @param arguments an array of arguments
	 */
	public Signature(String name, ImmutableArray<Parameter> arguments) {
		this.name = name;
		for(int i=0; i<arguments.size(); i++)
			if(arguments.get(i).equals(Unknown.UNKNOWN))
				throw Exceptions.parameterMayNotBeNull(i + 1);
		this.arguments = arguments;
	}
	
	/**
	 * Constructs a new signature from an array of arguments.
	 * 
	 * @param name a name
	 * @param arguments an array of arguments
	 */
	public Signature(String name, Parameter...arguments) {
		this(name, new ImmutableArray<>(arguments));
	}
	
	/**
	 * Constructs a new signature from an {@link java.lang.Iterable} of
	 * arguments.
	 * 
	 * @param name a name
	 * @param arguments an iterable list of arguments
	 */
	public Signature(String name, Iterable<? extends Parameter> arguments) {
		this(name, new ImmutableArray<>(arguments));
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			Signature otherSignature = (Signature) other;
			return name.equals(otherSignature.name) && arguments.equals(otherSignature.arguments);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), name, arguments);
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(other.getClass())) {
			Signature otherSignature = (Signature) other;
			int comparison = name.compareTo(otherSignature.name);
			if(comparison == 0)
				comparison = Utilities.compare(arguments, otherSignature.arguments);
			return comparison;
		}
		else
			return Logical.super.compareTo(other);
	}

	@Override
	public boolean isGround() {
		for(int i=0; i<arguments.size(); i++)
			if(!arguments.get(i).isGround())
				return false;
		return true;
	}

	@Override
	public Signature apply(Function<Object, Object> function) {
		ImmutableArray<Parameter> arguments = this.arguments.apply(function);
		if(arguments != this.arguments)
			return new Signature(name, arguments);
		else
			return this;
	}
	
	/**
	 * Tests whether this signature is the same as or more general than a given
	 * signature. In other words, this method returns true if there exists a
	 * {@link Substitution substituion} that can be applied to this signature
	 * to make it the same as the given signature.
	 * 
	 * @param other the other signature to test
	 * @return true if this signature is the same or more general, false
	 * otherwise
	 */
	public boolean covers(Signature other) {
		return name.equals(other.name) && arguments.size() == other.arguments.size() && Substitution.canSubstitute(((Signature) distinguish(other)).arguments, other.arguments);
	}
}