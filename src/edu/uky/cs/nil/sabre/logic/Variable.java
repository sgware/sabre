package edu.uky.cs.nil.sabre.logic;

import java.util.HashSet;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Type;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * A variable is a placeholder for a {@link Value value}. It is defined by a
 * {@link #name name} and what {@link #type type} of values it can have.
 * 
 * @author Stephen G. Ware
 */
public class Variable implements Parameter {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** A global list of all variables names ever used */
	private static transient final HashSet<String> names = new HashSet<>();
	
	/**
	 * Returns a variable of a given type whose name is similar to a given
	 * name but has never been used before in any logical formula.
	 * 
	 * @param name the suggested name
	 * @param type the type
	 * @return a variable of that type whose name is similar to the suggested
	 * name but has never been used before
	 */
	public static final Variable generate(String name, Type type) {
		String _name = name;
		int index = 1;
		while(names.contains(_name))
			_name = name + "-" + index++;
		return new Variable(_name, type);
	}
	
	/**
	 * Returns a variable of a given type whose name has never been used before
	 * in any logical formula.
	 * 
	 * @param type the type
	 * @return a variable of that type whose name is has never been used before
	 */
	public static final Variable generate(Type type) {
		return generate(type.name, type);
	}
	
	/** The name of the variable */
	public final String name;
	
	/** The type of values the variable could have */
	public final Type type;
	
	/**
	 * Constructs a new variable.
	 * 
	 * @param name the name
	 * @param type the type
	 */
	public Variable(String name, Type type) {
		this.name = name;
		if(type.isNumber())
			throw Exceptions.variableMayNotBeNumeric();
		this.type = type;
		names.add(name);
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			Variable otherVariable = (Variable) other;
			return name.equals(otherVariable.name) && type.equals(otherVariable.type);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), name, type);
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(other.getClass())) {
			Variable otherVariable = (Variable) other;
			return Utilities.compare(name, otherVariable.name, type, otherVariable.type);
		}
		else
			return Parameter.super.compareTo(other);
	}
	
	@Override
	public boolean isGround() {
		return false;
	}

	@Override
	public Variable apply(Function<Object, Object> function) {
		Type type = (Type) function.apply(this.type);
		if(type != this.type)
			return new Variable(name, type);
		else
			return this;
	}
	
	@Override
	public boolean isBoolean() {
		return type.isBoolean();
	}
	
	@Override
	public boolean isNumber() {
		return type.isNumber();
	}
	
	@Override
	public boolean isEntity() {
		return type.isEntity();
	}
	
	@Override
	public boolean isCharacter() {
		return type.isCharacter();
	}
	
	@Override
	public boolean is(Type type) {
		return this.type.is(type);
	}
	
	@Override
	public Variable simplify() {
		return this;
	}

	@Override
	public Value evaluate(State state) {
		throw Exceptions.mustBeGround(this);
	}
	
	@Override
	public Variable prepend(Parameter character) {
		return this;
	}
	
	@Override
	public Expression negate() {
		if(isBoolean())
			return new Conditional<>(this, False.FALSE, True.TRUE);
		else
			return Parameter.super.negate();
	}
	
	@Override
	public Conditional<Disjunction<Clause<Precondition>>> toValued() {
		if(isBoolean())
			return new Conditional<>(this, True.TRUE, False.FALSE).toValued();
		else if(isNumber())
			return Parameter.super.toValued();
		else {
			ImmutableSet<Value> values = type.getValues();
			Expression[] conditions = new Expression[values.size() - 1];
			Expression[] branches = new Expression[values.size()];
			for(int i=0; i<conditions.length; i++) {
				conditions[i] = new Comparison(Comparison.EQUAL_TO, this, values.get(i));
				branches[i] = values.get(i);
			}
			branches[branches.length - 1] = values.get(values.size() - 1);
			return new Conditional<>(conditions, branches).toValued();
		}
	}
	
	@Override
	public Disjunction<Clause<Precondition>> toPrecondition() {
		if(isBoolean())
			return new Comparison(Comparison.EQUAL_TO, this, True.TRUE).toPrecondition();
		else
			return Parameter.super.toPrecondition();
	}

	@Override
	public Clause<Effect> toEffect() {
		return Parameter.super.toEffect();
	}
	
	/**
	 * Ensures that deserialized variable names are added to the global list of
	 * variable names.
	 * 
	 * @return the deserialized variable
	 */
	private Object readResolve() {
		names.add(name);
		return this;
	}
}