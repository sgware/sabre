package edu.uky.cs.nil.sabre.comp;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Mapping;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.Type;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.Logical;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.util.ImmutableArray;

/**
 * A compiled mapping is a {@link Mapping mapping} whose {@link #get(Character)
 * get} method only accepts {@link Logical#isGround() ground} {@link Character
 * characters} and which always returns ground expressions.
 * 
 * @param <E> the type of expression this mapping will return
 * @author Stephen G. Ware
 */
public class CompiledMapping<E extends Expression> implements Mapping<E> {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * An {@link ImmutableArray array} that stores the expressions in this
	 * mapping, where the expression for each {@link Character character} is in
	 * the index corresponding to the {@link edu.uky.cs.nil.sabre.Entity#id ID
	 * number} for that character
	 */
	public final ImmutableArray<E> table;
	
	/**
	 * Constructs a new compiled mapping.
	 * 
	 * @param table an array of {@link Expression logical expressions} where
	 * the expression for each {@link Character character} is in the index
	 * corresponding to the {@link edu.uky.cs.nil.sabre.Entity#id ID number}
	 * of the character
	 */
	public CompiledMapping(ImmutableArray<E> table) {
		this.table = table;
	}
	
	/**
	 * Constructs a new compiled mapping.
	 * 
	 * @param table an array of {@link Expression logical expressions} where
	 * the expression for each {@link Character character} is in the index
	 * corresponding to the {@link edu.uky.cs.nil.sabre.Entity#id ID number}
	 * of the character
	 */
	@SuppressWarnings("unchecked")
	public CompiledMapping(E...table) {
		this(new ImmutableArray<>(table));
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), table);
	}
	
	@Override
	public String toString() {
		String string = "";
		if(table.size() > 0)
			string = "0 -> " + table.get(0);
		for(int i=1; i<table.size(); i++)
			string += "\n" + i + " -> " + table.get(i);
		return string;
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(other.getClass()))
			return Utilities.compare(table, ((CompiledMapping<?>) other).table);
		else
			return Mapping.super.compareTo(other);
	}

	@Override
	public boolean isBoolean() {
		for(int i=0; i<table.size(); i++)
			if(!table.get(i).isBoolean())
				return false;
		return true;
	}

	@Override
	public boolean isNumber() {
		for(int i=0; i<table.size(); i++)
			if(!table.get(i).isNumber())
				return false;
		return true;
	}

	@Override
	public boolean isEntity() {
		for(int i=0; i<table.size(); i++)
			if(!table.get(i).isEntity())
				return false;
		return true;
	}

	@Override
	public boolean isCharacter() {
		for(int i=0; i<table.size(); i++)
			if(!table.get(i).isCharacter())
				return false;
		return true;
	}

	@Override
	public boolean is(Type type) {
		for(int i=0; i<table.size(); i++)
			if(!table.get(i).is(type))
				return false;
		return true;
	}

	@Override
	public boolean isGround() {
		return true;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public CompiledMapping<?> apply(java.util.function.Function<Object, Object> function) {
		ImmutableArray<Expression> table = (ImmutableArray<Expression>) this.table.apply(function);
		if(table != this.table)
			return new CompiledMapping<>(table);
		else
			return this;
	}

	/**
	 * Returns a {@link edu.uky.cs.nil.sabre.logic.Logical#isGround() ground}
	 * {@link Expression expression} relevant to the given ground character.
	 * 
	 * @param character the character
	 * @return the ground expression
	 * @throws edu.uky.cs.nil.sabre.FormatException if the character is not
	 * ground or not of type {@code character}
	 */
	@Override
	public E get(Parameter character) {
		character.mustBeGround();
		character.mustBeCharacter();
		return get((Character) character);
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T extends Expression> Mapping<T> cast(Class<T> type) {
		for(int i=0; i<table.size(); i++)
			type.cast(table.get(i));
		return (CompiledMapping<T>) this;
	}
	
	/**
	 * Returns a {@link edu.uky.cs.nil.sabre.logic.Logical#isGround() ground}
	 * {@link Expression expression} relevant to the given ground character.
	 * 
	 * @param character the character
	 * @return the ground expression
	 */
	public E get(Character character) {
		return table.get(character.id);
	}
}