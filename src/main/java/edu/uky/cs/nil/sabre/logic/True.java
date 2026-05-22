package edu.uky.cs.nil.sabre.logic;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.io.DefaultParser;
import edu.uky.cs.nil.sabre.util.Unique;

/**
 * True is one of two {@link #TRUE singleton} {@link Value values} of type
 * {@code boolean} that represents the opposite of {@link False false}. True is
 * a {@link Disjunction disjunction} of one {@link Clause#EMPTY empty clause},
 * which means it is in {@link Expression#toPrecondition() disjunctive normal
 * form}. True is always defined and does not belong to any specific {@link
 * edu.uky.cs.nil.sabre.Universe universe}. True always returns itself when
 * {@link Expression#evaluate(State) evaluated}.
 * 
 * @author Stephen G. Ware
 */
public final class True extends Disjunction<Clause<Atom>> implements Unique, Value {
	
	/** The singleton true object */
	public static final True TRUE = new True();
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	private True() {
		super(Clause.EMPTY);
	}
	
	@Override
	public boolean equals(Object other) {
		return this == other;
	}
	
	@Override
	public int hashCode() {
		return 1;
	}
	
	@Override
	public String toString() {
		return DefaultParser.TRUE_KEYWORD;
	}
	
	@Override
	public int compareTo(Logical other) {
		if(other.equals(False.FALSE))
			return 1;
		else if(other.equals(this))
			return 0;
		else
			return -1;
	}

	@Override
	public True apply(Function<Object, Object> function) {
		return this;
	}
	
	@Override
	public True simplify() {
		return this;
	}
	
	@Override
	public True evaluate(State state) {
		return this;
	}
	
	@Override
	public True prepend(Parameter character) {
		return this;
	}
	
	/**
	 * Returns {@link False#FALSE false}.
	 * 
	 * @return {@link False#FALSE false}
	 */
	@Override
	public False negate() {
		return False.FALSE;
	}
	
	@Override
	public Conditional<Disjunction<Clause<Precondition>>> toValued() {
		return Value.super.toValued();
	}
	
	/**
	 * Returns itself, because {@link True#TRUE true} is in disjunctive normal
	 * form.
	 * 
	 * @return {@link True#TRUE true}
	 */
	@Override
	@SuppressWarnings("unchecked")
	public Disjunction<Clause<Precondition>> toPrecondition() {
		return (Disjunction<Clause<Precondition>>) (Disjunction<?>) this;
	}

	/**
	 * Returns {@link Clause#EMPTY the empty clause}.
	 * 
	 * @return {@link Clause#EMPTY the empty clause}
	 */
	@Override
	public Clause<Effect> toEffect() {
		return Clause.EMPTY.toEffect();
	}
	
	/**
	 * Ensures the same true constant is used by deserialized objects.
	 * 
	 * @return the true constant
	 */
	private Object readResolve() {
		return TRUE;
	}
}