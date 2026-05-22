package edu.uky.cs.nil.sabre.logic;

import java.util.function.Function;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.io.DefaultParser;
import edu.uky.cs.nil.sabre.util.Unique;

/**
 * False is one of two {@link #FALSE singleton} {@link Value values} of type
 * {@code boolean} that represents the opposite of {@link True true}. False is
 * a {@link Disjunction disjunction} of zero {@link Clause clauses}, which
 * means it is in {@link Expression#toPrecondition() disjunctive normal form}.
 * False is always defined and does not belong to any specific {@link
 * edu.uky.cs.nil.sabre.Universe universe}. False always returns itself when
 * {@link Expression#evaluate(State) evaluated}.
 * 
 * @author Stephen G. Ware
 */
public final class False extends Disjunction<Clause<Atom>> implements Unique, Value {
	
	/** The singleton false object */
	public static final False FALSE = new False();
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	private False() {
		super();
	}
	
	@Override
	public boolean equals(Object other) {
		return this == other;
	}
	
	@Override
	public int hashCode() {
		return 0;
	}
	
	@Override
	public String toString() {
		return DefaultParser.FALSE_KEYWORD;
	}
	
	@Override
	public int compareTo(Logical other) {
		if(other.equals(this))
			return 0;
		else
			return -1;
	}

	@Override
	public False apply(Function<Object, Object> function) {
		return this;
	}
	
	@Override
	public False simplify() {
		return this;
	}
	
	@Override
	public False evaluate(State state) {
		return this;
	}
	
	@Override
	public False prepend(Parameter character) {
		return this;
	}
	
	/**
	 * Returns {@link True#TRUE true}.
	 * 
	 * @return {@link True#TRUE true}
	 */
	@Override
	public True negate() {
		return True.TRUE;
	}
	
	@Override
	public Conditional<Disjunction<Clause<Precondition>>> toValued() {
		return Value.super.toValued();
	}
	
	/**
	 * Returns itself, because {@link False#FALSE false} is in disjunctive
	 * normal form.
	 * 
	 * @return {@link False#FALSE false}
	 */
	@Override
	@SuppressWarnings("unchecked")
	public Disjunction<Clause<Precondition>> toPrecondition() {
		return (Disjunction<Clause<Precondition>>) (Disjunction<?>) this;
	}
	
	/**
	 * Returns {@link Clause#NULL the null clause}.
	 * 
	 * @return {@link Clause#NULL the null clause}
	 */
	@Override
	public Clause<Effect> toEffect() {
		return Clause.NULL.toEffect();
	}
	
	/**
	 * Ensures the same false constant is used by deserialized objects.
	 * 
	 * @return the false constant
	 */
	private Object readResolve() {
		return FALSE;
	}
}