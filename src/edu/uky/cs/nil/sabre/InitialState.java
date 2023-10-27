package edu.uky.cs.nil.sabre;

import java.io.Serializable;
import java.util.HashMap;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Conjunction;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.Logical;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * An initial state is a {@link FiniteState finite state} which uses a {@link
 * Expression logical expression} to explicitly set the values of some fluents
 * and then assumes default values for all other fluents. The expression used
 * to construct this state should be something which can be {@link
 * Expression#toEffect() converted to an effect}, meaning it should be
 * deterministic (no disjunctions) and should use {@link
 * edu.uky.cs.nil.sabre.logic.Assignment assignments} rather than {@link
 * edu.uky.cs.nil.sabre.logic.Comparison comparisons} to specify the values of
 * fluents.
 * <p>
 * When a fluent with no {@link Fluent#characters characters} is not explicitly
 * assigned a value, we assume the {@link Universe#getDefaultValue(Type)
 * default value} based on the fluent's type.
 * <p>
 * When a character's beliefs about a fluent are not explicitly stated, we
 * assume the character believes its actual value. For example, if no value is
 * specified for the fluent {@code believes(B, alive(A))}, then it will have the
 * same value as {@code alive(A)}.
 * <p>
 * When beliefs about fluents with multiple {@link Fluent#characters characters}
 * are not explicitly stated, we assume the value is the same as the value
 * assigned to the same fluent with with the last character removed. For
 * example, if no value is specified for {@code
 * believes(A, believes(B, alive(A)))} then it will have the same value as
 * {@code believes(A, alive(A))}.
 * 
 * @author Stephen G. Ware
 */
public class InitialState implements Serializable, Logical, FiniteState {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** A logical expression representing this state */
	public final Clause<Effect> clause;
	
	/**
	 * Constructs a new initial state from a logical expression.
	 * 
	 * @param initial the expression
	 * @throws FormatException if the expression cannot be converted to an
	 * effect
	 */
	public InitialState(Expression initial) {
		HashState state = new HashState();
		impose(initial, state);
		this.clause = state.toClause();
	}
	
	private static final class HashState implements State {
		
		public final HashMap<Fluent, Value> values = new HashMap<>();

		@Override
		public Value getValue(Fluent fluent) {
			Value value = values.get(fluent);
			if(value != null)
				return value;
			else if(fluent.characters.size() == 0)
				return fluent.type.getDefaultValue();
			else
				return getValue(fluent.removeLastCharacter());
		}
		
		public Clause<Effect> toClause() {
			Clause<Effect> clause = Clause.EMPTY.toEffect();
			for(Fluent fluent : values.keySet()) {
				Value value = values.get(fluent);
				if((fluent.characters.size() == 0 && !value.equals(fluent.type.getDefaultValue())) || (fluent.characters.size() > 0 && !value.equals(getValue(fluent.removeLastCharacter()))))
					clause = clause.add(new Effect(fluent, value));
			}
			return clause;
		}
	}
	
	private static final void impose(Expression expression, HashState state) {
		if(expression instanceof Conjunction && !(expression instanceof Clause))
			for(Expression argument : ((Conjunction<?>) expression).arguments)
				impose(argument, state);
		else {
			Clause<Effect> clause = expression.toEffect();
			if(!clause.isGround())
				expression.mustBeGround();
			if(clause.equals(Clause.NULL))
				throw Exceptions.cannotImpose(expression);
			impose(clause, clause.size() - 1, state);
		}
	}
	
	private static final void impose(Clause<Effect> clause, int index, HashState state) {
		if(index >= 0) {
			Effect effect = clause.get(index);
			boolean condition = effect.condition.test(state);
			Value value = effect.value.evaluate(state);
			impose(clause, index - 1, state);
			if(condition)
				state.values.put(effect.fluent, value);
		}
	}
	
	@Override
	public boolean equals(Object other) {
		return getClass().equals(other.getClass()) && clause.equals(((InitialState) other).clause);
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), clause);
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(other.getClass()))
			return clause.compareTo(((InitialState) other).clause);
		else
			return Logical.super.compareTo(other);
	}
	
	@Override
	public boolean isGround() {
		return true;
	}

	@Override
	public InitialState apply(Function<Object, Object> function) {
		Clause<Effect> clause = ((Expression) function.apply(this.clause)).toEffect();
		if(clause == this.clause)
			return this;
		else
			return new InitialState(clause);
	}

	@Override
	public Value getValue(Fluent fluent) {
		for(Effect effect : clause)
			if(effect.fluent.equals(fluent))
				return (Value) effect.value;
		if(fluent.characters.size() == 0)
			return fluent.type.getDefaultValue();
		else
			return getValue(fluent.removeLastCharacter());
	}
	
	@Override
	public InitialState getBeliefs(Character character) {
		HashState state = new HashState();
		for(Effect effect : clause)
			if(effect.fluent.characters.size() == 0)
				state.values.put(effect.fluent, (Value) effect.value);
		for(Effect effect : clause) {
			if(effect.fluent.characters.size() > 0 && effect.fluent.characters.get(0).equals(character)) {
				state.values.put(effect.fluent, (Value) effect.value);
				state.values.put(effect.fluent.removeFirstCharacter(), (Value) effect.value);
			}
		}
		Clause<Effect> clause = state.toClause();
		if(clause.equals(this.clause))
			return this;
		else
			return new InitialState(clause);
	}
}