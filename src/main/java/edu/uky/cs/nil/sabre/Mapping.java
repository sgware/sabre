package edu.uky.cs.nil.sabre;

import edu.uky.cs.nil.sabre.logic.Clause;
import edu.uky.cs.nil.sabre.logic.Disjunction;
import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.Logical;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.logic.Simplifiable;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Typed;
import edu.uky.cs.nil.sabre.logic.Variable;
import edu.uky.cs.nil.sabre.util.ImmutableSet;

/**
 * A function which takes as input a {@link Character character} and returns a
 * {@link Expression logical expression} relevant to that character.
 * 
 * @param <E> the type of expression this mapping will return
 * @author Stephen G. Ware
 */
public interface Mapping<E extends Expression> extends Typed, Simplifiable {
	
	@Override
	public Mapping<?> apply(java.util.function.Function<Object, Object> function);

	@Override
	public default Mapping<?> simplify() {
		return (Mapping<?>) Simplifiable.super.simplify();
	}
	
	/**
	 * Returns an expression relevant to the given character.
	 * 
	 * @param character the character
	 * @return the expression
	 */
	public E get(Parameter character);
	
	/**
	 * Casts the type of expression returned by this mapping to a different
	 * type.
	 * 
	 * @param <T> the new type of expression this mapping should return
	 * @param type the class object for the new type
	 * @return a mapping that returns expressions of the new type
	 * @throws ClassCastException is the current type of expression cannot be
	 * cast to the new type
	 */
	public <T extends Expression> Mapping<T> cast(Class<T> type);
	
	/**
	 * A mapping defined by an {@link Expression expression} that contains a
	 * {@link Variable variable} representing a character. When {@link
	 * #get(Parameter)} is called, this mapping returns the expression but with
	 * the variable replaced by the given character. The type of the variable
	 * must be {@code character} or a supertype of {@code character}.
	 * 
	 * @author Stephen G. Ware
	 */
	public static class Function implements Mapping<Expression> {
		
		/** Serial version ID */
		private static final long serialVersionUID = Settings.VERSION_UID;
		
		/** The variable in the expression which represents the character */
		public final Variable variable;
		
		/** The expression */
		public final Expression argument;
		
		/**
		 * Constructs a new function.
		 * 
		 * @param variable a variable whose type must be {@code character} or a
		 * supertype of {@code character}
		 * @param argument an expression containing the variable
		 */
		public Function(Variable variable, Expression argument) {
			Type character = variable.type.universe.types.get(Settings.CHARACTER_TYPE_ID);
			if(!character.is(variable.type))
				throw Exceptions.variableMustBeCharacter(variable);
			this.variable = variable;
			this.argument = argument;
		}
		
		@Override
		public boolean equals(Object other) {
			if(getClass().equals(other.getClass())) {
				Function otherFunction = (Function) other;
				return variable.equals(otherFunction.variable) && argument.equals(otherFunction.argument);
			}
			return false;
		}
		
		@Override
		public int hashCode() {
			return Utilities.hashCode(getClass(), variable, argument);
		}
		
		@Override
		public String toString() {
			return Utilities.DEFAULT_PRINTER.toString(this);
		}
		
		@Override
		public int compareTo(Logical other) {
			if(getClass().equals(other.getClass())) {
				Function otherFunction = (Function) other;
				return Utilities.compare(variable, otherFunction.variable, argument, otherFunction.argument);
			}
			else
				return Mapping.super.compareTo(other);
		}
		
		@Override
		public boolean isGround() {
			return false;
		}
		
		@Override
		public Function apply(java.util.function.Function<Object, Object> function) {
			Variable variable = (Variable) function.apply(this.variable);
			Expression argument = (Expression) function.apply(this.argument);
			if(variable != this.variable || argument != this.argument)
				return new Function(variable, argument);
			else
				return this;
		}
		
		@Override
		public boolean isBoolean() {
			return argument.isBoolean();
		}

		@Override
		public boolean isNumber() {
			return argument.isNumber();
		}

		@Override
		public boolean isEntity() {
			return argument.isEntity();
		}

		@Override
		public boolean isCharacter() {
			return argument.isCharacter();
		}

		@Override
		public boolean is(Type type) {
			return argument.is(type);
		}
		
		@Override
		public Function simplify() {
			return (Function) Mapping.super.simplify();
		}
		
		@Override
		public Expression get(Parameter character) {
			return (Expression) argument.substitute(variable, character);
		}

		@Override
		@SuppressWarnings("unchecked")
		public <T extends Expression> Mapping<T> cast(Class<T> type) {
			type.cast(argument);
			return (Mapping<T>) this;
		}
	}
	
	/**
	 * A mapping which returns {@link True#TRUE true} for a defined list of
	 * characters and {@link False#FALSE false} for all others.
	 * 
	 * @author Stephen G. Ware
	 */
	public static class Predicate implements Mapping<Disjunction<Clause<Precondition>>> {

		/** Serial version ID */
		private static final long serialVersionUID = Settings.VERSION_UID;
		
		/** The characters for which this mapping should return true */
		private final ImmutableSet<Character> characters;
		
		/**
		 * Constructs a new predicate.
		 * 
		 * @param characters the characters for which this mapping should return
		 * true
		 */
		public Predicate(ImmutableSet<Character> characters) {
			this.characters = characters;
		}
		
		/**
		 * Constructs a new predicate.
		 * 
		 * @param characters the characters for which this mapping should return
		 * true
		 */
		public Predicate(Character...characters) {
			this(new ImmutableSet<>(characters));
		}
		
		@Override
		public boolean equals(Object other) {
			return getClass().equals(other.getClass()) && characters.equals(((Predicate) other).characters);
		}
		
		@Override
		public int hashCode() {
			return Utilities.hashCode(getClass(), characters);
		}
		
		@Override
		public String toString() {
			return characters.toString();
		}
		
		@Override
		public int compareTo(Logical other) {
			if(getClass().equals(other.getClass()))
				return Utilities.compare(characters, ((Predicate) other).characters);
			else
				return Mapping.super.compareTo(other);
		}
		
		@Override
		public boolean isGround() {
			return true;
		}

		@Override
		public Predicate apply(java.util.function.Function<Object, Object> function) {
			return this;
		}

		@Override
		public boolean isBoolean() {
			return true;
		}

		@Override
		public boolean isNumber() {
			return false;
		}

		@Override
		public boolean isEntity() {
			return false;
		}

		@Override
		public boolean isCharacter() {
			return false;
		}

		@Override
		public boolean is(Type type) {
			return type.isBoolean();
		}

		@Override
		public Disjunction<Clause<Precondition>> get(Parameter character) {
			if(characters.contains(character))
				return True.TRUE.toPrecondition();
			else
				return False.FALSE.toPrecondition();
		}

		@Override
		@SuppressWarnings("unchecked")
		public <T extends Expression> Mapping<T> cast(Class<T> type) {
			type.cast(True.TRUE.toPrecondition());
			type.cast(False.FALSE.toPrecondition());
			return (Mapping<T>) this;
		}
	}
}