package edu.uky.cs.nil.sabre.logic;

import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.io.DefaultParser;
import edu.uky.cs.nil.sabre.util.ImmutableSet;
import edu.uky.cs.nil.sabre.util.Unique;

/**
 * A quantified expression is a {@link Proposition proposition} which states
 * that a given {@link #argument} proposition holds for some number of possible
 * values that can be substituted for a {@link #variable}.
 * 
 * @author Stephen G. Ware
 */
public class Quantified implements Proposition {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;

	/**
	 * A quantifier is a singleton object that specifies for how many possible
	 * values a {@link Quantifier quantifier's} proposition must hold.
	 * 
	 * @author Stephen G. Ware
	 */
	public static abstract class Quantifier implements Comparable<Quantifier>, Serializable, Unique {
		
		/** Serial version ID */
		private static final long serialVersionUID = Quantified.serialVersionUID;
		
		private Quantifier() {}
		
		@Override
		public int compareTo(Quantifier other) {
			return hashCode() - other.hashCode();
		}
		
		/**
		 * Returns the quantifier this quantifier would become when {@link
		 * Quantifier#negate() negating a quantified expression}.
		 * 
		 * @return the negated quantifier
		 */
		public abstract Quantifier negate();
		
		/**
		 * Returns {@link Expression a logical expression} that is logically
		 * equivalent to a {@link Quantified quantified expression} with this
		 * quantifier but which does not contain the quantifier or variable.
		 * 
		 * @param variable the quantified variable
		 * @param argument an expression in which the variable appears
		 * @return a logical expression equivalent to a quantified expression
		 * where the variable is quantified with this quantifier but in which
		 * this quantifier and variable do not appear
		 */
		public Expression expand(Variable variable, Expression argument) {
			ImmutableSet<Value> values = variable.type.universe.getValues(variable.type);
			Expression[] arguments = new Expression[values.size()];
			for(int i=0; i<arguments.length; i++)
				arguments[i] = (Expression) argument.substitute(variable, values.get(i));
			return expand(arguments);
		}
		
		/**
		 * A utility method for {@link #expand(Variable, Expression)} that
		 * creates an expression from an array of arguments which are copies
		 * of a quantified expression with the quantified variable replaced
		 * with every possible value it can have.
		 * 
		 * @param arguments copies of a quantified expression where the
		 * variable has been replaced with every possible value
		 * @return a logical expression made from the arguments
		 */
		protected abstract Expression expand(Expression[] arguments);
	}
	
	/**
	 * The universal {@link Quantifier quantifier} specifies that a quantified
	 * expression must hold for every possible value its variable can have.
	 */
	public static final Quantifier UNIVERSAL = new Quantifier() {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1;
		
		@Override
		public int hashCode() {
			return 0;
		}
		
		@Override
		public String toString() {
			return DefaultParser.UNIVERSAL_QUANTIFICATION_KEYWORD;
		}
		
		/**
		 * Returns {@link Quantified#EXISTENTIAL the existential quantifier}.
		 * 
		 * @returns {@link Quantified#EXISTENTIAL the existential quantifier}
		 */
		@Override
		public Quantifier negate() {
			return EXISTENTIAL;
		}
		
		/**
		 * Returns a conjunction of its arguments.
		 * 
		 * @param arguments copies of a quantified expression where the
		 * variable has been replaced with every possible value
		 * @return a conjunction of the arguments
		 */
		@Override
		protected Conjunction<Expression> expand(Expression[] arguments) {
			return new Conjunction<>(arguments);
		}
		
		private Object readResolve() throws ObjectStreamException {
			return UNIVERSAL;
		}
	};
	
	/**
	 * The existential {@link Quantifier quantifier} specifies that a
	 * quantified expression must hold for at least one possible value its
	 * variable can have.
	 */
	public static final Quantifier EXISTENTIAL = new Quantifier() {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1;
		
		@Override
		public int hashCode() {
			return 1;
		}
		
		@Override
		public String toString() {
			return DefaultParser.EXISTENTIAL_QUANTIFICATION_KEYWORD;
		}
		
		/**
		 * Returns {@link Quantified#UNIVERSAL the universal quantifier}.
		 * 
		 * @returns {@link Quantified#UNIVERSAL the universal quantifier}
		 */
		@Override
		public Quantifier negate() {
			return UNIVERSAL;
		}
		
		/**
		 * Returns a disjunction of its arguments.
		 * 
		 * @param arguments copies of a quantified expression where the
		 * variable has been replaced with every possible value
		 * @return a disjunction of the arguments
		 */
		@Override
		protected Disjunction<Expression> expand(Expression[] arguments) {
			return new Disjunction<>(arguments);
		}
		
		private Object readResolve() throws ObjectStreamException {
			return EXISTENTIAL;
		}
	};
	
	/** The quantifier which specifies how frequently its argument must hold */
	public final Quantifier quantifier;
	
	/** The quantified variable used in the argument */
	public final Variable variable;
	
	/**
	 * A proposition in which the quantified variable appears and which must
	 * hold with the frequency specified by the quantifier
	 */
	public final Expression argument;
	
	/**
	 * Constructs a new quantified expression.
	 * 
	 * @param quantifier the quantifier that specifies how frequently its
	 * argument must hold
	 * @param variable the quantified variable used in the argument
	 * @param argument a proposition in which the quantified variable appears
	 * and which must hold with the frequency specified by the quantifier
	 */
	public Quantified(Quantifier quantifier, Variable variable, Expression argument) {
		this.quantifier = quantifier;
		this.variable = variable;
		argument.mustBeBoolean();
		this.argument = argument;
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			Quantified otherQuantified = (Quantified) other;
			return quantifier.equals(otherQuantified.quantifier) && variable.equals(otherQuantified.variable) && argument.equals(otherQuantified.argument);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), quantifier, variable, argument);
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(other.getClass())) {
			Quantified otherQuantified = (Quantified) other;
			return Utilities.compare(quantifier, otherQuantified.quantifier, variable, otherQuantified.variable, argument, otherQuantified.argument);
		}
		else
			return Proposition.super.compareTo(other);
	}
	
	@Override
	public boolean isGround() {
		return false;
	}

	@Override
	public Quantified apply(Function<Object, Object> function) {
		Variable variable = (Variable) function.apply(this.variable);
		Expression argument = (Expression) function.apply(this.argument);
		if(variable != this.variable || argument != this.argument)
			return new Quantified(quantifier, variable, argument);
		else
			return this;
	}
	
	@Override
	public Quantified simplify() {
		return (Quantified) Proposition.super.simplify();
	}
	
	/**
	 * Returns {@link True true} if the argument holds with the frequency
	 * required by the quantifier, or {@link False false} otherwise.
	 * 
	 * @param state the state in which this expression will be evaluated
	 * @return the Boolean value of this expression
	 */
	@Override
	public Value evaluate(State state) {
		return expand().evaluate(state);
	}
	
	@Override
	public Quantified prepend(Parameter character) {
		return new Quantified(quantifier, variable, new Epistemic(character, argument));
	}
	
	/**
	 * Returns a quantified expression with a {@link Quantifier#negate()
	 * negated quantifier} and a {@link Expression#negate() negated argument}.
	 * 
	 * @return a quantified expression with the opposite value of this
	 * quantified expression
	 */
	@Override
	public Quantified negate() {
		return new Quantified(quantifier.negate(), variable, new Negation(argument));
	}
	
	@Override
	public Disjunction<Clause<Precondition>> toPrecondition() {
		return expand().toPrecondition();
	}

	@Override
	public Clause<Effect> toEffect() {
		return expand().toEffect();
	}
	
	/**
	 * Returns {@link Expression a logical expression} that is logically
	 * equivalent to this expression but with the quantifier and variable
	 * removed.
	 * 
	 * @return a logically equivalent expression with the quantifier and
	 * variable removed
	 */
	public Expression expand() {
		return quantifier.expand(variable, argument);
	}
}