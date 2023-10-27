package edu.uky.cs.nil.sabre.logic;

import java.util.Collection;
import java.util.Iterator;
import java.util.TreeSet;
import java.util.function.Consumer;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.util.Countable;
import edu.uky.cs.nil.sabre.util.ImmutableArray;

/**
 * A disjunction is a {@link Proposition proposition} made up of zero to many
 * arguments (or disjuncts) which are {@link Expression expressions} of type
 * {@code boolean} such that at least one of them must {@link
 * Expression#evaluate(State) evaluate} to {@link True true} for the
 * disjunction to be true.
 * 
 * @param <E> the type of the disjunction's arguments
 * @author Stephen G. Ware
 */
public class Disjunction<E extends Expression> implements Iterable<E>, Countable, Proposition {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * The disjuncts which must contain at least one true expression for this
	 * disjunction to be true
	 */
	public final ImmutableArray<E> arguments;
	
	/**
	 * Constructs a new disjunction from an {@link ImmutableArray immutable
	 * array} of disjuncts. This constructor does not check the arguments to
	 * ensure they are of type {@code boolean}.
	 * 
	 * @param arguments the disjuncts
	 */
	protected Disjunction(ImmutableArray<E> arguments) {
		this.arguments = arguments;
	}
	
	/**
	 * Constructs a new disjunction from an array of disjuncts.
	 * 
	 * @param arguments the disjuncts
	 * @throws edu.uky.cs.nil.sabre.FormatException if any disjunct is not of
	 * type {@code boolean}
	 */
	@SafeVarargs
	public Disjunction(E...arguments) {
		this(new ImmutableArray<>(arguments));
		checkFormatting(this);
	}
	
	/**
	 * Constructs a new disjunction from an {@link Iterable iterable} of
	 * disjuncts.
	 * 
	 * @param arguments the disjuncts
	 * @throws edu.uky.cs.nil.sabre.FormatException if any disjunct is not of
	 * type {@code boolean}
	 */
	public Disjunction(Iterable<E> arguments) {
		this(new ImmutableArray<>(arguments));
		checkFormatting(this);
	}
	
	private static final void checkFormatting(Disjunction<?> disjunction) {
		for(int i=0; i<disjunction.size(); i++)
			disjunction.get(i).mustBeBoolean();
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass()))
			return arguments.equals(((Disjunction<?>) other).arguments);
		return false;
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), arguments);
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public Iterator<E> iterator() {
		return arguments.iterator();
	}
	
	@Override
	public void forEach(Consumer<? super E> consumer) {
		arguments.forEach(consumer);
	}
	
	@Override
	public int size() {
		return arguments.size();
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(other.getClass()))
			return Utilities.compare(arguments, ((Disjunction<?>) other).arguments);
		else
			return Proposition.super.compareTo(other);
	}

	@Override
	public boolean isGround() {
		for(int i=0; i<arguments.size(); i++)
			if(!arguments.get(i).isGround())
				return false;
		return true;
	}

	@Override
	@SuppressWarnings("unchecked")
	public Disjunction<?> apply(Function<Object, Object> function) {
		ImmutableArray<Expression> arguments = (ImmutableArray<Expression>) this.arguments.apply(function);
		if(arguments != this.arguments)
			return new Disjunction<>(arguments);
		else
			return this;
	}
	
	/**
	 * If any argument to this disjunction can be {@link
	 * Simplifiable#simplify() simplified} to {@link True true}, this method
	 * returns true. If any argument to this disjunction can be simplified to
	 * {@link False false} it is removed. If there are no arguments, or if all
	 * arguments can be removed, this method returns {@link False false}. If
	 * exactly one argument remains after simplification, it is returned
	 * instead of a disjunction. If two or more arguments remain, they will be
	 * put into {@link Comparable#compareTo(Object) their natural order}.
	 * 
	 * @return a simplified expression, or this expression if nothing could
	 * be simplified
	 */
	@Override
	public Expression simplify() {
		TreeSet<Expression> arguments = new TreeSet<>();
		for(Expression argument : this.arguments)
			if(collect(argument.simplify(), arguments))
				return True.TRUE;
		if(arguments.size() == 0)
			return False.FALSE;
		else if(arguments.size() == 1)
			return arguments.iterator().next();
		else
			return new Disjunction<>(arguments);
	}
	
	private static final boolean collect(Expression expression, Collection<Expression> arguments) {
		if(expression.equals(True.TRUE))
			return true;
		else if(expression instanceof Disjunction<?>) {
			for(Expression argument : (Disjunction<?>) expression)
				if(collect(argument, arguments))
					return true;
		}
		else if(!expression.equals(False.FALSE))
			arguments.add(expression);
		return false;
	}
	
	/**
	 * Returns {@link True true} if and only if at least one disjunct of this
	 * disjunction {@link Expression#evaluate(State) evaluates} to true. This
	 * method is short-circuiting, which means as soon as it encounters a true
	 * disjunct, it stops evaluating its arguments and returns {@link
	 * True#TRUE true}.
	 * 
	 * @return the value of this expression
	 */
	@Override
	public Value evaluate(State state) {
		for(int i=0; i<arguments.size(); i++)
			if(arguments.get(i).evaluate(state).equals(True.TRUE))
				return True.TRUE;
		return False.FALSE;
	}
	
	@Override
	public Disjunction<?> prepend(Parameter character) {
		return (Disjunction<?>) Proposition.super.prepend(character);
	}
	
	/**
	 * Returns a {@link Conjunction conjunction} in which each argument of this
	 * disjunction is {@link Negation negated}.
	 * 
	 * @return a conjunction of negated arguments
	 */
	@Override
	public Expression negate() {
		Expression[] arguments = new Expression[this.arguments.size()];
		for(int i=0; i<arguments.length; i++)
			arguments[i] = new Negation(this.arguments.get(i));
		return new Conjunction<>(arguments);
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public Disjunction<Clause<Precondition>> toPrecondition() {
		if(arguments.size() == 0)
			return False.FALSE.toPrecondition();
		else if(size() == 1 && arguments.get(0).equals(Clause.EMPTY))
			return True.TRUE.toPrecondition();
		else if(isPrecondition(this))
			return (Disjunction<Clause<Precondition>>) this;
		TreeSet<Clause<Precondition>> clauses = new TreeSet<>();
		for(Expression argument : arguments) {
			Disjunction<Clause<Precondition>> dnf = argument.toPrecondition();
			if(dnf.equals(True.TRUE))
				return True.TRUE.toPrecondition();
			else if(!dnf.equals(False.FALSE))
				for(Clause<Precondition> clause : dnf)
					clauses.add(clause);
		}
		if(clauses.size() == 0)
			return False.FALSE.toPrecondition();
		else
			return new Disjunction<>(clauses);
	}
	
	private static final boolean isPrecondition(Disjunction<?> disjunction) {
		if(!isClause(disjunction.get(0)))
			return false;
		for(int i=1; i<disjunction.size(); i++)
			if(!isClause(disjunction.get(i)) || disjunction.get(i-1).compareTo(disjunction.get(i)) > 0)
				return false;
		return true;
	}
	
	private static final boolean isClause(Expression expression) {
		if(expression instanceof Clause) {
			Clause<?> clause = (Clause<?>) expression;
			return clause.isPrecondition() && !clause.equals(Clause.NULL) && !clause.equals(Clause.EMPTY);
		}
		return false;
	}

	@Override
	public Clause<Effect> toEffect() {
		TreeSet<Clause<Effect>> clauses = new TreeSet<>();
		for(Expression argument : arguments) {
			Clause<Effect> clause = argument.toEffect();
			if(clause.equals(Clause.EMPTY))
				return Clause.EMPTY.toEffect();
			else if(!clause.equals(Clause.NULL))
				clauses.add(clause);
		}
		if(clauses.size() == 1)
			return clauses.iterator().next();
		else
			return Proposition.super.toEffect();
	}
	
	/**
	 * Tests whether a given object is one of the {@link #arguments arguments}
	 * (or disjuncts) of this disjunction.
	 * 
	 * @param object the object which may be an argument
	 * @return true if the object is one of this disjunction's arguments, false
	 * otherwise
	 */
	public boolean contains(Object object) {
		return arguments.contains(object);
	}
	
	/**
	 * Returns the {@link #arguments argument} corresponding to a given index.
	 * 
	 * @param index the index
	 * @return the argument at that index
	 */
	public E get(int index) {
		return arguments.get(index);
	}
}