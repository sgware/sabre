package edu.uky.cs.nil.sabre.logic;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Consumer;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.util.Countable;
import edu.uky.cs.nil.sabre.util.ImmutableArray;

/**
 * A conjunction is a {@link Proposition proposition} made up of zero to many
 * arguments (or conjuncts) which are {@link Expression expressions} of type
 * {@code boolean} and must all {@link Expression#evaluate(State) evaluate}
 * to {@link True true} for the conjunction to be true.
 * 
 * @param <E> the type of the conjunction's arguments
 * @author Stephen G. Ware
 */
public class Conjunction<E extends Expression> implements Iterable<E>, Countable, Proposition {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The conjuncts which must all be true for this conjunction to be true */
	public final ImmutableArray<E> arguments;
	
	/**
	 * Constructs a new conjunction from an {@link ImmutableArray immutable
	 * array} of conjuncts. This constructor does not check the arguments to
	 * ensure they are of type {@code boolean}.
	 * 
	 * @param arguments the conjuncts
	 */
	protected Conjunction(ImmutableArray<E> arguments) {
		this.arguments = arguments;
	}
	
	/**
	 * Constructs a new conjunction from an array of conjuncts.
	 * 
	 * @param arguments the conjuncts
	 * @throws edu.uky.cs.nil.sabre.FormatException if any conjunct is not of
	 * type {@code boolean}
	 */
	@SafeVarargs
	public Conjunction(E...arguments) {
		this(new ImmutableArray<>(arguments));
		checkFormatting(this);
	}
	
	/**
	 * Constructs a new conjunction from an {@link Iterable iterable} of
	 * conjuncts.
	 * 
	 * @param arguments the conjuncts
	 * @throws edu.uky.cs.nil.sabre.FormatException if any conjunct is not of
	 * type {@code boolean}
	 */
	public Conjunction(Iterable<E> arguments) {
		this(new ImmutableArray<>(arguments));
		checkFormatting(this);
	}
	
	private static final void checkFormatting(Conjunction<?> conjunction) {
		for(int i=0; i<conjunction.size(); i++)
			conjunction.get(i).mustBeBoolean();
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass()))
			return arguments.equals(((Conjunction<?>) other).arguments);
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
			return Utilities.compare(arguments, ((Conjunction<?>) other).arguments);
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
	public Conjunction<?> apply(Function<Object, Object> function) {
		ImmutableArray<Expression> arguments = (ImmutableArray<Expression>) this.arguments.apply(function);
		if(arguments != this.arguments)
			return new Conjunction<>(arguments);
		else
			return this;
	}
	
	/**
	 * If any argument to this conjunction can be {@link
	 * Simplifiable#simplify() simplified} to {@link False false}, this method
	 * returns false. If any argument to this conjunction can be simplified to
	 * {@link True true} it is removed. If there are no arguments, or if all
	 * arguments can be removed, this method returns {@link True true}. If
	 * exactly one argument remains after simplification, it is returned
	 * instead of a conjunction. If two or more arguments remain, they will be
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
				return False.FALSE;
		if(arguments.size() == 0)
			return True.TRUE;
		else if(arguments.size() == 1)
			return arguments.iterator().next();
		else
			return new Conjunction<>(arguments);
	}
	
	private static final boolean collect(Expression expression, Collection<Expression> arguments) {
		if(expression.equals(False.FALSE))
			return true;
		else if(expression instanceof Conjunction<?>) {
			for(Expression argument : (Conjunction<?>) expression)
				if(collect(argument, arguments))
					return true;
		}
		else if(!expression.equals(True.TRUE))
			arguments.add(expression);
		return false;
	}
	
	/**
	 * Returns {@link True true} if and only if every conjunct of this
	 * conjunction {@link Expression#evaluate(State) evaluates} to true. This
	 * method is short-circuiting, which means as soon as it encounters a false
	 * conjunct, it stops evaluating its arguments and returns {@link
	 * False#FALSE false}.
	 * 
	 * @return the value of this expression
	 */
	@Override
	public Value evaluate(State state) {
		for(int i=0; i<arguments.size(); i++)
			if(arguments.get(i).evaluate(state).equals(False.FALSE))
				return False.FALSE;
		return True.TRUE;
	}
	
	@Override
	public Conjunction<?> prepend(Parameter character) {
		return (Conjunction<?>) Proposition.super.prepend(character);
	}
	
	/**
	 * Returns a {@link Disjunction disjunction} in which each argument of this
	 * conjunction is {@link Negation negated}.
	 * 
	 * @return a disjunction of negated arguments
	 */
	@Override
	public Expression negate() {
		Expression[] arguments = new Expression[this.arguments.size()];
		for(int i=0; i<arguments.length; i++)
			arguments[i] = new Negation(this.arguments.get(i));
		return new Disjunction<>(arguments);
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public Disjunction<Clause<Precondition>> toPrecondition() {
		ArrayList<Disjunction<Clause<Precondition>>> arguments = new ArrayList<>();
		for(Expression argument : this.arguments) {
			Disjunction<Clause<Precondition>> dnf = argument.toPrecondition();
			if(dnf.equals(False.FALSE))
				return False.FALSE.toPrecondition();
			else if(!dnf.equals(True.TRUE))
				arguments.add(dnf);
		}
		TreeSet<Clause<Precondition>> clauses = new TreeSet<>();
		cartesian(arguments.toArray(new Disjunction[arguments.size()]), 0, new Clause[arguments.size()], clauses);
		if(clauses.size() == 0)
			return False.FALSE.toPrecondition();
		else if(clauses.size() == 1 && clauses.contains(Clause.EMPTY))
			return True.TRUE.toPrecondition();
		else
			return new Disjunction<>(clauses);
	}
	
	@SuppressWarnings("unchecked")
	private static final void cartesian(Disjunction<Clause<Precondition>>[] arguments, int argument, Clause<Precondition>[] parts, SortedSet<Clause<Precondition>> clauses) {
		if(argument == arguments.length) {
			Clause<Precondition> clause = (Clause<Precondition>) (Clause<?>) Clause.EMPTY;
			for(Clause<Precondition> c : parts)
				clause = clause.add(c);
			if(!clause.equals(Clause.NULL))
				clauses.add(clause);
		}
		else {
			for(Clause<Precondition> clause : arguments[argument]) {
				parts[argument] = clause;
				cartesian(arguments, argument + 1, parts, clauses);
			}
		}
	}

	@Override
	@SuppressWarnings("unchecked")
	public Clause<Effect> toEffect() {
		Clause<Effect> clause = (Clause<Effect>) (Clause<?>) Clause.EMPTY;
		for(Expression argument : arguments)
			for(Effect effect : argument.toEffect())
				clause = clause.add(effect);
		return clause;
	}
	
	/**
	 * Tests whether a given object is one of the {@link #arguments arguments}
	 * (or conjuncts) of this conjunction.
	 * 
	 * @param object the object which may be an argument
	 * @return true if the object is one of this conjunction's arguments, false
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