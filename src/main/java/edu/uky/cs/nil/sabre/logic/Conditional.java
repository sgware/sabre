package edu.uky.cs.nil.sabre.logic;

import java.util.ArrayList;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.Exceptions;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.Type;
import edu.uky.cs.nil.sabre.Utilities;
import edu.uky.cs.nil.sabre.util.ImmutableArray;

/**
 * A conditional is a {@link Expression logical expression} which can have one
 * of several possible {@link #branches values} depending on which of its
 * Boolean {@link #conditions conditions} evaluates to {@link True true}. A
 * conditional defines some number of condition/branch pairs. {@link
 * #conditions Conditions} must be of type {@code boolean}, but {@link
 * #branches branches} can be of any type. The first condition is paired with
 * the first branch, the second condition with the second branch, and so on.
 * A conditional evaluates its conditions in order; once a condition which
 * evaluates to {@link True true} is found, the corresponding branch is
 * evaluated and that value returned. There should always be exactly one more
 * branch than condition; the condition for the last branch is implicitly
 * {@link True true} in order to ensure that some condition will always hold.
 * A conditional can be thought of as an {@code if / else if ... / else}
 * statement in a programming language where the {@code else} branch is
 * mandatory.
 * <p>
 * A trivial conditional has zero conditions and one branch (whose condition
 * is implicitly {@link True true}). A trivial conditional is logically
 * equivalent to its only branch.
 * <p>
 * The return value of {@link Expression#toValued()} is a conditional whose
 * conditions are in {@link Expression#toPrecondition() disjunctive normal
 * form} and whose branches are {@link Expression#isValued() valued
 * expressions}. A conditional is not a valued expression, but removing
 * conditionals from inside an expression and converting it to potentially
 * many branches is an important step in converting an expression {@link
 * Expression#toValued() to a valued expression}. The {@link
 * Expression#toValued()} method of expressions which do not contain any
 * conditionals should return a trivial conditional.
 * <p>
 * When a conditional effect is converted to {@link #toPrecondition() 
 * disjunctive normal form as a precondition} the ordering of the conditions
 * may no longer be enforced, so the conditions are serialized. This means the
 * first condition is unchanged, but the second condition includes a negation
 * of the first condition, and the third condition includes a negation of the
 * first and second conditions, etc. The same is true when converting to {@link
 * #toEffect() disjunctive normal form as an effect}, since the conditional
 * will becomes a series of {@link Effect conditional effects} whose order can
 * no longer be enforced; the conditions will be serialized to preserve the
 * effects of the original ordering of the conditions.
 * 
 * @param <E> the type of expression of this conditional's condition; this type
 * must be compatible with {@link True true}, since the last condition of every
 * conditional is implicitly {@link True true}
 * @author Stephen G. Ware
 */
public class Conditional<E extends Expression> implements Expression {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * The conditions that will be checked in order (the first condition is
	 * paired with the first branch, the second with the second branch, etc.)
	 */
	public final ImmutableArray<E> conditions;
	
	/**
	 * The expressions which will be evaluated if their corresponding conditions
	 * evaluate to true
	 */
	public final ImmutableArray<Expression> branches;
	
	/**
	 * Constructs a new conditional with the given {@link ImmutableArray
	 * immutable arrays} of conditions and branches.
	 * 
	 * @param conditions the Boolean conditions, in the order they should be
	 * evaluated
	 * @param branches exactly one more branch than conditions, where the first
	 * branch is paired with the first condition, the second branch with the
	 * second condition, etc., and the last branch implicitly has a condition
	 * of {@link True true}
	 * @throws edu.uky.cs.nil.sabre.FormatException if any condition is not of
	 * type {@code boolean}
	 * @throws edu.uky.cs.nil.sabre.FormatException if the number of branches
	 * is not exactly the number of conditions plus one
	 */
	public Conditional(ImmutableArray<E> conditions, ImmutableArray<Expression> branches) {
		if(conditions.size() != branches.size() - 1)
			throw Exceptions.conditionBranchCount(conditions.size(), branches.size());
		for(int i=0; i<conditions.size(); i++)
			conditions.get(i).mustBeBoolean();
		this.conditions = conditions;
		this.branches = branches;
	}
	
	/**
	 * Constructs a new conditional with the given arrays of conditions and
	 * branches. See {@link #Conditional(ImmutableArray, ImmutableArray)}.
	 * 
	 * @param conditions the conditions
	 * @param branches the branches
	 */
	public Conditional(E[] conditions, Expression[] branches) {
		this(new ImmutableArray<>(conditions), new ImmutableArray<>(branches));
	}
	
	/**
	 * Constructs a new conditional with the given {@link Iterable iterables}
	 * of conditions and branches. See
	 * {@link #Conditional(ImmutableArray, ImmutableArray)}.
	 * 
	 * @param conditions the conditions
	 * @param branches the branches
	 */
	public Conditional(Iterable<E> conditions, Iterable<Expression> branches) {
		this(new ImmutableArray<>(conditions), new ImmutableArray<>(branches));
	}
	
	/**
	 * Constructs a conditional with a single condition and two branches. See
	 * {@link #Conditional(ImmutableArray, ImmutableArray)}.
	 * 
	 * @param condition the single condition
	 * @param positive the branch that will be evaluated if the condition is
	 * {@link True true}
	 * @param negative the branch that will be evaluated if the condition is
	 * {@link False false}
	 * @throws edu.uky.cs.nil.sabre.FormatException if the condition is not of
	 * type {@code boolean}
	 */
	public Conditional(E condition, Expression positive, Expression negative) {
		this(new ImmutableArray<>(condition), new ImmutableArray<>(positive, negative));
	}
	
	/**
	 * Constructs a trivial conditional whose implicit condition is {@link True
	 * true} and which will always evaluate to the value of its only branch.
	 * 
	 * @param branch the branch that will always be evaluated
	 */
	public Conditional(Expression branch) {
		this(new ImmutableArray<>(), new ImmutableArray<>(branch));
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			Conditional<?> otherConditional = (Conditional<?>) other;
			return conditions.equals(otherConditional.conditions) && branches.equals(otherConditional.branches);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), conditions.hashCode(), branches.hashCode());
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}
	
	@Override
	public int compareTo(Logical other) {
		if(getClass().equals(other.getClass())) {
			Conditional<?> otherConditional = (Conditional<?>) other;
			int comparison = Utilities.compare(conditions, otherConditional.conditions);
			if(comparison == 0)
				comparison = Utilities.compare(branches, otherConditional.branches);
			return comparison;
		}
		else
			return Expression.super.compareTo(other);
	}

	@Override
	public boolean isGround() {
		for(int i=0; i<branches.size(); i++)
			if(!getCondition(i).isGround() || !branches.get(i).isGround())
				return false;
		return true;
	}

	@Override
	public Conditional<?> apply(Function<Object, Object> function) {
		ImmutableArray<E> conditions = this.conditions.apply(function);
		ImmutableArray<Expression> branches = this.branches.apply(function);
		if(conditions != this.conditions || branches != this.branches)
			return new Conditional<>(conditions, branches);
		else
			return this;
	}
	
	/**
	 * Tests whether every branch of this conditional is of type {@code
	 * boolean}.
	 * 
	 * @return true if every branch is Boolean, false otherwise
	 */
	@Override
	public boolean isBoolean() {
		for(int i=0; i<branches.size(); i++)
			if(!branches.get(i).isBoolean())
				return false;
		return true;
	}
	
	/**
	 * Tests whether every branch of this conditional is of type {@code
	 * number}.
	 * 
	 * @return true if every branch is numeric, false otherwise
	 */
	@Override
	public boolean isNumber() {
		for(int i=0; i<branches.size(); i++)
			if(!branches.get(i).isNumber())
				return false;
		return true;
	}
	
	/**
	 * Tests whether every branch of this conditional is of type {@code 
	 * entity}.
	 * 
	 * @return true if every branch is of type {@code entity}, false otherwise
	 */
	@Override
	public boolean isEntity() {
		for(int i=0; i<branches.size(); i++)
			if(!branches.get(i).isEntity())
				return false;
		return true;
	}
	
	/**
	 * Tests whether every branch of this conditional is of type {@code 
	 * character}.
	 * 
	 * @return true if every branch is of type {@code character}, false
	 * otherwise
	 */
	@Override
	public boolean isCharacter() {
		for(int i=0; i<branches.size(); i++)
			if(!branches.get(i).isCharacter())
				return false;
		return true;
	}
	
	/**
	 * Tests whether every branch of this conditional is of a given {@link Type
	 * type}.
	 * 
	 * @param type the type being tested
	 * @return true if every branch is of this type or one of its subtypes,
	 * false otherwise
	 */
	@Override
	public boolean is(Type type) {
		for(int i=0; i<branches.size(); i++)
			if(!branches.get(i).is(type))
				return false;
		return true;
	}
	
	@Override
	public Expression simplify() {
		Conditional<?> simplified = (Conditional<?>) Expression.super.simplify();
		ArrayList<Expression> conditions = new ArrayList<>();
		ArrayList<Expression> branches = new ArrayList<>();
		for(int i=0; i<simplified.branches.size(); i++)
			if(collect(simplified.getCondition(i), simplified.branches.get(i), conditions, branches))
				break;
		if(conditions.size() == 0)
			return branches.get(0);
		else
			return new Conditional<>(conditions, branches);
	}
	
	private static final boolean collect(Expression condition, Expression branch, ArrayList<Expression> conditions, ArrayList<Expression> branches) {
		if(condition.equals(False.FALSE))
			return false;
		else if(branch instanceof Conditional) {
			Conditional<?> conditional = (Conditional<?>) branch;
			for(int i=0; i<conditional.branches.size(); i++)
				if(collect(new Conjunction<>(condition, conditional.getCondition(i)).simplify(), conditional.branches.get(i), conditions, branches))
					return true;
			return false;
		}
		else if(condition.equals(True.TRUE)) {
			branches.add(branch);
			return true;
		}
		else {
			conditions.add(condition);
			branches.add(branch);
			return false;
		}
	}

	/**
	 * Evaluates each {@link #conditions condition} in order until one of them
	 * evaluates to {@link True true}, and then evaluates the {@link #branches
	 * branch} corresponding to that condition and returns that {@link Value
	 * value}. Because the last branch implicitly has a condition of {@link
	 * True true}, some branch will always be evaluated. Evaluation of
	 * conditions is short circuiting, meaning that once a condition evaluates
	 * to true, evaluation of conditions will stop.
	 * 
	 * @param state the state in which this conditional will be evaluated
	 * @return the value of the branch corresponding to the true condition
	 */
	@Override
	public Value evaluate(State state) {
		for(int i=0; i<conditions.size(); i++)
			if(conditions.get(i).evaluate(state).equals(True.TRUE))
				return branches.get(i).evaluate(state);
		return branches.get(branches.size() - 1).evaluate(state);
	}
	
	@Override
	public Conditional<?> prepend(Parameter character) {
		return (Conditional<?>) Expression.super.prepend(character);
	}
	
	/**
	 * Returns a new conditional with the same {@link #conditions conditions}
	 * but whose {@link #branches branches} have been {@link
	 * Expression#negate() negated}.
	 * 
	 * @return a conditional with the same conditions but whose branches have
	 * been negated
	 * @throws edu.uky.cs.nil.sabre.FormatException if any branch cannot be
	 * negated, usually because it is not of type {@code boolean}
	 */
	@Override
	public Conditional<E> negate() {
		Expression[] branches = new Expression[this.branches.size()];
		for(int i=0; i<branches.length; i++)
			branches[i] = new Negation(this.branches.get(i));
		return new Conditional<>(conditions, new ImmutableArray<>(branches));
	}
	
	@Override
	public Conditional<Disjunction<Clause<Precondition>>> toValued() {
		if(isBoolean())
			return new Conditional<>(toPrecondition(), True.TRUE, False.FALSE);
		else {
			ArrayList<Disjunction<Clause<Precondition>>> conditions = new ArrayList<>();
			ArrayList<Expression> branches = new ArrayList<>();
			for(int i=0; i<this.branches.size(); i++) {
				Conditional<Disjunction<Clause<Precondition>>> inner = this.branches.get(i).toValued();
				for(int j=0; j<inner.branches.size(); j++) {
					Disjunction<Clause<Precondition>> condition = new Conjunction<>(getCondition(i), inner.getCondition(j)).toPrecondition();
					if(condition.equals(True.TRUE)) {
						branches.add(inner.branches.get(j).simplify());
						return new Conditional<>(conditions, branches);
					}
					else if(!condition.equals(False.FALSE)) {
						conditions.add(condition);
						branches.add(inner.branches.get(j).simplify());
					}
				}
			}
			return new Conditional<>(conditions, branches);
		}
	}
	
	/**
	 * Converts this conditional expression to {@link
	 * Expression#toPrecondition() disjunctive normal form}, but since the
	 * order of conditions may no longer be enforced in the resulting
	 * disjunction, the {@link #conditions conditions} are serialized. This
	 * means the second condition includes a negation of the first condition,
	 * the third condition includes a negation of the first and second
	 * conditions, and so on.
	 * 
	 * @return a disjunction of conjunctive clauses whose atoms are
	 * preconditions
	 */
	@Override
	public Disjunction<Clause<Precondition>> toPrecondition() {
		Expression[] conditions = serialize();
		Expression[] disjuncts = new Expression[conditions.length];
		for(int i=0; i<disjuncts.length; i++)
			disjuncts[i] = new Conjunction<>(conditions[i], branches.get(i));
		return new Disjunction<>(disjuncts).toPrecondition();
	}

	/**
	 * Converts this conditional expression to {@link Expression#toEffect()
	 * disjunctive normal form}, but since the order of conditions may no
	 * longer be forced in the resulting clause, the {@link #conditions
	 * conditions} are serialized. This means the second condition includes a
	 * negation of the first condition, the third condition includes a negation
	 * of the first and second conditions, and so on.
	 * 
	 * @return a conjunctive clause whose atoms are effects
	 */
	@Override
	public Clause<Effect> toEffect() {
		Clause<Effect> clause = Clause.EMPTY.toEffect();
		Expression[] conditions = serialize();
		for(int i=0; i<conditions.length; i++) {
			for(Effect effect : branches.get(i).toEffect()) {
				Disjunction<Clause<Precondition>> condition = new Conjunction<>(conditions[i], effect.condition).toPrecondition();
				if(!condition.equals(False.FALSE))
					clause = clause.add(new Effect(condition, effect.fluent, effect.value));
			}
		}
		return clause;
	}
	
	private Expression[] serialize() {
		Expression[] conditions = new Expression[branches.size()];
		conditions[0] = getCondition(0);
		for(int i=1; i<conditions.length; i++) {
			Expression[] conjuncts = new Expression[i + 1];
			for(int j=0; j<i; j++)
				conjuncts[j] = new Negation(getCondition(j));
			conjuncts[i] = getCondition(i);
			conditions[i] = new Conjunction<>(conjuncts);
		}
		return conditions;
	}
	
	/**
	 * Returns the {@link #conditions condition} at the given index, unless
	 * the index is for the last (implicitly true) condition, in which case
	 * {@link True true} is returned.
	 * 
	 * @param index the index of the desired condition
	 * @return the condition
	 */
	@SuppressWarnings("unchecked")
	public E getCondition(int index) {
		if(index == conditions.size())
			return (E) True.TRUE;
		else
			return conditions.get(index);
	}
	
	/**
	 * Returns the {@link #branches branch} at the given index.
	 * 
	 * @param index the index of the desired branch
	 * @return the branch
	 */
	public Expression getBranch(int index) {
		return branches.get(index);
	}
}