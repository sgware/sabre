package edu.uky.cs.nil.sabre;

import edu.uky.cs.nil.sabre.logic.Expression;
import edu.uky.cs.nil.sabre.logic.Parameter;
import edu.uky.cs.nil.sabre.util.ImmutableArray;

/**
 * A {@link Solution solution} that specifies a first {@link Action action},
 * explanations for that action, and the rest of a solution.
 * 
 * @param <A> the type of {@link Action action} this solution is made of
 * @author Stephen G. Ware
 */
public class SolutionPlan<A extends Action> implements Solution<A> {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * An empty list of solutions used when a solution has no explanations set
	 */
	private static final ImmutableArray<Solution<Action>> NO_EXPLANATIONS = new ImmutableArray<>();
	
	/** The first action in this solution */
	public final A first;
	
	/**
	 * Explanations for the action's consenting characters (except for {@link
	 * Solution#getCharacter() this solution's character})
	 */
	public final ImmutableArray<Solution<A>> explanations;
	
	/** The rest of the solution after the first action */
	public final Solution<A> rest;
	
	/**
	 * Constructs a new solution plan with the given first action, list of
	 * explanations, and the rest of the solution after the first action.
	 * 
	 * @param first the first action in the solution
	 * @param explanations the list of solutions for the action's consenting
	 * characters
	 * @param rest the rest of the solution after the first action
	 */
	protected SolutionPlan(A first, ImmutableArray<Solution<A>> explanations, Solution<A> rest) {
		this.first = first;
		this.explanations = explanations;
		this.rest = rest;
	}
	
	/**
	 * Constructs a new solution plan with the given first action and the rest
	 * of the solution after the first action. No explanations will be
	 * {@link Solution#setExplanation(Solution) set} for the action.
	 * 
	 * @param first the first action in the solution
	 * @param rest the rest of the solution after the first action
	 */
	@SuppressWarnings("unchecked")
	public SolutionPlan(A first, Solution<A> rest) {
		this(first, (ImmutableArray<Solution<A>>) (ImmutableArray<?>) NO_EXPLANATIONS, rest);
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			SolutionPlan<?> otherPlan = (SolutionPlan<?>) other;
			if(first.equals(otherPlan.first)) {
				for(Parameter character : first.consenting)
					if(!Utilities.equals(getExplanation((Character) character), otherPlan.getExplanation((Character) other)))
						return false;
				return rest.equals(otherPlan.rest);
			}
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		int hashCode = Utilities.hashCode(getClass(), rest, first);
		for(Parameter character : first.consenting)
			hashCode = Utilities.hashCode(hashCode, getExplanation((Character) character));
		return hashCode;
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}

	@Override
	public A get(int index) {
		if(index >= size())
			throw Exceptions.indexOutOfBounds(index);
		else if(index == 0)
			return first;
		else
			return rest.get(index - 1);
	}

	@Override
	public int size() {
		return 1 + rest.size();
	}

	@Override
	public Character getCharacter() {
		return rest.getCharacter();
	}

	@Override
	public Expression getGoal() {
		return rest.getGoal();
	}
	
	@Override
	public Solution<A> next() {
		return rest;
	}

	@Override
	public Solution<A> getExplanation(Character character) {
		if(Utilities.equals(getCharacter(), character))
			return this;
		for(Solution<A> explanation : explanations)
			if(Utilities.equals(explanation.getCharacter(), character))
				return explanation;
		return null;
	}

	@Override
	public Solution<A> prepend(A action) {
		return new SolutionPlan<>(action, this);
	}

	@Override
	public Solution<A> setExplanation(Solution<A> explanation) {
		if(Utilities.equals(getCharacter(), explanation.getCharacter()) || !first.consenting.contains(explanation.getCharacter()))
			throw Exceptions.noExplanationRequired(explanation.getCharacter(), first);
		else if(explanation.size() == 0 || !first.equals(explanation.get(0)))
			throw Exceptions.explanationMustStartWithAction(explanation, first);
		Solution<A> existing = getExplanation(explanation.getCharacter());
		if(existing == null)
			return new SolutionPlan<>(first, explanations.add(explanation), rest);
		else if(existing.equals(explanation))
			return this;
		else
			return new SolutionPlan<>(first, explanations.remove(existing).add(explanation), rest);
	}

	@Override
	public <T extends Action> Solution<T> cast(Class<T> type) {
		type.cast(first);
		rest.cast(type);
		return Solution.super.cast(type);
	}
}