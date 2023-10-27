package edu.uky.cs.nil.sabre;

import edu.uky.cs.nil.sabre.logic.Expression;

/**
 * An empty {@link Solution solution} that specifies an {@link Character
 * character} and goal and can be used as a starting point for building
 * non-empty solutions by {@link Solution#prepend(Action) prepending actions}.
 * 
 * @param <A> the type of {@link Action action} this solution is made of
 * @author Stephen G. Ware
 */
public class SolutionGoal<A extends Action> implements Solution<A> {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * The character whose intends to increase their utility, or null to
	 * represent the author
	 */
	public final Character character;
	
	/**
	 * A condition which, when satisfied, should lead to increased utility for
	 * the character
	 */
	public final Expression goal;
	
	/**
	 * Constructs a new solution goal with the given character and goal.
	 * 
	 * @param character the character who intends to increase their utility
	 * @param goal a condition which, when satisfied, should increase the
	 * character's utility
	 * @throws FormatException if the goal is not ground
	 */
	public SolutionGoal(Character character, Expression goal) {
		goal.mustBeGround();
		this.character = character;
		this.goal = goal;
	}
	
	/**
	 * Constructs a new solution goal for the author with the given goal
	 * expression.
	 * 
	 * @param goal a condition which, when satisfied, should increase the
	 * author's utility
	 */
	public SolutionGoal(Expression goal) {
		this(null, goal);
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			SolutionGoal<?> otherGoal = (SolutionGoal<?>) other;
			return Utilities.equals(character, otherGoal.character) && goal.equals(otherGoal.goal);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return Utilities.hashCode(getClass(), character, goal);
	}
	
	@Override
	public String toString() {
		return Utilities.DEFAULT_PRINTER.toString(this);
	}

	@Override
	public A get(int index) {
		throw Exceptions.indexOutOfBounds(index);
	}

	@Override
	public int size() {
		return 0;
	}

	@Override
	public Character getCharacter() {
		return character;
	}

	@Override
	public Expression getGoal() {
		return goal;
	}
	
	@Override
	public Solution<A> next() {
		return this;
	}

	@Override
	public Solution<A> getExplanation(Character character) {
		return null;
	}

	@Override
	public Solution<A> prepend(A action) {
		return new SolutionPlan<>(action, this);
	}

	@Override
	public Solution<A> setExplanation(Solution<A> explanation) {
		throw Exceptions.noExplanationForEmptyPlan();
	}
}