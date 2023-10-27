package edu.uky.cs.nil.sabre;

import java.io.Serializable;
import java.util.Iterator;

import edu.uky.cs.nil.sabre.logic.Expression;

/**
 * A solution is a {@link Plan plan} for an {@link #getCharacter() character} to
 * achieve a {@link #getGoal() goal} that also includes {@link
 * #getExplanation(Character) explanations} for every action in the plan.
 * Solutions are not checked for validity, so partial or incorrect solutions can
 * be constructed without causing an exception.
 * 
 * @param <A> the type of {@link Action action} this solution is made of
 * @author Stephen G. Ware
 */
public interface Solution<A extends Action> extends Serializable, Plan<A> {
	
	/**
	 * A solution iterator iterates through the actions in a {@link Solution
	 * solution}.
	 * 
	 * @param <A> the type of {@link Action action} this iterator
	 * @author Stephen G. Ware
	 */
	public static class SolutionIterator<A extends Action> implements Iterator<A> {

		private Solution<A> solution;
		
		/**
		 * Constructs a new solution iterator.
		 * 
		 * @param solution the solution whose actions this iterator will
		 * iterate through
		 */
		public SolutionIterator(Solution<A> solution) {
			this.solution = solution;
		}
		
		@Override
		public boolean hasNext() {
			return solution.size() > 0;
		}

		@Override
		public A next() {
			A action = solution.get(0);
			solution = solution.next();
			return action;
		}
	}
	
	@Override
	public default Iterator<A> iterator() {
		return new SolutionIterator<>(this);
	}

	/**
	 * Returns the {@link Character character} whose goal this solution will
	 * achieve or null if this solution is for the author's goal.
	 * 
	 * @return a character or null if the character is the author
	 */
	public Character getCharacter();
	
	/**
	 * Returns the goal this solution will achieve. The goal is a {@link
	 * Expression logical expression} which, if satisfied, would increase this
	 * solution's {@link #getCharacter() character's} utility.
	 * 
	 * @return the goal the character is attempting to achieve
	 */
	public Expression getGoal();
	
	/**
	 * Returns the remainder of this solution after the first action. If this
	 * solution has no actions, this method returns this solution.
	 * 
	 * @return this solution minus the first action
	 */
	public Solution<A> next();
	
	/**
	 * Returns a {@link Solution solution} explaining how the first action in
	 * this solution would contribute to increasing the given character's
	 * utility. If the given character is {@link #getCharacter() this solution's
	 * character}, this method returns this solution. If the given character is
	 * not a consenting character to the first action or if no solution has been
	 * {@link #setExplanation(Solution) set} for the given character, this
	 * method returns null. If an explanation is returned, its first action will
	 * be the same as the first action in this solution.
	 * 
	 * @param character the character for whom an explanation of this solution's
	 * first action is desired
	 * @return an explanation for how the character believes the first action in
	 * this solution will contribute to increasing their utility, or null if
	 * the character is not a consenting character to the action or no
	 * explanation has been set
	 */
	public Solution<A> getExplanation(Character character);
	
	/**
	 * Adds an action to the beginning of this solution. Validity is not
	 * checked, meaning there are no guarantees that this action actually
	 * contributes to the solution or is non-redundant. Initially, the action
	 * will have no explanations; those can be {@link #setExplanation(Solution)
	 * set}.
	 * 
	 * @param action the action to added to the beginning of the solution
	 * @return a new solution with the given action at the beginning
	 */
	public Solution<A> prepend(A action);
	
	/**
	 * Sets an explanation for the first action of this solution. The
	 * explanation will be set for {@link #getCharacter() the given solution's
	 * character}. If a different solution is already set for that character, it
	 * will be replaced. The explanation must not be for this solution's
	 * character, and it must begin with the same first action that this
	 * solution begins with.
	 * 
	 * @param explanation an explanation for the first action
	 * @return a new solution with this explanation set for its character
	 * @throws IllegalArgumentException if this solution has no actions
	 * @throws IllegalArgumentException if the given explanation does not begin
	 * with the same action as this solution
	 * @throws IllegalArgumentException if the given explanation is for the
	 * this solution's character or if the given solution is for a character who
	 * is not a consenting character of this solution's first action
	 */
	public Solution<A> setExplanation(Solution<A> explanation);
	
	/**
	 * Casts the type of action in this solution to a different type.
	 * 
	 * @param <T> the new type of action this solution should be made of
	 * @param type the class object for the new type
	 * @return a solution whose actions have been cast to the new type
	 * @throws ClassCastException if any action cannot be cast to the new type
	 */
	@SuppressWarnings("unchecked")
	public default <T extends Action> Solution<T> cast(Class<T> type) {
		return (Solution<T>) this;
	}
}