package edu.uky.cs.nil.sabre;

import java.util.Iterator;

/**
 * A {@link Plan plan} which is built by appending actions on to the end.
 * 
 * @param <A> the type of {@link Action action} this plan is made of
 * @author Stephen G. Ware
 */
public class TailPlan<A extends Action> implements Plan<A> {

	/** A plan with no actions */
	public static final TailPlan<Action> EMPTY = new TailPlan<>(null, null);

	/** All but the last action in the plan */
	public final TailPlan<A> rest;
	
	/** The last action in the plan */
	public final A last;
	
	/**
	 * Constructs a new plan.
	 * 
	 * @param rest all but the last action in the plan
	 * @param last the last action in the plan
	 */
	private TailPlan(TailPlan<A> rest, A last) {
		this.rest = rest;
		this.last = last;
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			TailPlan<?> otherPlan = (TailPlan<?>) other;
			return Utilities.equals(rest, otherPlan.rest) && Utilities.equals(last, otherPlan.last);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		if(last == null)
			return getClass().hashCode();
		else
			return Utilities.hashCode(last, rest);
	}
	
	@Override
	public String toString() {
		if(this.equals(EMPTY))
			return "";
		else if(rest.equals(EMPTY))
			return last.toString();
		else
			return rest.toString() + " " + last.toString();
	}
	
	@Override
	public int size() {
		if(last == null)
			return 0;
		else
			return 1 + rest.size();
	}
	
	private final class TailPlanIterator implements Iterator<A> {

		private int index = 0;
		
		@Override
		public boolean hasNext() {
			return index < size();
		}

		@Override
		public A next() {
			if(!hasNext())
				throw Exceptions.iteratorOutOfElements();
			return get(index++);
		}
	}
	
	@Override
	public Iterator<A> iterator() {
		return new TailPlanIterator();
	}
	
	@Override
	public A get(int index) {
		TailPlan<A> plan = this;
		int count = size() - index - 1;
		for(int i = 0; i < count && plan != null; i++)
			plan = plan.rest;
		if(plan.last == null)
			throw Exceptions.indexOutOfBounds(index);
		return plan.last;
	}
	
	/**
	 * Casts this plan to one which uses a different type of action.
	 * 
	 * @param <T> the new action type
	 * @param type the class object for the new action type
	 * @return a plan using the new action type
	 * @throws ClassCastException if any action cannot be cast to the new type
	 */
	@SuppressWarnings("unchecked")
	public <T extends Action> TailPlan<T> cast(Class<T> type) {
		if(last != null) {
			type.cast(last);
			rest.cast(type);
		}
		return (TailPlan<T>) this;
	}
	
	/**
	 * Returns a new plan with the given action added on to the end.
	 * 
	 * @param action the action which will be the last action of the new plan
	 * @return the new plan
	 */
	public TailPlan<A> append(A action) {
		return new TailPlan<>(this, action);
	}
}