package edu.uky.cs.nil.sabre;

import java.util.Iterator;

/**
 * A {@link Plan plan} which is built by prepending actions on to the
 * beginning.
 * 
 * @param <A> the type of {@link Action action} this plan is made of
 * @author Stephen G. Ware
 */
public class HeadPlan<A extends Action> implements Plan<A> {
	
	/** A plan with no actions */
	public static final HeadPlan<Action> EMPTY = new HeadPlan<>(null, null);
	
	/** The first action in the plan */
	public final A first;
	
	/** All but the first action in the plan */
	public final HeadPlan<A> rest;
	
	/**
	 * Constructs a new plan.
	 * 
	 * @param first the first action in the plan
	 * @param rest all but the first action in the plan
	 */
	private HeadPlan(A first, HeadPlan<A> rest) {
		this.first = first;
		this.rest = rest;
	}
	
	@Override
	public boolean equals(Object other) {
		if(getClass().equals(other.getClass())) {
			HeadPlan<?> otherPlan = (HeadPlan<?>) other;
			return Utilities.equals(first, otherPlan.first) && Utilities.equals(rest, otherPlan.rest);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		if(first == null)
			return getClass().hashCode();
		else
			return Utilities.hashCode(first, rest);
	}
	
	@Override
	public String toString() {
		if(this.equals(EMPTY))
			return "";
		else if(rest.equals(EMPTY))
			return first.toString();
		else
			return first.toString() + " " + rest.toString();
	}
	
	@Override
	public int size() {
		if(first == null)
			return 0;
		else
			return 1 + rest.size();
	}
	
	private final class HeadPlanIterator implements Iterator<A> {

		private HeadPlan<A> plan = HeadPlan.this;
		
		@Override
		public boolean hasNext() {
			return plan.first != null;
		}

		@Override
		public A next() {
			if(!hasNext())
				throw Exceptions.iteratorOutOfElements();
			A action = plan.first;
			plan = plan.rest;
			return action;
		}
	}

	@Override
	public Iterator<A> iterator() {
		return new HeadPlanIterator();
	}

	@Override
	public A get(int index) {
		HeadPlan<A> plan = this;
		for(int i = 0; i < index && plan != null; i++)
			plan = plan.rest;
		if(plan.first == null)
			throw Exceptions.indexOutOfBounds(index);
		return plan.first;
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
	public <T extends Action> HeadPlan<T> cast(Class<T> type) {
		if(first != null) {
			type.cast(first);
			rest.cast(type);
		}
		return (HeadPlan<T>) this;
	}
	
	/**
	 * Returns a new plan with the given action added to the start.
	 * 
	 * @param action the action which will be the first action of the new plan
	 * @return the new plan
	 */
	public HeadPlan<A> prepend(A action) {
		return new HeadPlan<>(action, this);
	}
}