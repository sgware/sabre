package edu.uky.cs.nil.sabre.etree;

import edu.uky.cs.nil.sabre.util.Worker.Status;

/**
 * A wrapper around a {@link Status status object} used by {@link
 * EventTreeBuilder an event tree builder} to keep track of how many branches
 * of an {@link EventTree event tree} have been built.
 * 
 * @author Stephen G. Ware
 */
public class EventTreeStatus {

	/** The status object this object will update */
	public final Status status;
	
	/** The number of branches the event tree builder has finished building */
	private int complete = 0;
	
	/** The number of branches the event tree builder has started building */
	private int total = 0;
	
	/**
	 * Constructs a new event tree status.
	 * 
	 * @param status the status object this object will update
	 */
	public EventTreeStatus(Status status) {
		this.status = status;
		this.status.setMessage("Building event tree: %d/%d branches", 0, 0);
	}
	
	/**
	 * Returns the number of branches the event tree builder has finished
	 * building.
	 * 
	 * @return the number of completed branches
	 */
	public int getCompleteBranches() {
		return complete;
	}
	
	/**
	 * Increments the number of branches the event tree builder has finished
	 * building by one. This should be called right before finishing a branch.
	 */
	public void incrementCompleteBranches() {
		status.update(0, ++complete);
	}
	
	/**
	 * Returns the number of branches the event tree builder has started
	 * building (including branches which have finished building).
	 * 
	 * @return the number of started branches
	 */
	public int getTotalBranches() {
		return total;
	}
	
	/**
	 * Increments the number of branches the event tree builder has started
	 * building by one. This should be called right as the builder begins
	 * building a new branch.
	 */
	public void incrementTotalBranches() {
		status.update(1, ++total);
	}
}