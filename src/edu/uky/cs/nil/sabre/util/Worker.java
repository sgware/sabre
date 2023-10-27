package edu.uky.cs.nil.sabre.util;

import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * Worker provides a convenient way to run long, time-consuming processes that
 * provide occasional updates on their progress. 
 * 
 * @author Stephen G. Ware
 */
public abstract class Worker {
	
	private static final int DEFAULT_FREQUENCY = 1;
	private static final TimeUnit DEFAULT_TIME_UNIT = TimeUnit.SECONDS;
	private static final Object[] NO_ARGUMENTS = new Object[0];

	/**
	 * A status object stores the message that will be displayed occasionally
	 * as a long-running process runs; the process should update this message
	 * to reflect its current status.
	 * <p>
	 * A status has two elements: a {@link String} message and an array of
	 * arguments of type {@link Object}. The value of each arguments can be
	 * updated via {@link #update(int, Object)}. When {@link #toString()} is
	 * called, the message and arguments will be passed to {@link
	 * String#format(String, Object...)}.
	 * <p>
	 * A status object is passed to {@link Getter#get(Status)} and {@link
	 * Runner#run(Status)} when these methods are called. This class assumes
	 * {@link #update(int, Object) updating} a status object has a very low
	 * cost and can be done frequently, whereas {@link #toString() generating}
	 * and displaying the message is more costly and will be done much less
	 * frequently.
	 * 
	 * @author Stephen G. Ware
	 */
	public static class Status {
		
		private String message = "working...";
		private Object[] arguments = NO_ARGUMENTS;
		
		/**
		 * Constructs a new status with a given message and arguments.
		 * 
		 * @param message the message, suitable to be passed to {@link
		 * String#format(String, Object...)}
		 * @param arguments the arguments to be passed to {@link
		 * String#format(String, Object...)}
		 */
		public Status(String message, Object...arguments) {
			this.message = message;
			this.arguments = arguments;
		}
		
		/**
		 * Constructs a new status with a default message and no arguments.
		 */
		public Status() {
			this("working...", NO_ARGUMENTS);
		}
		
		@Override
		public String toString() {
			return String.format(message, arguments);
		}
		
		/**
		 * Sets the format of the message this status object will display.
		 * When {@link #toString()} is called to display the message, the
		 * string and arguments will be passed to
		 * {@link String#format(String, Object...)}, so the string and
		 * arguments should be formatted accordingly.
		 * 
		 * @param message the format of the message
		 * @param arguments argument to be substituted into the message
		 */
		public void setMessage(String message, Object...arguments) {
			this.message = message;
			this.arguments = arguments;
		}
		
		/**
		 * Sets the message this status object will display. When the message
		 * is set via this method, the status will have no arguments.
		 * 
		 * @param message the message
		 */
		public void setMessage(String message) {
			setMessage(message, NO_ARGUMENTS);
		}
		
		/**
		 * Updates one of the arguments previously passed to {@link
		 * #setMessage(String, Object...)}. This method does not actually
		 * generate the status message (only {@link #toString()} does that), so
		 * it has a very low cost and can be called frequently.
		 * 
		 * @param index the index of the argument to update
		 * @param value the new value of the argument
		 * @throws IndexOutOfBoundsException if there is no argument defined
		 * at that index
		 */
		public void update(int index, Object value) {
			arguments[index] = value;
		}
	}
	
	/**
	 * A getter is a {@link Worker} whose long-running process returns a value.
	 * 
	 * @param <T> the type of value returned by the long-running process
	 * @author Stephen G. Ware
	 */
	@FunctionalInterface
	public static interface Getter<T> {
		
		/**
		 * This method runs for a long time and eventually returns a value.
		 * 
		 * @param status the status object to update while the method is
		 * running
		 * @return the value
		 * @throws Exception if an exception is thrown while the method is
		 * running
		 */
		public T get(Status status) throws Exception;
	}
	
	/**
	 * Runs a {@link Getter}, printing a {@link Status status update} from a
	 * given status to standard output with a given frequency until a value is
	 * returned or an exception is thrown.
	 * 
	 * @param <T> the type of value returned by the getter
	 * @param getter the getter
	 * @param status the status object to be updated while the getter runs and
	 * which will be printed to standard output each update
	 * @param frequency the number of time units that will pass between updates
	 * @param unit the time unit that will pass between updates
	 * @return the value returned by the getter
	 * @throws RuntimeException if the getter throws an exception
	 */
	public static final <T> T get(Getter<T> getter, Status status, int frequency, TimeUnit unit) {
		T result = null;
		Throwable throwable = null;
		boolean hasPrinted = false;
		int pad = 0;
		ExecutorService executor = Executors.newSingleThreadExecutor();
		Future<T> future = executor.submit(() -> {
			return getter.get(status);
		});
		do {
			try {
				result = future.get(frequency, unit);
			}
			// If the timeout is reached, print the status and wait longer.
			catch(TimeoutException e) {
				String string = status.toString();
				pad = Math.max(pad, string.length());
				System.out.print("\r" + String.format("%-" + pad + "s", string));
				hasPrinted = true;
			}
			// If the task is cancelled or the current thread gets interrupted, return null.
			catch(CancellationException | InterruptedException e) {
				throwable = e;
				break;
			}
			// If the task threw an exception, record it.
			catch(ExecutionException e) {
				throwable = e.getCause();
				break;
			}
		} while(!future.isDone());
		executor.shutdown();
		if(throwable instanceof Error)
			throw (Error) throwable;
		else if(throwable instanceof RuntimeException)
			throw (RuntimeException) throwable;
		else if(throwable != null)
			throw new RuntimeException(throwable);
		else {
			if(hasPrinted) {
				String string = status.toString();
				pad = Math.max(pad, string.length());
				System.out.println("\r" + String.format("%-" + pad + "s", string));
			}
			return result;
		}
	}
	
	/**
	 * Runs a {@link Getter}, printing a {@link Status status update} from a
	 * given status to standard output with default frequency until a value is
	 * returned or an exception is thrown.
	 * 
	 * @param <T> the type of value returned by the getter
	 * @param getter the getter
	 * @param status the status object to be updated while the getter runs and
	 * which will be printed to standard output each update
	 * @return the value returned by the getter
	 * @throws RuntimeException if the getter throws an exception
	 */
	public static final <T> T get(Getter<T> getter, Status status) {
		return get(getter, status, DEFAULT_FREQUENCY, DEFAULT_TIME_UNIT);
	}
	
	/**
	 * Runs a {@link Getter}, printing a {@link Status status update} to
	 * standard output with a given frequency until a value is returned or an
	 * exception is thrown.
	 * 
	 * @param <T> the type of value returned by the getter
	 * @param getter the getter
	 * @param frequency the number of time units that will pass between updates
	 * @param unit the time unit that will pass between updates
	 * @return the value returned by the getter
	 * @throws RuntimeException if the getter throws an exception
	 */
	public static final <T> T get(Getter<T> getter, int frequency, TimeUnit unit) {
		return get(getter, new Status(), frequency, unit);
	}
	
	/**
	 * Runs a {@link Getter}, printing a {@link Status status update} to
	 * standard output with default frequency until a value is returned or an
	 * exception is thrown.
	 * 
	 * @param <T> the type of value returned by the getter
	 * @param getter the getter
	 * @return the value returned by the getter
	 * @throws RuntimeException if the getter throws an exception
	 */
	public static final <T> T get(Getter<T> getter) {
		return get(getter, DEFAULT_FREQUENCY, DEFAULT_TIME_UNIT);
	}
	
	/**
	 * A runner is a {@link Worker} whose long-running process does not return
	 * a value.
	 * 
	 * @author Stephen G. Ware
	 */
	@FunctionalInterface
	public static interface Runner {
		
		/**
		 * This method runs for a long time.
		 * 
		 * @param status the status object to update while the method is
		 * running
		 * @throws Exception if an exception is thrown while the method is
		 * running
		 */
		public void run(Status status) throws Exception;
	}
	
	/**
	 * Runs a {@link Runner}, printing a {@link Status status update} from a
	 * given status to standard output with a given frequency until the runner
	 * is done or an exception is thrown.
	 * 
	 * @param runner the runner
	 * @param status the status object to be updated while the runner runs and
	 * which will be printed to standard output each update
	 * @param frequency the number of time units that will pass between updates
	 * @param unit the time unit that will pass between updates
	 * @throws RuntimeException if the getter throws an exception
	 */
	public static final void run(Runner runner, Status status, int frequency, TimeUnit unit) {
		get(s -> {
			runner.run(s);
			return null;
		}, status, frequency, unit);
	}
	
	
	/**
	 * Runs a {@link Runner}, printing a {@link Status status update} from a
	 * given status to standard output with default frequency until the runner
	 * is done or an exception is thrown.
	 * 
	 * @param runner the runner
	 * @param status the status object to be updated while the runner runs and
	 * which will be printed to standard output each update
	 * @throws RuntimeException if the getter throws an exception
	 */
	public static final void run(Runner runner, Status status) {
		run(runner, status, DEFAULT_FREQUENCY, DEFAULT_TIME_UNIT);
	}
	
	/**
	 * Runs a {@link Runner}, printing a {@link Status status update} to
	 * standard output with a given frequency until the runner is done or an
	 * exception is thrown.
	 * 
	 * @param runner the runner
	 * @param frequency the number of time units that will pass between updates
	 * @param unit the time unit that will pass between updates
	 * @throws RuntimeException if the getter throws an exception
	 */
	public static final void run(Runner runner, int frequency, TimeUnit unit) {
		run(runner, new Status(), frequency, unit);
	}
	
	/**
	 * Runs a {@link Runner}, printing a {@link Status status update} to
	 * standard output with the default frequency until the runner is done or
	 * an exception is thrown.
	 * 
	 * @param runner the runner
	 * @throws RuntimeException if the getter throws an exception
	 */
	public static final void run(Runner runner) {
		run(runner, DEFAULT_FREQUENCY, DEFAULT_TIME_UNIT);
	}
}