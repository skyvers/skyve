package org.skyve.metadata.sail.language;


import org.skyve.metadata.sail.execution.Executor;

/**
 * Implemented by every SAIL element that can be dispatched to an {@link Executor}.
 *
 * <p>This is the callback half of the Visitor pattern used by the SAIL execution model.
 * Calling {@code execute(executor)} on any {@code Executable} causes it to invoke the
 * matching, type-specific method on the executor (e.g. {@code executeAutomation},
 * {@code executeSave}), avoiding any need for {@code instanceof} checks in the executor.
 *
 * @see Executor
 */
public interface Executable {

	/**
	 * Dispatches this element to the appropriate method on {@code executor}.
	 *
	 * @param executor the executor visiting this element; must not be {@code null}
	 */
	public void execute(Executor executor);
}
