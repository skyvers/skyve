package org.skyve.metadata.sail.language;

import org.skyve.metadata.sail.execution.ExecutionOptions;
import org.skyve.metadata.sail.execution.Executor;

/**
 * Represents a SAIL element that can be executed by an {@link Executor}.
 * 
 * @author mike
 */
public interface Executable {

	public default void execute(Executor executor) {
		execute(executor, ExecutionOptions.defaultOptions());
	}

	public void execute(Executor executor, ExecutionOptions options);
}
