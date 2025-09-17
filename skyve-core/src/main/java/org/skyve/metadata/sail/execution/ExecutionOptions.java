package org.skyve.metadata.sail.execution;

/**
 * Encapsulates options for executing a SAIL automation script.
 * 
 * @author simeonsolomou
 */
public class ExecutionOptions {

	private final boolean windowed;

    private ExecutionOptions(boolean windowed) {
        this.windowed = windowed;
    }

    public boolean isWindowed() {
        return windowed;
    }

    public static ExecutionOptions defaultOptions() {
        return new ExecutionOptions(false);
    }

	public static ExecutionOptions windowed() {
        return new ExecutionOptions(true);
    }
}
