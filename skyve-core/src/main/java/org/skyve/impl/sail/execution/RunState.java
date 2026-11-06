package org.skyve.impl.sail.execution;

import java.util.Map;
import java.util.TreeMap;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * What a SAIL run shares between its steps: the run token that {@code {run}} stands for, and the variables that the
 * capture steps set for {@code {var:name}}.
 *
 * <p>Each automation normally makes its own run. A workflow shares one run between its automations, even when they are
 * executed by different executors (one per renderer), by giving each executor the same state with
 * {@link ContextualExecutor#shareRun(RunState)}.
 *
 * <p>Not thread-safe: the executors that share it run one after another on one thread.
 */
public final class RunState {
	private final String runToken;
	/** The variables by name, sorted so that a failure lists the defined names in order. */
	private final Map<String, String> variables = new TreeMap<>();

	/**
	 * Starts a run with a new run token and no variables.
	 * The token is the current time in milliseconds in base 36: 8 or more lower-case letters and digits.
	 */
	public RunState() {
		runToken = Long.toString(System.currentTimeMillis(), Character.MAX_RADIX);
	}

	/**
	 * Returns the token that replaces {@value ContextualExecutor#RUN_TOKEN}.
	 *
	 * @return the run token
	 */
	public @Nonnull String getRunToken() {
		return runToken;
	}

	/**
	 * Sets a variable, replacing any earlier value.
	 *
	 * @param name	the variable name
	 * @param value	the value
	 */
	public void putVariable(@Nonnull String name, @Nonnull String value) {
		variables.put(name, value);
	}

	/**
	 * Returns a variable's value.
	 *
	 * @param name	the variable name
	 * @return the value, or {@code null} if the variable has not been set in this run
	 */
	public @Nullable String getVariable(@Nonnull String name) {
		return variables.get(name);
	}

	/**
	 * Returns the variables, sorted by name.
	 *
	 * @return the variables (a live, sorted map)
	 */
	@Nonnull Map<String, String> variables() {
		return variables;
	}
}
