package org.skyve.impl.sail.execution;

import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicLong;

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
 * <p>An instance is not thread-safe: the executors that share it run one after another on one thread. Making
 * instances is: runs started on several threads at once (a load test's virtual users) get different tokens.
 */
public final class RunState {
	/** The last run token's time, so that runs started in the same millisecond get different tokens. */
	private static final AtomicLong LAST_TOKEN_TIME = new AtomicLong();

	private final String runToken;
	/** The variables by name, sorted so that a failure lists the defined names in order. */
	private final Map<String, String> variables = new TreeMap<>();

	/**
	 * Starts a run with a new run token and no variables.
	 * The token is the current time in milliseconds in base 36: 8 or more lower-case letters and digits. It is unique in
	 * the JVM: a run started in the same millisecond as the one before it (such as by a load test's virtual users)
	 * takes the next millisecond's token.
	 *
	 * <p>Side effects: advances the JVM-wide time of the last token.
	 */
	public RunState() {
		this(System.currentTimeMillis());
	}

	/**
	 * Starts a run with a token for a time, or the time after the last token's if that is not later.
	 *
	 * <p>Side effects: advances the JVM-wide time of the last token.
	 *
	 * @param millis	the current time in milliseconds
	 */
	RunState(long millis) {
		long time = LAST_TOKEN_TIME.accumulateAndGet(millis, (last, now) -> Math.max(now, last + 1L));
		runToken = Long.toString(time, Character.MAX_RADIX);
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
	 * @return the variables (a live, sorted map: copy it to keep a snapshot)
	 */
	public @Nonnull Map<String, String> variables() {
		return variables;
	}
}
