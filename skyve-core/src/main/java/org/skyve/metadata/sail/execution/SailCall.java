package org.skyve.metadata.sail.execution;

import java.util.Map;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * A handwritten Java step that a SAIL {@code <call className="…"/>} runs in the JVM that runs the automation, for what a
 * script cannot do in the page: browser DevTools, extra windows, fixtures in the test JVM.
 *
 * <p>Lifecycle: an implementation has a public no-argument constructor. The executor makes a new instance, from the
 * thread context class loader, for each {@code <call/>} it runs, calls {@link #call(Map, Executor)} once and drops it,
 * so an instance need keep no state between steps.
 *
 * <p>Threading: {@link #call(Map, Executor)} runs on the automation's thread, between the step before it and the step
 * after it. It may block, but nothing times it out.
 *
 * <p>Failures: an {@link AssertionError} is a failed assertion, which follows the test strategy: it stops the run under
 * {@code Assert}, and is recorded under {@code Verify}, where the run carries on with the next step. Any other
 * exception fails the step as a step that cannot do what it says, which stops the interaction even under
 * {@code Verify}.
 *
 * <p>Contexts: the executor's contexts are not changed by a call. A call that makes the browser show another view must be
 * followed by a navigate or context step, as a {@code <navigateLink/>} to a page that is not Skyve's must.
 *
 * @see org.skyve.metadata.sail.language.step.Call
 */
@FunctionalInterface
public interface SailCall {
	/**
	 * Runs the step.
	 *
	 * <p>A handler that needs the browser casts {@code executor} to the harness's class (in Skyve's own tests,
	 * {@code util.sail.InterpretedWebDriverExecutor}, whose {@code getDriver()} gives the {@code WebDriver}).
	 *
	 * @param parameters	the {@code <parameter/>}s by name, with {@code {run}} and {@code {var:name}} substituted in
	 * 						their values; a new map that the call may change
	 * @param executor	the executor running the automation
	 * @return the value for the step's {@code variable}, or {@code null} for none; a {@code null} fails a step that
	 * 			names a variable
	 * @throws AssertionError	if what the call checks does not hold
	 * @throws Exception	if the call cannot do what it says
	 */
	@Nullable String call(@Nonnull Map<String, String> parameters, @Nonnull Executor executor) throws Exception;
}
