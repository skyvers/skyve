package util.sail.call;

import java.time.Duration;
import java.time.Instant;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Supplier;

import org.openqa.selenium.WebDriver;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.execution.SailCall;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.InterpretedWebDriverExecutor;
import util.sail.Selenide;
import util.sail.browser.SailBrowserControl;

/**
 * The base of the standard SAIL calls: a Java step that a script runs with {@code <call className="util.sail.call.…">}
 * and its {@code <parameter/>}s, for what a script in the page can't do, such as taking the browser offline or
 * holding its requests.
 * <p>
 * Each call names the parameters it takes. Before it runs, the base checks that the step gives no others (an unknown
 * name is an error naming the call and the names it takes), and gives it the leg it runs in: the browser's driver,
 * the leg's Selenide, the browser's control and the base URL ({@link Context}). A call gets the browser's capability
 * interfaces only through {@link #require(Context, Class)}, which skips the test when the browser lacks one.
 * <p>
 * A call that returns a value sets the step's {@code variable}, if it names one. An {@link AssertionError} fails the step
 * under the automation's test strategy; an {@link IllegalArgumentException} (a parameter the call can't use) or any other
 * exception ends the interaction; a {@link org.opentest4j.TestAbortedException} skips the test.
 * <p>
 * Threading: the executor makes a new instance for each step; an instance is thread-confined.
 */
public abstract class AbstractSailCall implements SailCall {
	private final Set<String> parameterNames;

	/**
	 * Names the parameters the call takes.
	 *
	 * @param parameterNames	the names; none for a call that takes no parameters
	 */
	protected AbstractSailCall(@Nonnull String... parameterNames) {
		this.parameterNames = Set.of(parameterNames);
	}

	/**
	 * Checks the step's parameters against the names the call takes, then runs the call in the executor's browser.
	 *
	 * @param parameters	the step's parameters, with the tokens substituted
	 * @param executor	the executor running the automation: an {@link InterpretedWebDriverExecutor}
	 * @return the value for the step's variable, or null for none
	 * @throws IllegalArgumentException	if the step gives a parameter the call doesn't take, naming it and those it takes;
	 * 										or a parameter the call can't use
	 * @throws IllegalStateException	if the executor drives no browser
	 * @throws AssertionError	if what the call checks doesn't hold
	 * @throws Exception	if the call can't do what it says
	 */
	@Override
	public final @Nullable String call(@Nonnull Map<String, String> parameters, @Nonnull Executor executor) throws Exception {
		SailCallParameters checked = parameters(parameters);
		if (! (executor instanceof InterpretedWebDriverExecutor<?, ?> browser)) {
			throw new IllegalStateException(name() + " needs a browser, but runs under " + executor.getClass().getSimpleName());
		}
		return run(checked, new Context(browser));
	}

	/**
	 * Checks the parameters and runs the call in a leg given as it is: for the calls' unit tests.
	 *
	 * @param parameters	the step's parameters
	 * @param context	the leg
	 * @return the value for the step's variable, or null for none
	 * @throws Exception	as {@link #call(Map, Executor)} throws
	 */
	final @Nullable String call(@Nonnull Map<String, String> parameters, @Nonnull Context context) throws Exception {
		return run(parameters(parameters), context);
	}

	private @Nonnull SailCallParameters parameters(@Nonnull Map<String, String> parameters) {
		return new SailCallParameters(name(), parameters, parameterNames);
	}

	/**
	 * Does what the call says.
	 *
	 * @param parameters	the step's parameters, already checked against the names the call takes
	 * @param context	the leg it runs in
	 * @return the value for the step's {@code variable}, or null for none
	 * @throws IllegalArgumentException	if a parameter can't be used
	 * @throws AssertionError	if what the call checks doesn't hold
	 * @throws Exception	if the call can't do what it says
	 */
	protected abstract @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) throws Exception;

	/**
	 * Returns the call's name, for messages: its simple class name, as a script names it last.
	 *
	 * @return the name, such as {@code Offline}
	 */
	protected final @Nonnull String name() {
		return getClass().getSimpleName();
	}

	/**
	 * Returns one of the browser's capability interfaces, or skips the test when the browser lacks it, naming this call:
	 * the call can show nothing in this browser, so it hasn't failed.
	 *
	 * @param <T>	the interface's type
	 * @param context	the leg
	 * @param type	the capability's interface, such as {@code NetworkConditions.class}
	 * @return the interface, for the browser in the leg now
	 * @throws util.sail.browser.SailCapabilityUnavailableException	if the browser lacks the capability, which the
	 * 																	harness reports as skipped
	 */
	protected final @Nonnull <T> T require(@Nonnull Context context, @Nonnull Class<T> type) {
		return context.control().require(type, name());
	}

	/**
	 * Fails a check of what the browser's control records (the request log, the dialogs) when the record only began in
	 * this step: the control began watching as this step asked for it, so the record holds nothing from the steps
	 * before, and an empty record would pass a check that nothing happened.
	 *
	 * @param context	the leg, which knows when this step started
	 * @param since	when the record began
	 * @param what	what the record is, for the message, such as {@code request log}
	 * @throws IllegalStateException	if the record began in this step, telling the author to start watching earlier
	 */
	protected final void requireWatchingBefore(@Nonnull Context context, @Nonnull Instant since, @Nonnull String what) {
		if (beganInStep(context, since)) {
			throw new IllegalStateException(name() + " reads the " + what + ", which only began in this step, as the browser's control" +
												" began watching then: it holds nothing from the steps before. Put a call before the steps" +
												" to watch, such as RequestLog with clear=true, so watching begins there");
		}
	}

	/**
	 * Indicates whether a record of the browser's control began in the step that is running.
	 *
	 * @param context	the leg, which knows when this step started
	 * @param since	when the record began
	 * @return true if it began as this step ran, or later
	 */
	static boolean beganInStep(@Nonnull Context context, @Nonnull Instant since) {
		return ! since.isBefore(context.started);
	}

	/**
	 * The leg a call runs in: the browser and how it was started. The driver and the control are those of the browser
	 * running now, so they follow a call that starts the browser again.
	 * <p>
	 * Threading: thread-confined, as the executor is.
	 */
	public static final class Context {
		private final Supplier<WebDriver> driver;
		private final Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> selenide;
		private final Function<WebDriver, SailBrowserControl> controls;
		private final Supplier<String> runToken;
		/** When the step started, as a check compares when what it reads began. */
		private final Instant started = Instant.now();

		/**
		 * Makes the leg of an executor.
		 *
		 * @param executor	the executor running the automation
		 */
		Context(@Nonnull InterpretedWebDriverExecutor<?, ?> executor) {
			this(executor::getDriver, executor.getSelenide(), SailBrowserControl::of, executor::getRunToken);
		}

		/**
		 * Makes a leg from its parts, for unit tests.
		 *
		 * @param driver	gives the driver of the browser running now
		 * @param selenide	the leg's Selenide
		 * @param controls	gives a driver's control, as {@link SailBrowserControl#of(WebDriver)} does
		 * @param runToken	gives the run token
		 */
		Context(@Nonnull Supplier<WebDriver> driver,
				@Nonnull Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> selenide,
				@Nonnull Function<WebDriver, SailBrowserControl> controls,
				@Nonnull Supplier<String> runToken) {
			this.driver = driver;
			this.selenide = selenide;
			this.controls = controls;
			this.runToken = runToken;
		}

		/**
		 * Returns the driver of the browser running now. A call that changes what the page shows must be followed by a
		 * navigation step, as the executor's contexts don't follow it.
		 *
		 * @return the driver
		 */
		public @Nonnull WebDriver driver() {
			return driver.get();
		}

		/**
		 * Returns the leg's Selenide, which starts and stops the browser (see
		 * {@link InterpretedWebDriverExecutor#getSelenide()}) and knows how it was started.
		 *
		 * @return the Selenide
		 */
		public @Nonnull Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> selenide() {
			return selenide;
		}

		/**
		 * Returns the control of the browser running now. Get capability interfaces from it with
		 * {@link SailBrowserControl#require(Class, String)}, or with {@link AbstractSailCall#require(Context, Class)}.
		 *
		 * @return the control
		 */
		public @Nonnull SailBrowserControl control() {
			return controls.apply(driver());
		}

		/**
		 * Returns the base URL of the server the leg runs against.
		 *
		 * @return the URL, ending in {@code /}
		 * @throws IllegalStateException	if the browser has not started
		 */
		public @Nonnull String baseUrl() {
			return selenide.getBaseUrl();
		}

		/**
		 * Returns the run token, as {@code {run}} gives it.
		 *
		 * @return the token
		 */
		public @Nonnull String runToken() {
			return runToken.get();
		}

		/**
		 * Returns the leg's timeout for a wait on the browser ({@code sail.timeoutSeconds}): the one a call waits for
		 * unless a parameter gives another. It is a limit, not a delay.
		 *
		 * @return the timeout
		 */
		public @Nonnull Duration timeout() {
			return selenide.timeout();
		}
	}
}
