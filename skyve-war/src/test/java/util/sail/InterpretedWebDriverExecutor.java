package util.sail;

import java.beans.Introspector;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Supplier;

import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.NoSuchSessionException;
import org.openqa.selenium.NoSuchWindowException;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.ScriptTimeoutException;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriver.Timeouts;
import org.openqa.selenium.WebDriverException;
import org.opentest4j.TestAbortedException;
import org.skyve.CORE;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.metadata.view.NoOpViewVisitor;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.reference.ActionReference;
import org.skyve.impl.metadata.view.reference.ImplicitActionReference;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.sail.execution.WebDriverExecutor;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.sail.execution.SailCall;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.Procedure;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.Call;
import org.skyve.metadata.sail.language.step.CallParameter;
import org.skyve.metadata.sail.language.step.CaptureCell;
import org.skyve.metadata.sail.language.step.CaptureStep;
import org.skyve.metadata.sail.language.step.CaptureTitle;
import org.skyve.metadata.sail.language.step.CaptureValue;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.Execute;
import org.skyve.metadata.sail.language.step.ExecuteAsync;
import org.skyve.metadata.sail.language.step.Pause;
import org.skyve.metadata.sail.language.step.TestFailure;
import org.skyve.metadata.sail.language.step.TestGridRows;
import org.skyve.metadata.sail.language.step.TestMessage;
import org.skyve.metadata.sail.language.step.TestScript;
import org.skyve.metadata.sail.language.step.TestSuccess;
import org.skyve.metadata.sail.language.step.TestWidget;
import org.skyve.metadata.sail.language.step.WaitFor;
import org.skyve.metadata.sail.language.step.context.ClearContext;
import org.skyve.metadata.sail.language.step.context.PopContext;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.DataEnter.Match;
import org.skyve.metadata.sail.language.step.interaction.Upload;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridLink;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridClearFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter.FilterOperator;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSort;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridTool;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridTool.Tool;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionAutoComplete;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionPick;
import org.skyve.metadata.sail.language.step.interaction.navigation.CloseWindow;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateBack;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateCalendar;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMap;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateTree;
import org.skyve.metadata.sail.language.step.interaction.navigation.Refresh;
import org.skyve.metadata.sail.language.step.interaction.navigation.TestMenu;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.sail.language.step.interaction.session.Logout;
import org.skyve.metadata.sail.language.step.interaction.session.SwitchMode;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.WebAction;

import com.codeborne.selenide.ex.UIAssertionError;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Abstract base executor that interprets and executes web automation steps
 * using a generic {@link Selenide} implementation and automation context.
 * <p>
 * Implementations extend this class to bind specific automation contexts and
 * Selenide subclasses for UI testing across different web frameworks.
 * <p>
 * This class drives the automation itself and applies the automation's {@link TestStrategy}:
 * <ul>
 * <li>{@code Assert} (also used when no strategy is given) stops at the first failure and throws an {@link AssertionError}.</li>
 * <li>{@code Verify} records each failure and carries on. A failed assertion (a {@code testValue}, {@code testSuccess}
 * 		or {@code testFailure}, or the implicit success check after an action) carries on with the next step;
 * 		a step that throws carries on with the next interaction, as the rest of the interaction usually depends on it.
 * 		When the automation ends, an {@link AssertionError} lists every failure.</li>
 * <li>{@code None} makes no assertions. A step that throws still fails the automation.</li>
 * </ul>
 * The executor runs with {@link SailSettings} (see {@link #useSettings(SailSettings)}): {@value SailSettings#FAILURE_DIR}
 * (or an evidence request) names a directory to save a screenshot and the page source of each failure into, and
 * {@value SailSettings#TEST_STRATEGY} overrides the automation's test strategy (any case). Settings are resolved, with the
 * automation's own test strategy, when the automation starts: an invalid value fails before any step runs, and the
 * trace lists the settings that do not come from their defaults, with their sources. Tests that the override would
 * weaken (the admin suites, the strategy conformance test) refuse it: see
 * {@link SailSuite#refuseTestStrategyOverride(String, SailSettings)}.
 * <p>
 * <b>Views for the signed-in user.</b> Given {@link #useViewRoles(SailViewRoles)}, each {@code <login/>} sets the
 * persistence user that builds the views for the rest of the run: a user with the registered roles of the user
 * signing in, or a user with every role if that user is not registered. The user from before the first login is
 * restored when the automation ends.
 * <p>
 * <b>Renderer hooks.</b> This class implements what does not depend on the renderer: token substitution
 * ({@code {run}}, {@code {var:name}}), the variables, argument checks and the test strategy. It leaves these
 * hooks to the renderer executors. Each one throws {@link UnsupportedOperationException} until the renderer
 * overrides it, so a script that uses an unimplemented feature fails instead of passing.
 * <ul>
 * <li>{@link #filterListGrid(ListGridFilter, String, String, FilterOperator)} for {@code <listGridFilter/>};</li>
 * <li>{@link #clearListGridFilters(ListGridClearFilter)} for {@code <listGridClearFilter/>};</li>
 * <li>{@link #sortListGrid(ListGridSort, String, boolean)} for {@code <listGridSort/>};</li>
 * <li>{@link #readWidgetValue(CaptureValue)} for {@code <captureValue/>};</li>
 * <li>{@link #enterOption(DataEnter, String, Match)} for {@code <dataEnter match="code|label"/>}, called by the
 * 		renderer's {@code executeDataEnter};</li>
 * <li>{@link #pickLookupDescriptionRowContaining(LookupDescriptionPick, String)} for
 * 		{@code <lookupDescriptionPick rowContaining="…"/>}, called by the renderer's {@code executeLookupDescriptionPick};</li>
 * <li>{@link #autoCompleteLookupDescriptionAndPick(LookupDescriptionAutoComplete, String, String)} for
 * 		{@code <lookupDescriptionAutoComplete pick="…"/>}, called by the renderer's {@code executeLookupDescriptionAutoComplete};</li>
 * <li>{@link #clickLinkContaining(NavigateLink, String)} for {@code <navigateLink text="…"/>};</li>
 * <li>{@link #readGridCell(CaptureCell)} for {@code <captureCell/>};</li>
 * <li>{@link #readViewTitle(CaptureTitle)} for {@code <captureTitle/>};</li>
 * <li>{@link #clickDataGridLink(DataGridLink, String, boolean)} for {@code <dataGridLink/>};</li>
 * <li>{@link #clickLinkIntoNewWindow(NavigateLink, String)} for {@code <navigateLink text="…" window="new"/>};</li>
 * <li>{@link #pressListGridTool(ListGridTool, Tool)} for {@code <listGridTool/>}.</li>
 * </ul>
 * <b>Windows.</b> {@code <navigateLink window="new"/>} follows a link into the new window it opens (or opens its
 * {@code href} in a new tab), and the later steps run there; {@code <closeWindow/>} closes it and returns to its opener.
 * This class keeps the opener of each window it opened, and the core keeps their contexts
 * (see {@code ContextualExecutor.openWindowContexts()}). A window left open is closed when its interaction does not
 * complete, and when the automation ends, so the next interaction or automation starts in the window this one started in.
 * <p>
 * Every hook receives its text values with the tokens already substituted. Like any step, a hook throws when it
 * cannot do what the step says, and makes the implicit success check, {@link #checkSuccess(String)}, where the other
 * steps of its kind do. A hook that changes a list grid waits for the grid's fetch to end before it returns.
 *
 * @param <C> automation context type
 * @param <S> concrete Selenide implementation
 * @author simeonsolomou
 */
public abstract class InterpretedWebDriverExecutor<
	C extends AutomationContext<?, ?>,
	S extends Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?>> extends WebDriverExecutor<C> {

	/**
	 * System property naming a directory to save failure screenshots and page sources into.
	 * @deprecated	use {@link SailSettings#FAILURE_DIR}
	 */
	@Deprecated(since = "10")
	public static final String FAILURE_DIRECTORY_PROPERTY = SailSettings.FAILURE_DIR;
	/**
	 * System property that overrides the automation's test strategy (Assert, Verify or None), e.g. to list every failure.
	 * @deprecated	use {@link SailSettings#TEST_STRATEGY}
	 */
	@Deprecated(since = "10")
	public static final String TEST_STRATEGY_PROPERTY = SailSettings.TEST_STRATEGY;

	/** How often {@code <waitFor/>} and a timed {@code <testScript/>} run their script, in milliseconds: fixed, with no attribute. */
	private static final long SCRIPT_POLLING_MILLIS = 250L;

	protected S selenide;

	/** Failures recorded under the Verify strategy, in order. */
	private final List<String> failures = new ArrayList<>();
	/** What each failure under Verify threw, in order, for the report's causes. */
	private final List<SailFailure> thrown = new ArrayList<>();
	/** The interaction (or procedure) being executed, for failure messages. */
	private String procedureName;
	/** The 1-based number of the top-level step being executed, for failure messages. */
	private int stepNumber;
	/** The top-level step being executed. */
	private Step currentStep;
	/** The top-level step executed before the current one in the same procedure. */
	private Step previousStep;
	/** The top-level step to be executed after the current one in the same procedure. */
	private Step nextStep;
	/** The error shown by the last login, or null if it succeeded. */
	private String loginError;
	/**
	 * The refusal shown by the last {@code <upload testSuccess="false"/>} (on the upload page, which a user then closes),
	 * or null if it was not refused; see {@link #uploadRefused(String)}.
	 */
	private String uploadError;
	/** Counts failures, to name diagnostic files uniquely. */
	private int failureCount = 0;
	/** The roles to build the views with for each user that signs in, or null to keep the persistence user. */
	private SailViewRoles viewRoles;
	/** The persistence user when the first login of the run switched it, which the views are built from; or null. */
	private User viewTemplateUser;
	/** The variables given by {@link #useVariables(Map)}, as a code layer under the settings' variables; or null. */
	private SailSettings.Layer startingVariables;
	/** The settings given by {@link #useSettings(SailSettings)}, or null for the browser's. */
	private SailSettings settings;
	/** The settings of the running (or last) automation, with its own test strategy; or null before the first. */
	private SailSettings automationSettings;
	/** The step snapshots and video of the running automation, when the settings ask for them; or null. */
	private SailMedia media;
	/**
	 * The handles of the windows that opened the windows the automation is in, the most recent opener first, in step
	 * with the contexts the core sets aside for them; empty in the window the automation started in.
	 */
	private final Deque<String> openerWindows = new ArrayDeque<>(4);

	/**
	 * Runs the automation's before procedure, interactions and after procedure, applying the test strategy.
	 *
	 * @param automation	the automation to run
	 * @throws AssertionError	at the first failure under Assert, or at the end under Verify when anything failed,
	 * 							including when a step then skipped the test: a failure recorded first wins
	 * @throws TestAbortedException	when a step skips the test, such as a call that needs what the browser can't do,
	 * 									under every strategy, and no later step runs
	 */
	@Override
	public void executeAutomation(@Nonnull Automation automation) {
		// an invalid setting fails here, before any step runs
		SailSettings.Layer own = SailSettings.automation(null, automation.getTestStrategy());
		SailSettings resolved = (startingVariables == null) ? settings().with(own) : settings().with(startingVariables, own);
		automationSettings = resolved;
		List<String> described = resolved.describe();
		List<String> ignored = resolved.ignored();
		selenide.trace("SAIL settings: " + (described.isEmpty() ? "all defaults" : String.join("; ", described)) +
						(ignored.isEmpty() ? "" : "; ignored, as they are not SAIL settings: " + String.join(", ", ignored)));
		initialiseAutomation(automation);
		openerWindows.clear();
		for (Map.Entry<String, String> variable : resolved.variables().entrySet()) {
			String name = variable.getKey();
			// a run shared with earlier automations keeps what they captured
			if (getVariable(name) == null) {
				putVariable(name, variable.getValue());
			}
		}
		failures.clear();
		thrown.clear();
		media = SailMedia.start(selenide, automation, resolved);

		boolean passed = false;
		TestAbortedException skipped = null;
		try {
			Procedure before = automation.getBefore();
			if (before != null) {
				runSteps("Before Automation", before.getSteps(), true);
			}
			for (Interaction interaction : automation.getInteractions()) {
				executeInteraction(interaction);
			}
			Procedure after = automation.getAfter();
			if (after != null) {
				runSteps("After Automation", after.getSteps(), true);
			}
			passed = failures.isEmpty();
		}
		catch (TestAbortedException e) {
			// a failure recorded under Verify before the skip wins: the test fails with it, and says it was then skipped
			if (failures.isEmpty()) {
				throw e;
			}
			skipped = e;
		}
		finally {
			closeOpenWindows();
			restoreViewUser();
			if (media != null) {
				media.finish(passed);
				media = null;
			}
		}

		if (! failures.isEmpty()) {
			StringBuilder message = new StringBuilder(256);
			message.append("SAIL automation failed with ").append(failures.size()).append(" failure(s):");
			for (String failure : failures) {
				message.append("\n  - ").append(failure);
			}
			if (skipped != null) {
				message.append("\nThen a step skipped the test, so no later step ran: ").append(skipped.getMessage());
			}
			AssertionError error = new AssertionError(message.toString());
			// each step that threw is attached, with its causes and stack trace, as it would be under Assert
			for (SailFailure failure : thrown) {
				error.addSuppressed(failure);
			}
			if (skipped != null) {
				error.addSuppressed(skipped);
			}
			throw error;
		}
	}

	/**
	 * Runs the interaction's before procedure, steps and after procedure. Under Verify, the after procedure
	 * still runs when an earlier step throws. In a load run's virtual user ({@link SailLoad}), the interaction's
	 * duration and outcome go to the run's summary.
	 *
	 * @param interaction	the interaction to run
	 */
	@Override
	public void executeInteraction(@Nonnull Interaction interaction) {
		SailLoadProbe probe = SailLoadProbe.current();
		if (probe == null) {
			runInteraction(interaction);
			return;
		}
		int failuresBefore = failures.size();
		long started = probe.now();
		boolean returned = false;
		String failure = null;
		try {
			runInteraction(interaction);
			returned = true;
			// recorded under Verify: the first new failure
			failure = (failures.size() > failuresBefore) ? failures.get(failuresBefore) : null;
		}
		catch (SailFailure e) { // thrown under Assert
			failure = e.getMessage();
			throw e;
		}
		finally {
			probe.interaction(interaction.getName(), started, (! returned) || (failure != null), failure);
		}
	}

	/** Runs an interaction, as {@link #executeInteraction(Interaction)} describes. */
	private void runInteraction(@Nonnull Interaction interaction) {
		String name = interaction.getName();
		selenide.trace("Interaction: " + name);
		LOGGER.info("Execute Interaction {}", name);

		List<Step> steps = new ArrayList<>();
		Procedure before = interaction.getBefore();
		if (before != null) {
			steps.addAll(before.getSteps());
		}
		steps.addAll(interaction.getSteps());
		boolean completed = runSteps(name, steps, false);

		Procedure after = interaction.getAfter();
		if (after != null) {
			completed = runSteps(name + " (after)", after.getSteps(), false) && completed;
		}

		if (! completed) {
			// Start the next interaction in the window this one started in, with no stale contexts
			closeOpenWindows();
			clear();
		}
	}

	/**
	 * Runs steps in order.
	 *
	 * @param name	the procedure name for failure messages
	 * @param steps	the steps to run
	 * @param halt	true to stop with an error at a step that throws, whatever the strategy
	 * @return true if every step ran, false if a step threw and was recorded under Verify
	 */
	private boolean runSteps(@Nonnull String name, @Nonnull List<Step> steps, boolean halt) {
		procedureName = name;
		previousStep = null;
		for (int i = 0, l = steps.size(); i < l; i++) {
			Step step = steps.get(i);
			stepNumber = i + 1;
			currentStep = step;
			nextStep = (i + 1 < l) ? steps.get(i + 1) : null;
			int failuresBefore = failures.size();
			if (media != null) {
				media.stepStarted(name, stepNumber, step);
			}
			// what the page says about the window's OS focus as the step starts, for a failure's diagnosis; after a step
			// that opened, closed or switched windows there is no baseline, as those blur pages, and a blur can arrive late
			stepFocus = (windowChanges == windowChangesAtStepStart) ? selenide.focusProbe() : null;
			windowChangesAtStepStart = windowChanges;
			try {
				startingStep(step);
				step.execute(this);
				failOnServerErrorPage();
			}
			catch (SailFailure e) {
				// already described where it happened (Assert)
				if (media != null) {
					media.stepEnded(e.getMessage());
				}
				throw e;
			}
			catch (TestAbortedException e) {
				// skipped under every strategy: no later step runs
				if (media != null) {
					media.stepEnded("skipped: " + e.getMessage());
				}
				throw e;
			}
			catch (Throwable t) { // NOSONAR java:S1181 any step failure, including Selenide assertion errors, is a SAIL failure
				String failure = failureMessage(reason(t));
				if (media != null) {
					media.stepEnded(failure);
				}
				if ((! halt) && (TestStrategy.Verify == getTestStrategy())) {
					recordFailure(failure);
					SailFailure cause = new SailFailure(failure, t);
					thrown.add(cause);
					selenide.trace("    Cause: " + causes(t));
					return false;
				}
				diagnose();
				throw new SailFailure(failure, t);
			}
			if (media != null) {
				// a check that failed under Verify was recorded without throwing
				media.stepEnded((failures.size() > failuresBefore) ? failures.get(failures.size() - 1) : null);
			}
			previousStep = step;
		}
		return true;
	}

	/**
	 * Fails the step that has just run if the window shows Skyve's error page: a request of the step failed on the
	 * server, which sent the browser there. The page has no error message for the step's own success check to find,
	 * so the step, and later checks that a widget is not shown, would otherwise pass.
	 *
	 * @throws AssertionError	if the window shows the error page
	 */
	private void failOnServerErrorPage() {
		String errorPage = (selenide == null) ? null : selenide.serverErrorPage();
		if (errorPage != null) {
			throw new AssertionError(String.format("The server failed the request and sent the browser to its error page %s: " +
													"the server log has the failure under that reference",
													errorPage));
		}
	}

	/**
	 * Applies the test strategy to an assertion. Under Assert (or no strategy) a false condition throws;
	 * under Verify it is recorded and execution carries on; under None nothing is checked.
	 *
	 * @param condition	true if the assertion holds
	 * @param reason	describes the failure; called only when the condition is false
	 * @throws AssertionError	under Assert when the condition is false
	 */
	protected final void check(boolean condition, @Nonnull Supplier<String> reason) {
		if (condition || isNoneStrategy()) {
			return;
		}
		String failure = failureMessage(reason.get());
		if (TestStrategy.Verify == getTestStrategy()) {
			recordFailure(failure);
		}
		else {
			diagnose();
			throw new SailFailure(failure, null);
		}
	}

	/**
	 * Returns the automation's effective test strategy: the run's override ({@value SailSettings#TEST_STRATEGY} from
	 * {@code -D}, the environment, code or the properties file), else the automation's own.
	 *
	 * @return the test strategy, or null if neither gives one
	 */
	@Override
	public @Nullable TestStrategy getTestStrategy() {
		SailSettings resolved = automationSettings;
		if (resolved != null) {
			return resolved.testStrategy();
		}
		// no automation has started: steps run on their own
		SailSettings current = settings();
		return (current.runOverride(SailSettings.TEST_STRATEGY) != null) ? current.testStrategy() : super.getTestStrategy();
	}

	/**
	 * Returns the test strategy override of the settings outside code: {@value SailSettings#TEST_STRATEGY} from
	 * {@code -D}, the environment or the properties file (ignoring case).
	 *
	 * @return the strategy, or null if none of them sets it
	 * @throws IllegalArgumentException	if a setting is invalid, saying which values are valid
	 */
	public static @Nullable TestStrategy testStrategyOverride() {
		return testStrategyOverride(SailSettings.Environment.current());
	}

	/** Returns the test strategy override of the given sources; see {@link #testStrategyOverride()}. */
	static @Nullable TestStrategy testStrategyOverride(@Nonnull SailSettings.Environment environment) {
		SailSettings resolved = SailSettings.resolve(environment, List.of());
		return (resolved.runOverride(SailSettings.TEST_STRATEGY) == null) ? null : resolved.testStrategy();
	}

	/**
	 * Runs the automations with these settings, instead of those the browser was started with. The test bases and
	 * {@link SailWorkflow} give the settings of the test's code; the automation's own test strategy is added when it
	 * starts.
	 *
	 * <p>Side effects: replaces the settings given before.
	 *
	 * @param runSettings	the run's settings, resolved from the test's code and the browser configuration
	 */
	public final void useSettings(@Nonnull SailSettings runSettings) {
		settings = runSettings;
	}

	/**
	 * Returns the settings the automations run with: those given, else the browser's, else the settings outside code.
	 */
	private @Nonnull SailSettings settings() {
		SailSettings result = settings;
		if (result == null) {
			result = selenide.getSettings();
		}
		return (result == null) ? SailSettings.resolve() : result;
	}

	/**
	 * Indicates if the automation makes no assertions.
	 *
	 * @return true for the None test strategy
	 */
	protected final boolean isNoneStrategy() {
		return TestStrategy.None == getTestStrategy();
	}

	/**
	 * Called before each top-level step runs, for a renderer that keeps state across steps (such as the inline grid row
	 * that PrimeFaces' cell steps address). Does nothing here.
	 *
	 * @param step	the step about to run
	 */
	protected void startingStep(@Nonnull Step step) {
		// nothing by default
	}

	private void recordFailure(@Nonnull String failure) {
		failures.add(failure);
		selenide.trace("FAILED: " + failure);
		diagnose();
	}

	private @Nonnull String failureMessage(@Nonnull String reason) {
		StringBuilder result = new StringBuilder(128);
		result.append("Interaction \"").append(procedureName).append("\" step ").append(stepNumber);
		if (currentStep != null) {
			result.append(' ').append(describe(currentStep));
		}
		result.append(": ").append(reason);
		// a step that opened, closed or switched windows has no baseline either (see runSteps)
		Selenide.FocusProbe before = (windowChanges == windowChangesAtStepStart) ? stepFocus : null;
		String focus = focusNote(before, selenide.focusProbe(), focusLossHint());
		if (focus != null) {
			result.append("; ").append(focus);
		}
		String url = currentUrl();
		if (url != null) {
			result.append(" [at ").append(url).append(']');
		}
		return result.toString();
	}

	/** The page's focus probe as the current step started, or {@code null} if there was none. */
	private @Nullable Selenide.FocusProbe stepFocus;
	/**
	 * How many times the automation has opened, closed or switched windows (attempts included), so that the focus
	 * diagnostic does not take the blurs the harness itself causes for a loss of the OS focus.
	 */
	private long windowChanges = 0L;
	/** {@link #windowChanges} as the current step started. */
	private long windowChangesAtStepStart = 0L;

	/**
	 * Composes the diagnostic a failure message gets when the browser window lost the OS focus during the step, or did
	 * not have it when the step failed. A diagnostic only: it does not change whether the step passes.
	 *
	 * @param before	the probe as the step started, or {@code null}
	 * @param after		the probe when the step failed, or {@code null}
	 * @param hint		what losing the focus does in this renderer
	 * @return the note, or {@code null} when there is nothing to say: no probe, or the window kept the focus. A page
	 * 			that was replaced during the step (a new token, or a counter installed only now) counts as unknown, not
	 * 			as no loss; so does a step without a {@code before} probe, which the executor drops for a step that
	 * 			opened, closed or switched windows and for the step after one.
	 */
	static @Nullable String focusNote(@Nullable Selenide.FocusProbe before, @Nullable Selenide.FocusProbe after, @Nonnull String hint) {
		if (after == null) {
			return null;
		}
		long lost = 0L;
		if ((before != null) && (! after.installed()) && before.page().equals(after.page())) {
			lost = after.blurs() - before.blurs();
		}
		if (lost > 0L) {
			return String.format("the browser window lost the OS focus during this step (%d time%s); %s",
									Long.valueOf(lost), (lost == 1L) ? "" : "s", hint);
		}
		if (! after.hasFocus()) {
			return "the browser window did not have the OS focus when this step failed; " + hint;
		}
		return null;
	}

	/**
	 * Says what losing the OS focus does to the page, for the failure note of {@link #focusNote}.
	 *
	 * @return the hint
	 */
	@SuppressWarnings("static-method") // the renderer executors override it
	protected @Nonnull String focusLossHint() {
		return "another window (such as another headed browser) took it, which can interrupt typing or close an open drop-down";
	}

	private static @Nonnull String reason(@Nonnull Throwable t) {
		String message = t.getMessage();
		if (message == null) {
			message = "";
		}
		else {
			// Selenide messages run over several lines with screenshot details; the first line says what went wrong
			int newline = message.indexOf('\n');
			if (newline > 0) {
				message = message.substring(0, newline);
			}
			message = message.trim();
		}
		return t.getClass().getSimpleName() + (message.isEmpty() ? "" : ": " + message);
	}

	/**
	 * Describes a throwable's cause chain, and where it was thrown, for the trace.
	 *
	 * @param t	the throwable
	 * @return each cause's class and first message line, outermost first, then the top frame and the harness's frames
	 */
	private static @Nonnull String causes(@Nonnull Throwable t) {
		StringBuilder result = new StringBuilder(256);
		for (Throwable cause = t; cause != null; cause = (cause.getCause() == cause) ? null : cause.getCause()) {
			if (cause != t) {
				result.append(" <- caused by ");
			}
			result.append(reason(cause));
		}
		// where it was thrown, and the harness's frames that led there (the step's own lines)
		StackTraceElement[] frames = t.getStackTrace();
		int shown = 0;
		for (int i = 0; (i < frames.length) && (shown < 6); i++) {
			if ((i == 0) || frames[i].getClassName().startsWith("util.sail.")) {
				result.append("\n        at ").append(frames[i]);
				shown++;
			}
		}
		return result.toString();
	}

	private @Nullable String currentUrl() {
		try {
			WebDriver driver = selenide.driver;
			return (driver == null) ? null : driver.getCurrentUrl();
		}
		catch (@SuppressWarnings("unused") RuntimeException e) {
			return null;
		}
	}

	/**
	 * Saves a screenshot and the page source when the settings give a failure directory, named by the suite member (if
	 * any), the run token, the failure's number, the interaction and the step.
	 */
	private void diagnose() {
		SailSettings resolved = automationSettings;
		Path path = (resolved == null) ? settings().failureDir() : resolved.failureDir();
		WebDriver driver = selenide.driver;
		if ((path == null) || (driver == null)) {
			return;
		}
		try {
			Files.createDirectories(path);
			// the suite member and the run name the files, so that members and runs sharing the directory keep theirs
			String label = SailMedia.label();
			String prefix = String.format("%s%s-%02d-%s-step%02d",
												(label == null) ? "" : SailMedia.sanitise(label, 60) + '-',
												getRunToken(),
												Integer.valueOf(++failureCount),
												String.valueOf(procedureName).replaceAll("[^A-Za-z0-9]+", "_"),
												Integer.valueOf(stepNumber));
			Files.write(path.resolve(prefix + ".png"), ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES));
			Files.writeString(path.resolve(prefix + ".html"), driver.getPageSource(), StandardCharsets.UTF_8);
		}
		catch (IOException | RuntimeException e) {
			selenide.trace("Could not save failure diagnostics: " + e);
		}
	}

	/**
	 * Describes a step much as it appears in SAIL XML, e.g. {@code <dataEnter binding="text" value="x" />}.
	 *
	 * @param step	the step to describe
	 * @return the description
	 */
	public static @Nonnull String describe(@Nonnull Step step) {
		StringBuilder result = new StringBuilder(64);
		result.append('<').append(opCode(step));
		properties(step).forEach((property, value) -> result.append(' ').append(property).append("=\"").append(value).append('"'));
		return result.append(" />").toString();
	}

	/**
	 * Returns a step's op-code, as SAIL XML names it, e.g. {@code dataEnter}.
	 *
	 * @param step	the step
	 * @return the op-code
	 */
	static @Nonnull String opCode(@Nonnull Step step) {
		return Introspector.decapitalize(step.getClass().getSimpleName());
	}

	/**
	 * Returns a step's simple properties (those with a value that is not a collection), as text, with any password
	 * masked; {@link #describe(Step)} writes them as they are, and the SAIL media pages escape them as XML.
	 *
	 * @param step	the step
	 * @return the properties, by name
	 */
	static @Nonnull Map<String, String> properties(@Nonnull Step step) {
		Map<String, String> result = new LinkedHashMap<>();
		for (Method method : step.getClass().getMethods()) {
			String methodName = method.getName();
			if ((method.getParameterCount() > 0) ||
					Modifier.isStatic(method.getModifiers()) ||
					(method.getDeclaringClass() == Object.class)) {
				continue;
			}
			String property = null;
			if (methodName.startsWith("get") && (methodName.length() > 3)) {
				property = Introspector.decapitalize(methodName.substring(3));
			}
			else if (methodName.startsWith("is") && (methodName.length() > 2)) {
				property = Introspector.decapitalize(methodName.substring(2));
			}
			if (property == null) {
				continue;
			}
			try {
				Object value = method.invoke(step);
				if ((value != null) && (! (value instanceof Collection<?>))) {
					// never print a password into a failure message (they end up in test reports)
					boolean secret = property.toLowerCase().contains("password");
					result.put(property, secret ? "****" : value.toString());
				}
			}
			catch (@SuppressWarnings("unused") ReflectiveOperationException | RuntimeException e) {
				// not a simple property
			}
		}
		return result;
	}

	@Override
	public void executeClearContext(ClearContext clear) {
		clear();
	}

	@Override
	public void executePopContext(PopContext pop) {
		pop();
	}

	/**
	 * Signs in and waits for the outcome. A failed sign in fails the step, unless the next step is a
	 * {@code <testFailure />}, which then asserts that it failed. The tokens ({@code {run}}, {@code {var:name}}) of the
	 * customer, user and password are substituted. With {@code remember}, the Remember me box is set before the sign in is
	 * submitted.
	 *
	 * @param login	the login step
	 */
	@Override
	public void executeLogin(Login login) {
		String customer = login.getCustomer();
		if (customer != null) {
			customer = substituteTokens(customer);
		}
		String user = substituteTokens(login.getUser());
		String password = substituteTokens(login.getPassword());
		switchViewUser(login.getUser(), user);

		if (customer == null) {
			selenide.trace("Login as " + user);
		} else {
			selenide.trace("Login as " + customer + "/" + user);
		}
		selenide.login(customer, user, password, login.getRemember());
		loginError = selenide.getLoginError();

		if ((loginError != null) && (! (nextStep instanceof TestFailure)) && (! isNoneStrategy())) {
			throw new IllegalStateException(String.format("Login as %s failed: %s", user, loginError));
		}
		if (loginError == null) {
			useServerRoles(login.getUser(), user);
		}
	}

	/**
	 * After a sign in, builds the views for the rest of the run with the roles the server says the user has, when
	 * the user is not registered with {@link SailViewRoles} (a registration always wins). The server says so only when
	 * {@code sail.recording} is on; otherwise (no answer, or no roles) the views stay as
	 * {@link #switchViewUser(String, String)} built them (every role). A role the test's customer does not have is
	 * left out. Does nothing without {@link #useViewRoles}.
	 *
	 * <p>Side effects: may set the thread's persistence user; traces which roles the views are built with.
	 *
	 * @param scriptUserName	the login's user as written in the script
	 * @param userName	the login's user with the tokens substituted
	 */
	private void useServerRoles(@Nonnull String scriptUserName, @Nonnull String userName) {
		SailViewRoles roles = viewRoles;
		User template = viewTemplateUser;
		if ((roles == null) || (template == null) || (roles.roleNames(scriptUserName, userName) != null)) {
			return;
		}
		List<String> serverRoles = selenide.signedInRoleNames();
		if ((serverRoles == null) || serverRoles.isEmpty()) {
			selenide.trace(String.format("Views are built for %s with every role: the server does not say its roles (sail.recording off)", userName));
			return;
		}
		List<String> known = new ArrayList<>(serverRoles.size());
		for (String roleName : serverRoles) {
			try {
				SailUsers.userWithRoles(template, List.of(roleName));
				known.add(roleName);
			}
			catch (@SuppressWarnings("unused") IllegalArgumentException e) {
				selenide.trace(String.format("    %s's role %s is not a role of the test's customer: left out", userName, roleName));
			}
		}
		selenide.trace(String.format("Views are built for %s with the roles the server says it has: %s", userName, known));
		AbstractPersistence.get().setUser(SailUsers.userWithRoles(template, known));
	}

	/**
	 * Builds the views for the rest of the run with the roles registered for the user signing in, or with every role
	 * if the user is not registered: see {@link SailViewRoles}. Does nothing without {@link #useViewRoles(SailViewRoles)}.
	 *
	 * <p>Side effects: sets the thread's persistence user, and keeps the user it replaces the first time so that
	 * {@link #restoreViewUser()} can put it back.
	 *
	 * @param scriptUserName	the login's user as written in the script
	 * @param userName	the login's user with the tokens substituted
	 * @throws IllegalArgumentException	if a registered role is not a role of the customer
	 */
	private void switchViewUser(@Nonnull String scriptUserName, @Nonnull String userName) {
		SailViewRoles roles = viewRoles;
		if (roles == null) {
			return;
		}
		AbstractPersistence persistence = AbstractPersistence.get();
		User template = viewTemplateUser;
		if (template == null) {
			template = persistence.getUser();
			viewTemplateUser = template;
		}
		List<String> roleNames = roles.roleNames(scriptUserName, userName);
		UserImpl viewUser;
		if (roleNames == null) {
			viewUser = SailUsers.bootstrapLikeUser(template);
		}
		else {
			selenide.trace(String.format("Views are built for %s with the roles %s", userName, roleNames));
			viewUser = SailUsers.userWithRoles(template, roleNames);
		}
		persistence.setUser(viewUser);
	}

	/**
	 * Puts back the persistence user that the first login of the run replaced, if any.
	 */
	private void restoreViewUser() {
		User template = viewTemplateUser;
		if (template != null) {
			AbstractPersistence.get().setUser(template);
			viewTemplateUser = null;
		}
	}

	/**
	 * Builds the views of each user that signs in with the user's registered roles; see {@link SailViewRoles}.
	 * Call it before the automation runs.
	 *
	 * @param roles	the registered roles
	 */
	public final void useViewRoles(@Nonnull SailViewRoles roles) {
		viewRoles = roles;
	}

	/**
	 * Defines variables that every automation this executor runs starts with, for {@code {var:name}}, as if each had
	 * been captured. A recorded script signs in with {@code password="{var:password}"}, because the recorder never
	 * records a password: the test supplies it this way, and the script runs unedited. A {@code <captureValue/>} of the
	 * same name replaces the value for the rest of that automation, and a run shared with earlier automations keeps the
	 * value they captured. They are code-level settings below the test's own, so {@code -Dsail.var.name} (and the test's
	 * {@code settings().variable(...)}) beat them.
	 *
	 * <p>Side effects: replaces the variables given before.
	 *
	 * @param variables	the variables by name; copied
	 * @deprecated	use {@link SailSettings.Code#variable(String, String)} in the test's {@code settings()}
	 */
	@Deprecated(since = "10")
	public final void useVariables(@Nonnull Map<String, String> variables) {
		SailSettings.Code code = SailSettings.code();
		for (Map.Entry<String, String> variable : variables.entrySet()) {
			code.variable(variable.getKey(), variable.getValue());
		}
		startingVariables = code.layer(SailSettings.Level.CODE, "code: useVariables");
	}

	@Override
	public void executeLogout(Logout logout) {
		selenide.trace("Logout");
		selenide.logout();
	}

	@Override
	public void executeNavigateList(NavigateList list) {
		navigateList(list, WebAction.l, null);
	}

	/**
	 * Navigates to a tree of the documents in a query, document or model ({@code a=t}).
	 *
	 * @param tree	the step
	 */
	@Override
	public void executeNavigateTree(NavigateTree tree) {
		navigateList(tree, WebAction.t, null);
	}

	/**
	 * Navigates to a map of the documents in a query, document or model ({@code a=m}, with the geometry binding as {@code b}).
	 *
	 * @param map	the step
	 */
	@Override
	public void executeNavigateMap(NavigateMap map) {
		String geometryBinding = map.getGeometryBinding();
		if ((geometryBinding == null) && (map.getModelName() == null)) {
			throw new MetaDataException("<navigateMap /> needs a geometryBinding unless it names a model");
		}
		navigateList(map, WebAction.m, (geometryBinding == null) ? null : "&b=" + geometryBinding);
	}

	/**
	 * Navigates to a calendar of the documents in a query, document or model ({@code a=c}).
	 *
	 * @param calendar	the step
	 */
	@Override
	public void executeNavigateCalendar(NavigateCalendar calendar) {
		navigateList(calendar, WebAction.c, null);
	}

	/**
	 * Pushes a list context and opens the list-like page for the web action.
	 * The URL has the form the menus use: {@code ?a=<action>&m=<module>&q=<query or document>},
	 * or {@code ?a=<action>&m=<module>&d=<document>&q=<model>} for a model.
	 *
	 * @param list	the navigation step
	 * @param action	the web action ({@code l}, {@code t}, {@code m} or {@code c})
	 * @param suffix	more URL parameters, starting with {@code &}, or null
	 */
	private void navigateList(@Nonnull NavigateList list, @Nonnull WebAction action, @Nullable String suffix) {
		String moduleName = list.getModuleName();
		String documentName = list.getDocumentName();
		String queryName = list.getQueryName();
		String modelName = list.getModelName();

		PushListContext push = new PushListContext();
		push.setModuleName(moduleName);
		push.setDocumentName(documentName);
		push.setQueryName(queryName);
		push.setModelName(modelName);
		executePushListContext(push);

		String url = null;
		if (queryName != null) {
			selenide.trace(String.format("%s for query [%s.%s]", action, moduleName, queryName));
			url = String.format("?a=%s&m=%s&q=%s", action, moduleName, queryName);
		} else if (documentName != null) {
			if (modelName != null) {
				selenide.trace(String.format("%s for model [%s.%s.%s]", action, moduleName, documentName, modelName));
				url = String.format("?a=%s&m=%s&d=%s&q=%s", action, moduleName, documentName, modelName);
			} else {
				selenide.trace(String.format("%s for default query of [%s.%s]", action, moduleName, documentName));
				url = String.format("?a=%s&m=%s&q=%s", action, moduleName, documentName);
			}
		}
		if (url == null) {
			throw new MetaDataException("<" + Introspector.decapitalize(list.getClass().getSimpleName()) +
											" /> needs a query, or a document with an optional model");
		}
		if (suffix != null) {
			url += suffix;
		}
		selenide.get(url);
		failIfSignedOut("<" + Introspector.decapitalize(list.getClass().getSimpleName()) + " />");
	}

	/**
	 * Pushes an edit context and opens the edit view, of a new instance or of the {@code bizId}, whose tokens
	 * ({@code {run}}, {@code {var:name}}) are substituted, so that a script can open a record it captured the id of.
	 *
	 * @param edit	the step
	 */
	@Override
	public void executeNavigateEdit(NavigateEdit edit) {
		String moduleName = edit.getModuleName();
		String documentName = edit.getDocumentName();

		PushEditContext push = new PushEditContext();
		push.setModuleName(moduleName);
		push.setDocumentName(documentName);
		executePushEditContext(push);

		String bizId = edit.getBizId();
		if (bizId != null) {
			bizId = substituteTokens(bizId);
		}
		if (bizId == null) {
			selenide.trace(String.format("Edit new document [%s.%s] instance", moduleName, documentName));
			selenide.get(String.format("?a=e&m=%s&d=%s", moduleName, documentName));
		} else {
			selenide.trace(String.format("Edit document [%s.%s] instance with bizId %s", moduleName, documentName, bizId));
			selenide.get(String.format("?a=e&m=%s&d=%s&i=%s", moduleName, documentName, bizId));
		}
		failIfSignedOut("<navigateEdit />");
	}

	/**
	 * Opens the link: an absolute URL as is, otherwise relative to the base URL; or, with {@code text}, clicks the first
	 * displayed link on the page whose text contains it (see {@link #clickLinkContaining}). The {@code href}'s or
	 * {@code text}'s tokens ({@code {run}}, {@code {var:name}}) are substituted first.
	 * Any current context no longer has a document or view type, as the page is unknown.
	 * <p>
	 * With {@code window="new"}, the link is followed into the new window it opens, or the {@code href} is opened in a
	 * new tab, and the later steps run there: see {@link #followLinkIntoNewWindow(NavigateLink)}.
	 *
	 * @param link	the step
	 */
	@Override
	public void executeNavigateLink(NavigateLink link) {
		followLink(link);
		String href = link.getHref();
		if ((href == null) || (! isLoginHref(substituteTokens(href)))) {
			failIfSignedOut("<navigateLink />");
		}
	}

	/**
	 * Follows the link as {@link #executeNavigateLink(NavigateLink)} describes, without checking where it landed.
	 *
	 * @param link	the step
	 */
	private void followLink(@Nonnull NavigateLink link) {
		if (NavigateLink.Window.newWindow == link.getWindow()) {
			followLinkIntoNewWindow(link);
			return;
		}
		if (peek() != null) {
			super.executeNavigateLink(link);
		}
		String text = link.getText();
		if (text != null) {
			text = substituteTokens(text);
			selenide.trace("Follow the link containing '" + text + "'");
			clickLinkContaining(link, text);
			return;
		}
		String href = substituteTokens(Objects.requireNonNull(link.getHref(), "href"));
		selenide.trace("Navigate to link " + href);
		String relative = baseRelative(href);
		if (relative == null) {
			selenide.openUrl(href);
		}
		else {
			selenide.get(relative);
		}
	}

	/**
	 * Says whether an {@code href} is itself the sign in page, which a link may then land on: its last path segment,
	 * without the query or fragment, starts with {@code login} ({@code login}, {@code pages/login.jsp}).
	 *
	 * @param href	the href, with its tokens substituted
	 * @return true if the href is the sign in page
	 */
	private static boolean isLoginHref(@Nonnull String href) {
		String path = href.split("[?#]", 2)[0];
		return path.substring(path.lastIndexOf('/') + 1).toLowerCase(Locale.ROOT).startsWith("login");
	}

	/**
	 * Fails the step if the browser shows the sign in page, which is where a signed out browser lands: the step could not
	 * do what it says, so it stops the interaction under every test strategy, as a missing element does.
	 *
	 * @param step	the step, as the message names it
	 * @throws IllegalStateException	if the browser is on the sign in page
	 */
	protected final void failIfSignedOut(@Nonnull String step) {
		if (selenide.isLoginPage()) {
			throw new IllegalStateException(step + " landed on the login page: the browser is signed out " +
												"(session expired, or remember-me didn't sign it in)");
		}
	}

	/**
	 * Returns a link's {@code href} relative to the base URL, or null when it is an absolute URL (it has a scheme), which
	 * is opened as is.
	 *
	 * @param href	the href, with its tokens substituted
	 * @return the href without a leading {@code /}, or null for an absolute URL
	 */
	private static @Nullable String baseRelative(@Nonnull String href) {
		if (href.matches("^[a-zA-Z][a-zA-Z0-9+.-]*:.*")) {
			return null;
		}
		return href.startsWith("/") ? href.substring(1) : href;
	}

	/**
	 * Follows a link into the new tab or window it opens, with {@code text} (see {@link #clickLinkIntoNewWindow}), or opens
	 * the {@code href} in a new tab (an absolute URL as is, otherwise relative to the base URL), with its tokens
	 * substituted; waits for the new window's page, and makes the new window the one the later steps run in. The opener's
	 * contexts are set aside, untouched, until {@code <closeWindow/>}: the new window starts with none.
	 *
	 * @param link	the step, with {@code window="new"}
	 * @throws IllegalStateException	if no new window opens (the step fails, and the steps stay in this window)
	 */
	private void followLinkIntoNewWindow(@Nonnull NavigateLink link) {
		windowChanges++; // even if it fails: the browser may have opened, switched or closed a window
		String opener;
		String text = link.getText();
		if (text != null) {
			text = substituteTokens(text);
			selenide.trace("Follow the link containing '" + text + "' into a new window");
			opener = clickLinkIntoNewWindow(link, text);
		}
		else {
			String href = substituteTokens(Objects.requireNonNull(link.getHref(), "href"));
			selenide.trace("Open link " + href + " in a new window");
			String relative = baseRelative(href);
			opener = (relative == null) ? selenide.openUrlInNewWindow(href) : selenide.getInNewWindow(relative);
		}
		openerWindows.push(opener);
		openWindowContexts();
	}

	/**
	 * Closes the window that a {@code <navigateLink window="new"/>} opened and returns to its opener, then waits for the
	 * opener's page. The contexts pushed in the closed window are dropped and the opener's are put back as they were.
	 *
	 * @param close	the step
	 * @throws MetaDataException	if the automation is in the window it started in, so there is no window to close
	 */
	@Override
	public void executeCloseWindow(@Nonnull CloseWindow close) {
		String opener = openerWindows.peek();
		if (opener == null) {
			throw new MetaDataException("<closeWindow /> has no window to close: the automation is in the window it started in");
		}
		selenide.trace("Close the window and return to the window that opened it");
		windowChanges++;
		selenide.closeWindowAndReturnTo(opener);
		openerWindows.pop();
		closeWindowContexts();
	}

	/**
	 * Closes every window the automation opened and has not closed, innermost first, returning to the window it started
	 * in, and forgets their contexts. A window that cannot be closed is traced, not thrown, as this runs after a failure.
	 */
	private void closeOpenWindows() {
		while (! openerWindows.isEmpty()) {
			String opener = openerWindows.pop();
			selenide.trace("Close a window that was left open");
			windowChanges++;
			try {
				selenide.closeWindowAndReturnTo(opener);
			}
			catch (RuntimeException e) {
				selenide.trace("    could not close it: " + e);
			}
		}
		forgetOpenWindows();
	}

	/**
	 * Goes back to the previous page in the window's history, as the browser's Back button does, and waits for it (and
	 * for its renderer to settle). The executor cannot know which view that page shows, so the top context, if any, no
	 * longer has a document or view type, as after {@code <navigateLink/>}: a script pushes a context for the page.
	 *
	 * @param back	the step
	 * @throws IllegalStateException	if there is no earlier page in the window's history
	 */
	@Override
	public void executeNavigateBack(@Nonnull NavigateBack back) {
		selenide.trace("Navigate back");
		selenide.back();
		// not super.executeNavigateBack(): WebDriverExecutor's writes a generator comment
		C context = peek();
		if (context != null) {
			forgetView(context);
		}
	}

	/**
	 * Reloads the page, as the browser's Reload button does, and waits for it (and for its renderer to settle). The
	 * contexts are unchanged, as the URL is.
	 *
	 * @param refresh	the step
	 */
	@Override
	public void executeRefresh(@Nonnull Refresh refresh) {
		selenide.trace("Refresh the page");
		selenide.reload();
	}

	/**
	 * Presses a list grid's toolbar tool: see {@link #pressListGridTool}.
	 *
	 * @param tool	the step
	 */
	@Override
	public void executeListGridTool(@Nonnull ListGridTool tool) {
		Tool which = Objects.requireNonNull(tool.getTool(), "tool"); // checked by ListGridTool.execute()
		selenide.trace(String.format("Press the list grid's %s tool", which));
		pressListGridTool(tool, which);
	}

	/**
	 * Asserts that there are no error messages, or, straight after a login, that the login succeeded.
	 *
	 * @param success	the step
	 */
	@Override
	public void executeTestSuccess(TestSuccess success) {
		if (isNoneStrategy()) {
			return;
		}
		if (previousStep instanceof Login) {
			selenide.trace("Test login success");
			check(loginError == null, () -> "expected the login to succeed but it failed: " + loginError);
		}
		else if ((previousStep instanceof Upload) && (uploadError != null)) {
			selenide.trace("Test upload success");
			check(false, () -> "expected the upload to succeed but it was refused: " + uploadError);
		}
		else {
			selenide.trace("Test success");
			if (! (previousStep instanceof Logout)) {
				// the sign in page has no error messages, so a signed out browser would pass the check below
				check(! selenide.isLoginPage(), () -> "expected success, but the browser is on the login page (signed out)");
			}
			checkSuccess("expected no error messages");
		}
	}

	/**
	 * Asserts that there are no error messages after an action. This is the implicit check that actions
	 * make unless their {@code testSuccess} attribute is false.
	 *
	 * @param what	describes the assertion for the failure message
	 */
	protected void checkSuccess(@Nonnull String what) {
		if (isNoneStrategy()) {
			return;
		}
		check(selenide.verifySuccess(), () -> what + " but found: " + selenide.describeMessages());
	}

	/**
	 * Asserts that there is an error message, optionally containing the step's message, or, straight after a login,
	 * that the login failed. Straight after an upload that was refused on its upload page, the refusal counts as an
	 * error message, as well as the errors the view shows.
	 *
	 * @param failure	the step
	 */
	@Override
	public void executeTestFailure(TestFailure failure) {
		if (isNoneStrategy()) {
			return;
		}
		String message = failure.getMessage();
		if (message != null) {
			message = substituteTokens(message);
		}
		final String expected = message;
		if (previousStep instanceof Login) {
			selenide.trace("Test login failure" + ((expected == null) ? "" : " with message '" + expected + "'"));
			check((loginError != null) && ((expected == null) || loginError.contains(expected)),
					() -> (loginError == null) ?
							"expected the login to fail but it succeeded" :
							"expected the login error to contain '" + expected + "' but it was: " + loginError);
		}
		else if (expected == null) {
			selenide.trace("Test failure");
			String refusal = refusedUpload();
			check((refusal != null) || selenide.verifyFailure(),
					() -> "expected an error message but found: " + selenide.describeMessages());
		}
		else {
			selenide.trace(String.format("Test failure with message '%s'", message));
			String refusal = refusedUpload();
			check(((refusal != null) && refusal.contains(expected)) || selenide.verifyFailure(expected),
					() -> "expected an error message containing '" + expected + "' but found: " +
							((refusal == null) ? "" : "(the upload page said: " + refusal + ") ") + selenide.describeMessages());
		}
	}

	/**
	 * Writes the comment to the trace, with its tokens ({@code {run}}, {@code {var:name}}) substituted.
	 *
	 * @param comment	the step
	 */
	@Override
	public void executeComment(Comment comment) {
		String text = comment.getComment();
		selenide.trace((text == null) ? "" : substituteTokens(text));
	}

	/**
	 * Runs the step's script in the page (see {@link #runScript(String, boolean, boolean, Duration)}), and keeps its
	 * result, as text, in the step's variable if it names one. It runs under every test strategy, as it is not an
	 * assertion.
	 *
	 * @param execute	the step
	 * @throws IllegalStateException	if the step names a variable and the script gives {@code null} or {@code undefined}
	 */
	@Override
	public void executeExecute(@Nonnull Execute execute) {
		String script = Objects.requireNonNull(execute.getScript(), "<execute /> has no script");
		selenide.trace("Execute " + script.split("\\R", 2)[0]);
		keepResult("execute", execute.getVariable(), runScript(script, false, false, selenide.timeout()));
	}

	@Override
	public void executePause(Pause pause) {
		selenide.pause(pause.getMillis());
	}

	/**
	 * Runs the step's script in the page as an {@code async} function and waits for its promise to settle (see
	 * {@link #runScript(String, boolean, boolean, Duration)}), within the step's timeout, by default the settings'.
	 * Keeps the resolved value, as text, in the step's variable if it names one. It runs under every test strategy, as
	 * it is not an assertion.
	 *
	 * @param execute	the step
	 * @throws org.openqa.selenium.JavascriptException	if the promise rejects
	 * @throws IllegalStateException	if the promise does not settle in time, or the step names a variable and the value is
	 * 									{@code null} or {@code undefined}
	 */
	@Override
	public void executeExecuteAsync(@Nonnull ExecuteAsync execute) {
		String script = Objects.requireNonNull(execute.getScript(), "<executeAsync /> has no script");
		Integer timeout = execute.getTimeoutMillis();
		Duration limit = (timeout == null) ? selenide.timeout() : Duration.ofMillis(timeout.longValue());
		selenide.trace("Execute async " + script.split("\\R", 2)[0]);
		Object result;
		try {
			result = runScript(script, true, false, limit);
		}
		catch (ScriptTimeoutException e) {
			throw new IllegalStateException(String.format("<executeAsync /> did not settle within %d ms", Long.valueOf(limit.toMillis())), e);
		}
		keepResult("executeAsync", execute.getVariable(), result);
	}

	/**
	 * Runs the step's script in the page as an {@code async} function every {@value #SCRIPT_POLLING_MILLIS} ms until it
	 * gives a truthy value, within the step's timeout, by default the settings'. A run that throws, rejects or times out
	 * counts as false. This is a wait, not an assertion, so it runs under every test strategy.
	 *
	 * @param wait	the step
	 * @throws IllegalStateException	if the script does not hold in time; the message gives the timeout, the script's
	 * 									first line and the last error a run threw
	 */
	@Override
	public void executeWaitFor(@Nonnull WaitFor wait) {
		String script = Objects.requireNonNull(wait.getScript(), "<waitFor /> has no script");
		Integer timeout = wait.getTimeoutMillis();
		Duration limit = (timeout == null) ? selenide.timeout() : Duration.ofMillis(timeout.longValue());
		String line = script.split("\\R", 2)[0];
		selenide.trace("Wait for " + line);
		String[] lastError = new String[1];
		try {
			// the author's condition, polled at the fixed interval: the one wait mechanism, bounded by the step's timeout
			selenide.waitFor(limit, Duration.ofMillis(SCRIPT_POLLING_MILLIS), holds(script, limit, lastError));
		}
		catch (TimeoutException e) {
			throw new IllegalStateException(String.format("<waitFor /> did not hold within %d ms: %s%s",
															Long.valueOf(limit.toMillis()),
															line,
															(lastError[0] == null) ? "" : "; the last poll threw: " + lastError[0]),
												e);
		}
	}

	/**
	 * Asserts, under the test strategy, that the step's script gives a truthy value when run in the page as an
	 * {@code async} function: once, or with a timeout every {@value #SCRIPT_POLLING_MILLIS} ms until it does. A run that
	 * throws, rejects or times out counts as false. The failure's reason is the step's message, with the tokens
	 * substituted, or the script's first line, and then the last error a run threw. Skipped under None.
	 *
	 * @param test	the step
	 */
	@Override
	public void executeTestScript(@Nonnull TestScript test) {
		if (isNoneStrategy()) {
			return;
		}
		String script = Objects.requireNonNull(test.getScript(), "<testScript /> has no script");
		Integer timeout = test.getTimeoutMillis();
		Duration limit = (timeout == null) ? selenide.timeout() : Duration.ofMillis(timeout.longValue());
		String line = script.split("\\R", 2)[0];
		selenide.trace("Test script " + line);
		String[] lastError = new String[1];
		Function<WebDriver, Boolean> holds = holds(script, limit, lastError);
		boolean held;
		if (timeout == null) {
			held = (holds.apply(selenide.driver) != null);
		}
		else {
			try {
				// the author's condition, polled at the fixed interval: the one wait mechanism, bounded by the step's timeout
				selenide.waitFor(limit, Duration.ofMillis(SCRIPT_POLLING_MILLIS), holds);
				held = true;
			}
			catch (@SuppressWarnings("unused") TimeoutException e) {
				held = false;
			}
		}
		String message = test.getMessage();
		check(held, () -> ((message == null) ? "<testScript /> did not hold: " + line : substituteTokens(message)) +
							((lastError[0] == null) ? "" : "; it threw: " + lastError[0]));
	}

	/**
	 * Runs the step's {@link SailCall}: a new instance of the named class, from the thread context class loader, given
	 * the parameters with the tokens substituted in their values and this executor. Keeps the string it returns in the
	 * step's variable if it names one. An {@link AssertionError} from the call is a failed assertion, under the test
	 * strategy; a {@link TestAbortedException} skips the test, as it is; any other exception fails the step.
	 *
	 * <p>Complexity: O(p) in the number of parameters, plus the call.
	 *
	 * @param call	the step
	 * @throws MetaDataException	if the class cannot be loaded, is not a {@link SailCall}, or cannot be constructed with a
	 * 								public no-argument constructor
	 * @throws IllegalStateException	if the call throws anything but an {@link AssertionError} or a
	 * 									{@link TestAbortedException}, or the step names a variable and the call returns
	 * 									{@code null}
	 * @throws TestAbortedException	what the call threw to skip the test, such as a
	 * 									{@code util.sail.browser.SailCapabilityUnavailableException}
	 */
	@Override
	public void executeCall(@Nonnull Call call) {
		String className = call.getClassName();
		List<CallParameter> parameters = call.getParameters();
		Map<String, String> values = new HashMap<>(parameters.size() * 4 / 3 + 1);
		List<String> names = new ArrayList<>(parameters.size());
		for (CallParameter parameter : parameters) {
			String name = parameter.getName();
			values.put(name, substituteTokens(parameter.getValue()));
			names.add(name);
		}
		selenide.trace(String.format("Call %s with the parameters %s", className, names));

		Class<?> type;
		try {
			// not initialised yet: a class that is not a SailCall never runs its static initialiser
			type = Class.forName(className, false, Thread.currentThread().getContextClassLoader());
		}
		catch (ClassNotFoundException | LinkageError e) {
			throw new MetaDataException(String.format("<call className=\"%s\" /> names a class that cannot be loaded: %s", className, e), e);
		}
		if (! SailCall.class.isAssignableFrom(type)) {
			throw new MetaDataException(String.format("<call className=\"%s\" /> names a class that is not a %s", className, SailCall.class.getName()));
		}
		SailCall handler;
		try {
			handler = (SailCall) type.getConstructor().newInstance();
		}
		catch (InvocationTargetException e) {
			throw new MetaDataException(String.format("<call className=\"%s\" /> names a class whose constructor threw %s", className, e.getCause()), e);
		}
		catch (ExceptionInInitializerError e) {
			throw new MetaDataException(String.format("<call className=\"%s\" /> names a class whose static initialiser threw %s", className, e.getCause()), e);
		}
		catch (ReflectiveOperationException | LinkageError e) { // NoClassDefFoundError: its initialiser failed before
			throw new MetaDataException(String.format("<call className=\"%s\" /> names a class that cannot be constructed with a public no-argument constructor: %s",
														className, e),
										e);
		}

		String result;
		try {
			result = handler.call(values, this);
		}
		catch (SailFailure | TestAbortedException e) {
			// a step that the call ran through this executor failed, and said so; or the browser lacks what the call
			// needs, so the test is skipped, not failed
			throw e;
		}
		catch (UIAssertionError e) {
			// a Selenide wait or check failed: a step that throws, as it is for a gesture
			throw new IllegalStateException(String.format("<call className=\"%s\" /> threw %s", className, reason(e)), e);
		}
		catch (AssertionError e) {
			// the failure message has no stack trace: the trace shows where the call's assertion failed
			selenide.trace("    Cause: " + causes(e));
			String message = e.getMessage();
			check(false, () -> String.format("<call className=\"%s\" /> failed: %s", className, (message == null) ? e.toString() : message));
			return;
		}
		catch (Exception e) {
			if (e instanceof InterruptedException) {
				Thread.currentThread().interrupt();
			}
			throw new IllegalStateException(String.format("<call className=\"%s\" /> threw %s", className, reason(e)), e);
		}
		keepResult("call className=\"" + className + '"', call.getVariable(), result);
	}

	/**
	 * Keeps a script's or call's result, as text, in the step's variable, if the step names one.
	 *
	 * <p>Side effects: sets the variable, and says so in the trace.
	 *
	 * @param step	the step's element, and any attributes that tell it apart, for the message: {@code execute}
	 * @param variable	the step's variable, or {@code null} to keep nothing
	 * @param result	the result: kept as {@link String#valueOf(Object)}
	 * @throws IllegalStateException	if the step names a variable and the result is {@code null} (or JavaScript's
	 * 									{@code undefined})
	 */
	private void keepResult(@Nonnull String step, @Nullable String variable, @Nullable Object result) {
		if (variable == null) {
			return;
		}
		if (result == null) {
			throw new IllegalStateException(String.format("<%s variable=\"%s\" /> gave no value to keep in {var:%s}", step, variable, variable));
		}
		String value = String.valueOf(result);
		putVariable(variable, value);
		selenide.trace(String.format("Keep '%s' in {var:%s}", value, variable));
	}

	/**
	 * Makes the condition that {@code <waitFor/>} and {@code <testScript/>} poll: one run of the script (see
	 * {@link #runScript(String, boolean, boolean, Duration)}), true when it gives a truthy value. A run that throws a
	 * script error, times out or finds the page between documents counts as false, and is kept as the last error.
	 *
	 * @param script	the step's script
	 * @param limit	the script timeout of each run
	 * @param lastError	receives, at index 0, the reason the last run that threw gave
	 * @return the condition: {@link Boolean#TRUE}, or {@code null} for false
	 * @throws IllegalStateException	from the condition, if the browser's window or session is gone (a wait ignores a
	 * 									{@code NotFoundException}, which {@code NoSuchWindowException} is, so it is wrapped)
	 */
	private @Nonnull Function<WebDriver, Boolean> holds(@Nonnull String script, @Nonnull Duration limit, @Nonnull String[] lastError) {
		return d -> {
			try {
				return Boolean.TRUE.equals(runScript(script, true, true, limit)) ? Boolean.TRUE : null;
			}
			catch (NoSuchSessionException | NoSuchWindowException e) {
				throw new IllegalStateException("The browser's window or session is gone: " + reason(e), e);
			}
			catch (WebDriverException e) { // a script error, a script timeout, or the page between documents
				lastError[0] = reason(e);
				return null;
			}
		};
	}

	/**
	 * Returns the browser this executor drives, for a {@link SailCall} that needs it, such as for DevTools or another
	 * window. A call that changes what the page shows must be followed by a navigation or context step, as the
	 * contexts do not follow it.
	 *
	 * @return the driver of the browser running the automation
	 */
	public final @Nonnull WebDriver getDriver() {
		return selenide.driver;
	}

	/**
	 * Returns the Selenide that drives this leg's browser, for a {@link SailCall}. Restarting the browser through it,
	 * with {@code stopBrowser(); startBrowser(configuration)} at the start of a leg, is the supported way to change
	 * devices from a step: the Selenide remembers what it started with, so the next leg compares against, reuses or
	 * stops the restarted browser (see {@link SailWorkflow}). The executor's page contexts do not follow a restart, so
	 * a navigation or sign-in step must come next. Stopping the browser without starting it again is not supported.
	 *
	 * @return the Selenide of the browser running the automation
	 */
	public final @Nonnull S getSelenide() {
		return selenide;
	}

	/**
	 * Runs a script step's body in the page, with one argument, {@code sail}: {@code {run: "<run token>", vars: {...}}},
	 * a copy of the run token and the variables. The body is run as
	 * {@code return (function (sail) { BODY }).apply(this, arguments);}, or with {@code async function}, so
	 * {@code arguments[0]} is {@code sail} too, {@code return} gives the result, and WebDriver waits for a returned
	 * promise. The body is not token-substituted. With {@code truthy}, the result is JavaScript's {@code !!} of the
	 * async function's resolved value.
	 *
	 * <p>Side effects: sets the browser's script timeout to {@code limit} for the run, then restores the one before.
	 *
	 * @param body	the step's JavaScript
	 * @param async	true to run the body as an {@code async} function
	 * @param truthy	true to give whether the resolved value is truthy, as a {@link Boolean}; the body then runs as an
	 * 					{@code async} function whatever {@code async} says
	 * @param limit	the script timeout: how long the browser waits for the script, or its promise, to settle
	 * @return the result as WebDriver gives it ({@code null} for {@code null} or {@code undefined})
	 * @throws org.openqa.selenium.JavascriptException	if the script throws, or its promise rejects
	 * @throws ScriptTimeoutException	if the script does not settle within the limit
	 */
	private @Nullable Object runScript(@Nonnull String body, boolean async, boolean truthy, @Nonnull Duration limit) {
		Map<String, Object> sail = Map.of("run", getRunToken(), "vars", new HashMap<>(getRun().variables()));
		String script = "return (" + ((async || truthy) ? "async function" : "function") + " (sail) { " + body + "\n}).apply(this, arguments)" +
							(truthy ? ".then(function (result) { return !! result; });" : ";");
		WebDriver driver = selenide.driver;
		Timeouts timeouts = driver.manage().timeouts();
		Duration before = timeouts.getScriptTimeout();
		timeouts.scriptTimeout(limit);
		try {
			return ((JavascriptExecutor) driver).executeScript(script, sail);
		}
		finally {
			timeouts.scriptTimeout(before);
		}
	}

	/**
	 * Builds the exception thrown by a step that this renderer does not implement yet.
	 *
	 * @param element the SAIL element, such as {@code <upload/>}
	 * @return the exception to throw
	 */
	protected final @Nonnull UnsupportedOperationException notImplementedYet(@Nonnull String element) {
		return new UnsupportedOperationException(element + " is not implemented yet for " + getClass().getSimpleName());
	}

	/**
	 * Throws until a renderer implements {@code <upload/>}.
	 *
	 * @param upload the step
	 * @throws UnsupportedOperationException always
	 */
	@Override
	public void executeUpload(@Nonnull Upload upload) {
		throw notImplementedYet("<upload/>");
	}

	/**
	 * Throws until a renderer implements {@code <testMessage/>}.
	 *
	 * @param test the step
	 * @throws UnsupportedOperationException always
	 */
	@Override
	public void executeTestMessage(@Nonnull TestMessage test) {
		throw notImplementedYet("<testMessage/>");
	}

	/**
	 * Throws until a renderer implements {@code <testGridRows/>}.
	 *
	 * @param test the step
	 * @throws UnsupportedOperationException always
	 */
	@Override
	public void executeTestGridRows(@Nonnull TestGridRows test) {
		throw notImplementedYet("<testGridRows/>");
	}

	/**
	 * Throws until a renderer implements {@code <testWidget/>}.
	 *
	 * @param test the step
	 * @throws UnsupportedOperationException always
	 */
	@Override
	public void executeTestWidget(@Nonnull TestWidget test) {
		throw notImplementedYet("<testWidget/>");
	}

	/**
	 * Throws until a renderer implements {@code <navigateMenu/>}.
	 *
	 * @param menu the step
	 * @throws UnsupportedOperationException always
	 */
	@Override
	public void executeNavigateMenu(@Nonnull NavigateMenu menu) {
		throw notImplementedYet("<navigateMenu/>");
	}

	/**
	 * Throws until a renderer implements {@code <testMenu/>}.
	 *
	 * @param test the step
	 * @throws UnsupportedOperationException always
	 */
	@Override
	public void executeTestMenu(@Nonnull TestMenu test) {
		throw notImplementedYet("<testMenu/>");
	}

	/**
	 * Throws until a renderer implements {@code <switchMode/>}.
	 *
	 * @param switchMode the step
	 * @throws UnsupportedOperationException always
	 */
	@Override
	public void executeSwitchMode(@Nonnull SwitchMode switchMode) {
		throw notImplementedYet("<switchMode/>");
	}

	/**
	 * Filters a column of a list grid, or clears the column's filter: see {@link #filterListGrid}.
	 * Checks first that the column is a filterable column of the list grid's query, when it has one.
	 *
	 * @param filter the step
	 */
	@Override
	public void executeListGridFilter(@Nonnull ListGridFilter filter) {
		String column = filter.getColumn();
		checkListGridColumn(filter, filter.getModuleName(), filter.getDocumentName(), filter.getQueryName(), filter.getModelName(), column, Boolean.TRUE);
		String value = filter.getValue();
		if (value != null) {
			value = substituteTokens(value);
		}
		FilterOperator operator = filter.getEffectiveOperator();
		if (value == null) {
			selenide.trace(String.format("Clear the filter of column [%s]", column));
		}
		else {
			selenide.trace(String.format("Filter column [%s] %s '%s'", column, operator, value));
		}
		filterListGrid(filter, column, value, operator);
	}

	/**
	 * Clears every column filter of a list grid: see {@link #clearListGridFilters}.
	 *
	 * @param clear the step
	 */
	@Override
	public void executeListGridClearFilter(@Nonnull ListGridClearFilter clear) {
		selenide.trace("Clear the list grid's filters");
		clearListGridFilters(clear);
	}

	/**
	 * Sorts a list grid by a column: see {@link #sortListGrid}.
	 * Checks first that the column is a sortable column of the list grid's query, when it has one.
	 *
	 * @param sort the step
	 */
	@Override
	public void executeListGridSort(@Nonnull ListGridSort sort) {
		String column = sort.getColumn();
		checkListGridColumn(sort, sort.getModuleName(), sort.getDocumentName(), sort.getQueryName(), sort.getModelName(), column, Boolean.FALSE);
		boolean descending = Boolean.TRUE.equals(sort.getDescending());
		selenide.trace(String.format("Sort by column [%s] %s", column, descending ? "descending" : "ascending"));
		sortListGrid(sort, column, descending);
	}

	/**
	 * Reads the value a widget shows (see {@link #readWidgetValue}) into the step's variable, trimmed.
	 * A value is captured under every test strategy, as it is not an assertion.
	 *
	 * @param capture the step
	 * @throws IllegalStateException if the widget shows no value: a blank variable would match anything
	 */
	@Override
	public void executeCaptureValue(@Nonnull CaptureValue capture) {
		keep(capture, "[" + capture.getBinding() + "]", readWidgetValue(capture));
	}

	/**
	 * Reads the text a grid cell shows (see {@link #readGridCell}) into the step's variable, as {@code <captureValue/>}
	 * does. For a list grid, the column is checked first to be a column of the grid's query, when it has one.
	 *
	 * @param capture the step
	 * @throws IllegalStateException if the cell shows no value, or the pattern does not match it
	 */
	@Override
	public void executeCaptureCell(@Nonnull CaptureCell capture) {
		String column = capture.getColumn();
		if (capture.getBinding() == null) {
			checkListGridColumn(capture, capture.getModuleName(), capture.getDocumentName(), capture.getQueryName(), capture.getModelName(), column, null);
		}
		keep(capture, "column [" + column + "] of " + describeRow(capture), readGridCell(capture));
	}

	/**
	 * Reads the title the current view shows (see {@link #readViewTitle}) into the step's variable, as
	 * {@code <captureValue/>} does.
	 *
	 * @param capture the step
	 * @throws IllegalStateException if the view shows no title, or the pattern does not match it
	 */
	@Override
	public void executeCaptureTitle(@Nonnull CaptureTitle capture) {
		keep(capture, "the view's title", readViewTitle(capture));
	}

	/**
	 * Keeps a capture step's value in its variable: the text read, trimmed, or the part of it that the step's pattern
	 * picks (see {@link CaptureStep#extract(String, String)}), trimmed. A value is captured under every test strategy,
	 * as it is not an assertion.
	 *
	 * @param capture	the step
	 * @param what	what was read, for the trace and the failure, such as {@code [binding]}; the failure starts with it
	 * @param text	the text read, or null if nothing is shown
	 * @throws IllegalStateException if the text is blank, or the pattern does not match it (a blank variable would match
	 * 									anything)
	 */
	private void keep(@Nonnull CaptureStep capture, @Nonnull String what, @Nullable String text) {
		String variable = capture.getVariable();
		String read = (text == null) ? "" : text.trim();
		if (read.isEmpty()) {
			throw new IllegalStateException(String.format("%s shows no value to keep in {var:%s}", what, variable));
		}
		String value = read;
		String pattern = capture.getPattern();
		if (pattern != null) {
			String extracted = CaptureStep.extract(pattern, read);
			value = (extracted == null) ? "" : extracted.trim();
			if (value.isEmpty()) {
				throw new IllegalStateException(String.format("%s shows '%s', in which the pattern %s finds no value to keep in {var:%s}",
																what, read, pattern, variable));
			}
		}
		putVariable(variable, value);
		selenide.trace(String.format("Keep '%s' from %s in {var:%s}", value, what, variable));
	}

	/**
	 * Describes a capture cell step's row, for the trace and failures.
	 *
	 * @param capture	the step
	 * @return {@code row n}, or {@code the row containing 'text'} with the tokens substituted
	 */
	private @Nonnull String describeRow(@Nonnull CaptureCell capture) {
		Integer row = capture.getRow();
		if (row != null) {
			return "row " + row;
		}
		return "the row containing '" + substituteTokens(Objects.requireNonNull(capture.getRowContaining(), "rowContaining")) + "'";
	}

	/**
	 * Clicks a link in a data grid's row: see {@link #clickDataGridLink}. The link's text has its tokens substituted. A
	 * link that runs an action is pressed as an action's button is: its confirmation and questions are answered as the
	 * step's {@code confirm} says, and unless it was declined or {@code testSuccess} is false, the step checks that no
	 * error is shown; the view and its context stay. Any other link is followed as {@code <navigateLink/>} follows one:
	 * the top context loses its document and view type, so a script pushes a context for the page it reaches.
	 *
	 * @param link	the step
	 */
	@Override
	public void executeDataGridLink(@Nonnull DataGridLink link) {
		String text = substituteTokens(link.getText());
		boolean action = dataGridLinkRunsAction(link.getBinding(), text);
		selenide.trace(String.format("Click the link containing '%s' in a row of data grid [%s]%s",
										text, link.getBinding(), action ? "" : " (it leads to another page)"));
		boolean declined = clickDataGridLink(link, text, action);
		if (! action) {
			C context = peek();
			if (context != null) {
				context.setDocumentName(null);
				context.setViewType(null);
				context.setList(null);
			}
			return;
		}
		if (declined) {
			selenide.trace("    Declined: the action did not run");
		}
		else if (! Boolean.FALSE.equals(link.getTestSuccess())) {
			checkSuccess(String.format("expected no error messages after the link '%s' in data grid [%s]", text, link.getBinding()));
		}
	}

	/**
	 * Indicates if a link in a data grid's rows runs an action, from the view's metadata: the links of the context's
	 * view's data grid with the binding whose shown value contains the text (or, if none does, as with a value made by an
	 * expression, every link of the grid) all have an action reference. A grid that the view does not declare, or that has
	 * no links, counts as running an action, the usual kind of row link.
	 *
	 * @param binding	the data grid's binding
	 * @param text	the link's text, with the tokens substituted
	 * @return true if the link runs an action, false if it leads to another page
	 */
	private boolean dataGridLinkRunsAction(@Nonnull String binding, @Nonnull String text) {
		C context = peek();
		if (context == null) {
			return true;
		}
		String moduleName = context.getModuleName();
		String documentName = context.getDocumentName();
		ViewType viewType = context.getViewType();
		if ((moduleName == null) || (documentName == null) || (viewType == null)) {
			return true;
		}
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(moduleName);
		Document d = m.getDocument(c, documentName);
		String uxui = context.getUxui();
		View view = d.getView(uxui, c, viewType.toString());
		GridLinks links = new GridLinks((CustomerImpl) c, (ModuleImpl) m, (DocumentImpl) d, (ViewImpl) view,
											(uxui == null) ? "" : uxui, binding);
		links.visit();
		List<Link> candidates = new ArrayList<>();
		for (Link candidate : links.links) {
			String value = candidate.getLocalisedValue();
			if ((value != null) && value.contains(text)) {
				candidates.add(candidate);
			}
		}
		if (candidates.isEmpty()) {
			candidates = links.links;
		}
		for (Link candidate : candidates) {
			Object reference = candidate.getReference();
			if (! ((reference instanceof ActionReference) || (reference instanceof ImplicitActionReference))) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Collects the links of the columns of the data grid bound to a binding.
	 */
	private static final class GridLinks extends NoOpViewVisitor {
		private final String binding;
		/** The nesting depth inside the data grid, 0 when outside it. */
		private int depth = 0;
		final List<Link> links = new ArrayList<>();

		GridLinks(@Nonnull CustomerImpl customer,
					@Nonnull ModuleImpl module,
					@Nonnull DocumentImpl document,
					@Nonnull ViewImpl view,
					@Nonnull String uxui,
					@Nonnull String binding) {
			super(customer, module, document, view, uxui);
			this.binding = binding;
		}

		@Override
		public void visitDataGrid(@Nonnull DataGrid grid, boolean parentVisible, boolean parentEnabled) {
			if ((depth > 0) || binding.equals(grid.getBinding())) {
				depth++;
			}
		}

		@Override
		public void visitedDataGrid(@Nonnull DataGrid grid, boolean parentVisible, boolean parentEnabled) {
			if (depth > 0) {
				depth--;
			}
		}

		@Override
		public void visitLink(@Nonnull Link link, boolean parentVisible, boolean parentEnabled) {
			if (depth > 0) {
				links.add(link);
			}
		}
	}

	/**
	 * Records that the upload a renderer has just made with {@code testSuccess="false"} was refused on its upload page,
	 * which the renderer has then closed, as a user does after reading the refusal. A following {@code <testFailure/>}
	 * counts the refusal as an error, and a following {@code <testSuccess/>} fails with it.
	 *
	 * @param refusal	what the upload page said, or null if the upload was not refused there
	 */
	protected final void uploadRefused(@Nullable String refusal) {
		uploadError = refusal;
		if (refusal != null) {
			selenide.trace("    The upload was refused: " + refusal);
		}
	}

	/**
	 * Returns the refusal of the upload made by the previous step, if that step was an upload that was refused.
	 *
	 * @return the refusal, or null
	 */
	private @Nullable String refusedUpload() {
		return (previousStep instanceof Upload) ? uploadError : null;
	}

	/**
	 * Checks that a column named by a list grid step is a column of the list grid's query, and that it can be
	 * filtered or sorted. A model's columns are made by its code, so they are not checked.
	 *
	 * @param step	the step, for the messages
	 * @param moduleName	the step's module, or null for the context's (or, when the step names no list, the context's list's)
	 * @param documentName	the step's document, or null for the context's
	 * @param queryName	the step's query, or null for the document's default query (or the context's list's query)
	 * @param modelName	the step's model, or null
	 * @param column	the column's binding
	 * @param filter	true to check the column can be filtered, false to check it can be sorted, null to check neither
	 * @throws MetaDataException	if the list grid cannot be resolved, or the column is not one of its query, or
	 * 								cannot be filtered or sorted
	 */
	private void checkListGridColumn(@Nonnull Step step,
										@Nullable String moduleName,
										@Nullable String documentName,
										@Nullable String queryName,
										@Nullable String modelName,
										@Nonnull String column,
										@Nullable Boolean filter) {
		C context = peek();
		// A step that names no list means the list the context shows (see NavigateList.listGridIdentifier)
		PushListContext list = (context == null) ? null : context.getList();
		String listModuleName = moduleName;
		String listDocumentName = documentName;
		String listQueryName = queryName;
		String listModelName = modelName;
		if ((list != null) && (moduleName == null) && (documentName == null) && (queryName == null) && (modelName == null)) {
			listModuleName = list.getModuleName();
			listDocumentName = list.getDocumentName();
			listQueryName = list.getQueryName();
			listModelName = list.getModelName();
		}
		if (listModelName != null) {
			return;
		}
		String element = Introspector.decapitalize(step.getClass().getSimpleName());
		String m = (listModuleName != null) ? listModuleName : ((context == null) ? null : context.getModuleName());
		String d = (listDocumentName != null) ? listDocumentName : ((context == null) ? null : context.getDocumentName());
		if ((m == null) || ((listQueryName == null) && (d == null))) {
			throw new MetaDataException(String.format("<%s /> needs a module and a query or document, or a list context to take them from", element));
		}
		Customer c = CORE.getUser().getCustomer();
		Module module = c.getModule(m);
		MetaDataQueryDefinition query = null;
		if (listQueryName != null) {
			query = module.getMetaDataQuery(listQueryName);
			if (query == null) {
				// As the list grid does, an undefined query name is the name of a document to use the default query of
				query = module.getDocumentDefaultQuery(c, listQueryName);
			}
		}
		else if (d != null) {
			query = module.getDocumentDefaultQuery(c, d);
		}
		if (query == null) {
			throw new MetaDataException(String.format("<%s /> has no list grid query in module %s", element, m));
		}
		List<MetaDataQueryColumn> columns = query.getColumns();
		List<String> names = new ArrayList<>(columns.size());
		for (MetaDataQueryColumn candidate : columns) {
			String binding = candidate.getBinding();
			String name = (binding == null) ? candidate.getName() : binding;
			if (column.equals(name)) {
				if ((filter != null) && (candidate instanceof MetaDataQueryProjectedColumn projected)) {
					if (filter.booleanValue() && (! projected.isFilterable())) {
						throw new MetaDataException(String.format("<%s /> column [%s] of query %s.%s cannot be filtered",
																	element, column, m, query.getName()));
					}
					if ((! filter.booleanValue()) && (! projected.isSortable())) {
						throw new MetaDataException(String.format("<%s /> column [%s] of query %s.%s cannot be sorted",
																	element, column, m, query.getName()));
					}
				}
				return;
			}
			names.add(name);
		}
		throw new MetaDataException(String.format("<%s /> column [%s] is not a column of query %s.%s: its columns are %s",
													element, column, m, query.getName(), names));
	}

	/**
	 * Renderer hook for {@code <listGridFilter/>}: filters the column of the step's list grid, as a user does in the
	 * grid's column filters, then waits for the grid to fetch its rows. A filter on a column that is already filtered
	 * replaces it, and filters on different columns are ANDed.
	 *
	 * @param filter	the step, which names the list grid
	 * @param column	the column's binding, checked to be a filterable column of the grid's query (unless it has a model)
	 * @param value	the value with the tokens substituted, or null to clear the column's filter
	 * @param operator	how the column is compared with the value; ignored when the value is null
	 * @throws UnsupportedOperationException	until the renderer implements it
	 */
	protected void filterListGrid(@Nonnull ListGridFilter filter, @Nonnull String column, @Nullable String value, @Nonnull FilterOperator operator) {
		throw notImplementedYet("<listGridFilter/>");
	}

	/**
	 * Renderer hook for {@code <listGridClearFilter/>}: clears every column filter of the step's list grid, then waits
	 * for the grid to fetch its rows.
	 *
	 * @param clear	the step, which names the list grid
	 * @throws UnsupportedOperationException	until the renderer implements it
	 */
	protected void clearListGridFilters(@Nonnull ListGridClearFilter clear) {
		throw notImplementedYet("<listGridClearFilter/>");
	}

	/**
	 * Renderer hook for {@code <listGridSort/>}: sorts the step's list grid by the column alone, as a user does by
	 * clicking the column's header until it shows the direction, then waits for the grid to fetch its rows.
	 *
	 * @param sort	the step, which names the list grid
	 * @param column	the column's binding, checked to be a sortable column of the grid's query (unless it has a model)
	 * @param descending	true for descending, false for ascending
	 * @throws UnsupportedOperationException	until the renderer implements it
	 */
	protected void sortListGrid(@Nonnull ListGridSort sort, @Nonnull String column, boolean descending) {
		throw notImplementedYet("<listGridSort/>");
	}

	/**
	 * Renderer hook for {@code <captureValue/>}: reads the text that the first displayed widget for the binding shows,
	 * as {@code <testValue/>} reads it (a combo or radio shows its option's label, a check box true or false).
	 *
	 * @param capture	the step
	 * @return the displayed text, or null or blank if the widget shows no value (the step then fails)
	 * @throws UnsupportedOperationException	until the renderer implements it
	 */
	protected @Nullable String readWidgetValue(@Nonnull CaptureValue capture) {
		throw notImplementedYet("<captureValue/>");
	}

	/**
	 * Renderer hook for {@code <dataEnter match="code"/>} and {@code <dataEnter match="label"/>}: chooses the option
	 * of a combo or radio whose code or displayed label equals the value. The renderer's {@code executeDataEnter} calls
	 * it when the match is not {@code index}. It fails if the widget is not a combo or radio, or if no option or more
	 * than one has that code or label.
	 *
	 * @param dataEnter	the step
	 * @param value	the value with the tokens substituted
	 * @param match	{@link Match#code} or {@link Match#label}
	 * @throws UnsupportedOperationException	until the renderer implements it
	 */
	protected void enterOption(@Nonnull DataEnter dataEnter, @Nonnull String value, @Nonnull Match match) {
		throw notImplementedYet("<dataEnter match=\"" + match + "\"/>");
	}

	/**
	 * Renderer hook for {@code <lookupDescriptionPick rowContaining="…"/>}: opens the lookup's drop-down and picks the
	 * first row, in the order shown, whose text contains the given text (case-sensitive). The renderer's
	 * {@code executeLookupDescriptionPick} calls it when the step has {@code rowContaining}. It fails if no row does.
	 *
	 * @param pick	the step
	 * @param rowContaining	the text with the tokens substituted
	 * @throws UnsupportedOperationException	until the renderer implements it
	 */
	protected void pickLookupDescriptionRowContaining(@Nonnull LookupDescriptionPick pick, @Nonnull String rowContaining) {
		throw notImplementedYet("<lookupDescriptionPick rowContaining=\"…\"/>");
	}

	/**
	 * Renderer hook for {@code <lookupDescriptionAutoComplete pick="…"/>}: types the search text into the lookup and
	 * picks the first suggestion, in the order shown, whose text contains {@code pick} (case-sensitive). The renderer's
	 * {@code executeLookupDescriptionAutoComplete} calls it when the step has {@code pick}. It fails if no suggestion
	 * does.
	 *
	 * @param complete	the step
	 * @param search	the search text with the tokens substituted
	 * @param pick	the text the suggestion contains, with the tokens substituted
	 * @throws UnsupportedOperationException	until the renderer implements it
	 */
	protected void autoCompleteLookupDescriptionAndPick(@Nonnull LookupDescriptionAutoComplete complete, @Nonnull String search, @Nonnull String pick) {
		throw notImplementedYet("<lookupDescriptionAutoComplete pick=\"…\"/>");
	}

	/**
	 * Renderer hook for {@code <navigateLink text="…"/>}: clicks the first displayed link ({@code <a>}) on the page whose
	 * visible text contains the text (case-sensitive), as a user does, then waits for the page or view it leads to.
	 * It fails if no displayed link contains the text.
	 *
	 * @param link	the step
	 * @param text	the text with the tokens substituted
	 * @throws UnsupportedOperationException	until the renderer implements it
	 */
	protected void clickLinkContaining(@Nonnull NavigateLink link, @Nonnull String text) {
		throw notImplementedYet("<navigateLink text=\"…\"/>");
	}

	/**
	 * Renderer hook for {@code <captureCell/>}: reads the text that a cell of the step's grid shows: the cell in the
	 * column whose binding is the step's column, of the step's row ({@code row}, or the first row, in the order shown,
	 * that has a cell containing {@code rowContaining} with its tokens substituted). An input in the cell (an inline grid)
	 * reads as its value.
	 *
	 * @param capture	the step, which names the grid, the row and the column
	 * @return the displayed text, or null or blank if the cell shows no value (the step then fails)
	 * @throws UnsupportedOperationException	until the renderer implements it
	 */
	protected @Nullable String readGridCell(@Nonnull CaptureCell capture) {
		throw notImplementedYet("<captureCell/>");
	}

	/**
	 * Renderer hook for {@code <captureTitle/>}: reads the title that the current view shows, as a user reads it.
	 *
	 * @param capture	the step
	 * @return the displayed title, or null or blank if the view shows none (the step then fails)
	 * @throws UnsupportedOperationException	until the renderer implements it
	 */
	protected @Nullable String readViewTitle(@Nonnull CaptureTitle capture) {
		throw notImplementedYet("<captureTitle/>");
	}

	/**
	 * Renderer hook for {@code <dataGridLink/>}: clicks the first displayed link in the step's row of its data grid whose
	 * visible text contains the text (case-sensitive), as a user does. For a link that runs an action, it then answers the
	 * confirmation and questions as the step's {@code confirm} says (failing if it is false and nothing asks) and waits for
	 * the response; for any other link, it waits for the page or view it leads to. It fails if the row has no such
	 * displayed link, listing the row's links, or if the link is disabled.
	 *
	 * @param link	the step, which names the grid and the row
	 * @param text	the link's text, with the tokens substituted
	 * @param action	true if the link runs an action, false if it leads to another page
	 * @return true if the action's confirmation or a question was answered negatively, so the action did not run
	 * @throws UnsupportedOperationException	until the renderer implements it
	 */
	protected boolean clickDataGridLink(@Nonnull DataGridLink link, @Nonnull String text, boolean action) {
		throw notImplementedYet("<dataGridLink/>");
	}

	/**
	 * Renderer hook for {@code <navigateLink text="…" window="new"/>}: clicks the first displayed link ({@code <a>}) on the
	 * page whose visible text contains the text (case-sensitive; with {@code exact}, equals it), as a user does, switches to
	 * the new tab or window it opens and waits for its page (see {@link Selenide#openNewWindow(Runnable, String)}).
	 * It fails if no displayed link matches, or if no new window opens.
	 *
	 * @param link	the step
	 * @param text	the text with the tokens substituted
	 * @return the handle of the window the link was clicked in, which {@code <closeWindow/>} returns to
	 * @throws UnsupportedOperationException	until the renderer implements it
	 */
	protected @Nonnull String clickLinkIntoNewWindow(@Nonnull NavigateLink link, @Nonnull String text) {
		throw notImplementedYet("<navigateLink text=\"…\" window=\"new\"/>");
	}

	/**
	 * Renderer hook for {@code <listGridTool/>}: presses the tool of the step's list grid, as a user does with the grid's
	 * toolbar, then waits for what it does (the grid's rows fetched again for {@code refresh}; no row selected for
	 * {@code clearSelection}). A renderer whose list grids have no control for the tool fails, saying so.
	 *
	 * @param step	the step, which names the list grid
	 * @param tool	the tool
	 * @throws UnsupportedOperationException	until the renderer implements it
	 */
	protected void pressListGridTool(@Nonnull ListGridTool step, @Nonnull Tool tool) {
		throw notImplementedYet("<listGridTool/>");
	}

	/**
	 * A SAIL failure that already names its interaction and step.
	 */
	private static final class SailFailure extends AssertionError {
		private static final long serialVersionUID = 4217359713946618402L;

		SailFailure(@Nonnull String message, @Nullable Throwable cause) {
			super(message, cause);
		}
	}

	/**
	 * Resolves a test classpath resource (an {@code <upload />} file) to a file the browser can be given.
	 *
	 * @param resource	the resource path, e.g. {@code sail/files/sail-upload.txt}
	 * @return the file
	 * @throws MetaDataException	if the resource is not on the test classpath, or is not a file on disk
	 */
	protected static @Nonnull java.io.File testResourceFile(@Nonnull String resource) {
		java.net.URL url = Thread.currentThread().getContextClassLoader().getResource(resource);
		if (url == null) {
			throw new MetaDataException("<upload /> file " + resource + " is not on the test classpath");
		}
		try {
			return new java.io.File(url.toURI());
		}
		catch (java.net.URISyntaxException | IllegalArgumentException e) {
			throw new MetaDataException("<upload /> file " + resource + " is not a file on disk: " + url, e);
		}
	}

	/** A menu item of a module and the labels shown for it (module title, group labels, item label). */
	protected static final class SailMenuPath {
		/** The module's localised title. */
		public String moduleTitle;
		/** The labels of the groups that hold the item, outermost first. */
		public final List<String> groupLabels = new ArrayList<>();
		/** The item's label, or null when the path names only the module or is not in the menu. */
		public String itemLabel;
		/** The item, or null when the path names only the module or is not in the menu. */
		public org.skyve.metadata.module.menu.MenuItem item;

		/**
		 * Returns the group labels then the item label.
		 *
		 * @return the labels
		 */
		public @Nonnull List<String> labels() {
			List<String> result = new ArrayList<>(groupLabels);
			if (itemLabel != null) {
				result.add(itemLabel);
			}
			return result;
		}
	}

	/**
	 * Resolves a {@code /}-separated menu path: group names then the item name, each the metadata name or the shown name.
	 *
	 * @param moduleName	the module
	 * @param itemPath	the path, or null for the module only
	 * @return the path; its item is null if the path is not in the module's menu
	 */
	protected static @Nonnull SailMenuPath resolveMenuPath(@Nonnull String moduleName, @Nullable String itemPath) {
		org.skyve.metadata.customer.Customer c = org.skyve.CORE.getUser().getCustomer();
		org.skyve.metadata.module.Module m = c.getModule(moduleName);
		SailMenuPath result = new SailMenuPath();
		result.moduleTitle = m.getLocalisedTitle();
		if (itemPath == null) {
			return result;
		}
		String[] names = itemPath.split("/");
		List<org.skyve.metadata.module.menu.MenuItem> items = m.getMenu().getItems();
		for (int i = 0; i < names.length; i++) {
			String name = names[i].trim();
			org.skyve.metadata.module.menu.MenuItem found = null;
			for (org.skyve.metadata.module.menu.MenuItem candidate : items) {
				if (name.equals(candidate.getName()) || name.equals(candidate.getLocalisedName())) {
					found = candidate;
					break;
				}
			}
			if (found == null) {
				return result;
			}
			String label = found.getLocalisedName();
			if (label == null) {
				label = found.getName();
			}
			if (i < names.length - 1) {
				if (! (found instanceof org.skyve.metadata.module.menu.MenuGroup group)) {
					return result;
				}
				result.groupLabels.add(label);
				items = group.getItems();
			}
			else {
				result.itemLabel = label;
				result.item = found;
			}
		}
		return result;
	}

	/**
	 * Checks a {@code <testWidget />} against what the page shows, under the test strategy.
	 *
	 * @param test	the step
	 * @param identifier	the widget's identifier, for the messages
	 * @param shown	true if any rendering of the widget is shown
	 * @param enabled	true if a shown rendering is enabled
	 */
	protected final void checkWidget(@Nonnull TestWidget test, @Nonnull String identifier, boolean shown, boolean enabled) {
		Boolean expectedEnabled = test.getEnabled();
		if (expectedEnabled != null) {
			selenide.trace(String.format("test [%s] is shown and %s", identifier, expectedEnabled.booleanValue() ? "enabled" : "disabled"));
			check(shown && (enabled == expectedEnabled.booleanValue()),
					() -> String.format("expected [%s] to be shown and %s but it is %s",
											identifier,
											expectedEnabled.booleanValue() ? "enabled" : "disabled",
											(! shown) ? "not shown" : (enabled ? "enabled" : "disabled")));
		}
		else {
			boolean expectedVisible = test.isExpectedVisible();
			selenide.trace(String.format("test [%s] is %s", identifier, expectedVisible ? "shown" : "not shown"));
			check(shown == expectedVisible,
					() -> String.format("expected [%s] to be %s but it is %s",
											identifier,
											expectedVisible ? "shown" : "not shown",
											shown ? "shown" : "not shown"));
		}
	}

	/**
	 * Indicates if a row count is within a {@code <testGridRows />} step's bounds.
	 *
	 * @param test	the step
	 * @param rows	the number of rows counted
	 * @return true if the count is within the bounds
	 */
	protected static boolean rowCountHolds(@Nonnull TestGridRows test, int rows) {
		Integer max = test.getMaxRows();
		return (rows >= test.getEffectiveMinRows()) && ((max == null) || (rows <= max.intValue()));
	}

	/**
	 * Checks a {@code <testGridRows />} step's bounds against the rows counted, under the test strategy.
	 *
	 * @param test	the step
	 * @param identifier	the grid's identifier, for the message
	 * @param contains	the text the counted rows contain (with {@code {run}} substituted), or null for every row
	 * @param rows	the number of rows counted
	 */
	protected final void checkRowCount(@Nonnull TestGridRows test, @Nonnull String identifier, @Nullable String contains, int rows) {
		Integer max = test.getMaxRows();
		check(rowCountHolds(test, rows),
				() -> String.format("expected grid [%s] to have %s%s rows%s but it has %d",
										identifier,
										Integer.valueOf(test.getEffectiveMinRows()),
										(max == null) ? " or more" : " to " + max,
										(contains == null) ? "" : " containing '" + contains + "'",
										Integer.valueOf(rows)));
	}

	/**
	 * Checks a {@code <testMenu />} step against whether the menu shows the module or item, under the test strategy.
	 *
	 * @param test	the step
	 * @param path	the resolved menu path
	 * @param present	true if the menu shows it
	 */
	protected final void checkMenu(@Nonnull TestMenu test, @Nonnull SailMenuPath path, boolean present) {
		boolean expected = ! Boolean.FALSE.equals(test.getPresent());
		String what = (test.getItemName() == null) ? path.moduleTitle : path.moduleTitle + " / " + String.join(" / ", path.labels());
		selenide.trace(String.format("test menu %s is %s", what, expected ? "present" : "absent"));
		check(present == expected, () -> String.format("expected the menu %s %s but it %s", what,
														expected ? "to show" : "not to show", present ? "does" : "does not"));
	}

	/**
	 * Splits a list membership's value ({@code A|B}) into its members' descriptions, in order.
	 *
	 * @param value	the value
	 * @return the descriptions, trimmed, without blanks
	 */
	protected static @Nonnull List<String> membershipItems(@Nullable String value) {
		List<String> result = new ArrayList<>();
		if (value != null) {
			for (String item : value.split("\\|")) {
				String trimmed = item.trim();
				if (! trimmed.isEmpty()) {
					result.add(trimmed);
				}
			}
		}
		return result;
	}

	/**
	 * Returns whether a list membership shows the members expected: in the same order for an ordered membership, whose
	 * order is part of its value, and in any order otherwise, so that a script may list an unordered membership's members
	 * in any order.
	 *
	 * @param members	the members shown, in order
	 * @param expected	the members expected, in order
	 * @param ordered	whether the membership's collection is ordered
	 * @return whether they match
	 */
	static boolean membersMatch(@Nonnull List<String> members, @Nonnull List<String> expected, boolean ordered) {
		return ordered ? members.equals(expected) : new java.util.HashSet<>(members).equals(new java.util.HashSet<>(expected));
	}

	/**
	 * Returns whether a binding of the current context's document names an ordered collection, as an ordered list
	 * membership's binding does.
	 *
	 * @param binding	the binding
	 * @return {@code true} if it is an ordered collection; {@code false} otherwise, or when there is no document context
	 */
	protected boolean isOrderedCollection(@Nonnull String binding) {
		C context = peek();
		String moduleName = (context == null) ? null : context.getModuleName();
		String documentName = (context == null) ? null : context.getDocumentName();
		if ((moduleName == null) || (documentName == null)) {
			return false;
		}
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(moduleName);
		Document d = m.getDocument(c, documentName);
		TargetMetaData target = BindUtil.getMetaDataForBinding(c, m, d, binding);
		return (target.getAttribute() instanceof org.skyve.metadata.model.document.Collection collection) &&
				Boolean.TRUE.equals(collection.getOrdered());
	}
}
