package util.sail;

import java.beans.Introspector;
import java.io.IOException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.skyve.CORE;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.sail.execution.WebDriverExecutor;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.Procedure;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.CaptureValue;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.Execute;
import org.skyve.metadata.sail.language.step.Pause;
import org.skyve.metadata.sail.language.step.TestFailure;
import org.skyve.metadata.sail.language.step.TestGridRows;
import org.skyve.metadata.sail.language.step.TestMessage;
import org.skyve.metadata.sail.language.step.TestSuccess;
import org.skyve.metadata.sail.language.step.TestWidget;
import org.skyve.metadata.sail.language.step.context.ClearContext;
import org.skyve.metadata.sail.language.step.context.PopContext;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.DataEnter.Match;
import org.skyve.metadata.sail.language.step.interaction.Upload;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridClearFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter.FilterOperator;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSort;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionAutoComplete;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionPick;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateCalendar;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMap;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateTree;
import org.skyve.metadata.sail.language.step.interaction.navigation.TestMenu;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.sail.language.step.interaction.session.Logout;
import org.skyve.metadata.sail.language.step.interaction.session.SwitchMode;
import org.skyve.metadata.user.User;
import org.skyve.web.WebAction;

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
 * Set the system property {@value #FAILURE_DIRECTORY_PROPERTY} to a directory to save a screenshot and the page source
 * for each failure, and {@value #TEST_STRATEGY_PROPERTY} to override the automation's test strategy (any case; an
 * invalid value fails before any step runs). Tests that the override would weaken (the admin suites, the strategy
 * conformance test) refuse it: see {@link SailSuite#refuseTestStrategyOverride(String)}.
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
 * <li>{@link #clickLinkContaining(NavigateLink, String)} for {@code <navigateLink text="…"/>}.</li>
 * </ul>
 * Every hook receives its text values with the tokens already substituted. Like any step, a hook throws when it
 * cannot do what the step says, and makes the implicit success check, {@link #checkSuccess(String)}, where the other
 * steps of its kind do. A hook that changes a list grid waits for the grid's fetch to end before it returns.
 *
 * @param <AC> automation context type
 * @param <S> concrete Selenide implementation
 * @author simeonsolomou
 */
public abstract class InterpretedWebDriverExecutor<
	AC extends AutomationContext<?, ?>,
	S extends Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?>> extends WebDriverExecutor<AC> {

	/** System property naming a directory to save failure screenshots and page sources into. */
	public static final String FAILURE_DIRECTORY_PROPERTY = "sail.failureDir";
	/** System property that overrides the automation's test strategy (Assert, Verify or None), e.g. to list every failure. */
	public static final String TEST_STRATEGY_PROPERTY = "sail.testStrategy";

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
	/** Counts failures, to name diagnostic files uniquely. */
	private int failureCount = 0;
	/** The roles to build the views with for each user that signs in, or null to keep the persistence user. */
	private SailViewRoles viewRoles;
	/** The persistence user when the first login of the run switched it, which the views are built from; or null. */
	private User viewTemplateUser;
	/** The variables each automation starts with, by name; see {@link #useVariables(Map)}. */
	private final Map<String, String> startingVariables = new java.util.TreeMap<>();

	/**
	 * Runs the automation's before procedure, interactions and after procedure, applying the test strategy.
	 *
	 * @param automation	the automation to run
	 * @throws AssertionError	at the first failure under Assert, or at the end under Verify when anything failed
	 */
	@Override
	public void executeAutomation(@Nonnull Automation automation) {
		testStrategyOverride(); // an invalid override fails here, before any step runs
		initialiseAutomation(automation);
		for (Map.Entry<String, String> variable : startingVariables.entrySet()) {
			putVariable(variable.getKey(), variable.getValue());
		}
		failures.clear();
		thrown.clear();

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
		}
		finally {
			restoreViewUser();
		}

		if (! failures.isEmpty()) {
			StringBuilder message = new StringBuilder(256);
			message.append("SAIL automation failed with ").append(failures.size()).append(" failure(s):");
			for (String failure : failures) {
				message.append("\n  - ").append(failure);
			}
			AssertionError error = new AssertionError(message.toString());
			// each step that threw is attached, with its causes and stack trace, as it would be under Assert
			for (SailFailure failure : thrown) {
				error.addSuppressed(failure);
			}
			throw error;
		}
	}

	/**
	 * Runs the interaction's before procedure, steps and after procedure. Under Verify, the after procedure
	 * still runs when an earlier step throws.
	 *
	 * @param interaction	the interaction to run
	 */
	@Override
	public void executeInteraction(@Nonnull Interaction interaction) {
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
			// Start the next interaction with no stale contexts
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
			try {
				step.execute(this);
			}
			catch (SailFailure e) {
				// already described where it happened (Assert)
				throw e;
			}
			catch (Throwable t) { // NOSONAR java:S1181 any step failure, including Selenide assertion errors, is a SAIL failure
				String failure = failureMessage(reason(t));
				if ((! halt) && (TestStrategy.Verify == getTestStrategy())) {
					record(failure);
					SailFailure cause = new SailFailure(failure, t);
					thrown.add(cause);
					selenide.trace("    Cause: " + causes(t));
					return false;
				}
				diagnose();
				throw new SailFailure(failure, t);
			}
			previousStep = step;
		}
		return true;
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
			record(failure);
		}
		else {
			diagnose();
			throw new SailFailure(failure, null);
		}
	}

	/**
	 * Returns the automation's test strategy, or the one the {@value #TEST_STRATEGY_PROPERTY} system property names.
	 *
	 * @return the test strategy, or null if neither gives one
	 */
	@Override
	public @Nullable TestStrategy getTestStrategy() {
		TestStrategy override = testStrategyOverride();
		return (override != null) ? override : super.getTestStrategy();
	}

	/**
	 * Returns the test strategy that the {@value #TEST_STRATEGY_PROPERTY} system property names (ignoring case).
	 *
	 * @return the strategy, or null if the property is not set
	 * @throws IllegalArgumentException	if the property names no strategy, saying which are valid
	 */
	public static @Nullable TestStrategy testStrategyOverride() {
		String override = System.getProperty(TEST_STRATEGY_PROPERTY);
		if ((override == null) || override.isBlank()) {
			return null;
		}
		String name = override.trim();
		for (TestStrategy strategy : TestStrategy.values()) {
			if (strategy.name().equalsIgnoreCase(name)) {
				return strategy;
			}
		}
		throw new IllegalArgumentException(String.format("-D%s=%s is not a SAIL test strategy: use one of %s",
															TEST_STRATEGY_PROPERTY, override, java.util.Arrays.toString(TestStrategy.values())));
	}

	/**
	 * Indicates if the automation makes no assertions.
	 *
	 * @return true for the None test strategy
	 */
	protected final boolean isNoneStrategy() {
		return TestStrategy.None == getTestStrategy();
	}

	private void record(@Nonnull String failure) {
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
		String url = currentUrl();
		if (url != null) {
			result.append(" [at ").append(url).append(']');
		}
		return result.toString();
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
	 * Saves a screenshot and the page source when {@value #FAILURE_DIRECTORY_PROPERTY} is set.
	 */
	private void diagnose() {
		String directory = System.getProperty(FAILURE_DIRECTORY_PROPERTY);
		WebDriver driver = selenide.driver;
		if ((directory == null) || directory.isBlank() || (driver == null)) {
			return;
		}
		try {
			Path path = Path.of(directory);
			Files.createDirectories(path);
			String prefix = String.format("%02d-%s-step%02d", Integer.valueOf(++failureCount),
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
		result.append('<').append(Introspector.decapitalize(step.getClass().getSimpleName()));
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
					result.append(' ').append(property).append("=\"").append(secret ? "****" : value).append('"');
				}
			}
			catch (@SuppressWarnings("unused") ReflectiveOperationException | RuntimeException e) {
				// not a simple property
			}
		}
		return result.append(" />").toString();
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
	 * {@code <testFailure />}, which then asserts that it failed.
	 *
	 * @param login	the login step
	 */
	@Override
	public void executeLogin(Login login) {
		String customer = login.getCustomer();
		String user = substituteTokens(login.getUser());
		String password = substituteTokens(login.getPassword());
		switchViewUser(login.getUser(), user);

		if (customer == null) {
			selenide.trace("Login as " + user);
		} else {
			selenide.trace("Login as " + customer + "/" + user);
		}
		selenide.login(customer, user, password);
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
	 * same name replaces the value for the rest of that automation.
	 *
	 * <p>Side effects: replaces the variables given before.
	 *
	 * @param variables	the variables by name; copied
	 */
	public final void useVariables(@Nonnull Map<String, String> variables) {
		startingVariables.clear();
		startingVariables.putAll(variables);
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
	}

	/**
	 * Opens the link: an absolute URL as is, otherwise relative to the base URL; or, with {@code text}, clicks the first
	 * displayed link on the page whose text contains it (see {@link #clickLinkContaining}). The {@code href}'s or
	 * {@code text}'s tokens ({@code {run}}, {@code {var:name}}) are substituted first.
	 * Any current context no longer has a document or view type, as the page is unknown.
	 *
	 * @param link	the step
	 */
	@Override
	public void executeNavigateLink(NavigateLink link) {
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
		if (href.matches("^[a-zA-Z][a-zA-Z0-9+.-]*:.*")) {
			selenide.openUrl(href);
		}
		else {
			selenide.get(href.startsWith("/") ? href.substring(1) : href);
		}
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
		else {
			selenide.trace("Test success");
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
	 * that the login failed.
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
			check(selenide.verifyFailure(), () -> "expected an error message but found: " + selenide.describeMessages());
		}
		else {
			selenide.trace(String.format("Test failure with message '%s'", message));
			check(selenide.verifyFailure(expected),
					() -> "expected an error message containing '" + expected + "' but found: " + selenide.describeMessages());
		}
	}

	@Override
	public void executeComment(Comment comment) {
		selenide.trace(comment.getComment());
	}

	@Override
	public void executeExecute(Execute execute) {
		JavascriptExecutor js = (JavascriptExecutor) selenide.driver;
		js.executeScript(execute.getScript());
	}

	@Override
	public void executePause(Pause pause) {
		selenide.pause(pause.getMillis());
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
		checkListGridColumn(filter, filter.getModuleName(), filter.getDocumentName(), filter.getQueryName(), filter.getModelName(), column, true);
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
		checkListGridColumn(sort, sort.getModuleName(), sort.getDocumentName(), sort.getQueryName(), sort.getModelName(), column, false);
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
		String binding = capture.getBinding();
		String variable = capture.getVariable();
		String value = readWidgetValue(capture);
		if (value != null) {
			value = value.trim();
		}
		if ((value == null) || value.isEmpty()) {
			throw new IllegalStateException(String.format("[%s] shows no value to keep in {var:%s}", binding, variable));
		}
		putVariable(variable, value);
		selenide.trace(String.format("Keep [%s] '%s' in {var:%s}", binding, value, variable));
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
	 * @param filter	true to check the column can be filtered, false to check it can be sorted
	 * @throws MetaDataException	if the list grid cannot be resolved, or the column is not one of its query, or
	 * 								cannot be filtered or sorted
	 */
	private void checkListGridColumn(@Nonnull Step step,
										@Nullable String moduleName,
										@Nullable String documentName,
										@Nullable String queryName,
										@Nullable String modelName,
										@Nonnull String column,
										boolean filter) {
		AC context = peek();
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
				if (candidate instanceof MetaDataQueryProjectedColumn projected) {
					if (filter && (! projected.isFilterable())) {
						throw new MetaDataException(String.format("<%s /> column [%s] of query %s.%s cannot be filtered",
																	element, column, m, query.getName()));
					}
					if ((! filter) && (! projected.isSortable())) {
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
}
