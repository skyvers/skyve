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
import java.util.function.Supplier;

import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.sail.execution.WebDriverExecutor;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.Procedure;
import org.skyve.metadata.sail.language.Step;
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
import org.skyve.metadata.sail.language.step.interaction.Upload;
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
 * for each failure, and {@value #TEST_STRATEGY_PROPERTY} to override the automation's test strategy.
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

	/**
	 * Runs the automation's before procedure, interactions and after procedure, applying the test strategy.
	 *
	 * @param automation	the automation to run
	 * @throws AssertionError	at the first failure under Assert, or at the end under Verify when anything failed
	 */
	@Override
	public void executeAutomation(@Nonnull Automation automation) {
		initialiseAutomation(automation);
		failures.clear();

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

		if (! failures.isEmpty()) {
			StringBuilder message = new StringBuilder(256);
			message.append("SAIL automation failed with ").append(failures.size()).append(" failure(s):");
			for (String failure : failures) {
				message.append("\n  - ").append(failure);
			}
			throw new AssertionError(message.toString());
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
		String override = System.getProperty(TEST_STRATEGY_PROPERTY);
		if ((override != null) && (! override.isBlank())) {
			return TestStrategy.valueOf(override.trim());
		}
		return super.getTestStrategy();
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
					result.append(' ').append(property).append("=\"").append(value).append('"');
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
		String user = substituteRunToken(login.getUser());
		String password = substituteRunToken(login.getPassword());

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

	@Override
	public void executeNavigateEdit(NavigateEdit edit) {
		String moduleName = edit.getModuleName();
		String documentName = edit.getDocumentName();

		PushEditContext push = new PushEditContext();
		push.setModuleName(moduleName);
		push.setDocumentName(documentName);
		executePushEditContext(push);

		String bizId = edit.getBizId();
		if (bizId == null) {
			selenide.trace(String.format("Edit new document [%s.%s] instance", moduleName, documentName));
			selenide.get(String.format("?a=e&m=%s&d=%s", moduleName, documentName));
		} else {
			selenide.trace(String.format("Edit document [%s.%s] instance with bizId %s", moduleName, documentName, bizId));
			selenide.get(String.format("?a=e&m=%s&d=%s&i=%s", moduleName, documentName, bizId));
		}
	}

	/**
	 * Opens the link: an absolute URL as is, otherwise relative to the base URL.
	 * Any current context no longer has a document or view type, as the page is unknown.
	 *
	 * @param link	the step
	 */
	@Override
	public void executeNavigateLink(NavigateLink link) {
		if (peek() != null) {
			super.executeNavigateLink(link);
		}
		String href = link.getHref();
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
			message = substituteRunToken(message);
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
	private @Nonnull UnsupportedOperationException notImplementedYet(@Nonnull String element) {
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
	 * A SAIL failure that already names its interaction and step.
	 */
	private static final class SailFailure extends AssertionError {
		private static final long serialVersionUID = 4217359713946618402L;

		SailFailure(@Nonnull String message, @Nullable Throwable cause) {
			super(message, cause);
		}
	}
}
