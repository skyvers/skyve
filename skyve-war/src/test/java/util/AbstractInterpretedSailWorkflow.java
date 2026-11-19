package util;

import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.function.Supplier;

import org.junit.jupiter.api.AfterAll;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import util.sail.BrowserConfiguration;
import util.sail.SailServerLock;
import util.sail.SailSettings;
import util.sail.SailSuite;
import util.sail.SailViewRoles;
import util.sail.SailWorkflow;

/**
 * Base test class for SAIL workflows: several automations that run in order as one run, moving between renderers
 * (see {@link SailWorkflow}). For example, a client raises a request in a PrimeFaces UX/UI, then staff find it in
 * SmartClient by the run token or a captured value:
 *
 * <pre>
 * &#64;Test
 * void request() {
 * 	viewRoles("client{run}", "todo.ToDoClient");
 * 	sailWorkflow("sail/apps/request-1-client.xml", "sail/apps/request-2-staff.xml");
 * }
 * </pre>
 *
 * A script can also be a {@code <workflow>} that holds the automations itself, and a {@code <suite>} of independent
 * workflows runs with {@link #sailSuite(String)}.
 * <p>
 * Unlike the single-renderer test bases, no browser is started before a test: the workflow starts the browser each
 * automation needs. The server lock ({@link SailServerLock}) is taken by the first workflow and held until the class has
 * finished. When the class has finished, its tests' workflows are closed too ({@link SailWorkflow#close()}), which ends
 * a shared run's device profiles.
 * <p>
 * {@link #settings()} is the test's code level of {@link SailSettings}: every {@code -D} option can be set there, e.g.
 * {@code settings().headless(false).timeoutSeconds(60)}, before the workflow runs.
 */
@SuppressWarnings("resource") // closeWorkflowsAndReleaseServer() closes the tests' workflows; their setters return the same workflow
public abstract class AbstractInterpretedSailWorkflow extends AbstractH2Test {
	/**
	 * The workflows of the tests that have run, closed when the class has finished: each test has its own instance, and
	 * so its own workflow, unless the class has one instance for all its tests.
	 */
	private static final Queue<SailWorkflow> WORKFLOWS = new ConcurrentLinkedQueue<>();

	/** The roles the views are built with for the users the scripts sign in as. */
	private final SailViewRoles viewRoles = new SailViewRoles();
	private final SailWorkflow workflow = new SailWorkflow(viewRoles);

	/** Creates the test, whose workflow is closed when the class has finished. */
	protected AbstractInterpretedSailWorkflow() {
		WORKFLOWS.add(workflow);
	}

	/**
	 * Builds the views for a user that the scripts sign in as with the user's roles, instead of every role.
	 * Call it before running the workflow; see {@link SailViewRoles}. A user name with {@code {run}} matches the user
	 * that the workflow made with its run token.
	 *
	 * @param userName	the user name as a script signs in with it; may contain {@code {run}}
	 * @param roleNames	the user's roles: {@code module.Role}, or a customer role's name
	 */
	protected final void viewRoles(@Nonnull String userName, @Nonnull String... roleNames) {
		viewRoles.register(userName, roleNames);
	}

	/**
	 * Returns the test's code-level settings, to change before the workflow runs, e.g.
	 * {@code settings().variable("password", "x")}.
	 *
	 * @return the settings builder of the workflow
	 */
	protected final @Nonnull SailSettings.Code settings() {
		return workflow.settings();
	}

	/**
	 * Sets the browser for the automations of a user agent type, instead of {@link SailWorkflow#defaultBrowser}.
	 *
	 * @param userAgentType	the automations' user agent type
	 * @param configuration	makes the browser's configuration
	 */
	protected final void browser(@Nonnull UserAgentType userAgentType, @Nonnull Supplier<BrowserConfiguration> configuration) {
		workflow.browser(userAgentType, configuration);
	}

	/**
	 * Runs the automations of SAIL scripts on the test classpath, in order, as one run.
	 *
	 * @param resourceNames	the scripts' resource paths; each is a {@code <workflow>} or an {@code <automation>}
	 * @throws AssertionError	naming the automation that failed
	 */
	protected void sailWorkflow(@Nonnull String... resourceNames) {
		workflow.runResources(resourceNames);
	}

	/**
	 * Runs the automations of SAIL scripts given as XML, in order, as one run.
	 *
	 * @param scripts	the scripts; each is a {@code <workflow>} or an {@code <automation>}
	 * @throws AssertionError	naming the automation that failed
	 */
	protected void sailWorkflowString(@Nonnull String... scripts) {
		workflow.runStrings(scripts);
	}

	/**
	 * Runs SAIL script files, in order, as one run; see {@link #sailWorkflow(String...)}.
	 *
	 * @param files	the scripts' paths, absolute or relative to the working directory
	 * @throws AssertionError	naming the automation that failed
	 */
	protected void sailWorkflowFile(@Nonnull String... files) {
		workflow.runFiles(files);
	}

	/**
	 * Runs a SAIL {@code <suite>} on the test classpath: its members, each a run of its own, with this test's settings,
	 * view roles and browsers (see {@link SailSuite}).
	 *
	 * @param resourceName	the suite's resource path, e.g. {@code sail/admin/admin.suite.xml}
	 * @throws AssertionError	with every failed member, after a summary
	 */
	protected void sailSuite(@Nonnull String resourceName) {
		SailSuite.resource(resourceName, workflow).run();
	}

	/**
	 * Runs a SAIL file or classpath resource as the kind of document it is: a {@code <suite>}, a {@code <workflow>} or an
	 * {@code <automation>} (see {@link SailSuite#runEntry(String, util.sail.SailWorkflow)}).
	 *
	 * @param entry	a file path, or a classpath resource
	 * @throws AssertionError	if it fails
	 */
	protected void sailRun(@Nonnull String entry) {
		SailSuite.runEntry(entry, workflow);
	}

	/**
	 * Runs a SAIL {@code <suite>} on the test classpath, as {@link #sailSuite(String)} does, with each member in a
	 * browser of its own if asked (see {@link SailSuite#browserPerMember()}).
	 *
	 * @param resourceName	the suite's resource path, e.g. {@code sail/admin/admin.suite.xml}
	 * @param browserPerMember	true to start each member in a new browser; false to hand the browser on, as the default does
	 * @throws AssertionError	with every failed member, after a summary
	 */
	protected void sailSuite(@Nonnull String resourceName, boolean browserPerMember) {
		SailSuite suite = SailSuite.resource(resourceName, workflow);
		if (browserPerMember) {
			suite.browserPerMember();
		}
		suite.run();
	}

	/**
	 * Runs a SAIL {@code <suite>} file; see {@link #sailSuite(String)}.
	 *
	 * @param file	the suite's path, absolute or relative to the working directory
	 * @throws AssertionError	with every failed member, after a summary
	 */
	protected void sailSuiteFile(@Nonnull String file) {
		SailSuite.file(file, workflow).run();
	}

	/**
	 * Closes the workflows of the class's tests, which stops a browser a suite left open and ends a shared run's device
	 * profiles, then releases the server lock.
	 */
	@AfterAll
	static void closeWorkflowsAndReleaseServer() {
		try {
			for (SailWorkflow closing = WORKFLOWS.poll(); closing != null; closing = WORKFLOWS.poll()) {
				closing.close();
			}
		}
		finally {
			SailServerLock.releaseAll();
		}
	}
}
