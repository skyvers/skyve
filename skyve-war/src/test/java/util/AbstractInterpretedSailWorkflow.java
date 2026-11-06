package util;

import java.util.function.Supplier;

import org.junit.jupiter.api.AfterAll;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import util.sail.BrowserConfiguration;
import util.sail.SailServerLock;
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
 * A script can also be a {@code <workflow>} that holds the automations itself.
 * <p>
 * Unlike the single-renderer test bases, no browser is started before a test: the workflow starts the browser each
 * automation needs. The server lock ({@link SailServerLock}) is taken by the first workflow and held until the class has
 * finished.
 */
public abstract class AbstractInterpretedSailWorkflow extends AbstractH2Test {
	/** The roles the views are built with for the users the scripts sign in as. */
	private final SailViewRoles viewRoles = new SailViewRoles();
	private final SailWorkflow workflow = new SailWorkflow(viewRoles);

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

	@AfterAll
	static void releaseServer() {
		SailServerLock.releaseAll();
	}
}
