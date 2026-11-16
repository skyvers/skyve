package util;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import util.sail.BrowserConfiguration;
import util.sail.SailServerLock;
import util.sail.SailViewRoles;
import util.sail.PrimeFacesInterpretedWebDriverExecutor;
import util.sail.PrimeFacesSelenide;
import util.sail.SailUsers;

/**
 * Base test class for running SAIL scripts against a PrimeFaces UI in an interpreted browser session.
 * <p>
 * Each test class holds {@link SailServerLock} for its server ({@code -Dsail.baseUrl}) from before its first browser
 * starts until the class has finished, so SAIL tests against one server run one at a time, even in forked JVMs
 * (surefire's {@code forkCount}). A test whose server is in use waits for the other to finish: the one necessary wait.
 */
public abstract class AbstractPrimeFacesInterpretedSail extends AbstractH2Test {

	private BrowserConfiguration configuration;
	@Nonnull protected PrimeFacesSelenide selenium = new PrimeFacesSelenide();
	
	protected AbstractPrimeFacesInterpretedSail(@Nonnull BrowserConfiguration configuration) {
		this.configuration = configuration;
	}
	
	/** The roles the views are built with for the users the scripts sign in as. */
	private final SailViewRoles viewRoles = new SailViewRoles();

	/**
	 * Builds the views for a user that the scripts sign in as with the user's roles, instead of every role.
	 * Call it before running the scripts; see {@link SailViewRoles}.
	 *
	 * @param userName	the user name as a script signs in with it; may contain {@code {run}}
	 * @param roleNames	the user's roles: {@code module.Role}, or a customer role's name
	 */
	protected final void viewRoles(@Nonnull String userName, @Nonnull String... roleNames) {
		viewRoles.register(userName, roleNames);
	}

	/** The variables every automation of this test starts with; see {@link #variable(String, String)}. */
	private final java.util.Map<String, String> variables = new java.util.TreeMap<>();

	/**
	 * Defines a variable that every script this test runs starts with, for {@code {var:name}}. A recorded script signs
	 * in with {@code password="{var:password}"}: define {@code password} to run it unedited.
	 *
	 * @param name	the variable name
	 * @param value	the value
	 */
	protected final void variable(@Nonnull String name, @Nonnull String value) {
		variables.put(name, value);
	}

	@BeforeEach
	void setupBrowser() {
		// One SAIL test at a time per server, even across forked JVMs (see SailServerLock): may wait for another to finish
		SailServerLock.acquire(configuration.getBaseUrl());
		selenium.startBrowser(configuration);
	}

	@AfterAll
	static void releaseServer() {
		SailServerLock.releaseAll();
	}
	
	@AfterEach
	void tearDownBrowser() {
		selenium.stopBrowser();
	}
	
	protected void sailFile(String filePath) {
		Automation automation = XMLMetaData.unmarshalSAILFile(filePath);
		sail(automation);
	}
	
	/**
	 * Runs the SAIL script at the given test classpath resource.
	 *
	 * @param resourceName	the resource path, e.g. {@code sail/admin/pf/pf-admin-sail.xml}
	 * @throws IllegalArgumentException if the resource is missing or unreadable
	 */
	protected void sailResource(@Nonnull String resourceName) {
		try (InputStream stream = Thread.currentThread().getContextClassLoader().getResourceAsStream(resourceName)) {
			if (stream == null) {
				throw new IllegalArgumentException("SAIL resource " + resourceName + " is not on the test classpath");
			}
			String sail = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
			Automation automation = XMLMetaData.unmarshalSAILString(sail);
			sail(automation);
		} catch (IOException e) {
			throw new IllegalArgumentException("Could not read resource " + resourceName, e);
		}
	}
	
	protected void sailString(String sail) {
		Automation automation = XMLMetaData.unmarshalSAILString(sail);
		sail(automation);
	}
	
	/**
	 * Runs the automation. The PrimeFaces executor rebuilds each view's component tree in this JVM to find the
	 * client ids of the widgets on the page, so it runs as a user with the same permissions as the bootstrap user
	 * that the browser signs in as (every role of every module), not as the test's super user. Otherwise actions that
	 * the browser's user cannot see (such as Save on a document they cannot create) would be in the tree, shift the
	 * generated ids, and the executor would find the wrong elements. A login as a user registered with
	 * {@link #viewRoles(String, String...)} builds the views with that user's roles instead.
	 */
	private void sail(Automation automation) {
		AbstractPersistence persistence = AbstractPersistence.get();
		User testUser = persistence.getUser();
		persistence.setUser(bootstrapLikeUser(testUser));
		try (FacesUtil.SailFacesContextScope ignored = FacesUtil.withSailFacesContextIfNeeded()) {
			PrimeFacesInterpretedWebDriverExecutor executor = new PrimeFacesInterpretedWebDriverExecutor(selenium);
			executor.useViewRoles(viewRoles);
			executor.useVariables(variables);
			automation.execute(executor);
		}
		finally {
			persistence.setUser(testUser);
		}
	}

	/**
	 * Creates a user of the test user's customer with every role of every module, as Skyve's bootstrap user has.
	 *
	 * @param testUser	the test's user, whose customer, name and id are copied
	 * @return the user
	 * @see SailUsers#bootstrapLikeUser(User)
	 */
	protected static @Nonnull UserImpl bootstrapLikeUser(@Nonnull User testUser) {
		return SailUsers.bootstrapLikeUser(testUser);
	}
}
