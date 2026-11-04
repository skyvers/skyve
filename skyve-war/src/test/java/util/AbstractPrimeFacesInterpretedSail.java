package util;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import util.sail.BrowserConfiguration;
import util.sail.PrimeFacesInterpretedWebDriverExecutor;
import util.sail.PrimeFacesSelenide;

/**
 * Base test class for running SAIL scripts against a PrimeFaces UI in an interpreted browser session.
 */
public abstract class AbstractPrimeFacesInterpretedSail extends AbstractH2Test {

	private BrowserConfiguration configuration;
	@Nonnull protected PrimeFacesSelenide selenium = new PrimeFacesSelenide();
	
	protected AbstractPrimeFacesInterpretedSail(@Nonnull BrowserConfiguration configuration) {
		this.configuration = configuration;
	}
	
	@BeforeEach
	void setupBrowser() {
		selenium.startBrowser(configuration);
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
	 * generated ids, and the executor would find the wrong elements.
	 */
	private void sail(Automation automation) {
		AbstractPersistence persistence = AbstractPersistence.get();
		User testUser = persistence.getUser();
		persistence.setUser(bootstrapLikeUser(testUser));
		try (FacesUtil.SailFacesContextScope ignored = FacesUtil.withSailFacesContextIfNeeded()) {
			automation.execute(new PrimeFacesInterpretedWebDriverExecutor(selenium));
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
	 */
	protected static @Nonnull UserImpl bootstrapLikeUser(@Nonnull User testUser) {
		UserImpl result = new UserImpl();
		result.setCustomerName(testUser.getCustomerName());
		result.setName(testUser.getName());
		result.setId(testUser.getId());
		result.setContactId(testUser.getContactId());
		result.setContactName(testUser.getContactName());
		result.setDataGroupId(testUser.getDataGroupId());
		result.setHomeModuleName(testUser.getHomeModuleName());
		result.setLanguageTag(testUser.getLanguageTag());
		Customer customer = testUser.getCustomer();
		for (Module module : customer.getModules()) {
			for (Role role : module.getRoles()) {
				result.addRole((RoleImpl) role);
			}
		}
		return result;
	}
}
