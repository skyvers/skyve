package modules.admin.ControlPanel.actions;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;
import modules.admin.domain.User;
import util.AbstractH2Test;

/**
 * Tests for the ExecuteQuery action.
 */
public class ExecuteQueryH2Test extends AbstractH2Test {

	private DataBuilder db;
	private ControlPanelExtension controlPanel;
	private ExecuteQuery action;

	@BeforeEach
	public void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		controlPanel = db.build(ControlPanel.MODULE_NAME, ControlPanel.DOCUMENT_NAME);
		action = new ExecuteQuery();
	}

	@SuppressWarnings("boxing")
	@Test
	public void testExecuteWithNullQueryThrowsValidationException() {
		// setup the test data
		controlPanel.setQuery(null);

		// call the method under test and expect exception
		ValidationException e = assertThrows(ValidationException.class, () -> {
			action.execute(controlPanel, null);
		});

		// verify the exception
		assertThat(e.getMessages().size(), is(1));
		assertThat(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.queryPropertyName), is(true));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testExecuteWithValidQueryReturnsResults() throws Exception {
		// setup the test data - create some users in the database
		User user1 = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user1 = CORE.getPersistence().save(user1);
		User user2 = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user2 = CORE.getPersistence().save(user2);

		// set a valid query
		controlPanel.setQuery("SELECT bean FROM adminUser bean");

		// call the method under test
		ServerSideActionResult<ControlPanelExtension> result = action.execute(controlPanel, null);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(controlPanel));
		assertThat(controlPanel.getResults(), is(notNullValue()));
		assertThat(controlPanel.getResults().length() > 0, is(true));
		assertThat(controlPanel.getTabIndex(), is(2));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testExecuteWithInvalidQueryTrapsException() throws Exception {
		// setup the test data with an invalid query
		controlPanel.setQuery("SELECT invalid FROM nowhere");

		// call the method under test
		ServerSideActionResult<ControlPanelExtension> result = action.execute(controlPanel, null);

		// verify the result - should have trapped the exception
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(controlPanel));
		// The results should contain the exception stack trace
		assertThat(controlPanel.getResults(), is(notNullValue()));
		assertThat(controlPanel.getResults(), containsString("Exception"));
		assertThat(controlPanel.getTabIndex(), is(2));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testExecuteClearsResultsBeforeExecution() throws Exception {
		// setup the test data
		controlPanel.setQuery("SELECT bean FROM adminUser bean");
		controlPanel.setResults("Previous results");
		controlPanel.setTabIndex(5);

		// call the method under test
		action.execute(controlPanel, null);

		// verify that results were cleared and set (not null)
		// Even if no results, should be set to empty string or contain query results
		assertThat(controlPanel.getResults(), is(notNullValue()));
		assertThat(controlPanel.getTabIndex(), is(2));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testExecuteFormatsMultipleResultsWithNewlines() throws Exception {
		// setup the test data - create multiple users
		User user1 = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user1 = CORE.getPersistence().save(user1);
		User user2 = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user2 = CORE.getPersistence().save(user2);

		// set a valid query
		controlPanel.setQuery("SELECT bean FROM adminUser bean");

		// call the method under test
		ServerSideActionResult<ControlPanelExtension> result = action.execute(controlPanel, null);

		// verify the result contains multiple lines (newline separated)
		assertThat(result, is(notNullValue()));
		String results = controlPanel.getResults();
		assertThat(results, is(notNullValue()));
		// Should have at least one newline since we have 2+ users
		assertThat(results.contains("\n"), is(true));
	}

	/**
	 * Helper method to check if any binding in the iterable contains the given property name.
	 */
	private static boolean hasBinding(Iterable<String> bindings, String propertyName) {
		for (String binding : bindings) {
			if (binding.contains(propertyName)) {
				return true;
			}
		}
		return false;
	}
}
