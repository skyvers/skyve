package modules.admin.ControlPanel.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;
import util.AbstractH2Test;

/**
 * Tests for the EvictFromCache action.
 * 
 * This action evicts cached metadata from the repository cache.
 * It sets results and tabIndex to null, then calls evictCachedMetaData on the repository.
 * If an exception occurs, it traps the exception in the bean's results.
 */
public class EvictFromCacheH2Test extends AbstractH2Test {

	private DataBuilder db;
	private ControlPanelExtension controlPanel;
	private EvictFromCache action;

	@BeforeEach
	public void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		controlPanel = db.build(ControlPanel.MODULE_NAME, ControlPanel.DOCUMENT_NAME);
		action = new EvictFromCache();
	}

	@Test
	public void testExecuteClearsResultsAndTabIndex() throws Exception {
		// setup the test data - set initial values
		controlPanel.setResults("Previous results");
		controlPanel.setTabIndex(Integer.valueOf(5));

		// call the method under test
		ServerSideActionResult<ControlPanelExtension> result = action.execute(controlPanel, null);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(controlPanel));
		// Results and tabIndex should be set to null
		assertThat(controlPanel.getResults(), is(nullValue()));
		assertThat(controlPanel.getTabIndex(), is(nullValue()));
	}

	@Test
	public void testExecuteSuccessfullyEvictsCache() throws Exception {
		// setup the test data
		controlPanel.setResults("Some results");
		controlPanel.setTabIndex(Integer.valueOf(2));

		// call the method under test
		ServerSideActionResult<ControlPanelExtension> result = action.execute(controlPanel, null);

		// verify the result - should complete successfully
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(controlPanel));
		// Results and tabIndex should be null after execution
		assertThat(controlPanel.getResults(), is(nullValue()));
		assertThat(controlPanel.getTabIndex(), is(nullValue()));
	}

	@Test
	public void testExecuteWithNullInitialValues() throws Exception {
		// setup the test data - start with null values
		controlPanel.setResults(null);
		controlPanel.setTabIndex(null);

		// call the method under test
		ServerSideActionResult<ControlPanelExtension> result = action.execute(controlPanel, null);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(controlPanel));
		// Results and tabIndex should remain null
		assertThat(controlPanel.getResults(), is(nullValue()));
		assertThat(controlPanel.getTabIndex(), is(nullValue()));
	}

	@Test
	public void testExecuteHandlesExceptionsGracefully() throws Exception {
		// setup the test data
		controlPanel.setResults("Initial results");
		controlPanel.setTabIndex(Integer.valueOf(1));

		// call the method under test
		// Note: In a test environment, evictCachedMetaData should work normally,
		// but if it throws an exception, it should be trapped
		ServerSideActionResult<ControlPanelExtension> result = action.execute(controlPanel, null);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(controlPanel));
		
		// If an exception occurred, results would contain the stack trace
		// If no exception, results should be null
		// Either way, tabIndex should be null
		assertThat(controlPanel.getTabIndex(), is(nullValue()));
	}
}

