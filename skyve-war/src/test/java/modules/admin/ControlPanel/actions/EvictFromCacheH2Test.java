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
}

