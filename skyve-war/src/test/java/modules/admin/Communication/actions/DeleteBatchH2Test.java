package modules.admin.Communication.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.File;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import jakarta.inject.Inject;
import modules.admin.Communication.CommunicationExtension;
import modules.admin.domain.Communication;
import util.AbstractH2Test;

/**
 * Tests for the DeleteBatch action.
 */
public class DeleteBatchH2Test extends AbstractH2Test {

	private DataBuilder db;
	private CommunicationExtension communication;
	private File testBatchDir;
	
	@Inject
	private DeleteBatch action;

	@BeforeEach
	public void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);

		communication = db.build(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
	}

	@AfterEach
	public void cleanup() {
		// Clean up test batch directory if it exists
		if (testBatchDir != null && testBatchDir.exists()) {
			testBatchDir.delete();
		}
	}

	@Test
	@SuppressWarnings("boxing")
	public void testExecuteSetsRefreshBatchesToTrue() throws Exception {
		// setup the test data
		setupCommunicationWithBasePath();
		
		// call the method under test
		ServerSideActionResult<Communication> result = action.execute(communication, null);
		
		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean().getRefreshBatches(), is(Boolean.TRUE));
	}

	@Test
	public void testExecuteClearsSelectedBatchTimestampFolderName() throws Exception {
		// setup the test data
		setupCommunicationWithBasePath();
		communication.setSelectedBatchTimestampFolderName("batch-folder");
		
		// call the method under test
		ServerSideActionResult<Communication> result = action.execute(communication, null);
		
		// verify the selected batch is cleared
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean().getSelectedBatchTimestampFolderName(), is(nullValue()));
	}

	@Test
	public void testExecuteDeletesExistingBatchDirectory() throws Exception {
		// setup the test data with an actual batch directory
		setupCommunicationWithBasePath();
		String batchFolderName = "test-batch-" + System.currentTimeMillis();
		testBatchDir = new File(communication.getBasePath() + File.separator + batchFolderName);
		
		// Create the test batch directory
		if (!testBatchDir.exists()) {
			testBatchDir.mkdirs();
		}
		assertThat(testBatchDir.exists(), is(true));
		
		communication.setSelectedBatchTimestampFolderName(batchFolderName);
		
		// call the method under test
		ServerSideActionResult<Communication> result = action.execute(communication, null);
		
		// verify the batch directory is deleted
		assertThat(result, is(notNullValue()));
		assertThat(testBatchDir.exists(), is(false));
	}

	@Test
	public void testExecuteWithNonExistentBatchDirectory() throws Exception {
		// setup the test data with a non-existent batch directory
		setupCommunicationWithBasePath();
		communication.setSelectedBatchTimestampFolderName("non-existent-batch");
		
		// call the method under test - should not throw exception
		ServerSideActionResult<Communication> result = action.execute(communication, null);
		
		// verify the result is successful even with non-existent directory
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean().getSelectedBatchTimestampFolderName(), is(nullValue()));
	}

	/**
	 * Helper method to set up a communication with a valid base path.
	 */
	private void setupCommunicationWithBasePath() {
		// Use system temp directory for testing
		String tempDir = System.getProperty("java.io.tmpdir");
		communication.setBasePath(tempDir);
	}
}
