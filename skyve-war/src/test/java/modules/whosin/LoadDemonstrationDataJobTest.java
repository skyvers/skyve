package modules.whosin;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class LoadDemonstrationDataJobTest extends AbstractH2Test {
	@Test
	void cancelReturnsNull() {
		assertNull(new LoadDemonstrationDataJob().cancel());
	}

	@Test
	void executeAfterCancelLogsCaughtSetupFailure() throws Exception {
		LoadDemonstrationDataJob job = new LoadDemonstrationDataJob();
		job.cancel();

		job.execute();

		assertEquals(0, job.getPercentComplete());
		assertThat(job.getLog().get(0), containsString("Started Loading Demonstration Data"));
		assertThat(job.getLog().get(1), containsString("Encountered an error during the job run"));
	}
}
