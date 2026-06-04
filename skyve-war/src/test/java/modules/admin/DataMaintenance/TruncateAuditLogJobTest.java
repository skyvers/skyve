package modules.admin.DataMaintenance;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class TruncateAuditLogJobTest extends AbstractH2Test {
	@Test
	void cancelReturnsNull() {
		assertNull(new TruncateAuditLogJob().cancel());
	}

	@Test
	void executeWithoutBeanCompletesAndLogsStartAndFinish() throws Exception {
		TruncateAuditLogJob job = new TruncateAuditLogJob();

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertEquals(2, job.getLog().size());
		assertThat(job.getLog().get(0), containsString("Started Truncation Job"));
		assertThat(job.getLog().get(1), containsString("Finished Truncation Job"));
	}
}
