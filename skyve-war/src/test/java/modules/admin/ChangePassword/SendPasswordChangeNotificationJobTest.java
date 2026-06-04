package modules.admin.ChangePassword;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class SendPasswordChangeNotificationJobTest extends AbstractH2Test {
	@Test
	void persistJobExecutionOnSuccessReturnsFalse() {
		assertFalse(new SendPasswordChangeNotificationJob().persistJobExecutionOnSuccess());
	}

	@Test
	void executeWithoutConfiguredEmailOrUserLogsFailureAndStopsAtZeroPercent() throws Exception {
		SendPasswordChangeNotificationJob job = new SendPasswordChangeNotificationJob();

		job.execute();

		assertEquals(0, job.getPercentComplete());
		assertEquals(1, job.getLog().size());
		assertThat(job.getLog().get(0), containsString("Failed to send password change notification"));
	}
}
