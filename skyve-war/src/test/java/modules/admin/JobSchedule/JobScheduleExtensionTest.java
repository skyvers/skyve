package modules.admin.JobSchedule;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class JobScheduleExtensionTest {

	@Test
	void toJobScheduleMapsBizIdAndJobName() {
		JobScheduleExtension ext = new JobScheduleExtension();
		ext.setJobName("admin.jReindex");
		ext.setCronExpression("0 0 12 * * ?");

		org.skyve.job.JobSchedule result = ext.toJobSchedule();

		assertNotNull(result);
		assertEquals("admin.jReindex", result.getJobName());
		assertEquals("0 0 12 * * ?", result.getCronExpression());
	}

	@Test
	void bizKeyWithNullJobNameReturnsEmptyString() {
		JobScheduleExtension ext = new JobScheduleExtension();
		// getJobName() is null → NPE caught → returns ""
		assertEquals("", ext.bizKey());
	}

	@Test
	void getScheduleStringForValidCronExpression() {
		JobScheduleExtension ext = new JobScheduleExtension();
		ext.setCronExpression("0 0 12 * * ?");
		// Should return a human-readable description without throwing
		String result = ext.getScheduleString();
		assertNotNull(result);
	}
}
