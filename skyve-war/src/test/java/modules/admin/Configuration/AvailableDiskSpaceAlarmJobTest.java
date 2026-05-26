package modules.admin.Configuration;

import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
public class AvailableDiskSpaceAlarmJobTest {

	@Test
	void cancelReturnsNull() {
		AvailableDiskSpaceAlarmJob job = new AvailableDiskSpaceAlarmJob();
		assertNull(job.cancel());
	}
}
