package modules.admin.Configuration;

import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import util.AbstractSkyveTest;

@SuppressWarnings("static-method")
public class AvailableDiskSpaceAlarmJobTest extends AbstractSkyveTest {

	@Test
	void cancelReturnsNull() {
		AvailableDiskSpaceAlarmJob job = new AvailableDiskSpaceAlarmJob();
		assertNull(job.cancel());
	}
}
