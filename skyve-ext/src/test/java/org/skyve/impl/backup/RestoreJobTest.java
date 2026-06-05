package org.skyve.impl.backup;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;

@SuppressWarnings("static-method")
class RestoreJobTest {
	@Test
	void executeWithWrongBeanTypeLogsDataMaintenanceInstruction() throws Exception {
		RestoreJob job = new RestoreJob();
		job.setBean(mock(Bean.class));

		job.execute();

		assertEquals("Kick off the job with the appropriate options from the Data Maintenance page.", job.getLog().get(0));
	}
}
