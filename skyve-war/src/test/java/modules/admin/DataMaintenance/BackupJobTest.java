package modules.admin.DataMaintenance;

import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
public class BackupJobTest {

	@Test
	void cancelReturnsNull() {
		BackupJob job = new BackupJob();
		assertNull(job.cancel());
	}
}
