package modules.admin.DataMaintenance;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class RefreshDocumentTuplesJobTest extends AbstractH2Test {
	@Test
	void cancelReturnsNull() {
		assertNull(new RefreshDocumentTuplesJob().cancel());
	}

	@Test
	void executeWithNoRefreshDocumentsCompletesAndLogsLifecycle() throws Exception {
		DataMaintenanceExtension bean = new DataMaintenanceExtension();
		bean.setNotification(Boolean.FALSE);
		RefreshDocumentTuplesJob job = new RefreshDocumentTuplesJob();
		job.setBean(bean);

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertEquals(2, job.getLog().size());
		assertThat(job.getLog().get(0), containsString("Started Document Data Refresh Job"));
		assertThat(job.getLog().get(1), containsString("Finished Document Data Refresh Job"));
	}
}
