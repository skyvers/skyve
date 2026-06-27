package modules.admin.DataMaintenance;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import modules.test.domain.AllAttributesPersistent;
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

	@Test
	void executeWithIncludedDocumentCountsAndLogsCompletion() throws Exception {
		DataMaintenanceExtension bean = new DataMaintenanceExtension();
		bean.setNotification(Boolean.FALSE);
		bean.setRefreshOption(modules.admin.domain.DataMaintenance.RefreshOption.upsert);
		bean.setEvictOption(modules.admin.domain.DataMaintenance.EvictOption.none);
		modules.admin.domain.ModuleDocument doc = modules.admin.domain.ModuleDocument.newInstance();
		doc.setInclude(Boolean.TRUE);
		doc.setModuleName(AllAttributesPersistent.MODULE_NAME);
		doc.setDocumentName(AllAttributesPersistent.DOCUMENT_NAME);
		bean.getRefreshDocuments().add(doc);
		RefreshDocumentTuplesJob job = new RefreshDocumentTuplesJob();
		job.setBean(bean);

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertThat(job.getLog().get(1), containsString(AllAttributesPersistent.MODULE_NAME));
		assertThat(job.getLog().get(1), containsString(AllAttributesPersistent.DOCUMENT_NAME));
		assertThat(job.getLog().get(1), containsString("Completed"));
	}
}
