package modules.admin.Communication.models;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.metadata.view.model.list.Page;

import modules.admin.domain.Communication;
import modules.admin.domain.DownloadFolder;
import util.AbstractH2Test;

class BatchesModelH2Test extends AbstractH2Test {
	@TempDir
	private Path tempDir;

	@Test
	@SuppressWarnings("static-method")
	void postConstructPopulatesDrivingDocumentColumnsAndProjections() {
		BatchesModel model = new BatchesModel();

		model.postConstruct(CORE.getCustomer(), false);

		assertThat(model.getDescription(), is("All DownloadFolders"));
		assertThat(model.getDrivingDocument().getName(), is(DownloadFolder.DOCUMENT_NAME));
		assertThat(model.getColumns().size(), is(1));
		assertThat(model.getColumns().get(0).getBinding(), is(DownloadFolder.namePropertyName));
		assertThat(model.getProjections().contains(Bean.DOCUMENT_ID), is(true));
		assertThat(model.getProjections().contains(DownloadFolder.namePropertyName), is(true));
		assertNull(model.getFilter());
		assertNull(model.newFilter());
		model.putParameter("ignored", "ignored");
	}

	@Test
	void fetchReturnsReverseSortedPagedFoldersFromCommunicationBasePath() throws Exception {
		Files.createDirectory(tempDir.resolve("20240101000000"));
		Files.createDirectory(tempDir.resolve("20240102000000"));
		Files.createDirectory(tempDir.resolve("short"));
		Files.writeString(tempDir.resolve("20240103000000"), "not a directory", StandardCharsets.UTF_8);

		Communication communication = new TestCommunication();
		communication.setBasePath(tempDir.toString());

		BatchesModel model = new BatchesModel();
		model.setBean(communication);
		model.setStartRow(0);
		model.setEndRow(0);

		Page page = model.fetch();

		assertThat(page.getTotalRows(), is(2L));
		assertThat(page.getRows().size(), is(1));
		Bean row = page.getRows().get(0);
		assertThat(row, instanceOf(DynamicBean.class));
		DynamicBean dynamicRow = (DynamicBean) row;
		assertThat(dynamicRow.getBizModule(), is(DownloadFolder.MODULE_NAME));
		assertThat(dynamicRow.getBizDocument(), is(DownloadFolder.DOCUMENT_NAME));
		assertThat(dynamicRow.get(DownloadFolder.namePropertyName), is("20240102000000"));
		assertThat(page.getSummary().getBizDocument(), is(DownloadFolder.DOCUMENT_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void unsupportedListModelOperationsThrow() throws Exception {
		BatchesModel model = new BatchesModel();

		assertThrows(IllegalStateException.class, model::iterate);
		assertThrows(IllegalStateException.class, () -> model.update("id", new TreeMap<>()));
		assertThrows(IllegalStateException.class, () -> model.remove("id"));
	}

	private static final class TestCommunication extends Communication {
		private static final long serialVersionUID = 1L;

		@Override
		public void setTemplate(org.skyve.domain.app.admin.CommunicationTemplate template) {
			if (template instanceof modules.admin.domain.CommunicationTemplate communicationTemplate) {
				setTemplate(communicationTemplate);
			}
		}
	}
}
