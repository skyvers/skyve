package modules.admin.ImportExport.actions;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.Collections;

import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.metadata.controller.Download;

import modules.admin.ImportExport.ImportExportExtension;
import modules.admin.domain.User;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class RunExportH2Test extends AbstractH2Test {
	@Test
	void prepareDoesNotMutateBean() throws Exception {
		ImportExportExtension bean = new ImportExportExtension();
		bean.setModuleName("admin");

		new RunExport().prepare(bean, null);

		assertThat(bean.getModuleName(), is("admin"));
	}

	@Test
	void generateDownloadReturnsSpreadsheetDownloadForConfiguredDocument() throws Exception {
		ImportExportExtension bean = new ImportExportExtension();
		bean.setModuleName(User.MODULE_NAME);
		bean.setDocumentName(User.DOCUMENT_NAME);

		Download download = RunExport.generateDownload(bean, Collections.emptyList(), Boolean.TRUE, Boolean.TRUE);

		assertThat(download.getFileName(), containsString(User.MODULE_NAME + "_" + User.DOCUMENT_NAME));
		assertThat(download.getBytes(), is(notNullValue()));
		assertThat(download.getMimeType(), is(MimeType.xlsx));
	}
}
