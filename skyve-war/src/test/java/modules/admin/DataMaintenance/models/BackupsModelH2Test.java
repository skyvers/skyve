package modules.admin.DataMaintenance.models;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.TreeMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.view.model.list.Page;

import modules.admin.domain.DownloadFolder;
import util.AbstractH2Test;

class BackupsModelH2Test extends AbstractH2Test {
	@TempDir
	private Path tempDir;

	private String savedExternalBackupClass;

	@BeforeEach
	void disableExternalBackups() {
		savedExternalBackupClass = UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS;
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = null;
	}

	@AfterEach
	void restoreExternalBackups() {
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = savedExternalBackupClass;
	}

	@Test
	@SuppressWarnings("static-method")
	void postConstructPopulatesDrivingDocumentColumnsAndProjections() {
		BackupsModel model = new BackupsModel();

		model.postConstruct(CORE.getCustomer(), false);

		assertThat(model.getDescription(), is("All DownloadFolders"));
		assertThat(model.getDrivingDocument().getName(), is(DownloadFolder.DOCUMENT_NAME));
		assertThat(model.getColumns().size(), is(2));
		assertThat(model.getColumns().get(0).getBinding(), is(DownloadFolder.namePropertyName));
		assertThat(model.getColumns().get(1).getBinding(), is(DownloadFolder.sizePropertyName));
		assertThat(model.getProjections().contains(Bean.DOCUMENT_ID), is(true));
		assertThat(model.getProjections().contains(DownloadFolder.sizePropertyName), is(true));
		assertNull(model.getFilter());
		assertNull(model.newFilter());
		model.putParameter("ignored", "ignored");
	}

	@Test
	void fetchFoldersReturnsReverseSortedPagedFoldersWithFourteenCharacterNamesOnly() throws Exception {
		Files.createDirectory(tempDir.resolve("20240101000000"));
		Files.createDirectory(tempDir.resolve("20240102000000"));
		Files.createDirectory(tempDir.resolve("short"));
		Files.writeString(tempDir.resolve("20240103000000"), "not a directory", StandardCharsets.UTF_8);

		Page page = BackupsModel.fetchFolders(tempDir.toString(), 0, 0);

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
	void fetchFoldersReturnsEmptyPageWhenDirectoryHasNoMatchingFolders() {
		Page page = BackupsModel.fetchFolders(tempDir.toString(), 0, 10);

		assertThat(page.getTotalRows(), is(0L));
		assertThat(page.getRows().isEmpty(), is(true));
		assertThat(page.getSummary().getBizModule(), is(DownloadFolder.MODULE_NAME));
	}

	@Test
	void fetchBackupsReturnsZipFilesSortedByNameWithSizeInMegabytes() throws Exception {
		Files.write(tempDir.resolve("alpha.zip"), new byte[1024 * 1024]);
		Files.write(tempDir.resolve("beta.zip"), new byte[2 * 1024 * 1024]);
		Files.writeString(tempDir.resolve("ignored.txt"), "ignored", StandardCharsets.UTF_8);

		Page page = BackupsModel.fetchBackups(tempDir.toString(), 0, 1);

		assertThat(page.getTotalRows(), is(2L));
		assertThat(page.getRows().size(), is(2));
		DynamicBean first = (DynamicBean) page.getRows().get(0);
		DynamicBean second = (DynamicBean) page.getRows().get(1);
		assertThat(first.get(DownloadFolder.namePropertyName), is("alpha.zip"));
		assertThat(first.get(DownloadFolder.sizePropertyName), is(Long.valueOf(1)));
		assertThat(second.get(DownloadFolder.namePropertyName), is("beta.zip"));
		assertThat(second.get(DownloadFolder.sizePropertyName), is(Long.valueOf(2)));
	}

	@Test
	void fetchBackupsReturnsEmptyPageWhenNoZipFilesExist() throws Exception {
		Files.writeString(tempDir.resolve("ignored.txt"), "ignored", StandardCharsets.UTF_8);

		Page page = BackupsModel.fetchBackups(tempDir.toString(), 0, 10);

		assertThat(page.getTotalRows(), is(0L));
		assertThat(page.getRows().isEmpty(), is(true));
		assertThat(page.getSummary().getBizDocument(), is(DownloadFolder.DOCUMENT_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void unsupportedListModelOperationsThrow() throws Exception {
		BackupsModel model = new BackupsModel();

		assertThrows(IllegalStateException.class, model::iterate);
		assertThrows(IllegalStateException.class, () -> model.update("id", new TreeMap<>()));
		assertThrows(IllegalStateException.class, () -> model.remove("id"));
	}
}
