package modules.admin.DataMaintenance.models;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.backup.ExternalBackup;
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
	@SuppressWarnings({"static-method", "null"}) // a null here fails the test
	void postConstructPopulatesDrivingDocumentColumnsAndProjections() {
		BackupsModel model = new BackupsModel();

		model.postConstruct(CORE.getCustomer(), false);

		assertThat(model.getDescription(), is("All DownloadFolders"));
		assertThat(model.getDrivingDocument().getName(), is(DownloadFolder.DOCUMENT_NAME));
		assertEquals(2, model.getColumns().size());
		assertThat(model.getColumns().get(0).getBinding(), is(DownloadFolder.namePropertyName));
		assertThat(model.getColumns().get(1).getBinding(), is(DownloadFolder.sizePropertyName));
		assertTrue(model.getProjections().contains(Bean.DOCUMENT_ID));
		assertTrue(model.getProjections().contains(DownloadFolder.sizePropertyName));
		assertNull(model.getFilter());
		assertNull(model.newFilter());
		model.putParameter("ignored", "ignored");
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void fetchFoldersReturnsReverseSortedPagedFoldersWithFourteenCharacterNamesOnly() throws Exception {
		Files.createDirectory(tempDir.resolve("20240101000000"));
		Files.createDirectory(tempDir.resolve("20240102000000"));
		Files.createDirectory(tempDir.resolve("short"));
		Files.writeString(tempDir.resolve("20240103000000"), "not a directory", StandardCharsets.UTF_8);

		Page page = BackupsModel.fetchFolders(tempDir.toString(), 0, 0);

		assertEquals(2L, page.getTotalRows());
		assertEquals(1, page.getRows().size());
		Bean row = page.getRows().get(0);
		assertThat(row, instanceOf(DynamicBean.class));
		DynamicBean dynamicRow = (DynamicBean) row;
		assertThat(dynamicRow.getBizModule(), is(DownloadFolder.MODULE_NAME));
		assertThat(dynamicRow.getBizDocument(), is(DownloadFolder.DOCUMENT_NAME));
		assertThat(dynamicRow.get(DownloadFolder.namePropertyName), is("20240102000000"));
		assertThat(page.getSummary().getBizDocument(), is(DownloadFolder.DOCUMENT_NAME));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void fetchFoldersReturnsEmptyPageWhenDirectoryHasNoMatchingFolders() {
		Page page = BackupsModel.fetchFolders(tempDir.toString(), 0, 10);

		assertEquals(0L, page.getTotalRows());
		assertTrue(page.getRows().isEmpty());
		assertThat(page.getSummary().getBizModule(), is(DownloadFolder.MODULE_NAME));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void fetchBackupsListsTheNewestBackupFirstWithSizeInMegabytes() throws Exception {
		// Neither ascending nor descending name order is newest first here
		Path oldest = Files.write(tempDir.resolve("DAILY_20240101000000.zip"), new byte[1024 * 1024]);
		Path middle = Files.write(tempDir.resolve("DAILY_20240102000000.zip"), new byte[2 * 1024 * 1024]);
		Path newest = Files.write(tempDir.resolve("20240103000000.zip"), new byte[3 * 1024 * 1024]);
		Files.setLastModifiedTime(oldest, FileTime.fromMillis(1_000_000_000L));
		Files.setLastModifiedTime(middle, FileTime.fromMillis(2_000_000_000L));
		Files.setLastModifiedTime(newest, FileTime.fromMillis(3_000_000_000L));
		Files.writeString(tempDir.resolve("ignored.txt"), "ignored", StandardCharsets.UTF_8);

		Page page = BackupsModel.fetchBackups(tempDir.toString(), 0, 2);

		assertEquals(3L, page.getTotalRows());
		assertEquals(List.of("20240103000000.zip", "DAILY_20240102000000.zip", "DAILY_20240101000000.zip"), names(page));
		DynamicBean first = (DynamicBean) page.getRows().get(0);
		DynamicBean third = (DynamicBean) page.getRows().get(2);
		assertThat(first.get(DownloadFolder.sizePropertyName), is(Long.valueOf(3)));
		assertThat(third.get(DownloadFolder.sizePropertyName), is(Long.valueOf(1)));
	}

	@Test
	void fetchBackupsListsBackupsModifiedAtTheSameTimeByNameDescending() throws Exception {
		FileTime sameTime = FileTime.fromMillis(2_000_000_000L);
		Files.setLastModifiedTime(Files.write(tempDir.resolve("DAILY_20240101000000.zip"), new byte[0]), sameTime);
		Files.setLastModifiedTime(Files.write(tempDir.resolve("WEEKLY_20240101.zip"), new byte[0]), sameTime);

		Page page = BackupsModel.fetchBackups(tempDir.toString(), 0, 10);

		assertEquals(List.of("WEEKLY_20240101.zip", "DAILY_20240101000000.zip"), names(page));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void fetchBackupsKeepsTheNewestFirstOrderOfExternalBackups() throws Exception {
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = NewestFirstExternalBackup.class.getName();

		Page page = BackupsModel.fetchBackups(tempDir.toString(), 0, 10);

		assertEquals(NewestFirstExternalBackup.BACKUPS, names(page));
		DynamicBean first = (DynamicBean) page.getRows().get(0);
		assertThat(first.get(DownloadFolder.sizePropertyName), is(Long.valueOf(2)));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void fetchBackupsReturnsEmptyPageWhenNoZipFilesExist() throws Exception {
		Files.writeString(tempDir.resolve("ignored.txt"), "ignored", StandardCharsets.UTF_8);

		Page page = BackupsModel.fetchBackups(tempDir.toString(), 0, 10);

		assertEquals(0L, page.getTotalRows());
		assertTrue(page.getRows().isEmpty());
		assertThat(page.getSummary().getBizDocument(), is(DownloadFolder.DOCUMENT_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void unsupportedListModelOperationsThrow() {
		BackupsModel model = new BackupsModel();
		TreeMap<String, Object> properties = new TreeMap<>();

		assertThrows(IllegalStateException.class, model::iterate);
		assertThrows(IllegalStateException.class, () -> model.update("id", properties));
		assertThrows(IllegalStateException.class, () -> model.remove("id"));
	}

	@SuppressWarnings("null") // a null here fails the test
	private static List<Object> names(Page page) {
		List<Object> result = new ArrayList<>();
		for (Bean row : page.getRows()) {
			result.add(((DynamicBean) row).get(DownloadFolder.namePropertyName));
		}
		return result;
	}

	/**
	 * Lists its backups newest first, as {@link ExternalBackup#listBackups()} promises, in an order that is not
	 * name order. {@link ExternalBackup#getInstance()} instantiates it by name.
	 */
	public static class NewestFirstExternalBackup implements ExternalBackup {
		static final List<String> BACKUPS = List.of("b_newest.zip", "c_middle.zip", "a_oldest.zip");

		@Override
		public List<String> listBackups() {
			return new ArrayList<>(BACKUPS);
		}

		@Override
		public boolean exists(String backupName) {
			return BACKUPS.contains(backupName);
		}

		@Override
		public void downloadBackup(String backupName, OutputStream outputStream) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void uploadBackup(String backupFilePath) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void deleteBackup(String backupName) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void copyBackup(String srcBackupName, String destBackupName) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void moveBackup(String srcBackupName, String destBackupName) {
			throw new UnsupportedOperationException();
		}

		@Override
		public long getFileSize(String fileName) {
			return 2L * 1024L * 1024L;
		}
	}
}
