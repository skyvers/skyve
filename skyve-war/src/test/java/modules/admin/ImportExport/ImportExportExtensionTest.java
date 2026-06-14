package modules.admin.ImportExport;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import modules.admin.domain.ImportExportColumn;

@SuppressWarnings("static-method")
class ImportExportExtensionTest {
	@TempDir
	private Path tempDir;

	@Test
	void anyColumnHasExpressionWithNoColumnsReturnsFalse() {
		ImportExportExtension ext = new ImportExportExtension();
		assertFalse(ext.anyColumnHasExpression());
	}

	@Test
	void anyColumnHasExpressionReturnsTrueWhenAColumnShowsExpression() {
		ImportExportExtension ext = new ImportExportExtension();
		ImportExportColumn column = new ImportExportColumn();
		column.setBindingName(ImportExportUtil.EXPRESSION);
		ext.getImportExportColumns().add(column);

		assertTrue(ext.anyColumnHasExpression());
	}

	@Test
	void cleanupImportFileWithNullAbsolutePathDoesNothing() {
		ImportExportExtension ext = assertDoesNotThrow(ImportExportExtension::new);
		// getImportFileAbsolutePath() returns null → method exits immediately
		ext.cleanupImportFile();
	}

	@Test
	void cleanupImportFileDeletesExistingFileAndBaseFolder() throws Exception {
		Path baseFolder = tempDir.resolve("importExport");
		Files.createDirectories(baseFolder);
		Path upload = baseFolder.resolve("criteria.xlsx");
		Files.writeString(upload, "uploaded");
		TestImportExportExtension ext = new TestImportExportExtension(baseFolder);
		ext.setImportFileAbsolutePath(upload.toString());

		ext.cleanupImportFile();

		assertNull(ext.getImportFileAbsolutePath());
		assertFalse(Files.exists(upload));
		assertFalse(Files.exists(baseFolder));
	}

	private static class TestImportExportExtension extends ImportExportExtension {
		private static final long serialVersionUID = 1L;
		private final Path baseFolder;

		private TestImportExportExtension(Path baseFolder) {
			this.baseFolder = baseFolder;
		}

		@Override
		public String baseFolder() {
			return baseFolder.toString();
		}
	}
}
