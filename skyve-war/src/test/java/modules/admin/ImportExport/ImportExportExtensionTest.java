package modules.admin.ImportExport;

import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;

@SuppressWarnings("static-method")
class ImportExportExtensionTest {

	@Test
	void anyColumnHasExpressionWithNoColumnsReturnsFalse() {
		ImportExportExtension ext = new ImportExportExtension();
		assertFalse(ext.anyColumnHasExpression());
	}

	@Test
	void cleanupImportFileWithNullAbsolutePathDoesNothing() {
		ImportExportExtension ext = Assertions.assertDoesNotThrow(ImportExportExtension::new);
		// getImportFileAbsolutePath() returns null → method exits immediately
		ext.cleanupImportFile();
	}
}
