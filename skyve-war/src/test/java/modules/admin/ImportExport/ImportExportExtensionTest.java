package modules.admin.ImportExport;

import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
public class ImportExportExtensionTest {

	@Test
	void anyColumnHasExpressionWithNoColumnsReturnsFalse() {
		ImportExportExtension ext = new ImportExportExtension();
		assertFalse(ext.anyColumnHasExpression());
	}

	@Test
	void cleanupImportFileWithNullAbsolutePathDoesNothing() {
		ImportExportExtension ext = new ImportExportExtension();
		// getImportFileAbsolutePath() returns null → method exits immediately
		ext.cleanupImportFile();
	}
}
