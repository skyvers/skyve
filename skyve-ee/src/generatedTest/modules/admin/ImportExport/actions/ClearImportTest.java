package modules.admin.ImportExport.actions;

import modules.admin.ImportExport.ImportExportExtension;
import modules.admin.domain.ImportExport;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class ClearImportTest extends AbstractActionTest<ImportExportExtension, ClearImport> {

	@Override
	protected ClearImport getAction() {
		return new ClearImport();
	}

	@Override
	protected ImportExportExtension getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(ImportExport.MODULE_NAME, ImportExport.DOCUMENT_NAME);
	}
}