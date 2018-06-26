package modules.admin.ImportExport.actions;

import modules.admin.domain.ImportExport;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class RunImportTest extends AbstractActionTest<ImportExport, RunImport> {

	@Override
	protected RunImport getAction() {
		return new RunImport();
	}

	@Override
	protected ImportExport getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(ImportExport.MODULE_NAME, ImportExport.DOCUMENT_NAME);
	}
}