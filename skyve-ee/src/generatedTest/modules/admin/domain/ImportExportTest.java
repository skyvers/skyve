package modules.admin.domain;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class ImportExportTest extends AbstractDomainTest<ImportExport> {

	@Override
	protected ImportExport getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(ImportExport.MODULE_NAME, ImportExport.DOCUMENT_NAME);
	}
}