package modules.admin.ImportExport;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.ImportExport.actions.ResetColumns;
import modules.admin.ImportExport.actions.RunImport;
import modules.admin.domain.Contact;
import modules.admin.domain.ImportExport;

@SkyveFactory(excludedActions = { RunImport.class, ResetColumns.class })
public class ImportExportFactory {

	@SkyveFixture(types = FixtureType.crud)
	@SuppressWarnings("static-method")
	public ImportExportExtension crudInstance() {
		ImportExportExtension bean = new DataBuilder().fixture(FixtureType.crud).factoryBuild(ImportExport.MODULE_NAME,
				ImportExport.DOCUMENT_NAME);

		bean.setModuleName(Contact.MODULE_NAME);
		bean.setDocumentName(Contact.DOCUMENT_NAME);

		return bean;
	}
}
