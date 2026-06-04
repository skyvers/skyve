package modules.admin.ImportExport;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;

import modules.admin.domain.ImportExport;
import modules.admin.domain.ImportExport.Mode;
import modules.admin.domain.ImportExportColumn;
import modules.admin.domain.User;
import modules.admin.domain.UserProxy;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class ImportExportServiceH2Test extends AbstractH2Test {
	@Test
	void generateColumnsReturnsPersistentScalarDocumentAttributes() {
		ImportExportExtension bean = ImportExport.newInstance();
		bean.setModuleName(User.MODULE_NAME);
		bean.setDocumentName(User.DOCUMENT_NAME);

		List<ImportExportColumn> columns = new ImportExportService().generateColumns(bean);

		assertTrue(columns.stream().anyMatch(column -> UserProxy.userNamePropertyName.equals(column.getBindingName())));
		assertTrue(columns.stream().anyMatch(column -> column.getColumnName() != null));
	}

	@Test
	void updateColumnsForDocumentNameRebuildsExportColumns() throws Exception {
		ImportExportExtension bean = ImportExport.newInstance();
		bean.setMode(Mode.exportData);
		bean.setModuleName(User.MODULE_NAME);
		bean.setDocumentName(User.DOCUMENT_NAME);
		bean.addImportExportColumnsElement(ImportExportColumn.newInstance());

		new ImportExportService().updateColumns(ImportExport.documentNamePropertyName, bean);

		assertFalse(bean.getImportExportColumns().isEmpty());
		assertTrue(bean.getImportExportColumns().stream()
				.anyMatch(column -> UserProxy.userNamePropertyName.equals(column.getBindingName())));
	}

	@Test
	void updateColumnsIgnoresUnrelatedSource() throws Exception {
		ImportExportExtension bean = ImportExport.newInstance();
		bean.setMode(Mode.exportData);
		bean.setModuleName(User.MODULE_NAME);
		bean.setDocumentName(User.DOCUMENT_NAME);

		new ImportExportService().updateColumns("ignored", bean);

		assertTrue(bean.getImportExportColumns().isEmpty());
	}
}
