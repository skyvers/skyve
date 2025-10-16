package modules.admin.ImportExport;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

import jakarta.enterprise.inject.Default;
import modules.admin.ImportExport.actions.UploadSimpleImportDataFile;
import modules.admin.domain.ImportExport;
import modules.admin.domain.ImportExport.Mode;
import modules.admin.domain.ImportExportColumn;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient ImportExportService importExportService;
 */
@Default
public class ImportExportService {

	/**
	 * Updates the import/export column configuration based on changes to the document name or mode.
	 * When the document name changes, it clears existing columns and regenerates them.
	 * When switching to import mode with an existing file, it loads columns from the file.
	 * When switching to export mode without columns, it generates default columns from the document attributes.
	 * 
	 * @param source The name of the property that triggered the update
	 * @param bean The ImportExport instance containing the configuration to update
	 * @throws Exception If there's an error loading columns from file or accessing document metadata
	 */
	public void updateColumns(String source, ImportExportExtension bean) throws Exception {
		switch (source) {
			case ImportExport.documentNamePropertyName:
				// if changing document name, recreate default import export column config
				bean.getImportExportColumns().clear();
				//$FALL-THROUGH$
			case ImportExport.modePropertyName:
				if (Mode.importData.equals(bean.getMode()) && bean.getImportFileAbsolutePath() != null) {
					bean.getImportExportColumns().clear();
					UploadSimpleImportDataFile.loadColumnsFromFile(bean, new UploadException());
					if (bean.getLoadType() == null) {
						bean.setLoadType(ImportExportUtil.CREATE_RELATED_RECORDS_IF_THEY_DON_T_EXIST);
					}
				}
				if (Mode.exportData.equals(bean.getMode()) && bean.getImportExportColumns().size() == 0) {
					if (bean.getModuleName() != null && bean.getDocumentName() != null) {
						List<ImportExportColumn> columns = generateColumns(bean);
						for (ImportExportColumn c : columns) {
							bean.addImportExportColumnsElement(c);
						}
					}
				}
				break;
			default:
				break;
		}
	}

	/**
	 * Generates import/export column configurations from the scalar attributes of a specified document.
	 * Creates ImportExportColumn instances for all persistent attributes except unsupported types
	 * (collections, content, images, and inverse relationships). Each column is configured with
	 * the attribute's binding name and localized display name.
	 * 
	 * @param bean The ImportExport instance containing the module and document name to generate columns for
	 * @return A list of ImportExportColumn objects representing the document's exportable/importable attributes
	 */
	@SuppressWarnings("static-method")
	public List<ImportExportColumn> generateColumns(ImportExportExtension bean) {

		List<ImportExportColumn> columns = new ArrayList<>();
		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(bean.getModuleName());
		Document document = module.getDocument(customer, bean.getDocumentName());

		for (Attribute a : document.getAllAttributes(customer)) {
			if (a.isPersistent()) {
				// exclude unsupported types
				switch (a.getAttributeType()) {
					case collection:
					case content:
					case image:
					case inverseMany:
					case inverseOne:
						break;
					default:
						ImportExportColumn col = ImportExportColumn.newInstance();
						col.setBindingName(a.getName());
						col.setColumnName(a.getLocalisedDisplayName());
						columns.add(col);
						break;
				}
			}
		}

		return columns;
	}
}
