package modules.admin.ImportExport;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.ModulesUtil.DomainValueSortByDescription;
import modules.admin.ImportExport.actions.UploadSimpleImportDataFile;
import modules.admin.ImportExportColumn.ImportExportColumnBizlet;
import modules.admin.domain.ImportExport;
import modules.admin.domain.ImportExport.LoadType;
import modules.admin.domain.ImportExport.Mode;
import modules.admin.domain.ImportExportColumn;

public class ImportExportBizlet extends Bizlet<ImportExportExtension> {
	

	@Override
	public List<DomainValue> getConstantDomainValues(String attributeName) throws Exception {

		// list of modules
		if (ImportExport.moduleNamePropertyName.equals(attributeName)) {
			Customer customer = CORE.getUser().getCustomer();
			List<DomainValue> result = new ArrayList<>();
			for (Module module : customer.getModules()) {
				result.add(new DomainValue(module.getName(), module.getLocalisedTitle()));
			}
			Collections.sort(result, new DomainValueSortByDescription());
			return result;
		}

		return super.getConstantDomainValues(attributeName);
	}

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, ImportExportExtension bean) throws Exception {

		// list documents within modules
		if (ImportExport.documentNamePropertyName.equals(attributeName)) {
			Customer customer = CORE.getUser().getCustomer();
			List<DomainValue> result = new ArrayList<>();
			if (bean.getModuleName() != null) {
				Module module = customer.getModule(bean.getModuleName());
				for (String documentName : module.getDocumentRefs().keySet()) {
					Document document = module.getDocument(customer, documentName);
					if (document.isPersistable()) {
						result.add(new DomainValue(document.getName(), document.getLocalisedSingularAlias()));
					}
				}
			}
			Collections.sort(result, new DomainValueSortByDescription());
			return result;
		}

		return super.getDynamicDomainValues(attributeName, bean);
	}

	@Override
	public void preRerender(String source, ImportExportExtension bean, WebContext webContext) throws Exception {

		updateColumns(source, bean);

		super.preRerender(source, bean, webContext);
	}

	public static void updateColumns(String source, ImportExportExtension bean) throws Exception {
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
					bean.setLoadType(LoadType.createFind);
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
	 * Generate column configs from scalar attributes
	 */
	public static List<ImportExportColumn> generateColumns(ImportExportExtension bean) {

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

	@Override
	public void preDelete(ImportExportExtension bean) throws Exception {

		bean.cleanupImportFile();

		super.preDelete(bean);
	}

	@Override
	public void preSave(ImportExportExtension bean) throws Exception {

		// if user has changed mode - clean up any unused import file
		if (bean.originalValues().containsKey(ImportExport.modePropertyName) && Mode.exportData.equals(bean.getMode())) {
			bean.cleanupImportFile();
		}

		super.preSave(bean);
	}

	@Override
	public List<String> complete(String attributeName, String value, ImportExportExtension bean) throws Exception {
		if (ImportExportColumn.bindingNamePropertyName.equals(attributeName)) {
			List<String> bindingsList = new ArrayList<>();

			if (bindingsList.isEmpty()) {

				Persistence pers = CORE.getPersistence();
				User user = pers.getUser();
				Customer customer = user.getCustomer();
				Module module = customer.getModule(bean.getModuleName());
				Document document = module.getDocument(customer, bean.getDocumentName());

				for (Attribute a : document.getAllAttributes(customer)) {

					// exclude unimplemented types - some of these can be handled later
					if (!AttributeType.collection.equals(a.getAttributeType())
							&& !AttributeType.content.equals(a.getAttributeType())
							&& !AttributeType.image.equals(a.getAttributeType())
							&& !AttributeType.geometry.equals(a.getAttributeType())
							&& !AttributeType.inverseMany.equals(a.getAttributeType())
							&& !AttributeType.inverseOne.equals(a.getAttributeType())) {

						// also exclude non persistent fields
						if (a.isPersistent()) {
							if(AttributeType.association.equals(a.getAttributeType())) {
//								bindings.add(new DomainValue(a.getName() + Bean.BIZ_KEY, a.getDisplayName()));
								bindingsList.add(a.getName());
							} else {
								bindingsList.add(a.getName());
							}
						}
					}
				}

				bindingsList.add(ImportExportColumnBizlet.EXPRESSION);
			}

			return bindingsList;
		}
		return super.complete(attributeName, value, bean);
	}

}
