package modules.admin.ImportExport;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.DomainValueUtil.DomainValueSortByDescription;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.domain.ImportExport;
import modules.admin.domain.ImportExport.Mode;

public class ImportExportBizlet extends Bizlet<ImportExportExtension> {
	@Inject
	private transient ImportExportService importExportService;

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

		importExportService.updateColumns(source, bean);

		if (bean.getLoadType() == null) {
			bean.setLoadType(ImportExportUtil.CREATE_RELATED_RECORDS_IF_THEY_DON_T_EXIST);
		}

		super.preRerender(source, bean, webContext);
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
		List<String> results = new ArrayList<>();
		if (ImportExport.loadTypePropertyName.equals(attributeName)) {
			results.add(ImportExportUtil.CREATE_RELATED_RECORDS_IF_THEY_DON_T_EXIST);
			results.add(ImportExportUtil.CREATE_EVERYTHING_EVEN_IF_THERE_MIGHT_BE_DUPLICATES);
			return results;
		}
		return super.complete(attributeName, value, bean);
	}

}
