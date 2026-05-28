package modules.admin.ImportExport;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.types.Enumeration.DomainValueSortByDescription;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.domain.ImportExport;
import modules.admin.domain.ImportExport.Mode;

/**
 * Provides Bizlet behaviour for import/export document configuration.
 */
public class ImportExportBizlet extends Bizlet<ImportExportExtension> {
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	private transient ImportExportService importExportService;

	/**
	 * Supplies module choices for import/export configuration.
	 *
	 * @param attributeName
	 *        the requested attribute name
	 * @return sorted module values for {@link ImportExport#moduleNamePropertyName}, otherwise superclass values
	 * @throws Exception
	 *         if value resolution fails
	 */
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

	/**
	 * Supplies document choices based on the selected module.
	 *
	 * @param attributeName
	 *        the requested attribute name
	 * @param bean
	 *        the current import/export bean
	 * @return persistable document values for {@link ImportExport#documentNamePropertyName}, otherwise superclass values
	 * @throws Exception
	 *         if value resolution fails
	 */
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

	/**
	 * Rebuilds column definitions when controlling fields change.
	 *
	 * @param source
	 *        the source property that triggered rerender
	 * @param bean
	 *        the import/export bean
	 * @param webContext
	 *        the current web context
	 * @throws Exception
	 *         if column regeneration fails
	 */
	@Override
	public void preRerender(String source, ImportExportExtension bean, WebContext webContext) throws Exception {

		importExportService.updateColumns(source, bean);

		if (bean.getLoadType() == null) {
			bean.setLoadType(ImportExportUtil.CREATE_RELATED_RECORDS_IF_THEY_DON_T_EXIST);
		}

		super.preRerender(source, bean, webContext);
	}

	/**
	 * Cleans up uploaded import files before deleting the configuration record.
	 *
	 * @param bean
	 *        the bean being deleted
	 * @throws Exception
	 *         if cleanup or delete preparation fails
	 */
	@Override
	public void preDelete(ImportExportExtension bean) throws Exception {

		bean.cleanupImportFile();

		super.preDelete(bean);
	}

	/**
	 * Removes stale upload files when mode switches to export.
	 *
	 * @param bean
	 *        the bean being saved
	 * @throws Exception
	 *         if save preparation fails
	 */
	@Override
	public void preSave(ImportExportExtension bean) throws Exception {

		// if user has changed mode - clean up any unused import file
		if (bean.originalValues().containsKey(ImportExport.modePropertyName) && Mode.exportData.equals(bean.getMode())) {
			bean.cleanupImportFile();
		}

		super.preSave(bean);
	}

	/**
	 * Returns completion options for load-type text entry.
	 *
	 * @param attributeName
	 *        the attribute being completed
	 * @param value
	 *        the user-entered value prefix
	 * @param bean
	 *        the current bean
	 * @return available load-type options for {@link ImportExport#loadTypePropertyName}, otherwise superclass values
	 * @throws Exception
	 *         if completion fails
	 */
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
