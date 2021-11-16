package org.skyve.metadata.repository;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.metadata.controller.BizImportAction;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.metadata.view.model.comparison.ComparisonModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.map.MapModel;

public interface ProvidedRepository extends Repository {
	final String ROUTER_NAME = "router";
	final String ROUTER_NAMESPACE = ROUTER_NAME + '/';
	final String CUSTOMERS_NAME = "customers";
	final String CUSTOMERS_NAMESPACE = CUSTOMERS_NAME + '/';
	final String RESOURCES_NAMESPACE = "resources/";
	final String MODULES_NAME = "modules";
	final String MODULES_NAMESPACE = MODULES_NAME + '/';
	final String CONVERTERS_NAMESPACE = "converters/";
	final String VIEWS_NAME = "views";
	final String VIEWS_NAMESPACE = VIEWS_NAME + '/';
	final String MODELS_NAME = "models";
	final String MODELS_NAMESPACE = MODELS_NAME + '/';
	final String ACTIONS_NAME = "actions";
	final String ACTIONS_NAMESPACE = ACTIONS_NAME + '/';
	final String IMAGES_NAME = "images";
	final String IMAGES_NAMESPACE = IMAGES_NAME + '/';
	final String REPORTS_NAME = "reports";
	final String REPORTS_NAMESPACE = REPORTS_NAME + '/';
	final String QUERIES_NAME = "queries";
	final String QUERIES_NAMESPACE = QUERIES_NAME + '/';
	final String DOMAIN_NAME = "domain";
	final String DOMAIN_NAMESPACE = DOMAIN_NAME + '/';

	List<String> getAllCustomerNames();

	/**
	 * Used to return all module names defined in the modules area (not customer overridden definitions).
	 * 
	 * @return
	 */
	List<String> getAllVanillaModuleNames();

	/**
	 * 
	 * @param customer Can be null, which means get the un-overridden module.
	 * @param moduleName
	 * @return
	 */
	Module getModule(Customer customer, String moduleName);

	/**
	 * 
	 * @param customer Can be null, which means get the un-overridden document.
	 * @param module
	 * @param documentName
	 * @return
	 */
	Document getDocument(Customer customer, Module module, String documentName);

	/**
	 * 
	 * @param uxui
	 * @param customer
	 * @param document
	 * @param viewType
	 * @return
	 */
	View getView(String uxui, Customer customer, Document document, String name);

	<T extends Bean> Bizlet<T> getBizlet(Customer customer, Document document, boolean runtime);

	/**
	 * 
	 * @param customer
	 * @param document
	 * @param imageName
	 * @param runtime
	 * @return
	 */
	<T extends Bean> DynamicImage<T> getDynamicImage(Customer customer,
														Document document,
														String imageName,
														boolean runtime);

	<T extends Bean, C extends Bean> ComparisonModel<T, C> getComparisonModel(Customer customer, Document document, String modelName, boolean runtime);
	
	<T extends Bean> MapModel<T> getMapModel(Customer customer, Document document, String modelName, boolean runtime);

	<T extends Bean> ChartModel<T> getChartModel(Customer customer, Document document, String modelName, boolean runtime);

	<T extends Bean> ListModel<T> getListModel(Customer customer, Document document, String modelName, boolean runtime);
	
	ServerSideAction<Bean> getServerSideAction(Customer customer, Document document, String className, boolean runtime);

	BizExportAction getBizExportAction(Customer customer, Document document, String className, boolean runtime);

	BizImportAction getBizImportAction(Customer customer, Document document, String className, boolean runtime);

	DownloadAction<Bean> getDownloadAction(Customer customer, Document document, String className, boolean runtime);

	UploadAction<Bean> getUploadAction(Customer customer, Document document, String className, boolean runtime);

	void validateCustomerForGenerateDomain(Customer customer);

	void validateModuleForGenerateDomain(Customer customer, Module module);

	void validateDocumentForGenerateDomain(Customer customer, Document document);

	void validateViewForGenerateDomain(Customer customer, Document document, View view, String uxui);

	/**
	 * @return The global router that is not module specific.
	 */
	Router getGlobalRouter();

	/**
	 * @return A list of module-specific routers.
	 */
	List<Router> getModuleRouters();
	
	/**
	 * 
	 * @param customer
	 * @param document
	 * @param reportName
	 * @return
	 */
	String getReportFileName(Customer customer, Document document, String reportName);
	
	Class<?> getJavaClass(Customer customer, String fullyQualifiedJavaCodeName);
	
	/**
	 * Overridden to return a UserImpl in this interface.
	 */
	@Override
	UserImpl retrieveUser(String userPrincipal);
	
	/**
	 * 
	 * @param user
	 */
	void resetMenus(User user);
	
	
	/**
	 * Populate the permissions available to a user.
	 * @param user the user to populate permissions for
	 */
	void populatePermissions(User user);
	
	
	/**
	 * Clear the permissions and menus available to a user, and then re-populate the user with new permissions.
	 * <br />
	 * This is equivalent to clearing permissions and menus from a user, and then calling {@link Repository#populatePermissions(User)}
	 * and {@link Repository#resetMenus(User)}
	 * @param user the user to reset user permissions for
	 */
	void resetUserPermissions(User user);
	
	default Document findNearestPersistentUnmappedSuperDocument(Customer customer, Module module, Document document) {
		Document result = null;
		
		Extends inherits = document.getExtends();
		if (inherits != null) {
			result = module.getDocument(customer, inherits.getDocumentName());
			if (result == null) {
				throw new MetaDataException("Document " + document.getName() + 
												" extends document " + inherits.getDocumentName() +
												" which does not exist in module " + module.getName());
			}
			Persistent inheritsPersistent = result.getPersistent();
			if (inheritsPersistent != null) {
				if (ExtensionStrategy.mapped.equals(inheritsPersistent.getStrategy())) {
					Extends baseInherits = result.getExtends();
					if (baseInherits != null) { // only recurse if we have a base document to recurse to
						Module baseModule = getModule(customer, result.getOwningModuleName());
						result = findNearestPersistentUnmappedSuperDocument(customer, baseModule, result);
					}
				}
			}
		}
		
		if (result != null) {
			Persistent persistent = result.getPersistent();
			if ((persistent == null) || 
					(persistent.getName() == null) ||
					ExtensionStrategy.mapped.equals(persistent.getStrategy())) {
				result = null;
			}
		}
		return result;
	}
}
