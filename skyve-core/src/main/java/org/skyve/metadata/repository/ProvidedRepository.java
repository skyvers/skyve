package org.skyve.metadata.repository;

import java.sql.Connection;
import java.util.List;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
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

public interface ProvidedRepository extends CachedRepository {
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
	final String DOMAIN_NAME = "domain";
	final String DOMAIN_NAMESPACE = DOMAIN_NAME + '/';
	final String BIZLET_SUFFIX = "Bizlet";
	public final String JASPER_SUFFIX = "Jasper";
	public final String FREEMARKER_SUFFIX = "Freemarker";
	final String META_DATA_SUFFIX = "MetaData";

	@Nonnull List<String> getAllCustomerNames();

	/**
	 * Used to return all module names defined in the modules area (not customer overridden definitions).
	 * 
	 * @return
	 */
	@Nonnull List<String> getAllVanillaModuleNames();

	/**
	 * 
	 * @param customer Can be null, which means get the un-overridden module.
	 * @param moduleName
	 * @return
	 */
	@Nonnull Module getModule(@Nullable Customer customer, @Nonnull String moduleName);

	/**
	 * 
	 * @param customer Can be null, which means get the un-overridden document.
	 * @param module
	 * @param documentName
	 * @return
	 */
	@Nonnull Document getDocument(@Nullable Customer customer, @Nonnull Module module, @Nonnull String documentName);

	/**
	 * 
	 * @param uxui
	 * @param customer
	 * @param document
	 * @param viewType
	 * @return
	 */
	@Nullable View getView(@Nullable String uxui, @Nullable Customer customer, @Nonnull Document document, @Nonnull String name);

	boolean getUseScaffoldedViews();
	
	/**
	 * 
	 * @param customer
	 * @param document
	 * @param actionName
	 * @return
	 */
	@Nullable ActionMetaData getMetaDataAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String actionName);

	@Nullable String vtable(@Nonnull String customerName, @Nonnull String key);

	@Nullable <T extends Bean> Bizlet<T> getBizlet(@Nonnull Customer customer, @Nonnull Document document, boolean runtime);

	/**
	 * 
	 * @param customer
	 * @param document
	 * @return
	 */
	@Nullable BizletMetaData getMetaDataBizlet(@Nullable Customer customer, @Nonnull Document document);

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
	
	<T extends Bean> MapModel<T> getMapModel(@Nullable Customer customer, @Nonnull Document document, @Nonnull String modelName, boolean runtime);

	<T extends Bean> ChartModel<T> getChartModel(Customer customer, Document document, String modelName, boolean runtime);

	<T extends Bean> ListModel<T> getListModel(Customer customer, Document document, String modelName, boolean runtime);
	
	ServerSideAction<Bean> getServerSideAction(Customer customer, Document document, String className, boolean runtime);

	BizExportAction getBizExportAction(Customer customer, Document document, String className, boolean runtime);

	BizImportAction getBizImportAction(Customer customer, Document document, String className, boolean runtime);

	DownloadAction<Bean> getDownloadAction(Customer customer, Document document, String className, boolean runtime);

	UploadAction<Bean> getUploadAction(Customer customer, Document document, String className, boolean runtime);

	void validateCustomerForGenerateDomain(@Nonnull Customer customer);

	void validateModuleForGenerateDomain(@Nonnull Customer customer, @Nonnull Module module);

	void validateDocumentForGenerateDomain(@Nonnull Customer customer, @Nonnull Document document);

	void validateViewForGenerateDomain(@Nonnull Customer customer, @Nonnull Document document, @Nonnull View view, @Nonnull String uxui);

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
	
	@Nullable Class<?> getJavaClass(@Nonnull Customer customer, @Nonnull String fullyQualifiedJavaCodeName);
	
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
	
	/**
	 * Populate user data from a data store using the given connection.
	 * @param user User to populate.
	 * @param connection	The connection to use.
	 */
	void populateUser(User user, Connection connection);
	
	/**
	 * Return a list of admin.JobSchedule projections with at least the following document attributes populated
	 * "jobName", startTime", endTime", "cronExpression", "disabled", "user"
	 * @return	The Job Schedules
	 */
	List<Bean> retrieveAllJobSchedulesForAllCustomers();
	
	/**
	 * Return a list of admin.ReportTemplate projections with at least the following document attributes populated
	 * "name", startTime", endTime", "cronExpression", "scheduled", "user"
	 * @return	The Report Templates.
	 */
	List<Bean> retrieveAllReportSchedulesForAllCustomers();
	
	/**
	 * Return the name of the public user for a customer set on the Configuration document.
	 * @param customerName	The customer to get the public user for.
	 * @return	Return the public user name (without the customer name)
	 */
	public String retrievePublicUserName(String customerName);
	
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
	
	/**
	 * Return its delegating repository or itself if it is not a delegate.
	 */
	ProvidedRepository getDelegator();

	/**
	 * Set the delegating repository.
	 */
	void setDelegator(ProvidedRepository delegator);
}
