package org.skyve.metadata.repository;

import java.sql.Connection;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.job.UserJobSchedule;
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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Repository API for resolving metadata, UI artifacts, and runtime components for modules/documents.
 * Supports customer overrides and runtime resolution, and exposes permission/user population helpers.
 *
 * <p>Nullability is expressed via {@link Nonnull} and {@link Nullable} on parameters and return values.
 */
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

	/**
	 * Return all configured customer names.
	 *
	 * @return non-null list of customer names (may be empty)
	 */
	@Nonnull List<String> getAllCustomerNames();

	/**
	 * Return all module names defined in the modules area (excluding customer overrides).
	 *
	 * @return non-null list of base module names (may be empty)
	 */
	@Nonnull List<String> getAllVanillaModuleNames();

	/**
	 * Resolve a module by name, optionally using a customer override.
	 *
	 * @param customer optional customer; if {@code null}, return the un-overridden module
	 * @param moduleName module name to resolve
	 * @return resolved module
	 */
	@Nonnull Module getModule(@Nullable Customer customer, @Nonnull String moduleName);

	/**
	 * Resolve a document by name, optionally using a customer override.
	 *
	 * @param customer optional customer; if {@code null}, return the un-overridden document
	 * @param module module that owns the document
	 * @param documentName document name to resolve
	 * @return resolved document
	 */
	@Nonnull Document getDocument(@Nullable Customer customer, @Nonnull Module module, @Nonnull String documentName);

	/**
	 * Resolve a view by name for the given document and UX/UI, optionally using a customer override.
	 *
	 * @param uxui optional UX/UI name
	 * @param customer optional customer override
	 * @param document document the view belongs to
	 * @param name view name
	 * @return view if found; otherwise {@code null}
	 */
	@Nullable View getView(@Nullable String uxui, @Nullable Customer customer, @Nonnull Document document, @Nonnull String name);

	/**
	 * @return {@code true} if scaffolded views should be used; otherwise {@code false}
	 */
	boolean getUseScaffoldedViews();
	
	/**
	 * Resolve an action metadata definition for the given document.
	 *
	 * @param customer optional customer override
	 * @param document document the action belongs to
	 * @param actionName action name
	 * @return action metadata if found; otherwise {@code null}
	 */
	@Nullable ActionMetaData getMetaDataAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String actionName);

	/**
	 * Resolve a customer-specific vtable entry.
	 *
	 * @param customerName customer name
	 * @param key vtable key
	 * @return resolved value; otherwise {@code null}
	 */
	@Nullable String vtable(@Nonnull String customerName, @Nonnull String key);

	/**
	 * Resolve a bizlet instance for a document.
	 *
	 * @param customer customer
	 * @param document document
	 * @param runtime {@code true} to prefer runtime overrides
	 * @return bizlet if present; otherwise {@code null}
	 */
	@Nullable <T extends Bean> Bizlet<T> getBizlet(@Nonnull Customer customer, @Nonnull Document document, boolean runtime);

	/**
	 * Resolve a bizlet metadata definition for a document.
	 *
	 * @param customer optional customer override
	 * @param document document
	 * @return bizlet metadata if found; otherwise {@code null}
	 */
	@Nullable BizletMetaData getMetaDataBizlet(@Nullable Customer customer, @Nonnull Document document);

	/**
	 * Resolve a dynamic image definition for a document.
	 *
	 * @param customer optional customer override
	 * @param document document
	 * @param imageName image name
	 * @param runtime {@code true} to prefer runtime overrides
	 * @return dynamic image
	 */
	@Nonnull <T extends Bean> DynamicImage<T> getDynamicImage(@Nullable Customer customer,
																@Nonnull Document document,
																@Nonnull String imageName,
																boolean runtime);

	/**
	 * Resolve a comparison model for a document.
	 *
	 * @param customer optional customer override
	 * @param document document
	 * @param modelName model name
	 * @param runtime {@code true} to prefer runtime overrides
	 * @return comparison model
	 */
	@Nonnull <T extends Bean, C extends Bean> ComparisonModel<T, C> getComparisonModel(@Nullable Customer customer,
																						@Nonnull Document document,
																						@Nonnull String modelName,
																						boolean runtime);

	/**
	 * Resolve a map model for a document.
	 *
	 * @param customer optional customer override
	 * @param document document
	 * @param modelName model name
	 * @param runtime {@code true} to prefer runtime overrides
	 * @return map model
	 */
	@Nonnull <T extends Bean> MapModel<T> getMapModel(@Nullable Customer customer,
														@Nonnull Document document,
														@Nonnull String modelName,
														boolean runtime);

	/**
	 * Resolve a chart model for a document.
	 *
	 * @param customer optional customer override
	 * @param document document
	 * @param modelName model name
	 * @param runtime {@code true} to prefer runtime overrides
	 * @return chart model
	 */
	@Nonnull <T extends Bean> ChartModel<T> getChartModel(@Nullable Customer customer,
															@Nonnull Document document,
															@Nonnull String modelName,
															boolean runtime);

	/**
	 * Resolve a list model for a document.
	 *
	 * @param customer optional customer override
	 * @param document document
	 * @param modelName model name
	 * @param runtime {@code true} to prefer runtime overrides
	 * @return list model
	 */
	@Nonnull <T extends Bean> ListModel<T> getListModel(@Nullable Customer customer,
															@Nonnull Document document,
															@Nonnull String modelName,
															boolean runtime);

	/**
	 * Resolve a server-side action implementation.
	 *
	 * @param customer optional customer override
	 * @param document document the action belongs to
	 * @param className action class name (fully qualified)
	 * @param runtime {@code true} to prefer runtime overrides
	 * @return server-side action
	 */
	@Nonnull ServerSideAction<Bean> getServerSideAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String className, boolean runtime);

	/**
	 * Resolve a biz export action implementation.
	 *
	 * @param customer optional customer override
	 * @param document document the action belongs to
	 * @param className action class name (fully qualified)
	 * @param runtime {@code true} to prefer runtime overrides
	 * @return export action
	 */
	@Nonnull BizExportAction getBizExportAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String className, boolean runtime);

	/**
	 * Resolve a biz import action implementation.
	 *
	 * @param customer optional customer override
	 * @param document document the action belongs to
	 * @param className action class name (fully qualified)
	 * @param runtime {@code true} to prefer runtime overrides
	 * @return import action
	 */
	@Nonnull BizImportAction getBizImportAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String className, boolean runtime);

	/**
	 * Resolve a download action implementation.
	 *
	 * @param customer optional customer override
	 * @param document document the action belongs to
	 * @param className action class name (fully qualified)
	 * @param runtime {@code true} to prefer runtime overrides
	 * @return download action
	 */
	@Nonnull DownloadAction<Bean> getDownloadAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String className, boolean runtime);

	/**
	 * Resolve an upload action implementation.
	 *
	 * @param customer optional customer override
	 * @param document document the action belongs to
	 * @param className action class name (fully qualified)
	 * @param runtime {@code true} to prefer runtime overrides
	 * @return upload action
	 */
	@Nonnull UploadAction<Bean> getUploadAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String className, boolean runtime);

	/**
	 * Validate a customer for domain generation.
	 *
	 * @param customer customer to validate
	 */
	void validateCustomerForGenerateDomain(@Nonnull Customer customer);

	/**
	 * Validate a module for domain generation.
	 *
	 * @param customer customer
	 * @param module module to validate
	 */
	void validateModuleForGenerateDomain(@Nonnull Customer customer, @Nonnull Module module);

	/**
	 * Validate a document for domain generation.
	 *
	 * @param customer customer
	 * @param document document to validate
	 */
	void validateDocumentForGenerateDomain(@Nonnull Customer customer, @Nonnull Document document);

	/**
	 * Validate a view for domain generation.
	 *
	 * @param customer customer
	 * @param document document
	 * @param view view to validate
	 * @param uxui UX/UI name
	 */
	void validateViewForGenerateDomain(@Nonnull Customer customer, @Nonnull Document document, @Nonnull View view, @Nonnull String uxui);

	/**
	 * @return the global router that is not module specific
	 */
	@Nonnull Router getGlobalRouter();

	/**
	 * @return list of module-specific routers
	 */
	@Nonnull List<Router> getModuleRouters();

	/**
	 * Resolve the report file name for a document.
	 *
	 * @param customer optional customer override
	 * @param document document the report belongs to
	 * @param reportName report name
	 * @return report file name if found; otherwise {@code null}
	 */
	@Nullable String getReportFileName(@Nullable Customer customer, @Nonnull Document document, @Nonnull String reportName);

	/**
	 * Resolve a Java class by fully qualified name.
	 *
	 * @param customer optional customer override
	 * @param fullyQualifiedJavaCodeName fully qualified class name
	 * @return class if found; otherwise {@code null}
	 */
	@Nullable Class<?> getJavaClass(@Nullable Customer customer, @Nonnull String fullyQualifiedJavaCodeName);

	/**
	 * Overridden to return a {@link UserImpl} in this interface.
	 *
	 * @param userPrincipal user principal
	 * @return user if found; otherwise {@code null}
	 */
	@Override
	@Nullable UserImpl retrieveUser(@Nonnull String userPrincipal);

	/**
	 * Clear any cached menus for the given user.
	 *
	 * @param user user to reset menus for
	 */
	void resetMenus(@Nonnull User user);
	
	
	/**
	 * Populate the permissions available to a user.
	 *
	 * @param user the user to populate permissions for
	 * @return {@code true} if the user's permissions are successfully populated
	 */
	boolean populatePermissions(@Nonnull User user);
	
	
	/**
	 * Clear permissions and menus, then re-populate permissions and menus for the user.
	 * This is equivalent to clearing permissions and menus from a user, then calling
	 * {@link Repository#populatePermissions(User)} and {@link Repository#resetMenus(User)}.
	 *
	 * @param user the user to reset permissions for
	 */
	void resetUserPermissions(@Nonnull User user);
	
	/**
	 * Populate user data from a data store using the given connection.
	 *
	 * @param user user to populate
	 * @param connection connection to use
	 * @return {@code true} if user is successfully populated
	 */
	boolean populateUser(@Nonnull User user, @Nonnull Connection connection);
	
	/**
	 * Retrieve all scheduled jobs for all customers (jobs that are not disabled).
	 *
	 * @return list of {@link UserJobSchedule} containing job schedules and their associated users
	 */
	@Nonnull List<UserJobSchedule> retrieveAllScheduledJobsForAllCustomers();
	
	/**
	 * Retrieve all scheduled reports for all customers (reports flagged as scheduled).
	 *
	 * @return list of {@link UserJobSchedule} containing report schedules and their associated users
	 */
	@Nonnull List<UserJobSchedule> retrieveAllScheduledReportsForAllCustomers();
	
	/**
	 * Return the name of the public user for a customer set on the Configuration document.
	 *
	 * @param customerName customer to get the public user for
	 * @return public user name (without the customer name), or {@code null} if not configured
	 */
	public @Nullable String retrievePublicUserName(@Nonnull String customerName);
	
	/**
	 * Find the nearest persistent single or joined super-document in the inheritance hierarchy for a document.
	 *
	 * @param customer optional customer override
	 * @param module module containing the document
	 * @param document document to inspect
	 * @return nearest persistent single/joined super-document, or {@code null} if none
	 */
	default @Nullable Document findNearestPersistentSingleOrJoinedSuperDocument(@Nullable Customer customer, @Nonnull Module module, @Nonnull Document document) {
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
				ExtensionStrategy strategy = inheritsPersistent.getStrategy();
				if (ExtensionStrategy.mapped.equals(strategy)) {
					Extends baseInherits = result.getExtends();
					if (baseInherits != null) { // only recurse if we have a base document to recurse to
						Module baseModule = getModule(customer, result.getOwningModuleName());
						result = findNearestPersistentSingleOrJoinedSuperDocument(customer, baseModule, result);
					}
				}
			}
		}
		
		if (result != null) {
			Persistent persistent = result.getPersistent();
			if (persistent == null) {
				result = null; 
			}
			else if (persistent.getName() == null) {
				result = null;
			}
			else {
				ExtensionStrategy strategy = persistent.getStrategy();
				if (ExtensionStrategy.mapped.equals(strategy)) {
					result = null;
				}
			}
		}
		return result;
	}
}
