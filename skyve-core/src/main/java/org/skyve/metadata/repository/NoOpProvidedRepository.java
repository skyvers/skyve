package org.skyve.metadata.repository;

import java.io.File;
import java.sql.Connection;
import java.util.Collections;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.job.UserJobSchedule;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.metadata.controller.BizImportAction;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.customer.Customer;
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
 * Defines the NoOpProvidedRepository type.
 */
public class NoOpProvidedRepository extends ProvidedRepositoryFactory {
	/**
	 * Executes evictCachedMetaData.
	 * @param customer the customer
	 */
	@Override
	public void evictCachedMetaData(@Nullable Customer customer) {
		// do nothing
	}

	/**
	 * Executes findResourceFile.
	 * @param resourcePath the resourcePath
	 * @param customerName the customerName
	 * @param moduleName the moduleName
	 * @return the result
	 */
	@Override
	public @Nullable File findResourceFile(@Nonnull String resourcePath, @Nullable String customerName, @Nullable String moduleName) {
		return null;
	}

	/**
	 * Returns the router.
	 * @return the result
	 */
	@Override
	public @Nullable Router getRouter() {
		return null;
	}

	/**
	 * Returns the customer.
	 * @param customerName the customerName
	 * @return the result
	 */
	@Override
	public @Nullable Customer getCustomer(@Nonnull String customerName) {
		return null;
	}

	/**
	 * Returns the dynamicImage.
	 * @param customer the customer
	 * @param document the document
	 * @param imageName the imageName
	 * @param runtime the runtime
	 * @return the result
	 */
	@Override
	public @Nullable <T extends Bean> DynamicImage<T> getDynamicImage(@Nullable Customer customer, @Nonnull Document document, @Nonnull String imageName, boolean runtime) {
		return null;
	}

	/**
	 * Returns the view.
	 * @param uxui the uxui
	 * @param customer the customer
	 * @param document the document
	 * @param name the name
	 * @return the result
	 */
	@Override
	public @Nullable View getView(@Nullable String uxui, @Nullable Customer customer, @Nonnull Document document, @Nonnull String name) {
		return null;
	}

	/**
	 * Returns the metaDataAction.
	 * @param customer the customer
	 * @param document the document
	 * @param actionName the actionName
	 * @return the result
	 */
	@Override
	public @Nullable ActionMetaData getMetaDataAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String actionName) {
		return null;
	}

	/**
	 * Returns the comparisonModel.
	 * @param customer the customer
	 * @param document the document
	 * @param modelName the modelName
	 * @param runtime the runtime
	 * @return the result
	 */
	@Override
	public @Nullable <T extends Bean, C extends Bean> ComparisonModel<T, C> getComparisonModel(@Nullable Customer customer, @Nonnull Document document, @Nonnull String modelName, boolean runtime) {
		return null;
	}

	/**
	 * Returns the mapModel.
	 * @param customer the customer
	 * @param document the document
	 * @param modelName the modelName
	 * @param runtime the runtime
	 * @return the result
	 */
	@Override
	public @Nullable <T extends Bean> MapModel<T> getMapModel(@Nullable Customer customer, @Nonnull Document document, @Nonnull String modelName, boolean runtime) {
		return null;
	}

	/**
	 * Returns the chartModel.
	 * @param customer the customer
	 * @param document the document
	 * @param modelName the modelName
	 * @param runtime the runtime
	 * @return the result
	 */
	@Override
	public @Nullable <T extends Bean> ChartModel<T> getChartModel(@Nullable Customer customer, @Nonnull Document document, @Nonnull String modelName, boolean runtime) {
		return null;
	}

	/**
	 * Returns the listModel.
	 * @param customer the customer
	 * @param document the document
	 * @param modelName the modelName
	 * @param runtime the runtime
	 * @return the result
	 */
	@Override
	public @Nullable <T extends Bean> ListModel<T> getListModel(@Nullable Customer customer, @Nonnull Document document, @Nonnull String modelName, boolean runtime) {
		return null;
	}

	/**
	 * Returns the serverSideAction.
	 * @param customer the customer
	 * @param document the document
	 * @param className the className
	 * @param runtime the runtime
	 * @return the result
	 */
	@Override
	public @Nullable ServerSideAction<Bean> getServerSideAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String className, boolean runtime) {
		return null;
	}

	/**
	 * Returns the bizExportAction.
	 * @param customer the customer
	 * @param document the document
	 * @param className the className
	 * @param runtime the runtime
	 * @return the result
	 */
	@Override
	public @Nullable BizExportAction getBizExportAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String className, boolean runtime) {
		return null;
	}

	/**
	 * Returns the bizImportAction.
	 * @param customer the customer
	 * @param document the document
	 * @param className the className
	 * @param runtime the runtime
	 * @return the result
	 */
	@Override
	public @Nullable BizImportAction getBizImportAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String className, boolean runtime) {
		return null;
	}

	/**
	 * Returns the downloadAction.
	 * @param customer the customer
	 * @param document the document
	 * @param className the className
	 * @param runtime the runtime
	 * @return the result
	 */
	@Override
	public @Nullable DownloadAction<Bean> getDownloadAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String className, boolean runtime) {
		return null;
	}

	/**
	 * Returns the uploadAction.
	 * @param customer the customer
	 * @param document the document
	 * @param className the className
	 * @param runtime the runtime
	 * @return the result
	 */
	@Override
	public @Nullable UploadAction<Bean> getUploadAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String className, boolean runtime) {
		return null;
	}

	/**
	 * Returns the dataFactory.
	 * @param customer the customer
	 * @param document the document
	 * @return the result
	 */
	@Override
	public @Nullable Object getDataFactory(@Nullable Customer customer, @Nonnull Document document) {
		return null;
	}

	/**
	 * Executes retrieveUser.
	 * @param userName the userName
	 * @return the result
	 */
	@Override
	public @Nullable UserImpl retrieveUser(@Nonnull String userName) {
		return null;
	}

	/**
	 * Executes resetMenus.
	 * @param user the user
	 */
	@Override
	public void resetMenus(@Nonnull User user) {
		// do nothing
	}

	/**
	 * Executes populatePermissions.
	 * @param user the user
	 * @return the result
	 */
	@Override
	public boolean populatePermissions(@Nonnull User user) {
		return false;
	}

	/**
	 * Executes resetUserPermissions.
	 * @param user the user
	 */
	@Override
	public void resetUserPermissions(@Nonnull User user) {
		// do nothing
	}

	/**
	 * Executes populateUser.
	 * @param user the user
	 * @param connection the connection
	 * @return the result
	 */
	@Override
	public boolean populateUser(@Nonnull User user, @Nonnull Connection connection) {
		return false;
	}

	/**
	 * Executes retrieveAllScheduledJobsForAllCustomers.
	 * @return the result
	 */
	@Override
	public @Nonnull List<UserJobSchedule> retrieveAllScheduledJobsForAllCustomers() {
		return Collections.emptyList();
	}

	/**
	 * Executes retrieveAllScheduledReportsForAllCustomers.
	 * @return the result
	 */
	@Override
	public @Nonnull List<UserJobSchedule> retrieveAllScheduledReportsForAllCustomers() {
		return Collections.emptyList();
	}

	/**
	 * Executes retrievePublicUserName.
	 * @param customerName the customerName
	 * @return the result
	 */
	@Override
	public @Nullable String retrievePublicUserName(@Nonnull String customerName) {
		return null;
	}

	/**
	 * Returns the allCustomerNames.
	 * @return the result
	 */
	@Override
	public @Nonnull List<String> getAllCustomerNames() {
		return Collections.emptyList();
	}

	/**
	 * Returns the allVanillaModuleNames.
	 * @return the result
	 */
	@Override
	public @Nonnull List<String> getAllVanillaModuleNames() {
		return Collections.emptyList();
	}

	/**
	 * Returns the module.
	 * @param customer the customer
	 * @param moduleName the moduleName
	 * @return the result
	 */
	@Override
	public @Nullable Module getModule(@Nullable Customer customer, @Nonnull String moduleName) {
		return null;
	}

	/**
	 * Returns the document.
	 * @param customer the customer
	 * @param module the module
	 * @param documentName the documentName
	 * @return the result
	 */
	@Override
	public @Nullable Document getDocument(@Nullable Customer customer, @Nonnull Module module, @Nonnull String documentName) {
		return null;
	}

	/**
	 * Returns the bizlet.
	 * @param customer the customer
	 * @param document the document
	 * @param runtime the runtime
	 * @return the result
	 */
	@Override
	public @Nullable <T extends Bean> Bizlet<T> getBizlet(@Nullable Customer customer, @Nonnull Document document, boolean runtime) {
		return null;
	}

	/**
	 * Returns the metaDataBizlet.
	 * @param customer the customer
	 * @param document the document
	 * @return the result
	 */
	@Override
	public @Nullable BizletMetaData getMetaDataBizlet(@Nullable Customer customer, @Nonnull Document document) {
		return null;
	}

	/**
	 * Executes validateCustomerForGenerateDomain.
	 * @param customer the customer
	 */
	@Override
	public void validateCustomerForGenerateDomain(@Nonnull Customer customer) {
		// do nothing
	}

	/**
	 * Executes validateModuleForGenerateDomain.
	 * @param customer the customer
	 * @param module the module
	 */
	@Override
	public void validateModuleForGenerateDomain(@Nonnull Customer customer, @Nonnull Module module) {
		// do nothing
	}

	/**
	 * Executes validateDocumentForGenerateDomain.
	 * @param customer the customer
	 * @param document the document
	 */
	@Override
	public void validateDocumentForGenerateDomain(@Nonnull Customer customer, @Nonnull Document document) {
		// do nothing
	}

	/**
	 * Executes validateViewForGenerateDomain.
	 * @param customer the customer
	 * @param document the document
	 * @param view the view
	 * @param uxui the uxui
	 */
	@Override
	public void validateViewForGenerateDomain(@Nonnull Customer customer, @Nonnull Document document, @Nonnull View view, @Nonnull String uxui) {
		// do nothing
	}

	/**
	 * Returns the globalRouter.
	 * @return the result
	 */
	@Override
	public @Nullable Router getGlobalRouter() {
		return null;
	}

	/**
	 * Returns the moduleRouters.
	 * @return the result
	 */
	@Override
	public @Nonnull List<Router> getModuleRouters() {
		return Collections.emptyList();
	}

	/**
	 * Returns the reportFileName.
	 * @param customer the customer
	 * @param document the document
	 * @param reportName the reportName
	 * @return the result
	 */
	@Override
	public @Nullable String getReportFileName(@Nullable Customer customer, @Nonnull Document document, @Nonnull String reportName) {
		return null;
	}

	/**
	 * Returns the javaClass.
	 * @param customer the customer
	 * @param fullyQualifiedJavaCodeName the fullyQualifiedJavaCodeName
	 * @return the result
	 */
	@Override
	public @Nullable Class<?> getJavaClass(@Nullable Customer customer, @Nonnull String fullyQualifiedJavaCodeName) {
		return null;
	}

	/**
	 * Executes vtable.
	 * @param customerName the customerName
	 * @param key the key
	 * @return the result
	 */
	@Override
	public @Nullable String vtable(@Nonnull String customerName, @Nonnull String key) {
		return null;
	}
	
	/**
	 * Returns the useScaffoldedViews.
	 * @return the result
	 */
	@Override
	public boolean getUseScaffoldedViews() {
		return false;
	}
}
