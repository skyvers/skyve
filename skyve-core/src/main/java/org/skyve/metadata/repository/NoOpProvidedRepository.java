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

/**
 * Defines the NoOpProvidedRepository type.
 */
public class NoOpProvidedRepository extends ProvidedRepositoryFactory {
	/**
	 * Executes evictCachedMetaData.
	 * @param customer the customer
	 */
	@Override
	public void evictCachedMetaData(Customer customer) {
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
	public File findResourceFile(String resourcePath, String customerName, String moduleName) {
		return null;
	}

	/**
	 * Returns the router.
	 * @return the result
	 */
	@Override
	public Router getRouter() {
		return null;
	}

	/**
	 * Returns the customer.
	 * @param customerName the customerName
	 * @return the result
	 */
	@Override
	public Customer getCustomer(String customerName) {
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
	public <T extends Bean> DynamicImage<T> getDynamicImage(Customer customer, Document document, String imageName, boolean runtime) {
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
	public View getView(String uxui, Customer customer, Document document, String name) {
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
	public ActionMetaData getMetaDataAction(Customer customer, Document document, String actionName) {
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
	public <T extends Bean, C extends Bean> ComparisonModel<T, C> getComparisonModel(Customer customer, Document document, String modelName, boolean runtime) {
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
	public <T extends Bean> MapModel<T> getMapModel(Customer customer, Document document, String modelName, boolean runtime) {
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
	public <T extends Bean> ChartModel<T> getChartModel(Customer customer, Document document, String modelName, boolean runtime) {
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
	public <T extends Bean> ListModel<T> getListModel(Customer customer, Document document, String modelName, boolean runtime) {
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
	public ServerSideAction<Bean> getServerSideAction(Customer customer, Document document, String className, boolean runtime) {
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
	public BizExportAction getBizExportAction(Customer customer, Document document, String className, boolean runtime) {
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
	public BizImportAction getBizImportAction(Customer customer, Document document, String className, boolean runtime) {
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
	public DownloadAction<Bean> getDownloadAction(Customer customer, Document document, String className, boolean runtime) {
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
	public UploadAction<Bean> getUploadAction(Customer customer, Document document, String className, boolean runtime) {
		return null;
	}

	/**
	 * Returns the dataFactory.
	 * @param customer the customer
	 * @param document the document
	 * @return the result
	 */
	@Override
	public Object getDataFactory(Customer customer, Document document) {
		return null;
	}

	/**
	 * Executes retrieveUser.
	 * @param userName the userName
	 * @return the result
	 */
	@Override
	public UserImpl retrieveUser(String userName) {
		return null;
	}

	/**
	 * Executes resetMenus.
	 * @param user the user
	 */
	@Override
	public void resetMenus(User user) {
		// do nothing
	}

	/**
	 * Executes populatePermissions.
	 * @param user the user
	 * @return the result
	 */
	@Override
	public boolean populatePermissions(User user) {
		return false;
	}

	/**
	 * Executes resetUserPermissions.
	 * @param user the user
	 */
	@Override
	public void resetUserPermissions(User user) {
		// do nothing
	}

	/**
	 * Executes populateUser.
	 * @param user the user
	 * @param connection the connection
	 * @return the result
	 */
	@Override
	public boolean populateUser(User user, Connection connection) {
		return false;
	}

	/**
	 * Executes retrieveAllScheduledJobsForAllCustomers.
	 * @return the result
	 */
	@Override
	public List<UserJobSchedule> retrieveAllScheduledJobsForAllCustomers() {
		return Collections.emptyList();
	}

	/**
	 * Executes retrieveAllScheduledReportsForAllCustomers.
	 * @return the result
	 */
	@Override
	public List<UserJobSchedule> retrieveAllScheduledReportsForAllCustomers() {
		return Collections.emptyList();
	}

	/**
	 * Executes retrievePublicUserName.
	 * @param customerName the customerName
	 * @return the result
	 */
	@Override
	public String retrievePublicUserName(String customerName) {
		return null;
	}

	/**
	 * Returns the allCustomerNames.
	 * @return the result
	 */
	@Override
	public List<String> getAllCustomerNames() {
		return Collections.emptyList();
	}

	/**
	 * Returns the allVanillaModuleNames.
	 * @return the result
	 */
	@Override
	public List<String> getAllVanillaModuleNames() {
		return Collections.emptyList();
	}

	/**
	 * Returns the module.
	 * @param customer the customer
	 * @param moduleName the moduleName
	 * @return the result
	 */
	@Override
	public Module getModule(Customer customer, String moduleName) {
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
	public Document getDocument(Customer customer, Module module, String documentName) {
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
	public <T extends Bean> Bizlet<T> getBizlet(Customer customer, Document document, boolean runtime) {
		return null;
	}

	/**
	 * Returns the metaDataBizlet.
	 * @param customer the customer
	 * @param document the document
	 * @return the result
	 */
	@Override
	public BizletMetaData getMetaDataBizlet(Customer customer, Document document) {
		return null;
	}

	/**
	 * Executes validateCustomerForGenerateDomain.
	 * @param customer the customer
	 */
	@Override
	public void validateCustomerForGenerateDomain(Customer customer) {
		// do nothing
	}

	/**
	 * Executes validateModuleForGenerateDomain.
	 * @param customer the customer
	 * @param module the module
	 */
	@Override
	public void validateModuleForGenerateDomain(Customer customer, Module module) {
		// do nothing
	}

	/**
	 * Executes validateDocumentForGenerateDomain.
	 * @param customer the customer
	 * @param document the document
	 */
	@Override
	public void validateDocumentForGenerateDomain(Customer customer, Document document) {
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
	public void validateViewForGenerateDomain(Customer customer, Document document, View view, String uxui) {
		// do nothing
	}

	/**
	 * Returns the globalRouter.
	 * @return the result
	 */
	@Override
	public Router getGlobalRouter() {
		return null;
	}

	/**
	 * Returns the moduleRouters.
	 * @return the result
	 */
	@Override
	public List<Router> getModuleRouters() {
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
	public String getReportFileName(Customer customer, Document document, String reportName) {
		return null;
	}

	/**
	 * Returns the javaClass.
	 * @param customer the customer
	 * @param fullyQualifiedJavaCodeName the fullyQualifiedJavaCodeName
	 * @return the result
	 */
	@Override
	public Class<?> getJavaClass(Customer customer, String fullyQualifiedJavaCodeName) {
		return null;
	}

	/**
	 * Executes vtable.
	 * @param customerName the customerName
	 * @param key the key
	 * @return the result
	 */
	@Override
	public String vtable(String customerName, String key) {
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
