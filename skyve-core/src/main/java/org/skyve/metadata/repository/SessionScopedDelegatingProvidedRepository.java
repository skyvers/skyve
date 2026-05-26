package org.skyve.metadata.repository;

import java.io.File;
import java.sql.Connection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
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
 * Routes repository lookups to a per-session delegate repository.
 *
 * <p>Delegates are keyed by {@code User#getSessionId()}. If no delegate exists
 * for the current session, lookup methods return {@code null} or an empty
 * result according to method contract.
 *
 * <p>Threading: thread-safe for delegate registration/removal and lookup via
 * {@link ConcurrentHashMap}; thread affinity for user/persistence context is
 * provided by {@link AbstractPersistence}.
 */
public class SessionScopedDelegatingProvidedRepository extends ProvidedRepositoryFactory {
	private ConcurrentHashMap<String, ProvidedRepository> sessionScopedDelegates = new ConcurrentHashMap<>();
	
	/**
	 * Returns the sessionDelegate.
	 * @return the result
	 */
	public @Nullable ProvidedRepository getSessionDelegate() {
		if (AbstractPersistence.isPresent()) {
			User user = AbstractPersistence.get().getUser();
			if (user != null) {
				return getSessionDelegate(user);
			}
		}
		return null;
	}
	
	/**
	 * Returns the sessionDelegate.
	 * @param user the user
	 * @return the result
	 */
	public @Nullable ProvidedRepository getSessionDelegate(@Nonnull User user) {
		String sessionId = user.getSessionId();
		return (sessionId == null) ? null : sessionScopedDelegates.get(sessionId);
	}
	
	/**
	 * Sets the sessionDelegate.
	 * @param delegate the delegate
	 */
	public void setSessionDelegate(@Nonnull ProvidedRepository delegate) {
		if (AbstractPersistence.isPresent()) {
			User user = AbstractPersistence.get().getUser();
			if (user == null) {
				throw new IllegalStateException("No user on the persistence on this thread");
			}
			setSessionDelegate(user, delegate);
		}
		else {
			throw new IllegalStateException("No persistence on this thread");
		}
	}
	
	/**
	 * Sets the sessionDelegate.
	 * @param user the user
	 * @param delegate the delegate
	 */
	public void setSessionDelegate(@Nonnull User user, @Nonnull ProvidedRepository delegate) {
		String sessionId = user.getSessionId();
		if (sessionId == null) {
			throw new IllegalStateException("User " + user.getName() + " does not belong to a session");
		}
		sessionScopedDelegates.put(sessionId, delegate);
	}

	/**
	 * Executes removeSessionDelegate.
	 */
	public void removeSessionDelegate() {
		if (AbstractPersistence.isPresent()) {
			User user = AbstractPersistence.get().getUser();
			if (user == null) {
				throw new IllegalStateException("No user on the persistence on this thread");
			}
			removeSessionDelegate(user);
		}
		else {
			throw new IllegalStateException("No persistence on this thread");
		}
	}
	
	/**
	 * Executes removeSessionDelegate.
	 * @param user the user
	 */
	public void removeSessionDelegate(@Nonnull User user) {
		String sessionId = user.getSessionId();
		if (sessionId == null) {
			throw new IllegalStateException("User " + user.getName() + " does not belong to a session");
		}
		sessionScopedDelegates.remove(sessionId);
	}

	/**
	 * Executes evictCachedMetaData.
	 * @param customer the customer
	 */
	@Override
	public void evictCachedMetaData(Customer customer) {
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			delegate.evictCachedMetaData(customer);
		}
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
		File result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.findResourceFile(resourcePath, customerName, moduleName);
		}
		return result;
	}

	/**
	 * Returns the router.
	 * @return the result
	 */
	@Override
	public Router getRouter() {
		Router result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getRouter();
		}
		return result;
	}

	/**
	 * Returns the customer.
	 * @param customerName the customerName
	 * @return the result
	 */
	@Override
	public Customer getCustomer(String customerName) {
		Customer result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getCustomer(customerName);
		}
		return result;
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
		DynamicImage<T> result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getDynamicImage(customer, document, imageName, runtime);
		}
		return result;
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
		View result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getView(uxui, customer, document, name);
		}
		return result;
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
		ActionMetaData result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getMetaDataAction(customer, document, actionName);
		}
		return result;
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
		ComparisonModel<T, C> result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getComparisonModel(customer, document, modelName, runtime);
		}
		return result;
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
		MapModel<T> result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getMapModel(customer, document, modelName, runtime);
		}
		return result;
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
		ChartModel<T> result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getChartModel(customer, document, modelName, runtime);
		}
		return result;
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
		ListModel<T> result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getListModel(customer, document, modelName, runtime);
		}
		return result;
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
		ServerSideAction<Bean> result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getServerSideAction(customer, document, className, runtime);
		}
		return result;
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
		BizExportAction result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getBizExportAction(customer, document, className, runtime);
		}
		return result;
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
		BizImportAction result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getBizImportAction(customer, document, className, runtime);
		}
		return result;
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
		DownloadAction<Bean> result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getDownloadAction(customer, document, className, runtime);
		}
		return result;
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
		UploadAction<Bean> result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getUploadAction(customer, document, className, runtime);
		}
		return result;
	}

	/**
	 * Returns the dataFactory.
	 * @param customer the customer
	 * @param document the document
	 * @return the result
	 */
	@Override
	public Object getDataFactory(Customer customer, Document document) {
		Object result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getDataFactory(customer, document);
		}
		return result;
	}

	/**
	 * Executes retrieveUser.
	 * @param userName the userName
	 * @return the result
	 */
	@Override
	public UserImpl retrieveUser(String userName) {
		UserImpl result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.retrieveUser(userName);
		}
		return result;
	}
	
	/**
	 * Executes retrieveAllScheduledJobsForAllCustomers.
	 * @return the result
	 */
	@Override
	public List<UserJobSchedule> retrieveAllScheduledJobsForAllCustomers() {
		List<UserJobSchedule> result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.retrieveAllScheduledJobsForAllCustomers();
		}
		return result;
	}

	/**
	 * Executes retrieveAllScheduledReportsForAllCustomers.
	 * @return the result
	 */
	@Override
	public List<UserJobSchedule> retrieveAllScheduledReportsForAllCustomers() {
		List<UserJobSchedule> result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.retrieveAllScheduledReportsForAllCustomers();
		}
		return result;
	}
	
	/**
	 * Executes retrievePublicUserName.
	 * @param customerName the customerName
	 * @return the result
	 */
	@Override
	public String retrievePublicUserName(String customerName) {
		String result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.retrievePublicUserName(customerName);
		}
		return result;
	}
	
	/**
	 * Executes resetMenus.
	 * @param user the user
	 */
	@Override
	public void resetMenus(User user) {
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			delegate.resetMenus(user);
		}
	}

	/**
	 * Executes populatePermissions.
	 * @param user the user
	 * @return the result
	 */
	@Override
	public boolean populatePermissions(User user) {
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			return delegate.populatePermissions(user);
		}

		return false;
	}

	/**
	 * Executes resetUserPermissions.
	 * @param user the user
	 */
	@Override
	public void resetUserPermissions(User user) {
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			delegate.resetUserPermissions(user);
		}
	}

	/**
	 * Executes populateUser.
	 * @param user the user
	 * @param connection the connection
	 * @return the result
	 */
	@Override
	public boolean populateUser(User user, Connection connection) {
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			return delegate.populateUser(user, connection);
		}

		return false;
	}
	
	/**
	 * Returns the allCustomerNames.
	 * @return the result
	 */
	@Override
	public List<String> getAllCustomerNames() {
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			return delegate.getAllCustomerNames();
		}
		return Collections.emptyList();
	}

	/**
	 * Returns the allVanillaModuleNames.
	 * @return the result
	 */
	@Override
	public List<String> getAllVanillaModuleNames() {
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			return delegate.getAllVanillaModuleNames();
		}
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
		Module result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getModule(customer, moduleName);
		}
		return result;
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
		Document result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getDocument(customer, module, documentName);
		}
		return result;
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
		Bizlet<T> result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getBizlet(customer, document, runtime);
		}
		return result;
	}

	/**
	 * Returns the metaDataBizlet.
	 * @param customer the customer
	 * @param document the document
	 * @return the result
	 */
	@Override
	public BizletMetaData getMetaDataBizlet(Customer customer, Document document) {
		BizletMetaData result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getMetaDataBizlet(customer, document);
		}
		return result;
	}

	/**
	 * Executes validateCustomerForGenerateDomain.
	 * @param customer the customer
	 */
	@Override
	public void validateCustomerForGenerateDomain(Customer customer) {
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			delegate.validateCustomerForGenerateDomain(customer);
		}
	}

	/**
	 * Executes validateModuleForGenerateDomain.
	 * @param customer the customer
	 * @param module the module
	 */
	@Override
	public void validateModuleForGenerateDomain(Customer customer, Module module) {
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			delegate.validateModuleForGenerateDomain(customer, module);
		}
	}

	/**
	 * Executes validateDocumentForGenerateDomain.
	 * @param customer the customer
	 * @param document the document
	 */
	@Override
	public void validateDocumentForGenerateDomain(Customer customer, Document document) {
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			delegate.validateDocumentForGenerateDomain(customer, document);
		}
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
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			delegate.validateViewForGenerateDomain(customer, document, view, uxui);
		}
	}

	/**
	 * Returns the globalRouter.
	 * @return the result
	 */
	@Override
	public Router getGlobalRouter() {
		Router result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getGlobalRouter();
		}
		return result;
	}

	/**
	 * Returns the moduleRouters.
	 * @return the result
	 */
	@Override
	public List<Router> getModuleRouters() {
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			return delegate.getModuleRouters();
		}
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
		String result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getReportFileName(customer, document, reportName);
		}
		return result;
	}
	
	/**
	 * Returns the javaClass.
	 * @param customer the customer
	 * @param key the key
	 * @return the result
	 */
	@Override
	public Class<?> getJavaClass(Customer customer, String key) {
		Class<?> result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.getJavaClass(customer, key);
		}
		return result;
	}
	
	/**
	 * Executes vtable.
	 * @param customerName the customerName
	 * @param key the key
	 * @return the result
	 */
	@Override
	public String vtable(String customerName, String key) {
		String result = null;
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			result = delegate.vtable(customerName, key);
		}
		return result;
	}
	
	/**
	 * Returns the useScaffoldedViews.
	 * @return the result
	 */
	@Override
	public boolean getUseScaffoldedViews() {
		ProvidedRepository delegate = getSessionDelegate();
		if (delegate != null) {
			return delegate.getUseScaffoldedViews();
		}
		return false;
	}
}
