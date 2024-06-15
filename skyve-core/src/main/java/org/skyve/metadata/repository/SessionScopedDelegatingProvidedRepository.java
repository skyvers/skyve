package org.skyve.metadata.repository;

import java.io.File;
import java.sql.Connection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.ProvidedRepositoryDelegate;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
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

/**
 * Implements a repository that delegates to a map of other repository delegates keyed by the User's session ID.
 * This is thread-safe and setSessionDelegate() and removeSessionDelegate() can be called safely at any time.
 * All ProvidedRepositoryDelegate implementations can call getDelegator() to recursively get the top of the delegating hierarchy
 * to call repository functions on related meta-data.
 */
public class SessionScopedDelegatingProvidedRepository extends ProvidedRepositoryDelegate {
	private ConcurrentHashMap<String, ProvidedRepository> sessionScopedDelegates = new ConcurrentHashMap<>();
	
	private ProvidedRepository getDelegate() {
		String sessionId = null;
		if (AbstractPersistence.isPresent()) {
			User user = AbstractPersistence.get().getUser();
			if (user != null) {
				sessionId = user.getSessionId();
			}
		}
		return (sessionId == null) ? null : sessionScopedDelegates.get(sessionId);
	}
	
	public void setSessionDelegate(@Nonnull User user, @Nonnull ProvidedRepository delegate) {
		String sessionId = user.getSessionId();
		if (sessionId == null) {
			throw new IllegalStateException("User " + user.getName() + " does not belong to a session");
		}
		ProvidedRepository oldDelegate = sessionScopedDelegates.put(sessionId, delegate);
		if (oldDelegate != null) {
			oldDelegate.setDelegator(null);
		}
		delegate.setDelegator(this);
	}

	public void removeSessionDelegate(@Nonnull User user) {
		String sessionId = user.getSessionId();
		if (sessionId == null) {
			throw new IllegalStateException("User " + user.getName() + " does not belong to a session");
		}
		ProvidedRepository oldDelegate = sessionScopedDelegates.remove(sessionId);
		if (oldDelegate != null) {
			oldDelegate.setDelegator(null);
		}
	}

	@Override
	public void evictCachedMetaData(Customer customer) {
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			delegate.evictCachedMetaData(customer);
		}
	}

	@Override
	public File findResourceFile(String resourcePath, String customerName, String moduleName) {
		File result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.findResourceFile(resourcePath, customerName, moduleName);
		}
		return result;
	}

	@Override
	public Router getRouter() {
		Router result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getRouter();
		}
		return result;
	}

	@Override
	public Customer getCustomer(String customerName) {
		Customer result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getCustomer(customerName);
		}
		return result;
	}

	@Override
	public <T extends Bean> DynamicImage<T> getDynamicImage(Customer customer, Document document, String imageName, boolean runtime) {
		DynamicImage<T> result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getDynamicImage(customer, document, imageName, runtime);
		}
		return result;
	}

	@Override
	public View getView(String uxui, Customer customer, Document document, String name) {
		View result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getView(uxui, customer, document, name);
		}
		return result;
	}

	@Override
	public ActionMetaData getMetaDataAction(Customer customer, Document document, String actionName) {
		ActionMetaData result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getMetaDataAction(customer, document, actionName);
		}
		return result;
	}

	@Override
	public <T extends Bean, C extends Bean> ComparisonModel<T, C> getComparisonModel(Customer customer, Document document, String modelName, boolean runtime) {
		ComparisonModel<T, C> result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getComparisonModel(customer, document, modelName, runtime);
		}
		return result;
	}

	@Override
	public <T extends Bean> MapModel<T> getMapModel(Customer customer, Document document, String modelName, boolean runtime) {
		MapModel<T> result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getMapModel(customer, document, modelName, runtime);
		}
		return result;
	}

	@Override
	public <T extends Bean> ChartModel<T> getChartModel(Customer customer, Document document, String modelName, boolean runtime) {
		ChartModel<T> result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getChartModel(customer, document, modelName, runtime);
		}
		return result;
	}

	@Override
	public <T extends Bean> ListModel<T> getListModel(Customer customer, Document document, String modelName, boolean runtime) {
		ListModel<T> result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getListModel(customer, document, modelName, runtime);
		}
		return result;
	}

	@Override
	public ServerSideAction<Bean> getServerSideAction(Customer customer, Document document, String className, boolean runtime) {
		ServerSideAction<Bean> result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getServerSideAction(customer, document, className, runtime);
		}
		return result;
	}

	@Override
	public BizExportAction getBizExportAction(Customer customer, Document document, String className, boolean runtime) {
		BizExportAction result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getBizExportAction(customer, document, className, runtime);
		}
		return result;
	}

	@Override
	public BizImportAction getBizImportAction(Customer customer, Document document, String className, boolean runtime) {
		BizImportAction result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getBizImportAction(customer, document, className, runtime);
		}
		return result;
	}

	@Override
	public DownloadAction<Bean> getDownloadAction(Customer customer, Document document, String className, boolean runtime) {
		DownloadAction<Bean> result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getDownloadAction(customer, document, className, runtime);
		}
		return result;
	}

	@Override
	public UploadAction<Bean> getUploadAction(Customer customer, Document document, String className, boolean runtime) {
		UploadAction<Bean> result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getUploadAction(customer, document, className, runtime);
		}
		return result;
	}

	@Override
	public Object getDataFactory(Customer customer, Document document) {
		Object result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getDataFactory(customer, document);
		}
		return result;
	}

	@Override
	public UserImpl retrieveUser(String userName) {
		UserImpl result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.retrieveUser(userName);
		}
		return result;
	}
	
	@Override
	public List<Bean> retrieveAllJobSchedulesForAllCustomers() {
		List<Bean> result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.retrieveAllJobSchedulesForAllCustomers();
		}
		return result;
	}

	@Override
	public List<Bean> retrieveAllReportSchedulesForAllCustomers() {
		List<Bean> result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.retrieveAllReportSchedulesForAllCustomers();
		}
		return result;
	}
	
	@Override
	public String retrievePublicUserName(String customerName) {
		String result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.retrievePublicUserName(customerName);
		}
		return result;
	}
	
	@Override
	public void resetMenus(User user) {
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			delegate.resetMenus(user);
		}
	}

	@Override
	public void populatePermissions(User user) {
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			delegate.populatePermissions(user);
		}
	}

	@Override
	public void resetUserPermissions(User user) {
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			delegate.resetUserPermissions(user);
		}
	}

	@Override
	public void populateUser(User user, Connection connection) {
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			delegate.populateUser(user, connection);
		}
	}
	
	@Override
	public List<String> getAllCustomerNames() {
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			return delegate.getAllCustomerNames();
		}
		return Collections.emptyList();
	}

	@Override
	public List<String> getAllVanillaModuleNames() {
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			return delegate.getAllVanillaModuleNames();
		}
		return Collections.emptyList();
	}

	@Override
	public Module getModule(Customer customer, String moduleName) {
		Module result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getModule(customer, moduleName);
		}
		return result;
	}

	@Override
	public Document getDocument(Customer customer, Module module, String documentName) {
		Document result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getDocument(customer, module, documentName);
		}
		return result;
	}

	@Override
	public <T extends Bean> Bizlet<T> getBizlet(Customer customer, Document document, boolean runtime) {
		Bizlet<T> result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getBizlet(customer, document, runtime);
		}
		return result;
	}

	@Override
	public BizletMetaData getMetaDataBizlet(Customer customer, Document document) {
		BizletMetaData result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getMetaDataBizlet(customer, document);
		}
		return result;
	}

	@Override
	public void validateCustomerForGenerateDomain(Customer customer) {
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			delegate.validateCustomerForGenerateDomain(customer);
		}
	}

	@Override
	public void validateModuleForGenerateDomain(Customer customer, Module module) {
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			delegate.validateModuleForGenerateDomain(customer, module);
		}
	}

	@Override
	public void validateDocumentForGenerateDomain(Customer customer, Document document) {
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			delegate.validateDocumentForGenerateDomain(customer, document);
		}
	}

	@Override
	public void validateViewForGenerateDomain(Customer customer, Document document, View view, String uxui) {
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			delegate.validateViewForGenerateDomain(customer, document, view, uxui);
		}
	}

	@Override
	public Router getGlobalRouter() {
		Router result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getGlobalRouter();
		}
		return result;
	}

	@Override
	public List<Router> getModuleRouters() {
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			return delegate.getModuleRouters();
		}
		return Collections.emptyList();
	}

	@Override
	public String getReportFileName(Customer customer, Document document, String reportName) {
		String result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getReportFileName(customer, document, reportName);
		}
		return result;
	}
	
	@Override
	public Class<?> getJavaClass(Customer customer, String key) {
		Class<?> result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.getJavaClass(customer, key);
		}
		return result;
	}
	
	@Override
	public String vtable(String customerName, String key) {
		String result = null;
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			result = delegate.vtable(customerName, key);
		}
		return result;
	}
	
	@Override
	public boolean getUseScaffoldedViews() {
		ProvidedRepository delegate = getDelegate();
		if (delegate != null) {
			return delegate.getUseScaffoldedViews();
		}
		return false;
	}
}
