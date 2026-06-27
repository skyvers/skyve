package org.skyve.metadata.repository;

import java.io.File;
import java.sql.Connection;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
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
 * Composes multiple {@link ProvidedRepository} delegates as an ordered lookup
 * chain.
 *
 * <p>Query methods iterate delegates in insertion order and return the first
 * non-null result. Mutation-style methods broadcast to all delegates.
 *
 * <p>Threading: chain structure updates are safe via
 * {@link CopyOnWriteArrayList}; delegate method thread-safety depends on each
 * delegate implementation.
 */
public class DelegatingProvidedRepositoryChain extends ProvidedRepositoryFactory {
	/**
	 * The list of delegate repositories.
	 * This is a CopyOnWriteArrayList so that read operations are not synchronized and all 
	 * delegate iterations will be safe from concurrent modifications by multiple threads.
	 * This is a suitable choice as this is a small, stable, read-mostly collection.
	 */
	protected List<ProvidedRepository> delegates = new CopyOnWriteArrayList<>();
	
	/**
	 * Creates a new DelegatingProvidedRepositoryChain instance.
	 * @param delegates the delegates
	 */
	public DelegatingProvidedRepositoryChain(@Nonnull ProvidedRepository... delegates) {
		for (ProvidedRepository delegate : delegates) {
			addDelegate(delegate);
		}
	}

	/**
	 * Executes addDelegate.
	 * @param delegate the delegate
	 */
	public void addDelegate(@Nonnull ProvidedRepository delegate) {
		delegates.add(delegate);
	}

	/**
	 * Executes addDelegate.
	 * @param index the index
	 * @param delegate the delegate
	 */
	public void addDelegate(int index, @Nonnull ProvidedRepository delegate) {
		delegates.add(index, delegate);
	}

	/**
	 * Executes removeDelegate.
	 * @param delegate the delegate
	 */
	public void removeDelegate(@Nonnull ProvidedRepository delegate) {
		delegates.remove(delegate);
	}

	/**
	 * Executes removeDelegate.
	 * @param index the index
	 */
	public void removeDelegate(int index) {
		delegates.remove(index);
	}

	/**
	 * Executes evictCachedMetaData.
	 * @param customer the customer
	 */
	@Override
	public void evictCachedMetaData(Customer customer) {
		for (ProvidedRepository delegate : delegates) {
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
		for (ProvidedRepository delegate : delegates) {
			File result = delegate.findResourceFile(resourcePath, customerName, moduleName);
			if (result != null) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the router.
	 * @return the result
	 */
	@Override
	public Router getRouter() {
		for (ProvidedRepository delegate : delegates) {
			Router result = delegate.getRouter();
			if (result != null) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the customer.
	 * @param customerName the customerName
	 * @return the result
	 */
	@Override
	public Customer getCustomer(String customerName) {
		for (ProvidedRepository delegate : delegates) {
			Customer result = delegate.getCustomer(customerName);
			if (result != null) {
				return result;
			}
		}
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
		for (ProvidedRepository delegate : delegates) {
			DynamicImage<T> result = delegate.getDynamicImage(customer, document, imageName, runtime);
			if (result != null) {
				return result;
			}
		}
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
		for (ProvidedRepository delegate : delegates) {
			View result = delegate.getView(uxui, customer, document, name);
			if (result != null) {
				return result;
			}
		}
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
		for (ProvidedRepository delegate : delegates) {
			ActionMetaData result = delegate.getMetaDataAction(customer, document, actionName);
			if (result != null) {
				return result;
			}
		}
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
		for (ProvidedRepository delegate : delegates) {
			ComparisonModel<T, C> result = delegate.getComparisonModel(customer, document, modelName, runtime);
			if (result != null) {
				return result;
			}
		}
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
		for (ProvidedRepository delegate : delegates) {
			MapModel<T> result = delegate.getMapModel(customer, document, modelName, runtime);
			if (result != null) {
				return result;
			}
		}
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
		for (ProvidedRepository delegate : delegates) {
			ChartModel<T> result = delegate.getChartModel(customer, document, modelName, runtime);
			if (result != null) {
				return result;
			}
		}
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
		for (ProvidedRepository delegate : delegates) {
			ListModel<T> result = delegate.getListModel(customer, document, modelName, runtime);
			if (result != null) {
				return result;
			}
		}
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
		for (ProvidedRepository delegate : delegates) {
			ServerSideAction<Bean> result = delegate.getServerSideAction(customer, document, className, runtime);
			if (result != null) {
				return result;
			}
		}
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
		for (ProvidedRepository delegate : delegates) {
			BizExportAction result = delegate.getBizExportAction(customer, document, className, runtime);
			if (result != null) {
				return result;
			}
		}
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
		for (ProvidedRepository delegate : delegates) {
			BizImportAction result = delegate.getBizImportAction(customer, document, className, runtime);
			if (result != null) {
				return result;
			}
		}
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
		for (ProvidedRepository delegate : delegates) {
			DownloadAction<Bean> result = delegate.getDownloadAction(customer, document, className, runtime);
			if (result != null) {
				return result;
			}
		}
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
		for (ProvidedRepository delegate : delegates) {
			UploadAction<Bean> result = delegate.getUploadAction(customer, document, className, runtime);
			if (result != null) {
				return result;
			}
		}
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
		for (ProvidedRepository delegate : delegates) {
			Object result = delegate.getDataFactory(customer, document);
			if (result != null) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Executes retrieveUser.
	 * @param userName the userName
	 * @return the result
	 */
	@Override
	public UserImpl retrieveUser(String userName) {
		for (ProvidedRepository delegate : delegates) {
			UserImpl result = delegate.retrieveUser(userName);
			if (result != null) {
				return result;
			}
		}
		return null;
	}
	
	/**
	 * Executes retrieveAllScheduledJobsForAllCustomers.
	 * @return the result
	 */
	@Override
	public List<UserJobSchedule> retrieveAllScheduledJobsForAllCustomers() {
		List<UserJobSchedule> result = new ArrayList<>(10);
		for (ProvidedRepository delegate : delegates) {
			result.addAll(delegate.retrieveAllScheduledJobsForAllCustomers());
		}
		return result;
	}

	/**
	 * Executes retrieveAllScheduledReportsForAllCustomers.
	 * @return the result
	 */
	@Override
	public List<UserJobSchedule> retrieveAllScheduledReportsForAllCustomers() {
		List<UserJobSchedule> result = new ArrayList<>(10);
		for (ProvidedRepository delegate : delegates) {
			result.addAll(delegate.retrieveAllScheduledReportsForAllCustomers());
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
		for (ProvidedRepository delegate : delegates) {
			String result = delegate.retrievePublicUserName(customerName);
			if (result != null) {
				return result;
			}
		}
		return null;
	}
	
	/**
	 * Executes resetMenus.
	 * @param user the user
	 */
	@Override
	public void resetMenus(User user) {
		for (ProvidedRepository delegate : delegates) {
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
	    for (ProvidedRepository delegate : delegates) {
			if (delegate.populatePermissions(user)) {
	            return true;
	        }
	    }
	    return false;
	}

	/**
	 * Executes resetUserPermissions.
	 * @param user the user
	 */
	@Override
	public void resetUserPermissions(User user) {
		for (ProvidedRepository delegate : delegates) {
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
	@SuppressWarnings("resource") // Connection lifecycle is owned by the caller and delegates.
	public boolean populateUser(User user, Connection connection) {
		for (ProvidedRepository delegate : delegates) {
			if (delegate.populateUser(user, connection)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Returns the allCustomerNames.
	 * @return the result
	 */
	@Override
	public List<String> getAllCustomerNames() {
		List<String> result = new ArrayList<>(10);
		for (ProvidedRepository delegate : delegates) {
			result.addAll(delegate.getAllCustomerNames());
		}
		return result;
	}

	/**
	 * Returns the allVanillaModuleNames.
	 * @return the result
	 */
	@Override
	public List<String> getAllVanillaModuleNames() {
		List<String> result = new ArrayList<>(10);
		for (ProvidedRepository delegate : delegates) {
			result.addAll(delegate.getAllVanillaModuleNames());
		}
		return result;
	}

	/**
	 * Returns the module.
	 * @param customer the customer
	 * @param moduleName the moduleName
	 * @return the result
	 */
	@Override
	public Module getModule(Customer customer, String moduleName) {
		for (ProvidedRepository delegate : delegates) {
			Module result = delegate.getModule(customer, moduleName);
			if (result != null) {
				return result;
			}
		}
		throw new MetaDataException(moduleName + " does not exist" + ((customer == null) ? "" : " for customer " + customer.getName()));
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
		for (ProvidedRepository delegate : delegates) {
			Document result = delegate.getDocument(customer, module, documentName);
			if (result != null) {
				return result;
			}
		}
		throw new MetaDataException(documentName + " does not exist for module " + module.getName() + ((customer == null) ? "" : " for customer " + customer.getName()));
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
		for (ProvidedRepository delegate : delegates) {
			Bizlet<T> result = delegate.getBizlet(customer, document, runtime);
			if (result != null) {
				return result;
			}
		}
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
		for (ProvidedRepository delegate : delegates) {
			BizletMetaData result = delegate.getMetaDataBizlet(customer, document);
			if (result != null) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Executes validateCustomerForGenerateDomain.
	 * @param customer the customer
	 */
	@Override
	public void validateCustomerForGenerateDomain(Customer customer) {
		for (ProvidedRepository delegate : delegates) {
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
		for (ProvidedRepository delegate : delegates) {
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
		for (ProvidedRepository delegate : delegates) {
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
		for (ProvidedRepository delegate : delegates) {
			delegate.validateViewForGenerateDomain(customer, document, view, uxui);
		}
	}

	/**
	 * Returns the globalRouter.
	 * @return the result
	 */
	@Override
	public Router getGlobalRouter() {
		for (ProvidedRepository delegate : delegates) {
			Router result = delegate.getGlobalRouter();
			if (result != null) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the moduleRouters.
	 * @return the result
	 */
	@Override
	public List<Router> getModuleRouters() {
		List<Router> result = new ArrayList<>(10);
		for (ProvidedRepository delegate : delegates) {
			result.addAll(delegate.getModuleRouters());
		}
		return result;
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
		for (ProvidedRepository delegate : delegates) {
			String result = delegate.getReportFileName(customer, document, reportName);
			if (result != null) {
				return result;
			}
		}
		return null;
	}
	
	/**
	 * Returns the javaClass.
	 * @param customer the customer
	 * @param key the key
	 * @return the result
	 */
	@Override
	public Class<?> getJavaClass(Customer customer, String key) {
		for (ProvidedRepository delegate : delegates) {
			Class<?> result = delegate.getJavaClass(customer, key);
			if (result != null) {
				return result;
			}
		}
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
		for (ProvidedRepository delegate : delegates) {
			String result = delegate.vtable(customerName, key);
			if (result != null) {
				return result;
			}
		}
		return null;
	}
	
	/**
	 * Returns the useScaffoldedViews.
	 * @return the result
	 */
	@Override
	public boolean getUseScaffoldedViews() {
		for (ProvidedRepository delegate : delegates) {
			boolean use = delegate.getUseScaffoldedViews();
			if (use) {
				return true;
			}
		}
		return false;
	}
}
