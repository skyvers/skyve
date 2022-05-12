package org.skyve.metadata.repository;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.ProvidedRepositoryDelegate;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.user.UserImpl;
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

public class DelegatingProvidedRepositoryChain extends ProvidedRepositoryDelegate {
	protected List<ProvidedRepository> delegates;
	
	public DelegatingProvidedRepositoryChain(ProvidedRepository... delegates) {
		this.delegates = new ArrayList<>(delegates.length);
		for (ProvidedRepository delegate : delegates) {
			addDelegate(delegate);
		}
	}

	public synchronized void addDelegate(ProvidedRepository delegate) {
		if (delegates.add(delegate)) {
			delegate.setDelegator(this);
		}
	}

	public synchronized void addDelegate(int index, ProvidedRepository delegate) {
		delegates.add(index, delegate);
		delegate.setDelegator(this);
	}

	public synchronized void removeDelegate(ProvidedRepository delegate) {
		if (delegates.remove(delegate)) {
			delegate.setDelegator(null);
		}
	}

	public synchronized void removeDelegate(int index) {
		ProvidedRepository delegate = delegates.remove(index);
		if (delegate != null) {
			delegate.setDelegator(null);
		}
	}

	@Override
	public void evictCachedMetaData(Customer customer) {
		for (ProvidedRepository delegate : delegates) {
			delegate.evictCachedMetaData(customer);
		}
	}

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

	@Override
	public Object getDataFactory(Customer customer, String moduleName, String documentName) {
		for (ProvidedRepository delegate : delegates) {
			Object result = delegate.getDataFactory(customer, moduleName, documentName);
			if (result != null) {
				return result;
			}
		}
		return null;
	}

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

	@Override
	public void resetMenus(User user) {
		for (ProvidedRepository delegate : delegates) {
			delegate.resetMenus(user);
		}
	}

	@Override
	public void populatePermissions(User user) {
		for (ProvidedRepository delegate : delegates) {
			delegate.populatePermissions(user);
		}
	}

	@Override
	public void resetUserPermissions(User user) {
		for (ProvidedRepository delegate : delegates) {
			delegate.resetUserPermissions(user);
		}
	}

	@Override
	public List<String> getAllCustomerNames() {
		List<String> result = new ArrayList<>(10);
		for (ProvidedRepository delegate : delegates) {
			List<String> customerNames = delegate.getAllCustomerNames();
			if (customerNames != null) {
				result.addAll(customerNames);
			}
		}
		return result;
	}

	@Override
	public List<String> getAllVanillaModuleNames() {
		List<String> result = new ArrayList<>(10);
		for (ProvidedRepository delegate : delegates) {
			List<String> moduleNames = delegate.getAllVanillaModuleNames();
			if (moduleNames != null) {
				result.addAll(moduleNames);
			}
		}
		return result;
	}

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

	@Override
	public void validateCustomerForGenerateDomain(Customer customer) {
		for (ProvidedRepository delegate : delegates) {
			delegate.validateCustomerForGenerateDomain(customer);
		}
	}

	@Override
	public void validateModuleForGenerateDomain(Customer customer, Module module) {
		for (ProvidedRepository delegate : delegates) {
			delegate.validateModuleForGenerateDomain(customer, module);
		}
	}

	@Override
	public void validateDocumentForGenerateDomain(Customer customer, Document document) {
		for (ProvidedRepository delegate : delegates) {
			delegate.validateDocumentForGenerateDomain(customer, document);
		}
	}

	@Override
	public void validateViewForGenerateDomain(Customer customer, Document document, View view, String uxui) {
		for (ProvidedRepository delegate : delegates) {
			delegate.validateViewForGenerateDomain(customer, document, view, uxui);
		}
	}

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

	@Override
	public List<Router> getModuleRouters() {
		List<Router> result = new ArrayList<>(10);
		for (ProvidedRepository delegate : delegates) {
			List<Router> routers = delegate.getModuleRouters();
			if (routers != null) {
				result.addAll(routers);
			}
		}
		return result;
	}

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
