package org.skyve.impl.metadata.repository;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.deltaspike.core.api.provider.BeanProvider;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SkyveException;
import org.skyve.impl.metadata.repository.customer.CustomerMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerModuleMetaData;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.RouterMerger;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.metadata.controller.BizImportAction;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Dynamic;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.metadata.view.model.comparison.ComparisonModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.map.MapModel;

public abstract class FileSystemRepository extends MutableCachedRepository {
	protected String absolutePath;
	private String canonicalBasePath;
	protected boolean loadClasses = true;
	// class maps
	private ConcurrentHashMap<String, Class<?>> classes = new ConcurrentHashMap<>();
	
	/**
	 * Absolute path constructor
	 * Prevent external instantiation.
	 */
	protected FileSystemRepository(String absolutePath) {
		this.absolutePath = absolutePath.replace('\\', '/');
		if (this.absolutePath.charAt(this.absolutePath.length() - 1) != '/') {
			this.absolutePath += '/';
		}
		if (canonicalBasePath == null) {
			try {
				canonicalBasePath = new File(absolutePath).getCanonicalPath();
			}
			catch (IOException e) {
				throw new MetaDataException("Cannot determine canonical absolute path", e);
			}
		}
		populateKeys();
	}

	/**
	 * Absolute path and load classes constructor
	 * Prevent external instantiation.
	 */
	protected FileSystemRepository(String absolutePath, boolean loadClasses) {
		this(absolutePath);
		this.loadClasses = loadClasses;
	}

	/**
	 * Default constructor
	 * Prevent external instantiation.
	 */
	protected FileSystemRepository() {
		this(UtilImpl.getAbsoluteBasePath());
	}

	@Override
	public void populateKeys() {
		// Add router key
		addKey(ROUTER_KEY);
		
		StringBuilder sb = new StringBuilder(256);
		for (String customerName : getAllCustomerNames()) {
			// Add customer key
			addKey(CUSTOMERS_NAMESPACE + customerName);
			
			CustomerMetaData customer = loadCustomer(customerName);
			for (CustomerModuleMetaData module : customer.getModules().getModules()) {
				String moduleName = module.getName();
				// populate the module XML file from the customer overridden area.
				populateModuleLocation(customerName, moduleName);

				// Populate modules from the customer overridden area
				sb.setLength(0);
				sb.append(CUSTOMERS_NAMESPACE).append(customerName).append('/');
				sb.append(MODULES_NAMESPACE).append(moduleName).append('/');
				String key = sb.toString();
				if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info("module location = " + key);
				populateDocumentLocations(key);
			}
		}

		// Populate from the module area
		for (String moduleName : getAllVanillaModuleNames()) {
			// populate the repository location of the module XML file.
			populateModuleLocation(null, moduleName);

			sb.setLength(0);
			sb.append(MODULES_NAMESPACE).append(moduleName).append('/');
			String key = sb.toString();
			if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info("module location = " + key);
			populateDocumentLocations(key);
		}
	}

	private void populateModuleLocation(String customerName, String moduleName) {
		StringBuilder sb = new StringBuilder(256);
		if (customerName == null) {
			String key = MODULES_NAMESPACE + moduleName;
			sb.setLength(0);
			sb.append(absolutePath).append(key).append('/').append(moduleName).append(".xml");
			File moduleFile = new File(sb.toString());
			if (moduleFile.exists()) {
				if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(moduleName + " -> " + key);
				addKey(key);
			}
		}
		else {
			sb.append(CUSTOMERS_NAMESPACE).append(customerName).append('/');
			sb.append(MODULES_NAMESPACE).append(moduleName);
			String key = sb.toString();
			sb.setLength(0);
			sb.append(absolutePath).append(key).append('/').append(moduleName).append(".xml");
			File moduleFile = new File(sb.toString());
			if (moduleFile.exists()) {
				if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(moduleName + " -> " + key);
				addKey(key);
			}
		}
	}

	private void populateDocumentLocations(String key) {
		StringBuilder sb = new StringBuilder(256);

		File moduleDirectory = new File(absolutePath + key);
		if (moduleDirectory.exists() && moduleDirectory.isDirectory()) {
			for (File moduleFile : moduleDirectory.listFiles()) {
				String moduleFileName = moduleFile.getName();
				if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info("module file name = " + moduleFileName);

				// we have found some modules
				if (moduleFile.isDirectory()) {
					for (File documentFile : moduleFile.listFiles()) {
						String documentFileName = documentFile.getName();
						// found the document actions directory
						if (documentFileName.equals(ACTIONS_NAME) && documentFile.isDirectory()) {
							for (File actionFile : documentFile.listFiles()) {
								String actionFileName = actionFile.getName();
								if (actionFileName.endsWith(".class") || actionFileName.endsWith(".java")) {
									String actionName = actionFileName.substring(0, actionFileName.lastIndexOf('.'));

									sb.setLength(0);
									sb.append(key).append(moduleFileName).append('/');
									sb.append(ACTIONS_NAMESPACE).append(actionName);
									String actionLocation = sb.toString();
									if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("Action ").append(actionName).append(" -> ").append(actionLocation).toString());
									addKey(actionLocation);
								}
							}
						}
						// found the document images directory
						else if (documentFileName.equals(IMAGES_NAME) && documentFile.isDirectory()) {
							for (File imageFile : documentFile.listFiles()) {
								String imageFileName = imageFile.getName();
								if (imageFileName.endsWith(".class") || imageFileName.endsWith(".java")) {
									String imageName = imageFileName.substring(0, imageFileName.lastIndexOf('.'));
									sb.setLength(0);
									sb.append(key).append(moduleFileName).append('/');
									sb.append(IMAGES_NAMESPACE).append(imageName);
									String imageLocation = sb.toString();
									if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("Dynamic Image ").append(imageName).append(" -> ").append(imageLocation).toString());
									addKey(imageLocation);
								}
							}
						}
						// found the document models directory
						else if (documentFileName.equals(MODELS_NAME) && documentFile.isDirectory()) {
							for (File modelFile : documentFile.listFiles()) {
								String modelFileName = modelFile.getName();
								if (modelFileName.endsWith(".class") || modelFileName.endsWith(".java")) {
									String modelName = modelFileName.substring(0, modelFileName.lastIndexOf('.'));
									sb.setLength(0);
									sb.append(key).append(moduleFileName).append('/');
									sb.append(MODELS_NAMESPACE).append(modelName);
									String modelLocation = sb.toString();
									if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("Model ").append(modelName).append(" -> ").append(modelLocation).toString());
									addKey(modelLocation);
								}
							}
						}
						// found the document reports directory
						else if (documentFileName.equals(REPORTS_NAME) && documentFile.isDirectory()) {
							for (File reportFile : documentFile.listFiles()) {
								String reportFileName = reportFile.getName();
								if (reportFileName.endsWith(".jasper")) {
									String reportName = reportFileName.substring(0, reportFileName.length() - 7);
									sb.setLength(0);
									sb.append(key).append(moduleFileName).append('/');
									sb.append(REPORTS_NAMESPACE).append(reportName);
									String reportLocation = sb.toString();
									if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("Report ").append(reportName).append(" -> ").append(reportLocation).toString());
									addKey(reportLocation);
								}
							}
						}
						// found the document views directory
						else if (documentFileName.equals(VIEWS_NAME) && documentFile.isDirectory()) {
							for (File viewFile : documentFile.listFiles()) {
								String viewFileName = viewFile.getName();
								if (viewFileName.endsWith(".xml")) { // found a view file
									String viewType = viewFileName.substring(0, viewFileName.length() - 4);
									sb.setLength(0);
									sb.append(key).append(moduleFileName).append('/');
									sb.append(VIEWS_NAMESPACE).append(viewType);
									String viewLocation = sb.toString();
									if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("View ").append(viewType).append(" -> ").append(viewLocation).toString());
									addKey(viewLocation);
								}
								else if (viewFile.isDirectory()) {
									for (File uxuiViewFile : viewFile.listFiles()) {
										String uxuiViewFileName = uxuiViewFile.getName();
										if (uxuiViewFileName.endsWith(".xml")) { // found a view file
											String viewType = uxuiViewFileName.substring(0, uxuiViewFileName.length() - 4);
											sb.setLength(0);
											sb.append(key).append(moduleFileName).append('/');
											sb.append(VIEWS_NAMESPACE).append(viewFileName).append('/').append(viewType);
											String viewLocation = sb.toString();
											if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("View ").append(viewType).append(" -> ").append(viewLocation).toString());
											addKey(viewLocation);
										}
									}
								}
							}
						}
						// found the bizlet class file
						else if (documentFileName.equals(moduleFileName + "Bizlet.class")) {
							sb.setLength(0);
							sb.append(key).append(moduleFileName).append('/');
							sb.append(moduleFileName).append("Bizlet");
							String bizletLocation = sb.toString();
							if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("Bizlet ").append(moduleFileName).append(" -> ").append(bizletLocation).toString());
							addKey(bizletLocation);
						}
						// found the extension class file
						else if (documentFileName.equals(moduleFileName + "Extension.class")) {
							sb.setLength(0);
							sb.append(key).append(moduleFileName).append('/');
							sb.append(moduleFileName).append("Extension");
							String extensionLocation = sb.toString();
							if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("Extension ").append(moduleFileName).append(" -> ").append(extensionLocation).toString());
							addKey(extensionLocation);
						}
						// found the factory class file
						else if (documentFileName.equals(moduleFileName + "Factory.class")) {
							sb.setLength(0);
							sb.append(key).append(moduleFileName).append('/');
							sb.append(moduleFileName).append("Factory");
							String factoryLocation = sb.toString();
							if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("Factory ").append(moduleFileName).append(" -> ").append(factoryLocation).toString());
							addKey(factoryLocation);
						}
						// found the document definition file
						else if (documentFileName.equals(moduleFileName + ".xml")) {
							String documentLocation = key + moduleFileName;
							if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("Document ").append(moduleFileName).append(" -> ").append(documentLocation).toString());
							addKey(documentLocation);
						}
					} // for (all document files)
				} // if (moduleFile is a directory)
			} // for (all module files)
		} // if (module directory exists)
	}

	@Override
	public Router loadRouter() {
		Router result = null;

		try {
			final Map<String, Long> routersFileInfo = routersFileInfo(true, true);
			if (routersFileInfo.isEmpty()) {
				throw new MetaDataException("No routers found.");
			}
			final List<Router> routers = new ArrayList<>(routersFileInfo.size());
			for (String path : routersFileInfo.keySet()) {
				Router router = XMLMetaData.unmarshalRouterFile(path);
				router = router.convert(ROUTER_NAME, getDelegator());
				routers.add(router);
			}

			result = new RouterMerger().mergeRouters(routers);
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}
		
		return result;
	}

	/**
	 * Return the max(lastModified) from all the router files
	 */
	@Override
	public long routerLastModifiedMillis() {
		final Map<String, Long> routersFileInfo = routersFileInfo(true, true);
		if (routersFileInfo.isEmpty()) {
			return Long.MIN_VALUE;
		}
 		return routersFileInfo.values().stream().max(Comparator.naturalOrder()).get().longValue();
	}
	
	private Map<String, Long> routersFileInfo(boolean includeGlobal, boolean includeModule) {
		Map<String, Long> result = new LinkedHashMap<>(); // keep the global one first
		StringBuilder sb = new StringBuilder(256);
		String s = null;
		File f = null;
		
		if (includeGlobal) {
			sb.append(absolutePath);
			sb.append(ROUTER_NAMESPACE).append(ROUTER_NAME).append(".xml");
			s = sb.toString();
			f = new File(s);
			if (f.exists()) {
				result.put(s, Long.valueOf(f.lastModified()));
			}
			sb.setLength(0);
		}
		
		if (includeModule) {
			// Add customer overrides
			for (String customerName : getAllCustomerNames()) {
				final Customer customer = getCustomer(customerName);
				for (Module module : customer.getModules()) {
					sb.append(absolutePath);
					sb.append(CUSTOMERS_NAMESPACE).append(customerName).append('/').append(module.getName()).append("/").append(ROUTER_NAME).append(".xml");
					s = sb.toString();
					f = new File(s);
					if (f.exists()) {
						result.put(s, Long.valueOf(f.lastModified()));
					}
					sb.setLength(0);
				}
			}
			
			// Add vanilla modules
			for (String moduleName : getAllVanillaModuleNames()) {
				sb.append(absolutePath);
				sb.append(MODULES_NAMESPACE).append(moduleName).append("/").append(ROUTER_NAME).append(".xml");
				s = sb.toString();
				f = new File(s);
				if (f.exists()) {
					result.put(s, Long.valueOf(f.lastModified()));
				}
				sb.setLength(0);
			}
		}
		
		return result;
	}
	
	@Override
	public Router getGlobalRouter() {
		final Map<String, Long> routersFileInfo = routersFileInfo(true, false);
 		Optional<String> path = routersFileInfo.keySet().stream().findFirst();
 		if (path.isPresent()) {
			Router router = XMLMetaData.unmarshalRouterFile(path.get());
			return router.convert(ROUTER_NAME, getDelegator());
 		}
 		return null;
	}
	
	/**
	 * @return A list of self-contained module Routers.
	 */
	@Override
	public List<Router> getModuleRouters() {
		final Map<String, Long> routersFileInfo = routersFileInfo(false, true);
		final List<Router> result = new ArrayList<>(routersFileInfo.size());
		for (String path : routersFileInfo.keySet()) {
			Router router = XMLMetaData.unmarshalRouterFile(path);
			router = router.convert(ROUTER_NAME, getDelegator());
			result.add(router);
		}
		
		return result;
	}

	@Override
	public List<String> getAllCustomerNames() {
		List<String> result = new ArrayList<>();
		
		File customersDirectory = new File(absolutePath + CUSTOMERS_NAMESPACE);
		if (customersDirectory.exists() && customersDirectory.isDirectory()) {
			for (File customerDirectory : customersDirectory.listFiles()) {
				if (customerDirectory.isDirectory() && (customerDirectory.getName().charAt(0) != '.')) {
					result.add(customerDirectory.getName());
				}
			}
		}

		return result;
	}

	@Override
	public List<String> getAllVanillaModuleNames() {
		List<String> result = new ArrayList<>();

		File modulesDirectory = new File(absolutePath + MODULES_NAMESPACE);
		if (modulesDirectory.exists() && modulesDirectory.isDirectory()) {
			for (File moduleDirectory : modulesDirectory.listFiles()) {
				if (moduleDirectory.isDirectory() && (moduleDirectory.getName().charAt(0) != '.')) {
					// make sure there is a module.xml with the same name as the module directory to cater for deleted modules
					for (File moduleChild : moduleDirectory.listFiles()) {
						if (moduleChild.getName().equals(moduleDirectory.getName() + ".xml")) {
							result.add(moduleDirectory.getName());
							break;
						}
					}
				}
			}
		}

		return result;
	}

	private String customerPath(String customerName) {
		StringBuilder result = new StringBuilder(256);
		result.append(absolutePath);
		result.append(CUSTOMERS_NAMESPACE);
		result.append(customerName).append('/').append(customerName).append(".xml");
		return result.toString();
	}
	
	@Override
	public CustomerMetaData loadCustomer(String customerName) {
		CustomerMetaData result = null;
		
		try {
			String path = customerPath(customerName);
			result = XMLMetaData.unmarshalCustomerFile(path);
			if (! customerName.equals(result.getName())) {
				throw new MetaDataException("Customer is defined with file name of " + path + 
												" but the name attribute is " + result.getName());
			}
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}

		return result;
	}
	
	@Override
	public long customerLastModifiedMillis(String customerName) {
		String path = customerPath(customerName);
		File f = new File(path);
		if (f.exists()) {
			return f.lastModified();
		}
		return Long.MIN_VALUE;
	}
	
	private String modulePath(String customerName, String moduleName) {
		StringBuilder result = new StringBuilder(256);
		result.append(absolutePath);
		if (customerName != null) {
			result.append(CUSTOMERS_NAMESPACE).append(customerName).append('/');
		}
		result.append(MODULES_NAMESPACE);
		result.append(moduleName).append('/').append(moduleName).append(".xml");
		return result.toString();
	}
	
	@Override
	public ModuleMetaData loadModule(String customerName, String moduleName) {
		ModuleMetaData result = null;
		
		try {
			String path = modulePath(customerName, moduleName);
			result = XMLMetaData.unmarshalModuleFile(path);
			if (! moduleName.equals(result.getName())) {
				throw new MetaDataException("Module is defined with file name of " + path + 
												" but the name attribute is " + result.getName());
			}
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}

		return result;
	}

	@Override
	public long moduleLastModifiedMillis(String customerName, String moduleName) {
		String path = modulePath(customerName, moduleName);
		File f = new File(path);
		if (f.exists()) {
			return f.lastModified();
		}
		return Long.MIN_VALUE;
	}
	
	private String documentPath(String customerName, String moduleName, String documentName) {
		StringBuilder result = new StringBuilder(256);
		result.append(absolutePath);
		if (customerName != null) {
			result.append(CUSTOMERS_NAMESPACE).append(customerName).append('/');
		}
		result.append(MODULES_NAMESPACE);
		result.append(moduleName).append('/');
		result.append(documentName).append('/').append(documentName).append(".xml");
		return result.toString();
	}
	
	@Override
	public DocumentMetaData loadDocument(String customerName, String moduleName, String documentName) {
		DocumentMetaData result = null;
		
		try {
			String path = documentPath(customerName, moduleName, documentName);
			result = XMLMetaData.unmarshalDocumentFile(path);
			if (! documentName.equals(result.getName())) {
				throw new MetaDataException("Document is defined with file name of " + path + 
												" but the name attribute is " + result.getName());
			}
		} // try (populate Metadata)
		catch (MetaDataException e) {
			throw e;
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}

		return result;
	}

	@Override
	public long documentLastModifiedMillis(String customerName, String moduleName, String documentName) {
		String path = documentPath(customerName, moduleName, documentName);
		File f = new File(path);
		if (f.exists()) {
			return f.lastModified();
		}
		return Long.MIN_VALUE;
	}

	private String viewPath(String customerName, String moduleName, String documentName, String uxui, String viewName) {
		StringBuilder result = new StringBuilder(256);
		result.append(absolutePath);
		if (customerName != null) {
			result.append(CUSTOMERS_NAMESPACE).append(customerName).append('/');
		}
		result.append(MODULES_NAMESPACE);
		result.append(moduleName).append('/');
		result.append(documentName).append('/').append(VIEWS_NAMESPACE);
		if (uxui != null) {
			result.append(uxui).append('/');
		}
		result.append(viewName).append(".xml");
		return result.toString();
	}
	
	@Override
	public ViewMetaData loadView(String customerName, String moduleName, String documentName, String uxui, String viewName) {
		ViewMetaData result = null;
		
		try {
			String path = viewPath(customerName, moduleName, documentName, uxui, viewName);
			result = XMLMetaData.unmarshalViewFile(path);
			if (! viewName.equals(result.getName())) {
				throw new MetaDataException("View is defined with file name of " + path + 
												" but the name attribute is " + result.getName());
			}
		} // try (populate Metadata)
		catch (MetaDataException e) {
			throw e;
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}
		
		return result;
	}

	@Override
	public long viewLastModifiedMillis(String customerName, String moduleName, String documentName, String uxui, String viewName) {
		String path = viewPath(customerName, moduleName, documentName, uxui, viewName);
		File f = new File(path);
		if (f.exists()) {
			return f.lastModified();
		}
		return Long.MIN_VALUE;
	}
	
	@Override
	public <T extends Bean> Bizlet<T> getBizlet(Customer customer, Document document, boolean runtime) {
		// If dynamic, use the bizletClassName if defined
		Dynamic dynamic = document.getDynamism();
		if (dynamic != null) {
			String bizletClassName = dynamic.getBizletClassName();
			if (bizletClassName == null) {
				return null;
			}
			addKey(bizletClassName);
			return getJavaMetaData(customer, bizletClassName, false, runtime);
		}

		// Otherwise load from the file system
		StringBuilder key = new StringBuilder(128);
		String documentName = document.getName();
		key.append(ProvidedRepository.MODULES_NAMESPACE).append(document.getOwningModuleName()).append('/');
		key.append(documentName).append('/').append(documentName).append("Bizlet");
		return getJavaMetaData(customer, key.toString(), false, runtime);
	}

	@Override
	public <T extends Bean> DynamicImage<T> getDynamicImage(Customer customer, Document document, String imageName, boolean runtime) {
		// If dynamic, use the images map
		Dynamic dynamic = document.getDynamism();
		if (dynamic != null) {
			String imageClassName = dynamic.getImages().get(imageName);
			if (imageClassName == null) {
				throw new MetaDataException(imageName + " is not defined under dynamic.images in document " + document.getName());
			}
			addKey(imageClassName);
			return getJavaMetaData(customer, imageClassName, true, runtime);
		}

		// Otherwise load from the file system
		StringBuilder key = new StringBuilder(128);
		key.append(ProvidedRepository.MODULES_NAMESPACE).append(document.getOwningModuleName()).append('/');
		key.append(document.getName()).append('/');
		key.append(ProvidedRepository.IMAGES_NAMESPACE).append(imageName);
		return getJavaMetaData(customer, key.toString(), true, runtime);
	}

	protected <T extends MetaData> T getModel(Customer customer, Document document, String modelName, boolean runtime) {
		// If dynamic, use the models map
		Dynamic dynamic = document.getDynamism();
		if (dynamic != null) {
			String modelClassName = dynamic.getModels().get(modelName);
			if (modelClassName == null) {
				throw new MetaDataException(modelName + " is not defined under dynamic.models in document " + document.getName());
			}
			addKey(modelClassName);
			return getJavaMetaData(customer, modelClassName, true, runtime);
		}

		// Otherwise load from the file system
		StringBuilder key = new StringBuilder(128);
		key.append(ProvidedRepository.MODULES_NAMESPACE).append(document.getOwningModuleName()).append('/');
		key.append(document.getName()).append('/');
		key.append(ProvidedRepository.MODELS_NAMESPACE).append(modelName);
		return getJavaMetaData(customer, key.toString(), true, runtime);
	}

	@Override
	public <T extends Bean, C extends Bean> ComparisonModel<T, C> getComparisonModel(Customer customer, 
																						Document document, 
																						String modelName,
																						boolean runtime) {
		return getModel(customer, document, modelName, runtime);
	}

	@Override
	public <T extends Bean> MapModel<T> getMapModel(Customer customer, Document document, String modelName, boolean runtime) {
		return getModel(customer, document, modelName, runtime);
	}

	@Override
	public <T extends Bean> ChartModel<T> getChartModel(Customer customer,
															Document document,
															String modelName,
															boolean runtime) {
		return getModel(customer, document, modelName, runtime);
	}

	@Override
	public <T extends Bean> ListModel<T> getListModel(Customer customer, Document document, String modelName, boolean runtime) {
		return getModel(customer, document, modelName, runtime);
	}

	protected <T extends MetaData> T getAction(Customer customer, Document document, String actionName, boolean assertExistence, boolean runtime) {
		// If dynamic, use the actions map
		Dynamic dynamic = document.getDynamism();
		if (dynamic != null) {
			String actionClassName = dynamic.getActions().get(actionName);
			if (actionClassName == null) {
				if (assertExistence) {
					throw new MetaDataException(actionName + " is not defined under dynamic.actions in document " + document.getName());
				}
				return null;
			}
			addKey(actionClassName);
			return getJavaMetaData(customer, actionClassName, assertExistence, runtime);
		}

		// Otherwise load from the file system
		StringBuilder key = new StringBuilder(128);
		key.append(ProvidedRepository.MODULES_NAMESPACE).append(document.getOwningModuleName()).append('/');
		key.append(document.getName()).append('/');
		key.append(ProvidedRepository.ACTIONS_NAMESPACE).append(actionName);
		return getJavaMetaData(customer, key.toString(), assertExistence, runtime);
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public ServerSideAction<Bean> getServerSideAction(Customer customer, Document document, String actionName, boolean runtime) {
		MetaData result = getAction(customer, document, actionName, true, runtime);
		if (loadClasses) {
			return (ServerSideAction<Bean>) result;
		}

		return null;
	}

	@Override
	public BizExportAction getBizExportAction(Customer customer, Document document, String exportActionName, boolean runtime) {
		MetaData result = getAction(customer, document, exportActionName, true, runtime);
		if (loadClasses) {
			return (BizExportAction) result;
		}
		
		return null;
	}

	@Override
	public BizImportAction getBizImportAction(Customer customer, Document document, String importActionName, boolean runtime) {
		MetaData result = getAction(customer, document, importActionName, true, runtime);
		if (loadClasses) {
			return (BizImportAction) result;
		}
		
		return null;
	}

	@Override
	@SuppressWarnings("unchecked")
	public DownloadAction<Bean> getDownloadAction(Customer customer, Document document, String downloadActionName, boolean runtime) {
		MetaData result = getAction(customer, document, downloadActionName, true, runtime);
		if (loadClasses) {
			return (DownloadAction<Bean>) result;
		}
		
		return null;
	}

	@Override
	public UploadAction<Bean> getUploadAction(Customer customer, Document document, String uploadActionName, boolean runtime) {
		return getAction(customer, document, uploadActionName, true, runtime);
	}

	@Override
	public Object getDataFactory(Customer customer, Document document) {
		Object result = null;

		final String moduleName = document.getOwningModuleName();
		final String documentName = document.getName();
		
		try {
			Class<?> factoryClass = null;

			// If dynamic, use the bizletClassName if defined
			Dynamic dynamic = document.getDynamism();
			if (dynamic != null) {
				String dataFactoryClassName = dynamic.getDataFactoryClassName();
				if (dataFactoryClassName != null) {
					addKey(dataFactoryClassName);
					factoryClass = getJavaClass(customer, dataFactoryClassName);
				}
			}
			// Otherwise load from the file system
			else {
				String key = new StringBuilder(128).append(MODULES_NAMESPACE).append(moduleName).append('/').append(documentName).append('/').append(documentName).append("Factory").toString();
				factoryClass = getJavaClass(customer, key);
			}
			
			if (factoryClass != null) {
				result = factoryClass.getDeclaredConstructor().newInstance();
			}
		}
		catch (Exception e) {
			throw new MetaDataException("Cannot create a new instance of " + moduleName + '.' + documentName + " data factory", e);
		}
		
		return result;
	}

	/**
	 * 
	 * @param customer if <code>null</code>, the entire repository goes.
	 */
	@Override
	public void evictCachedMetaData(Customer customer) {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		if (customer == null) {
			persistence.disposeAllPersistenceInstances();
		}
		else {
			// TODO drop the customer's class loader
		}

		persistence = AbstractPersistence.get();
		persistence.setUser(user);
		
		classes.clear();
		
		super.evictCachedMetaData(customer);
	}

	@Override
	public Class<?> getJavaClass(Customer customer, String key) {
		Class<?> result = null;
		
		String newKey = vtable(customer.getName(), key);
		if (newKey != null) {
			result = classes.computeIfAbsent(newKey, k -> {
				String className = k.replace('/', '.');
				if (this.loadClasses) {
					try {
						return Class.forName(className, true, Thread.currentThread().getContextClassLoader());
					}
					catch (Exception e) {
						throw new MetaDataException("A problem was encountered loading class " + className, e);
					}
				}

				// Not loading classes
				// check for a java file and return a MetaData implementation
				// NB WidgetReference is a pretty simple MetaData implementation
				if (new File(this.absolutePath + className + ".java").exists()) {
					return WidgetReference.class;
				}
				
				return null;
			});
		}
		
		return result;
	}
	
	/**
	 * If customer is null, we must be looking for a repository code that does not 
	 * rely on the customer's vtable - not overloaded by a customer.
	 * 
	 * @param <T> The type of the metadata.
	 * @param customer The customer to load the code for, or null
	 * @param key
	 * @param assertExistence
	 * @param runtime	Are we really running or just generating etc.
	 * @return a new instance of the specified java class name or null if it does not exist in the customers vtable
	 */
	@SuppressWarnings("unchecked")
	public final <T extends MetaData> T getJavaMetaData(Customer customer, 
															String key,
															boolean assertExistence,
															boolean runtime) {
		T result = null;
		
		Class<?> type = getJavaClass(customer, key);
		if (type == null) {
			if (assertExistence) {
				throw new MetaDataException(key + " does not exist in the repository vtable");
			}
		}
		else {
			try {
				result = (T) type.getDeclaredConstructor().newInstance();
				if (runtime) {
					BeanProvider.injectFields(result);
				}
			}
			catch (SkyveException e) {
				throw e;
			}
			catch (Exception e) {
				throw new MetaDataException("A problem was encountered instantiating class " + type, e);
			}
		}
		return result;
	}

	@Override
	public String getReportFileName(Customer customer, Document document, String reportName) {
		StringBuilder path = new StringBuilder(64);
		path.append(MODULES_NAMESPACE).append(document.getOwningModuleName()).append('/');
		path.append(document.getName()).append('/');
		path.append(REPORTS_NAMESPACE).append(reportName);
		String key = path.toString();
		String result = vtable(customer.getName(), key);
		if (result == null) {
			throw new IllegalArgumentException("Report " + reportName + " for document " + 
												document.getOwningModuleName() + '.' + document.getName() + " is not defined.");
		}

		path.setLength(0);
		path.append(absolutePath).append(result).append(".jasper");
		return path.toString();
	}

	/**
	 * Check in &lt;customer-name&gt;/&lt;module-name&gt;/resources folder, 
	 * check in &lt;module-name&gt;/resources folder, 
	 * check in &lt;customer-name&gt;/resources folder, 
	 * check in resources folder.
	 * 
	 * @param imagePath The relative path to the image
	 * @param customerName The name of the customer.
	 * @param moduleName The name of the module.
	 * @return The resource file.
	 */
	@Override
	public final File findResourceFile(String resourcePath, String customerName, String moduleName) {
		File file = null;
		StringBuilder path = new StringBuilder(64);

		if (moduleName != null) {
			// Check customer module folder, if we have a customer to play with
			if (customerName != null) {
				path.append(absolutePath);
				path.append(CUSTOMERS_NAMESPACE);
				path.append(customerName);
				path.append('/');
				path.append(moduleName);
				path.append('/');
				path.append(RESOURCES_NAMESPACE);
				path.append(resourcePath);
				file = new File(path.toString());
				if (file.exists()) {
					return protect(file);
				}
			}
			
			// Check module folder
			path.setLength(0);
			path.append(absolutePath);
			path.append(MODULES_NAMESPACE);
			path.append(moduleName);
			path.append('/');
			path.append(RESOURCES_NAMESPACE);
			path.append(resourcePath);
			file = new File(path.toString());
			if (file.exists()) {
				return protect(file);
			}
		}

		// Check customer folder, if we have a customer to play with
		if (customerName != null) {
			path.setLength(0);
			path.append(absolutePath);
			path.append(CUSTOMERS_NAMESPACE);
			path.append(customerName);
			path.append('/');
			path.append(RESOURCES_NAMESPACE);
			path.append(resourcePath);
			file = new File(path.toString());
			if (file.exists()) {
				return protect(file);
			}
		}
		
		path.setLength(0);
		path.append(absolutePath);
		path.append(RESOURCES_NAMESPACE);
		path.append(resourcePath);
		return protect(new File(path.toString()));
	}

	// Ensure that the file path asks for doesn't break out of the project / web root directory
	private File protect(File file) {
		// resolve ../ and symbolic links with canonical path
		String pathToTest = null;
		try {
			pathToTest = file.getCanonicalPath();
		}
		catch (Exception e) {
			throw new SecurityException("File path not within web root.", e);
		}

		if (! pathToTest.startsWith(canonicalBasePath)) {
			throw new SecurityException("File path " + pathToTest + " is not within web root " + canonicalBasePath);
		}
		return file;
	}
}
