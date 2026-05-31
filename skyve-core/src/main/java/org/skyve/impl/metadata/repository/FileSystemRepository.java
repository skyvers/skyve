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

import org.skyve.domain.Bean;
import org.skyve.domain.messages.SkyveException;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerModuleMetaData;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.repository.router.Router;
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
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Abstract base for metadata repositories backed by the local file system.
 *
 * <p>Loads customer, module, document, view, and router descriptors by scanning the
 * {@code absolutePath} directory tree.  Unmarshals each XML file with JAXB and
 * populates the in-memory cache held by {@link MutableCachedRepository}.
 *
 * <p>Concrete subclasses must supply the scanner implementation
 * ({@link #populateCustomers(Customer)} and related hooks) and decide the physical
 * layout of descriptor files.
 *
 * <p>Threading: not thread-safe for mutations.  Repository loading is performed
 * once during application startup; thereafter all public accessors are read-only
 * and safe for concurrent use.
 *
 * @see MutableCachedRepository
 * @see org.skyve.metadata.repository.ProvidedRepository
 */
public abstract class FileSystemRepository extends MutableCachedRepository {

    private static final Logger XML_LOGGER = Category.XML.logger();

	protected String absolutePath;
	// used to stop resources paths breaking out of the web root (eg ../../../../)
	private String canonicalBasePath;
	protected boolean loadClasses = true;
	// class maps
	private ConcurrentHashMap<String, Class<?>> classes = new ConcurrentHashMap<>();
	
	/**
	 * Absolute path constructor
	 * Prevent external instantiation.
	 * Use a ConcurrentHashMap for the cache as it is thread-safe and performant for mostly-read operations.
	 *
	 * <p>Normalises path separators, ensures a trailing slash, resolves a canonical
	 * base path used by resource path traversal checks, and then discovers all
	 * metadata keys.
	 *
	 * @param absolutePath absolute path to the repository root
	 */
	protected FileSystemRepository(@Nonnull String absolutePath) {
		super(new ConcurrentHashMap<>());
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
	 *
	 * @param absolutePath absolute path to the repository root
	 * @param loadClasses whether Java metadata classes should be loaded reflectively
	 */
	protected FileSystemRepository(@Nonnull String absolutePath, boolean loadClasses) {
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
	
	/**
	 * Rebuilds repository lookup keys by scanning customer and module filesystem locations.
	 *
	 * <p>This indexes modules, documents, actions, images, models, reports, views, and
	 * document-level companions such as bizlets, extensions, and factories.
	 */
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
				if (UtilImpl.XML_TRACE) XML_LOGGER.info("module location = {}", key);
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
			if (UtilImpl.XML_TRACE) XML_LOGGER.info("module location = {}", key);
			populateDocumentLocations(key);
		}
	}

	private void populateModuleLocation(@Nullable String customerName, @Nonnull String moduleName) {
		StringBuilder sb = new StringBuilder(256);
		if (customerName == null) {
			String key = MODULES_NAMESPACE + moduleName;
			sb.setLength(0);
			sb.append(absolutePath).append(key).append('/').append(moduleName).append(".xml");
			File moduleFile = new File(sb.toString());
			if (moduleFile.exists()) {
				if (UtilImpl.XML_TRACE) XML_LOGGER.info("{} -> {}", moduleName, key);
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
				if (UtilImpl.XML_TRACE) XML_LOGGER.info("{} -> {}", moduleName, key);
				addKey(key);
			}
		}
	}

	private void populateDocumentLocations(@Nonnull String key) {
		StringBuilder sb = new StringBuilder(256);

		File moduleDirectory = new File(absolutePath + key);
		if (moduleDirectory.exists() && moduleDirectory.isDirectory()) {
			File[] moduleFiles = moduleDirectory.listFiles();
			if (moduleFiles != null) {
				for (File moduleFile : moduleFiles) {
					String moduleFileName = moduleFile.getName();
					if (UtilImpl.XML_TRACE) XML_LOGGER.info("module file name = {}", moduleFileName);
	
					// we have found some modules
					if (moduleFile.isDirectory()) {
						File[] documentFiles = moduleFile.listFiles();
						if (documentFiles != null) {
							for (File documentFile : documentFiles) {
								String documentFileName = documentFile.getName();
								// found the document actions directory
								if (documentFileName.equals(ACTIONS_NAME) && documentFile.isDirectory()) {
									File[] files = documentFile.listFiles();
									if (files != null) {
										for (File actionFile : files) {
											String actionFileName = actionFile.getName();
											if (actionFileName.endsWith(".class") || actionFileName.endsWith(".java") || actionFileName.endsWith(".xml")) {
												String actionName = actionFileName.substring(0, actionFileName.lastIndexOf('.'));
			
												sb.setLength(0);
												sb.append(key).append(moduleFileName).append('/');
												sb.append(ACTIONS_NAMESPACE).append(actionName);
												if (actionFileName.endsWith(".xml")) {
													sb.append(META_DATA_SUFFIX);
												}
												String actionLocation = sb.toString();
												if (UtilImpl.XML_TRACE) XML_LOGGER.info(new StringBuilder(128).append("Action ").append(actionName).append(" -> ").append(actionLocation).toString());
												addKey(actionLocation);
											}
										}
									}
								}
								// found the document images directory
								else if (documentFileName.equals(IMAGES_NAME) && documentFile.isDirectory()) {
									File[] files = documentFile.listFiles();
									if (files != null) {
										for (File imageFile : files) {
											String imageFileName = imageFile.getName();
											if (imageFileName.endsWith(".class") || imageFileName.endsWith(".java")) {
												String imageName = imageFileName.substring(0, imageFileName.lastIndexOf('.'));
												sb.setLength(0);
												sb.append(key).append(moduleFileName).append('/');
												sb.append(IMAGES_NAMESPACE).append(imageName);
												String imageLocation = sb.toString();
												if (UtilImpl.XML_TRACE) XML_LOGGER.info(new StringBuilder(128).append("Dynamic Image ").append(imageName).append(" -> ").append(imageLocation).toString());
												addKey(imageLocation);
											}
										}
									}
								}
								// found the document models directory
								else if (documentFileName.equals(MODELS_NAME) && documentFile.isDirectory()) {
									File[] files = documentFile.listFiles();
									if (files != null) {
										for (File modelFile : files) {
											String modelFileName = modelFile.getName();
											if (modelFileName.endsWith(".class") || modelFileName.endsWith(".java")) {
												String modelName = modelFileName.substring(0, modelFileName.lastIndexOf('.'));
												sb.setLength(0);
												sb.append(key).append(moduleFileName).append('/');
												sb.append(MODELS_NAMESPACE).append(modelName);
												String modelLocation = sb.toString();
												if (UtilImpl.XML_TRACE) XML_LOGGER.info(new StringBuilder(128).append("Model ").append(modelName).append(" -> ").append(modelLocation).toString());
												addKey(modelLocation);
											}
										}
									}
								}
								// found the document reports directory
								else if (documentFileName.equals(REPORTS_NAME) && documentFile.isDirectory()) {
									File[] files = documentFile.listFiles();
									if (files != null) {
										for (File reportFile : files) {
											String reportFileName = reportFile.getName();
											if (reportFileName.endsWith(".jasper")) {
												String reportName = reportFileName.substring(0, reportFileName.length() - 7);
												sb.setLength(0);
												sb.append(key).append(moduleFileName).append('/');
												sb.append(REPORTS_NAMESPACE).append(reportName).append(JASPER_SUFFIX);
												String reportLocation = sb.toString();
												if (UtilImpl.XML_TRACE) XML_LOGGER.info(new StringBuilder(128).append("Jasper Report ").append(reportName).append(" -> ").append(reportLocation).toString());
												addKey(reportLocation);
											}
											else if (reportFileName.endsWith(".ftlh")) {
												String reportName = reportFileName.substring(0, reportFileName.length() - 5);
												sb.setLength(0);
												sb.append(key).append(moduleFileName).append('/');
												sb.append(REPORTS_NAMESPACE).append(reportName).append(FREEMARKER_SUFFIX);
												String reportLocation = sb.toString();
												if (UtilImpl.XML_TRACE) XML_LOGGER.info(new StringBuilder(128).append("Freemarker Report ").append(reportName).append(" -> ").append(reportLocation).toString());
												addKey(reportLocation);
											}
										}
									}
								}
								// found the document views directory
								else if (documentFileName.equals(VIEWS_NAME) && documentFile.isDirectory()) {
									File[] viewFiles = documentFile.listFiles();
									if (viewFiles != null) {
										for (File viewFile : viewFiles) {
											String viewFileName = viewFile.getName();
											if (viewFileName.endsWith(".xml")) { // found a view file
												String viewType = viewFileName.substring(0, viewFileName.length() - 4);
												sb.setLength(0);
												sb.append(key).append(moduleFileName).append('/');
												sb.append(VIEWS_NAMESPACE).append(viewType);
												String viewLocation = sb.toString();
												if (UtilImpl.XML_TRACE) XML_LOGGER.info(new StringBuilder(128).append("View ").append(viewType).append(" -> ").append(viewLocation).toString());
												addKey(viewLocation);
											}
											else if (viewFile.isDirectory()) {
												File[] uxuiViewFiles = viewFile.listFiles();
												if (uxuiViewFiles != null) {
													for (File uxuiViewFile : uxuiViewFiles) {
														String uxuiViewFileName = uxuiViewFile.getName();
														if (uxuiViewFileName.endsWith(".xml")) { // found a view file
															String viewType = uxuiViewFileName.substring(0, uxuiViewFileName.length() - 4);
															sb.setLength(0);
															sb.append(key).append(moduleFileName).append('/');
															sb.append(VIEWS_NAMESPACE).append(viewFileName).append('/').append(viewType);
															String viewLocation = sb.toString();
															if (UtilImpl.XML_TRACE) XML_LOGGER.info(new StringBuilder(128).append("View ").append(viewType).append(" -> ").append(viewLocation).toString());
															addKey(viewLocation);
														}
													}
												}
											}
										}
									}
								}
								// found the bizlet class file
								else if (documentFileName.equals(moduleFileName + "Bizlet.class")) {
									sb.setLength(0);
									sb.append(key).append(moduleFileName).append('/');
									sb.append(moduleFileName).append(BIZLET_SUFFIX);
									String bizletLocation = sb.toString();
									if (UtilImpl.XML_TRACE) XML_LOGGER.info(new StringBuilder(128).append("Bizlet ").append(moduleFileName).append(" -> ").append(bizletLocation).toString());
									addKey(bizletLocation);
								}
								// found the bizlet metadata file
								else if (documentFileName.equals(moduleFileName + "Bizlet.xml")) {
									sb.setLength(0);
									sb.append(key).append(moduleFileName).append('/');
									sb.append(moduleFileName).append(BIZLET_SUFFIX).append(META_DATA_SUFFIX);
									String bizletLocation = sb.toString();
									if (UtilImpl.XML_TRACE) XML_LOGGER.info(new StringBuilder(128).append("BizletMetaData ").append(moduleFileName).append(" -> ").append(bizletLocation).toString());
									addKey(bizletLocation);
								}
								// found the extension class file
								else if (documentFileName.equals(moduleFileName + "Extension.class")) {
									sb.setLength(0);
									sb.append(key).append(moduleFileName).append('/');
									sb.append(moduleFileName).append("Extension");
									String extensionLocation = sb.toString();
									if (UtilImpl.XML_TRACE) XML_LOGGER.info(new StringBuilder(128).append("Extension ").append(moduleFileName).append(" -> ").append(extensionLocation).toString());
									addKey(extensionLocation);
								}
								// found the factory class file
								else if (documentFileName.equals(moduleFileName + "Factory.class")) {
									sb.setLength(0);
									sb.append(key).append(moduleFileName).append('/');
									sb.append(moduleFileName).append("Factory");
									String factoryLocation = sb.toString();
									if (UtilImpl.XML_TRACE) XML_LOGGER.info(new StringBuilder(128).append("Factory ").append(moduleFileName).append(" -> ").append(factoryLocation).toString());
									addKey(factoryLocation);
								}
								// found the document definition file
								else if (documentFileName.equals(moduleFileName + ".xml")) {
									String documentLocation = key + moduleFileName;
									if (UtilImpl.XML_TRACE) XML_LOGGER.info(new StringBuilder(128).append("Document ").append(moduleFileName).append(" -> ").append(documentLocation).toString());
									addKey(documentLocation);
								}
							} // for (all document files)
						}
					} // if (moduleFile is a directory)
				} // for (all module files)
			}
		} // if (module directory exists)
	}
	
	/**
	 * Loads the effective router by merging the global router and all discovered module routers.
	 *
	 * @return the merged router definition
	 * @throws MetaDataException if no router files are found or router XML cannot be parsed
	 */
	@Override
	public Router loadRouter() {
		Router result = null;

		try {
			final Map<String, Long> routersFileInfo = routersFileInfo(true, true);
			if (routersFileInfo.isEmpty()) {
				throw new MetaDataException("No routers found.");
			}
			for (String path : routersFileInfo.keySet()) {
				Router router = XMLMetaData.unmarshalRouterFile(path);
				router = router.convert(ROUTER_NAME);
				if (result == null) { // first path is the global router
					result = router;
				}
				else { // module routers
					result.merge(router);
				}
			}
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}
		
		return result;
	}

	/**
	 * Returns the latest modification timestamp across all router XML files.
	 *
	 * @return the newest router file timestamp, or {@link Long#MIN_VALUE} if no router exists
	 */
	@Override
	public long routerLastModifiedMillis() {
		final Map<String, Long> routersFileInfo = routersFileInfo(true, true);
		if (routersFileInfo.isEmpty()) {
			return Long.MIN_VALUE;
		}
 		return routersFileInfo.values().stream().max(Comparator.naturalOrder()).get().longValue();
	}
	
	private @Nonnull Map<String, Long> routersFileInfo(boolean includeGlobal, boolean includeModule) {
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
				if (customer == null) {
					throw new MetaDataException("Customer " + customerName + " does not exist.");
				}
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
	
	/**
	 * Returns the global router definition only, without merging module routers.
	 *
	 * @return the converted global router, or {@code null} when no global router exists
	 */
	@Override
	public Router getGlobalRouter() {
		final Map<String, Long> routersFileInfo = routersFileInfo(true, false);
 		Optional<String> path = routersFileInfo.keySet().stream().findFirst();
 		if (path.isPresent()) {
			Router router = XMLMetaData.unmarshalRouterFile(path.get());
			return router.convert(ROUTER_NAME);
 		}
 		return null;
	}
	
	/**
	 * Returns module router definitions without the global router.
	 *
	 * @return converted, self-contained module routers in discovery order
	 */
	@Override
	public List<Router> getModuleRouters() {
		final Map<String, Long> routersFileInfo = routersFileInfo(false, true);
		final List<Router> result = new ArrayList<>(routersFileInfo.size());
		for (String path : routersFileInfo.keySet()) {
			Router router = XMLMetaData.unmarshalRouterFile(path);
			router = router.convert(ROUTER_NAME);
			result.add(router);
		}
		
		return result;
	}

	/**
	 * Lists customer names available under the repository customer namespace.
	 *
	 * @return customer directory names that are not hidden entries
	 */
	@Override
	public List<String> getAllCustomerNames() {
		List<String> result = new ArrayList<>();
		
		File customersDirectory = new File(absolutePath + CUSTOMERS_NAMESPACE);
		if (customersDirectory.exists() && customersDirectory.isDirectory()) {
			File[] files = customersDirectory.listFiles();
			if (files != null) {
				for (File customerDirectory : files) {
					if (customerDirectory.isDirectory() && (customerDirectory.getName().charAt(0) != '.')) {
						result.add(customerDirectory.getName());
					}
				}
			}
		}

		return result;
	}

	/**
	 * Lists vanilla module names from the module namespace.
	 *
	 * <p>A module is considered valid only when its directory contains a module XML file
	 * named after that module.
	 *
	 * @return valid vanilla module names
	 */
	@Override
	public List<String> getAllVanillaModuleNames() {
		List<String> result = new ArrayList<>();

		File modulesDirectory = new File(absolutePath + MODULES_NAMESPACE);
		if (modulesDirectory.exists() && modulesDirectory.isDirectory()) {
			File[] moduleDirectories = modulesDirectory.listFiles();
			if (moduleDirectories != null) {
				for (File moduleDirectory : moduleDirectories) {
					if (moduleDirectory.isDirectory() && (moduleDirectory.getName().charAt(0) != '.')) {
						// make sure there is a module.xml with the same name as the module directory to cater for deleted modules
						File[] moduleChildren = moduleDirectory.listFiles();
						if (moduleChildren != null) {
							for (File moduleChild : moduleChildren) {
								if (moduleChild.getName().equals(moduleDirectory.getName() + ".xml")) {
									result.add(moduleDirectory.getName());
									break;
								}
							}
						}
					}
				}
			}
		}

		return result;
	}

	private @Nonnull String customerPath(@Nonnull String customerName) {
		StringBuilder result = new StringBuilder(256);
		result.append(absolutePath);
		result.append(CUSTOMERS_NAMESPACE);
		result.append(customerName).append('/').append(customerName).append(".xml");
		return result.toString();
	}
	
	/**
	 * Loads customer metadata from the customer definition XML file.
	 *
	 * @param customerName the customer name to resolve
	 * @return unmarshalled customer metadata
	 * @throws MetaDataException if metadata cannot be loaded or the XML name does not match
	 */
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
	
	/**
	 * Returns the last modified timestamp of a customer metadata file.
	 *
	 * @param customerName the customer name to resolve
	 * @return file timestamp in milliseconds, or {@link Long#MIN_VALUE} when the file is missing
	 */
	@Override
	public long customerLastModifiedMillis(String customerName) {
		String path = customerPath(customerName);
		File f = new File(path);
		if (f.exists()) {
			return f.lastModified();
		}
		return Long.MIN_VALUE;
	}
	
	private @Nonnull String modulePath(@Nullable String customerName, @Nonnull String moduleName) {
		StringBuilder result = new StringBuilder(256);
		result.append(absolutePath);
		if (customerName != null) {
			result.append(CUSTOMERS_NAMESPACE).append(customerName).append('/');
		}
		result.append(MODULES_NAMESPACE);
		result.append(moduleName).append('/').append(moduleName).append(".xml");
		return result.toString();
	}
	
	/**
	 * Loads module metadata for a customer override or a vanilla module.
	 *
	 * @param customerName the customer name for override resolution; may be {@code null}
	 * @param moduleName the module name
	 * @return unmarshalled module metadata
	 * @throws MetaDataException if metadata cannot be loaded or the XML name does not match
	 */
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

	/**
	 * Returns the last modified timestamp of a module metadata file.
	 *
	 * @param customerName the customer name for override resolution; may be {@code null}
	 * @param moduleName the module name
	 * @return file timestamp in milliseconds, or {@link Long#MIN_VALUE} when the file is missing
	 */
	@Override
	public long moduleLastModifiedMillis(String customerName, String moduleName) {
		String path = modulePath(customerName, moduleName);
		File f = new File(path);
		if (f.exists()) {
			return f.lastModified();
		}
		return Long.MIN_VALUE;
	}
	
	private @Nonnull String documentPath(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName) {
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
	
	/**
	 * Loads document metadata for a customer override or a vanilla module document.
	 *
	 * @param customerName the customer name for override resolution; may be {@code null}
	 * @param moduleName the owning module name
	 * @param documentName the document name
	 * @return unmarshalled document metadata
	 * @throws MetaDataException if metadata cannot be loaded or the XML name does not match
	 */
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

	/**
	 * Returns the last modified timestamp of a document metadata file.
	 *
	 * @param customerName the customer name for override resolution; may be {@code null}
	 * @param moduleName the owning module name
	 * @param documentName the document name
	 * @return file timestamp in milliseconds, or {@link Long#MIN_VALUE} when the file is missing
	 */
	@Override
	public long documentLastModifiedMillis(String customerName, String moduleName, String documentName) {
		String path = documentPath(customerName, moduleName, documentName);
		File f = new File(path);
		if (f.exists()) {
			return f.lastModified();
		}
		return Long.MIN_VALUE;
	}

	private @Nonnull String viewPath(@Nullable String customerName,
										@Nonnull String moduleName,
										@Nonnull String documentName,
										@Nullable String uxui,
										@Nonnull String viewName) {
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
	
	/**
	 * Loads a view metadata definition from the repository.
	 *
	 * @param customerName the customer name for override resolution; may be {@code null}
	 * @param moduleName the owning module name
	 * @param documentName the owning document name
	 * @param uxui the UX/UI variant folder; may be {@code null}
	 * @param viewName the view name
	 * @return unmarshalled view metadata
	 * @throws MetaDataException if metadata cannot be loaded or the XML name does not match
	 */
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

	/**
	 * Returns the last modified timestamp of a view metadata file.
	 *
	 * @param customerName the customer name for override resolution; may be {@code null}
	 * @param moduleName the owning module name
	 * @param documentName the owning document name
	 * @param uxui the UX/UI variant folder; may be {@code null}
	 * @param viewName the view name
	 * @return file timestamp in milliseconds, or {@link Long#MIN_VALUE} when the file is missing
	 */
	@Override
	public long viewLastModifiedMillis(String customerName, String moduleName, String documentName, String uxui, String viewName) {
		String path = viewPath(customerName, moduleName, documentName, uxui, viewName);
		File f = new File(path);
		if (f.exists()) {
			return f.lastModified();
		}
		return Long.MIN_VALUE;
	}
	
	private @Nonnull String actionPath(@Nullable String customerName,
										@Nonnull String moduleName,
										@Nonnull String documentName,
										@Nonnull String actionName) {
		StringBuilder result = new StringBuilder(256);
		result.append(absolutePath);
		if (customerName != null) {
			result.append(CUSTOMERS_NAMESPACE).append(customerName).append('/');
		}
		result.append(MODULES_NAMESPACE);
		result.append(moduleName).append('/');
		result.append(documentName).append('/').append(ACTIONS_NAMESPACE);
		result.append(actionName).append(".xml");
		return result.toString();
	}
	
	/**
	 * Loads action metadata for a document action XML definition.
	 *
	 * @param customerName the customer name for override resolution; may be {@code null}
	 * @param moduleName the owning module name
	 * @param documentName the owning document name
	 * @param actionName the action name
	 * @return unmarshalled action metadata
	 * @throws MetaDataException if metadata cannot be loaded or the XML name does not match
	 */
	@Override
	public ActionMetaData loadMetaDataAction(String customerName, String moduleName, String documentName, String actionName) {
		ActionMetaData result = null;
		
		try {
			String path = actionPath(customerName, moduleName, documentName, actionName);
			result = XMLMetaData.unmarshalActionFile(path);
			if (! actionName.equals(result.getName())) {
				throw new MetaDataException("Action is defined with file name of " + path + 
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

	/**
	 * Returns the last modified timestamp of an action metadata file.
	 *
	 * @param customerName the customer name for override resolution; may be {@code null}
	 * @param moduleName the owning module name
	 * @param documentName the owning document name
	 * @param actionName the action name
	 * @return file timestamp in milliseconds, or {@link Long#MIN_VALUE} when the file is missing
	 */
	@Override
	public long metaDataActionLastModifiedMillis(String customerName, String moduleName, String documentName, String actionName) {
		String path = actionPath(customerName, moduleName, documentName, actionName);
		File f = new File(path);
		if (f.exists()) {
			return f.lastModified();
		}
		return Long.MIN_VALUE;
	}

	/**
	 * Resolves and instantiates the Bizlet for a document.
	 *
	 * <p>Dynamic documents use the configured dynamic bizlet class name. Static documents
	 * are resolved from the repository bizlet class convention.
	 *
	 * @param <T> the document bean type
	 * @param customer the customer context used for class resolution
	 * @param document the target document
	 * @param runtime whether runtime dependency injection should be applied
	 * @return the Bizlet instance, or {@code null} when no Bizlet is configured
	 */
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
		key.append(documentName).append('/').append(documentName).append(BIZLET_SUFFIX);
		return getJavaMetaData(customer, key.toString(), false, runtime);
	}

	private @Nonnull String bizletPath(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName) {
		StringBuilder result = new StringBuilder(256);
		result.append(absolutePath);
		if (customerName != null) {
			result.append(CUSTOMERS_NAMESPACE).append(customerName).append('/');
		}
		result.append(MODULES_NAMESPACE);
		result.append(moduleName).append('/');
		result.append(documentName).append('/');
		result.append(documentName).append(BIZLET_SUFFIX).append(".xml");
		return result.toString();
	}
	
	/**
	 * Loads Bizlet metadata for a document.
	 *
	 * @param customerName the customer name for override resolution; may be {@code null}
	 * @param moduleName the owning module name
	 * @param documentName the owning document name
	 * @return unmarshalled Bizlet metadata
	 * @throws MetaDataException if metadata cannot be loaded
	 */
	@Override
	public BizletMetaData loadMetaDataBizlet(String customerName, String moduleName, String documentName) {
		BizletMetaData result = null;
		
		try {
			String path = bizletPath(customerName, moduleName, documentName);
			result = XMLMetaData.unmarshalBizletFile(path);
		} // try (populate Metadata)
		catch (MetaDataException e) {
			throw e;
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}
		
		return result;
	}

	/**
	 * Returns the last modified timestamp of a Bizlet metadata file.
	 *
	 * @param customerName the customer name for override resolution; may be {@code null}
	 * @param moduleName the owning module name
	 * @param documentName the owning document name
	 * @return file timestamp in milliseconds, or {@link Long#MIN_VALUE} when the file is missing
	 */
	@Override
	public long metaDataBizletLastModifiedMillis(String customerName, String moduleName, String documentName) {
		String path = bizletPath(customerName, moduleName, documentName);
		File f = new File(path);
		if (f.exists()) {
			return f.lastModified();
		}
		return Long.MIN_VALUE;
	}

	/**
	 * Resolves and instantiates a dynamic image handler for a document image.
	 *
	 * @param <T> the document bean type
	 * @param customer the customer context used for class resolution
	 * @param document the target document
	 * @param imageName the image action name
	 * @param runtime whether runtime dependency injection should be applied
	 * @return the dynamic image handler instance
	 * @throws MetaDataException if the image is not configured for a dynamic document
	 */
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

	/**
	 * Resolves and instantiates a document model class by name.
	 *
	 * <p>Supports both dynamic document model mappings and filesystem-backed
	 * model classes. The returned model is not post-constructed by this method.
	 *
	 * @param <T> the metadata model type
	 * @param customer the customer context for vtable resolution; may be {@code null}
	 * @param document the owning document
	 * @param modelName the model key under {@code dynamic.models} or {@code models/}
	 * @param runtime whether runtime dependency injection should be applied
	 * @return an instantiated model metadata object
	 */
	protected @Nonnull <T extends MetaData> T getModel(@Nullable Customer customer,
														@Nonnull Document document,
														@Nonnull String modelName,
														boolean runtime) {
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

	/**
	 * Resolves and post-constructs a comparison model.
	 *
	 * @param <T> the main bean type
	 * @param <C> the comparison bean type
	 * @param customer the customer context used for class resolution
	 * @param document the target document
	 * @param modelName the model name
	 * @param runtime whether runtime dependency injection should be applied
	 * @return an initialized comparison model
	 */
	@Override
	public <T extends Bean, C extends Bean> ComparisonModel<T, C> getComparisonModel(Customer customer, 
																						Document document, 
																						String modelName,
																						boolean runtime) {
		ComparisonModel<T, C> result = getModel(customer, document, modelName, runtime);
		result.postConstruct(customer, runtime);
		return result;
	}

	/**
	 * Resolves and post-constructs a map model.
	 *
	 * @param <T> the document bean type
	 * @param customer the customer context used for class resolution
	 * @param document the target document
	 * @param modelName the model name
	 * @param runtime whether runtime dependency injection should be applied
	 * @return an initialized map model
	 */
	@Override
	public <T extends Bean> MapModel<T> getMapModel(Customer customer, Document document, String modelName, boolean runtime) {
		MapModel<T> result = getModel(customer, document, modelName, runtime);
		result.postConstruct(customer, runtime);
		return result;
	}

	/**
	 * Resolves and post-constructs a chart model.
	 *
	 * @param <T> the document bean type
	 * @param customer the customer context used for class resolution
	 * @param document the target document
	 * @param modelName the model name
	 * @param runtime whether runtime dependency injection should be applied
	 * @return an initialized chart model
	 */
	@Override
	public <T extends Bean> ChartModel<T> getChartModel(Customer customer,
															Document document,
															String modelName,
															boolean runtime) {
		ChartModel<T> result = getModel(customer, document, modelName, runtime);
		result.postConstruct(customer, runtime);
		return result;
	}

	/**
	 * Resolves and post-constructs a list model.
	 *
	 * @param <T> the document bean type
	 * @param customer the customer context used for class resolution
	 * @param document the target document
	 * @param modelName the model name
	 * @param runtime whether runtime dependency injection should be applied
	 * @return an initialized list model
	 */
	@Override
	public <T extends Bean> ListModel<T> getListModel(Customer customer, Document document, String modelName, boolean runtime) {
		ListModel<T> result = getModel(customer, document, modelName, runtime);
		result.postConstruct(customer, runtime);
		return result;
	}

	/**
	 * Resolves and instantiates a class-backed action by name.
	 *
	 * <p>Looks up dynamic action mappings first, then repository action classes.
	 * When {@code assertExistence} is {@code true}, missing mappings raise
	 * {@link MetaDataException}; otherwise this method returns {@code null}.
	 *
	 * @param <T> the action metadata type
	 * @param customer the customer context for vtable resolution; may be {@code null}
	 * @param document the owning document
	 * @param actionName action key
	 * @param assertExistence whether missing actions should fail fast
	 * @param runtime whether runtime dependency injection should be applied
	 * @return the instantiated action metadata, or {@code null} when not found and existence is not asserted
	 */
	protected @Nullable <T extends MetaData> T getClassAction(@Nullable Customer customer,
																@Nonnull Document document,
																@Nonnull String actionName,
																boolean assertExistence,
																boolean runtime) {
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
	
	/**
	 * Resolves a server-side action class and returns it when class loading is enabled.
	 *
	 * @param customer the customer context used for class resolution
	 * @param document the target document
	 * @param actionName the action name
	 * @param runtime whether runtime dependency injection should be applied
	 * @return a server-side action instance, or {@code null} when class loading is disabled
	 */
	@Override
	@SuppressWarnings("unchecked")
	public ServerSideAction<Bean> getServerSideAction(Customer customer, Document document, String actionName, boolean runtime) {
		MetaData result = getClassAction(customer, document, actionName, true, runtime);
		if (loadClasses) {
			return (ServerSideAction<Bean>) result;
		}

		return null;
	}

	/**
	 * Resolves a BizExport action class and returns it when class loading is enabled.
	 *
	 * @param customer the customer context used for class resolution
	 * @param document the target document
	 * @param exportActionName the export action name
	 * @param runtime whether runtime dependency injection should be applied
	 * @return a BizExport action instance, or {@code null} when class loading is disabled
	 */
	@Override
	public BizExportAction getBizExportAction(Customer customer, Document document, String exportActionName, boolean runtime) {
		MetaData result = getClassAction(customer, document, exportActionName, true, runtime);
		if (loadClasses) {
			return (BizExportAction) result;
		}
		
		return null;
	}

	/**
	 * Resolves a BizImport action class and returns it when class loading is enabled.
	 *
	 * @param customer the customer context used for class resolution
	 * @param document the target document
	 * @param importActionName the import action name
	 * @param runtime whether runtime dependency injection should be applied
	 * @return a BizImport action instance, or {@code null} when class loading is disabled
	 */
	@Override
	public BizImportAction getBizImportAction(Customer customer, Document document, String importActionName, boolean runtime) {
		MetaData result = getClassAction(customer, document, importActionName, true, runtime);
		if (loadClasses) {
			return (BizImportAction) result;
		}
		
		return null;
	}

	/**
	 * Resolves a download action class and returns it when class loading is enabled.
	 *
	 * @param customer the customer context used for class resolution
	 * @param document the target document
	 * @param downloadActionName the download action name
	 * @param runtime whether runtime dependency injection should be applied
	 * @return a download action instance, or {@code null} when class loading is disabled
	 */
	@Override
	@SuppressWarnings("unchecked")
	public DownloadAction<Bean> getDownloadAction(Customer customer, Document document, String downloadActionName, boolean runtime) {
		MetaData result = getClassAction(customer, document, downloadActionName, true, runtime);
		if (loadClasses) {
			return (DownloadAction<Bean>) result;
		}
		
		return null;
	}

	/**
	 * Resolves an upload action class.
	 *
	 * @param customer the customer context used for class resolution
	 * @param document the target document
	 * @param uploadActionName the upload action name
	 * @param runtime whether runtime dependency injection should be applied
	 * @return an upload action instance
	 */
	@Override
	public UploadAction<Bean> getUploadAction(Customer customer, Document document, String uploadActionName, boolean runtime) {
		return getClassAction(customer, document, uploadActionName, true, runtime);
	}

	/**
	 * Resolves and instantiates the document data factory class.
	 *
	 * <p>Dynamic documents use {@code dynamic.dataFactoryClassName} when configured;
	 * otherwise the standard repository factory naming convention is used.
	 *
	 * @param customer the customer context used for class resolution
	 * @param document the target document
	 * @return a new data factory instance, or {@code null} when no data factory exists
	 * @throws MetaDataException if the factory class exists but cannot be instantiated
	 */
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
	 * Evicts cached metadata and resets persistence state after cache invalidation.
	 *
	 * <p>Side effects: disposes persistence instances when {@code customer} is
	 * {@code null}, restores the calling user on the new persistence context,
	 * clears Java class caches, and delegates metadata cache eviction to the
	 * superclass.
	 *
	 * @param customer the customer whose metadata should be evicted, or {@code null} to evict all
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

	/**
	 * Resolves a Java class from the repository vtable and class cache.
	 *
	 * <p>When class loading is disabled, this method returns {@link WidgetReference} for
	 * matching source files to support metadata-only scenarios.
	 *
	 * @param customer the customer context used for override resolution; may be {@code null}
	 * @param key repository key to resolve
	 * @return the resolved class, {@link WidgetReference}, or {@code null} if not found
	 */
	@Override
	public Class<?> getJavaClass(Customer customer, String key) {
		Class<?> result = null;
		
		String newKey = vtable((customer == null) ? null : customer.getName(), key);
		if (newKey != null) {
			result = classes.computeIfAbsent(newKey, k -> {
				String className = k.replace('/', '.');
				if (this.loadClasses) {
					try {
						return Thread.currentThread().getContextClassLoader().loadClass(className);
					}
					catch (@SuppressWarnings("unused") Exception e) {
						return null; // its up to the caller to assert existence if required
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
	 * Instantiates a Java-backed metadata class resolved through the repository vtable.
	 *
	 * <p>When {@code customer} is provided, customer overrides are resolved first;
	 * otherwise the vanilla repository namespace is used.
	 *
	 * @param <T> the metadata type
	 * @param customer the customer context for vtable lookup, or {@code null}
	 * @param key repository key for the Java metadata class
	 * @param assertExistence whether a missing class should raise {@link MetaDataException}
	 * @param runtime whether runtime dependency injection should be applied
	 * @return a new metadata instance, or {@code null} when not found and existence is not asserted
	 */
	@SuppressWarnings("unchecked")
	public final @Nullable <T extends MetaData> T getJavaMetaData(@Nullable Customer customer,
																	@Nonnull String key,
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
					UtilImpl.inject(result);
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

	/**
	 * Resolves the physical report output filename for a named document report.
	 *
	 * <p>Checks Jasper first, then Freemarker report variants using vtable resolution.
	 * The returned path includes the repository root and output extension.
	 *
	 * @param customer the customer context used for override resolution; may be {@code null}
	 * @param document the owning document
	 * @param reportName the logical report name
	 * @return the absolute report output filename, or {@code null} when no report exists
	 */
	@Override
	public String getReportFileName(Customer customer, Document document, String reportName) {
		StringBuilder path = new StringBuilder(64);
		path.append(MODULES_NAMESPACE).append(document.getOwningModuleName()).append('/');
		path.append(document.getName()).append('/');
		path.append(REPORTS_NAMESPACE).append(reportName);
		String key = path.toString() + JASPER_SUFFIX;
		String customerName = (customer == null) ? null : customer.getName();
		String result = vtable(customerName, key);
		if (result == null) {
			key = path.toString() + FREEMARKER_SUFFIX;
			result = vtable(customerName, key);
			if (result != null) {
				path.setLength(0);
				path.append(absolutePath).append(result);
				path.setLength(path.length() - FREEMARKER_SUFFIX.length()); // remove the suffix
				path.append(".flth");
				return path.toString();
			}
		}
		else {
			path.setLength(0);
			path.append(absolutePath).append(result);
			path.setLength(path.length() - JASPER_SUFFIX.length()); // remove the suffix
			path.append(".jasper");
			return path.toString();
		}

		// not found
		return null;
	}

	/**
	 * Resolves a resource file using repository override precedence.
	 *
	 * <p>Lookup order is customer module resources, module resources, customer
	 * resources, then global resources. Returned files are canonicalised and
	 * validated to stay under the configured repository root.
	 *
	 * @param resourcePath the relative resource path
	 * @param customerName the customer name, or {@code null} for non-customer lookup
	 * @param moduleName the module name, or {@code null} to skip module folders
	 * @return the resolved file (which may not exist when only the final fallback is used)
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

	/**
	 * Ensures the resolved resource path remains within the repository root.
	 *
	 * @param file the candidate file
	 * @return the same file when its canonical path is under the configured base path
	 * @throws SecurityException when canonicalization fails or path traversal is detected
	 */
	private @Nonnull File protect(@Nonnull File file) {
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
