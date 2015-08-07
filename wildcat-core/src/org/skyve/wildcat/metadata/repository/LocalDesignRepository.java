package org.skyve.wildcat.metadata.repository;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.skyve.domain.Bean;
import org.skyve.domain.types.Enumeration;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.metadata.controller.BizImportAction;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.module.query.QueryColumn;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.comparison.ComparisonModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.map.MapModel;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.generate.ViewGenerator;
import org.skyve.wildcat.metadata.customer.CustomerImpl;
import org.skyve.wildcat.metadata.model.document.DocumentImpl;
import org.skyve.wildcat.metadata.model.document.Inverse;
import org.skyve.wildcat.metadata.model.document.Inverse.InverseRelationship;
import org.skyve.wildcat.metadata.module.menu.AbstractDocumentMenuItem;
import org.skyve.wildcat.metadata.module.menu.AbstractDocumentOrQueryOrModelMenuItem;
import org.skyve.wildcat.metadata.module.menu.EditItem;
import org.skyve.wildcat.metadata.module.menu.MapItem;
import org.skyve.wildcat.metadata.module.menu.TreeItem;
import org.skyve.wildcat.metadata.repository.customer.CustomerMetaData;
import org.skyve.wildcat.metadata.repository.document.DocumentMetaData;
import org.skyve.wildcat.metadata.repository.module.ModuleMetaData;
import org.skyve.wildcat.metadata.repository.router.Router;
import org.skyve.wildcat.metadata.repository.router.UxUi;
import org.skyve.wildcat.metadata.repository.view.ViewMetaData;
import org.skyve.wildcat.metadata.user.ActionPrivilege;
import org.skyve.wildcat.metadata.user.Privilege;
import org.skyve.wildcat.metadata.user.RoleImpl;
import org.skyve.wildcat.metadata.user.UserImpl;
import org.skyve.wildcat.metadata.view.ViewImpl;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

/**
 * Do not instantiate directly, use the RepositoryFactory.
 * 
 * @author Mike
 */
public class LocalDesignRepository extends AbstractRepository {
	/**
	 * The cache. MetaData File Location -> MetaData
	 */
	private static Map<String, MetaData> cache = new HashMap<>();

	@Override
	@SuppressWarnings("unchecked")
	protected <T extends MetaData> T get(String name) {
		return (T) cache.get(name);
	}

	/**
	 * Cache a piece of named metadata.
	 * 
	 * @param name The name of the metadata
	 * @param metaData The metadata.
	 * @throws MetaDataException When a name clash occurs.
	 */
	@Override
	protected void put(String name, MetaData metaData) throws MetaDataException {
		MetaData oldMetaData = cache.put(name, metaData);
		if (oldMetaData != null) {
			throw new MetaDataException("NAME CLASH - " + name + " is already used for " + oldMetaData);
		}
	}

	@Override
	public void evictCachedMetaData(Customer customer) throws MetaDataException {
		// TODO evict for a certain customer needs attention
		super.evictCachedMetaData(customer);

		if (customer == null) {
			cache = new HashMap<>();
		}
	}

	@Override
	public Router getRouter() throws MetaDataException {
		String routerKey = ROUTER_NAMESPACE + ROUTER_NAME;
		Router result = null;
		if (! UtilImpl.DEV_MODE) {
			result = get(routerKey);
		}
		if (result == null) {
			synchronized (this) {
				// check again in case this thread was stalled by another in the same spot
				if (! UtilImpl.DEV_MODE) {
					result = get(routerKey);
				}
				if (result == null) {
					try {
						StringBuilder sb = new StringBuilder(256);
						sb.append(UtilImpl.getAbsoluteBasePath());
						sb.append(ROUTER_NAMESPACE).append(ROUTER_NAME).append(".xml");
						result = XMLUtil.unmarshalRouter(sb.toString());
						result = result.convert(ROUTER_NAME);
						if (! UtilImpl.DEV_MODE) {
							put(routerKey, result);
						}
					}
					catch (Exception e) {
						throw new MetaDataException(e);
					}
				}
			}
		}
		
		return result;
	}

	@Override
	public Customer getCustomer(String customerName) throws MetaDataException {
		String customerKey = CUSTOMERS_NAMESPACE + customerName;
		Customer result = get(customerKey);
		if (result == null) {
			synchronized (this) {
				// check again in case this thread was stalled by another in the same spot
				result = get(customerKey);
				if (result == null) {
					try {
						StringBuilder sb = new StringBuilder(256);
						sb.append(UtilImpl.getAbsoluteBasePath());
						sb.append(CUSTOMERS_NAMESPACE);
						sb.append(customerName).append('/').append(customerName).append(".xml");
						CustomerMetaData customer = XMLUtil.unmarshalCustomer(sb.toString());
						if (! customerName.equals(customer.getName())) {
							throw new MetaDataException("Customer is defined with file name of " + sb.toString() + 
															" but the name attribute is " + customer.getName());
						}
						result = customer.convert(customerName);
						populateVTable((CustomerImpl) result);
						put(customerKey, result);
					}
					catch (Exception e) {
						throw new MetaDataException(e);
					}
				}
			}
		}

		return result;
	}

	@Override
	public List<String> getAllCustomerNames() throws MetaDataException {
		List<String> result = new ArrayList<>();
		
		File customersDirectory = new File(UtilImpl.getAbsoluteBasePath() + CUSTOMERS_NAMESPACE);
		if (customersDirectory.exists() && customersDirectory.isDirectory()) {
			for (File customerDirectory : customersDirectory.listFiles()) {
				if (customerDirectory.isDirectory() && (! customerDirectory.getName().equals(SUBVERSION_DIRECTORY))) {
					result.add(customerDirectory.getName());
				}
			}
		}

		return result;
	}

	@Override
	public List<String> getAllVanillaModuleNames() throws MetaDataException {
		List<String> result = new ArrayList<>();

		File modulesDirectory = new File(UtilImpl.getAbsoluteBasePath() + MODULES_NAMESPACE);
		if (modulesDirectory.exists() && modulesDirectory.isDirectory()) {
			for (File moduleDirectory : modulesDirectory.listFiles()) {
				if (moduleDirectory.isDirectory() && (! moduleDirectory.getName().equals(SUBVERSION_DIRECTORY))) {
					result.add(moduleDirectory.getName());
				}
			}
		}

		return result;
	}

	private void populateVTable(CustomerImpl customer) throws MetaDataException {
		// determine vtable stuff for overrides etc

		StringBuilder sb = new StringBuilder(256);
		for (String moduleName : customer.getModuleNames()) {
			// populate the repository location of the module XML file.
			populateModuleLocation(customer, moduleName);

			// Populate from the customer overridden area first
			sb.setLength(0);
			sb.append(CUSTOMERS_NAMESPACE).append(customer.getName()).append('/');
			sb.append(MODULES_NAMESPACE).append(moduleName).append('/');
			String moduleLocation = sb.toString();
			if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info("module location = " + moduleLocation);
			populateDocumentLocations(customer, moduleLocation, moduleName);

			// Populate from the module area last (these will NOT replace the customer overrides).
			sb.setLength(0);
			sb.append(MODULES_NAMESPACE).append(moduleName).append('/');
			moduleLocation = sb.toString();
			if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info("module location = " + moduleLocation);
			populateDocumentLocations(customer, moduleLocation, moduleName);
		}

		customer.determineDependencies();
	}

	private void populateModuleLocation(CustomerImpl customer, String moduleName) throws MetaDataException {
		StringBuilder sb = new StringBuilder(256);
		Map<String, String> vtable = customer.getVTable();
		sb.append(CUSTOMERS_NAMESPACE).append(customer.getName()).append('/');
		sb.append(MODULES_NAMESPACE).append(moduleName);
		String moduleLocation = sb.toString();
		sb.setLength(0);
		sb.append(UtilImpl.getAbsoluteBasePath());
		sb.append(moduleLocation).append('/');
		sb.append(moduleName).append(".xml");
		File moduleFile = new File(sb.toString());
		if (moduleFile.exists()) {
			if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(moduleName + " -> " + moduleLocation);
			vtable.put(moduleName, moduleLocation);
		}
		else {
			moduleLocation = MODULES_NAMESPACE + moduleName;
			sb.setLength(0);
			sb.append(UtilImpl.getAbsoluteBasePath());
			sb.append(moduleLocation).append('/');
			sb.append(moduleName).append(".xml");
			moduleFile = new File(sb.toString());
			if (moduleFile.exists()) {
				if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(moduleName + " -> " + moduleLocation);
				vtable.put(moduleName, moduleLocation);
			}
			else {
				throw new MetaDataException("Cannot determine the location of module " + moduleName + 
												" for customer " + customer.getName() + "(base path is " + UtilImpl.getAbsoluteBasePath() + ')');
			}
		}
	}

	private void populateDocumentLocations(CustomerImpl customer, String moduleLocation, String moduleName) {
		StringBuilder sb = new StringBuilder(256);
		Map<String, String> vtable = customer.getVTable();
		
		File customerModuleDirectory = new File(UtilImpl.getAbsoluteBasePath() + moduleLocation);
		if (customerModuleDirectory.exists() && customerModuleDirectory.isDirectory()) {
			for (File moduleFile : customerModuleDirectory.listFiles()) {
				String moduleFileName = moduleFile.getName();
				if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info("module file name = " + moduleFileName);

				// found a programmatic queries directory
				if (moduleFile.isDirectory() && moduleFileName.equals(QUERIES_NAME)) {
					for (File queryFile : moduleFile.listFiles()) {
						String queryFileName = queryFile.getName();
						if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("query file name = ").append(queryFileName).toString());
						if (queryFileName.endsWith(".class")) {
							String queryName = queryFileName.substring(0, queryFileName.length() - 6);
							sb.setLength(0);
							sb.append(moduleName).append(".queries.").append(queryName);
							String fullyQualifiedQueryName = sb.toString();
							if (! vtable.containsKey(fullyQualifiedQueryName)) {
								sb.setLength(0);
								sb.append(moduleLocation).append(QUERIES_NAMESPACE).append(queryName);
								String queryLocation = sb.toString();
								if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("Query ").append(fullyQualifiedQueryName).append(" -> ").append(queryLocation).toString());
								vtable.put(fullyQualifiedQueryName, queryLocation);
							}
						}
					}
				}
				// we have found some modules
				else if (moduleFile.isDirectory()) {
					sb.setLength(0);
					sb.append(moduleName).append('.').append(moduleFileName);
					String fullyQualifiedDocumentName = sb.toString();

					for (File documentFile : moduleFile.listFiles()) {
						String documentFileName = documentFile.getName();
						// found the document actions directory
						if (documentFileName.equals(ACTIONS_NAME) && documentFile.isDirectory()) {
							for (File actionFile : documentFile.listFiles()) {
								String actionFileName = actionFile.getName();
								if (actionFileName.endsWith(".class")) {
									String actionName = actionFileName.substring(0, actionFileName.length() - 6);
									sb.setLength(0);
									sb.append(fullyQualifiedDocumentName).append(".actions.").append(actionName);
									String fullyQualifiedActionName = sb.toString();
									if (! vtable.containsKey(fullyQualifiedActionName)) {
										sb.setLength(0);
										sb.append(moduleLocation).append(moduleFileName).append('/');
										sb.append(ACTIONS_NAMESPACE).append(actionName);
										String actionLocation = sb.toString();
										if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("Action ").append(fullyQualifiedActionName).append(" -> ").append(actionLocation).toString());
										vtable.put(fullyQualifiedActionName, actionLocation);
									}
								}
								else if (actionFileName.equals(ACTIONS_NAME + ".xml")) {
									String fullyQualifiedActionConfigsName = fullyQualifiedDocumentName + ".actionconfigs";
									if (! vtable.containsKey(fullyQualifiedActionConfigsName)) {
										sb.setLength(0);
										sb.append(moduleLocation).append(moduleFileName).append('/');
										sb.append(ACTIONS_NAMESPACE).append(ACTIONS_NAME);
										String actionConfigsLocation = sb.toString();

										if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("ActionConfigs ").append(fullyQualifiedActionConfigsName).append(" -> ").append(actionConfigsLocation).toString());
										vtable.put(fullyQualifiedActionConfigsName, actionConfigsLocation);
									}
								}
							}
						}
						// found the document images directory
						else if (documentFileName.equals(IMAGES_NAME) && documentFile.isDirectory()) {
							for (File imageFile : documentFile.listFiles()) {
								String imageFileName = imageFile.getName();
								if (imageFileName.endsWith(".class")) {
									String imageName = imageFileName.substring(0, imageFileName.length() - 6);
									sb.setLength(0);
									sb.append(fullyQualifiedDocumentName).append(".images.").append(imageName);
									String fullyQualifiedImageName = sb.toString();
									if (! vtable.containsKey(fullyQualifiedImageName)) {
										sb.setLength(0);
										sb.append(moduleLocation).append(moduleFileName).append('/');
										sb.append(IMAGES_NAMESPACE).append(imageName);
										String imageLocation = sb.toString();
										if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("Dynamic Image ").append(fullyQualifiedImageName).append(" -> ").append(imageLocation).toString());
										vtable.put(fullyQualifiedImageName, imageLocation);
									}
								}
							}
						}
						// found the document models directory
						else if (documentFileName.equals(MODELS_NAME) && documentFile.isDirectory()) {
							for (File modelFile : documentFile.listFiles()) {
								String modelFileName = modelFile.getName();
								if (modelFileName.endsWith(".class")) {
									String modelName = modelFileName.substring(0, modelFileName.length() - 6);
									sb.setLength(0);
									sb.append(fullyQualifiedDocumentName).append(".models.").append(modelName);
									String fullyQualifiedModelName = sb.toString();
									if (! vtable.containsKey(fullyQualifiedModelName)) {
										sb.setLength(0);
										sb.append(moduleLocation).append(moduleFileName).append('/');
										sb.append(MODELS_NAMESPACE).append(modelName);
										String modelLocation = sb.toString();
										if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("Model ").append(fullyQualifiedModelName).append(" -> ").append(modelLocation).toString());
										vtable.put(fullyQualifiedModelName, modelLocation);
									}
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
									sb.append(fullyQualifiedDocumentName).append(".reports.").append(reportName);
									String fullyQualifiedReportName = sb.toString();
									if (! vtable.containsKey(fullyQualifiedReportName)) {
										sb.setLength(0);
										sb.append(moduleLocation).append(moduleFileName).append('/');
										sb.append(REPORTS_NAMESPACE).append(reportName);
										String reportLocation = sb.toString();
										if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("Report ").append(fullyQualifiedReportName).append(" -> ").append(reportLocation).toString());
										vtable.put(fullyQualifiedReportName, reportLocation);
									}
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
									sb.append(fullyQualifiedDocumentName).append(".views.").append(viewType);
									String fullyQualifiedViewType = sb.toString();
									if (! vtable.containsKey(fullyQualifiedViewType)) {
										sb.setLength(0);
										sb.append(moduleLocation).append(moduleFileName).append('/');
										sb.append(VIEWS_NAMESPACE).append(viewType);
										String viewLocation = sb.toString();
										if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("View ").append(fullyQualifiedViewType).append(" -> ").append(viewLocation).toString());
										vtable.put(fullyQualifiedViewType, viewLocation);
									}
								}
								else if (viewFile.isDirectory()) {
									for (File uxuiViewFile : viewFile.listFiles()) {
										String uxuiViewFileName = uxuiViewFile.getName();
										if (uxuiViewFileName.endsWith(".xml")) { // found a view file
											String viewType = uxuiViewFileName.substring(0, uxuiViewFileName.length() - 4);
											sb.setLength(0);
											sb.append(fullyQualifiedDocumentName).append(".views.");
											sb.append(viewFileName).append('.').append(viewType);
											String fullyQualifiedViewType = sb.toString();
											if (! vtable.containsKey(fullyQualifiedViewType)) {
												sb.setLength(0);
												sb.append(moduleLocation).append(moduleFileName).append('/');
												sb.append(VIEWS_NAMESPACE).append(viewFileName).append('/').append(viewType);
												String viewLocation = sb.toString();
												if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("View ").append(fullyQualifiedViewType).append(" -> ").append(viewLocation).toString());
												vtable.put(fullyQualifiedViewType, viewLocation);
											}
										}
									}
								}
							}
						}
						// found the bizlet class file
						else if (documentFileName.equals(moduleFileName + "Bizlet.class")) {
							String fullyQualifiedBizletName = fullyQualifiedDocumentName + "Bizlet";
							if (! vtable.containsKey(fullyQualifiedBizletName)) {
								sb.setLength(0);
								sb.append(moduleLocation).append(moduleFileName).append('/');
								sb.append(moduleFileName).append("Bizlet");
								String bizletLocation = sb.toString();
								if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("Bizlet ").append(fullyQualifiedBizletName).append(" -> ").append(bizletLocation).toString());
								vtable.put(fullyQualifiedBizletName, bizletLocation);
							}
						}
						// found the document definition file
						else if (documentFileName.equals(moduleFileName + ".xml")) {
							if (! vtable.containsKey(fullyQualifiedDocumentName)) {
								String documentLocation = moduleLocation + moduleFileName;
								if (UtilImpl.XML_TRACE) UtilImpl.LOGGER.info(new StringBuilder(128).append("Document ").append(fullyQualifiedDocumentName).append(" -> ").append(documentLocation).toString());
								vtable.put(fullyQualifiedDocumentName, documentLocation);
							}
						}
					} // for (all document files)
				} // if (moduleFile is a directory)
			} // for (all module files)
		} // if (module directory exists)
	}

	@Override
	public Module getModule(Customer customer, String moduleName) throws MetaDataException {
		CustomerImpl internalCustomer = (CustomerImpl) customer;
		String moduleLocation = null;
		if (customer == null) {
			moduleLocation = MODULES_NAMESPACE + moduleName;
		}
		else {
			moduleLocation = internalCustomer.getVTable().get(moduleName);
		}
		if (moduleLocation == null) {
			throw new MetaDataException("Module " + moduleName + " does not exist in any location for customer " +
											((customer == null) ? "<NO CUSTOMER>" : customer.getName()));
		}
		Module result = get(moduleLocation);
		if (result == null) {
			synchronized (this) {
				// check again in case this thread was stalled by another in the same spot
				result = get(moduleLocation);
				if (result == null) {
					try {
						StringBuilder sb = new StringBuilder(256);
						sb.append(UtilImpl.getAbsoluteBasePath());
						sb.append(moduleLocation).append('/');
						sb.append(moduleName).append(".xml");
						ModuleMetaData module = XMLUtil.unmarshalModule(sb.toString());
						if (! moduleName.equals(module.getName())) {
							throw new MetaDataException("Module is defined with file name of " + sb.toString() + 
															" but the name attribute is " + module.getName());
						}
						
						sb.setLength(0);
						sb.append(moduleName).append(" (").append((customer == null) ? "null" : customer.getName()).append(')');
						result = module.convert(sb.toString());
						put(moduleLocation, result);
					}
					catch (Exception e) {
						throw new MetaDataException(e);
					}
				}
			}
		}

		return result;
	}

	@Override
	public Document getDocument(Customer customer, Module module, String documentName) throws MetaDataException {
		DocumentRef ref = module.getDocumentRefs().get(documentName);
		if (ref == null) {
			throw new IllegalArgumentException(documentName + " does not exist for this module - " + module.getName());
		}
		String documentModuleName = ((ref.getReferencedModuleName() == null) ? module.getName() : ref.getReferencedModuleName());

		StringBuilder sb = new StringBuilder(256);
		String documentLocation = null;
		if (customer == null) {
			sb.setLength(0);
			sb.append(MODULES_NAMESPACE).append(documentModuleName).append('/').append(documentName);
			documentLocation = sb.toString();
		}
		else {
			String moduleNameDotDocumentName = ref.getModuleNameDotDocumentName(documentName);
			documentLocation = ((CustomerImpl) customer).getVTable().get(moduleNameDotDocumentName);
		}
		if (documentLocation == null) {
			throw new IllegalArgumentException("Document " + documentName + 
												" does not exist for module " + module.getName() +
												" in customer " + 
												((customer == null) ? "<NO CUSTOMER>" : customer.getName()));
		}

		Document result = get(documentLocation);
		if (result == null) {
			synchronized (this) {
				// check again in case this thread was stalled by another in the same spot
				result = get(documentLocation);
				if (result == null) {
					try {
						DocumentImpl internalResult = null;
						sb.setLength(0);
						sb.append(UtilImpl.getAbsoluteBasePath());
						sb.append(documentLocation).append('/');
						sb.append(documentName).append(".xml");
						DocumentMetaData document = XMLUtil.unmarshalDocument(sb.toString());
						if (! documentName.equals(document.getName())) {
							throw new MetaDataException("Document is defined with file name of " + sb.toString() + 
															" but the name attribute is " + document.getName());
						}

						sb.setLength(0);
						sb.append(module.getName()).append('.').append(documentName);
						sb.append(" (").append((customer == null) ? "null" : customer.getName()).append(')');
						result = document.convert(sb.toString());
						internalResult = (DocumentImpl) result;
						internalResult.setOwningModuleName(documentModuleName);

						// check each document reference query name links to a module query
						for (String referenceName : result.getReferenceNames()) {
							String queryName = result.getReferenceByName(referenceName).getQueryName();
							Module documentModule = getModule(customer, documentModuleName);
							if ((queryName != null) && (documentModule.getDocumentQuery(queryName) == null)) {
								StringBuilder mde = new StringBuilder(documentName);
								mde.append(" : The reference ");
								mde.append(referenceName);
								mde.append(" has a query ");
								mde.append(queryName);
								mde.append(" that does not exist in module ");
								if (customer != null) {
									mde.append(customer.getName());
									mde.append(".");
								}
								mde.append(documentModuleName);
								
								throw new MetaDataException(mde.toString());
							}
						}

						// Add actions in privileges to the document to enable good view generation
						for (Role role : module.getRoles()) {
							for (Privilege privilege : ((RoleImpl) role).getPrivileges()) {
								if (privilege instanceof ActionPrivilege) {
									ActionPrivilege actionPrivilege = (ActionPrivilege) privilege;
									if (actionPrivilege.getDocumentName().equals(result.getName())) {
										internalResult.getDefinedActionNames().add(actionPrivilege.getName());
									}
								}
							}
						}

						put(documentLocation, result);
					} // try (populate Metadata)
					catch (Exception e) {
						throw new MetaDataException(e);
					}
				}
			}
		} // if (cache entry not created yet)

		return result;
	}

	@Override
	public <T extends Bean> Bizlet<T> getBizlet(Customer customer, Document document) throws MetaDataException {
		StringBuilder fullyQualifiedBizletName = new StringBuilder(64);
		fullyQualifiedBizletName.append(document.getOwningModuleName()).append('.');
		fullyQualifiedBizletName.append(document.getName()).append("Bizlet");
		return getJavaCode(customer, fullyQualifiedBizletName.toString(), false);
	}

	
	@Override
	@SuppressWarnings("unchecked")
	public Class<Enumeration> getEnum(org.skyve.wildcat.metadata.model.document.field.Enumeration enumeration)
	throws MetaDataException {
		// No enum overriding, but there might be referencing
		String encapulatingClasNameForEnumeration = getEncapsulatingClassNameForEnumeration(enumeration);
		
		StringBuilder fullyQualifiedEnumName = new StringBuilder(64);
		fullyQualifiedEnumName.append(encapulatingClasNameForEnumeration);
		fullyQualifiedEnumName.append('$').append(enumeration.toJavaIdentifier());

		try {
			return (Class<Enumeration>) Class.forName(fullyQualifiedEnumName.toString(), true, Thread.currentThread().getContextClassLoader());
		}
		catch (Exception e) {
			throw new MetaDataException("A problem was encountered loading enum " + fullyQualifiedEnumName.toString(), e);
		}
	}

	@Override
	public <T extends Bean> DynamicImage<T> getDynamicImage(Customer customer, Document document, String imageName) throws MetaDataException {
		StringBuilder fullyQualifiedImageName = new StringBuilder(128);
		fullyQualifiedImageName.append(document.getOwningModuleName()).append('.').append(document.getName());
		fullyQualifiedImageName.append(".images.").append(imageName);
		return getJavaCode(customer, fullyQualifiedImageName.toString(), true);
	}

	@Override
	public View getView(String uxui,
							Customer customer, 
							Document document, 
							ViewType viewType)
	throws MetaDataException {
		StringBuilder sb = new StringBuilder(256);
		sb.append(document.getOwningModuleName()).append('.').append(document.getName());
		sb.append(".views.").append(uxui).append('.').append(viewType.toString());
		String fullyQualifiedViewName = sb.toString();
		Map<String, String> vtable = ((CustomerImpl) customer).getVTable();
		String viewLocation = vtable.get(fullyQualifiedViewName);
		View result = null;
		if (viewLocation == null) { // there DNE a purpose built view for this UX/UI - look for the general view to use.
			sb.setLength(0);
			sb.append(document.getOwningModuleName()).append('.').append(document.getName());
			sb.append(".views.").append(viewType.toString());
			fullyQualifiedViewName = sb.toString();
			viewLocation = vtable.get(fullyQualifiedViewName);
		}
		
		if (viewLocation != null) { // there is a view defined on the file system (or one has been generated previously)
			if (! UtilImpl.DEV_MODE) {
				result = get(viewLocation);
			}
			if (result == null) {
				synchronized (this) {
					// check again in case this thread was stalled by another in the same spot
					if (! UtilImpl.DEV_MODE) {
						result = get(viewLocation);
					}
					if (result == null) {
						try {
							sb.setLength(0);
							sb.append(UtilImpl.getAbsoluteBasePath());
							sb.append(viewLocation).append(".xml");
							ViewMetaData view = XMLUtil.unmarshalView(sb.toString());
							if (! viewType.equals(view.getType())) {
								throw new MetaDataException("View is defined with file name of " + sb.toString() + 
																" but the type attribute is " + view.getType());
							}

							sb.setLength(0);
							sb.append(document.getOwningModuleName()).append('.').append(document.getName());
							sb.append('.').append(viewType).append(" (").append(customer.getName()).append(')');
							result = view.convert(sb.toString());
							if (! UtilImpl.DEV_MODE) {
								put(viewLocation, result);
							}
						}
						catch (Exception e) {
							throw new MetaDataException(e);
						}
					}
				}
			}
		}
		else { // there is no view defined on the file system (and not generated previously)
			if ((viewType == ViewType.edit) || (viewType == ViewType.pick) || (viewType == ViewType.params)) {
				synchronized (this) {
					sb.setLength(0);
					sb.append(document.getOwningModuleName()).append('.').append(document.getName());
					String documentLocation = vtable.get(sb.toString());
					sb.setLength(0);
					sb.append(documentLocation).append('/').append(VIEWS_NAMESPACE).append(viewType.toString());
					viewLocation = sb.toString();
					// Consider the scenario where an edit view is not defined for a document.
					// One customer logs in and the view is generated and cached.
					// Another customer logs in and will reach this point - cached by another customer.
					if (! UtilImpl.DEV_MODE) {
						result = get(viewLocation);
					}
					if (result == null) {
						result = ViewGenerator.generate(customer, document, viewType);
						if (! UtilImpl.DEV_MODE) {
							put(viewLocation, result);
						}
					}
					// Now add a vtable entry for this customer.
					if (! UtilImpl.DEV_MODE) {
						vtable.put(fullyQualifiedViewName, viewLocation);
					}
				}
			}
		}

		return result;
	}

	@Override
	public  <T extends Bean, C extends Bean> ComparisonModel<T, C> getComparisonModel(Customer customer, Document document, String modelName)
	throws MetaDataException {
		StringBuilder fullyQualifiedActionName = new StringBuilder(128);
		fullyQualifiedActionName.append(document.getOwningModuleName()).append('.').append(document.getName());
		fullyQualifiedActionName.append(".models.").append(modelName);
		return getJavaCode(customer, fullyQualifiedActionName.toString(), true);
	}

	@Override
	public  <T extends Bean> MapModel<T> getMapModel(Customer customer, Document document, String modelName)
	throws MetaDataException {
		StringBuilder fullyQualifiedActionName = new StringBuilder(128);
		fullyQualifiedActionName.append(document.getOwningModuleName()).append('.').append(document.getName());
		fullyQualifiedActionName.append(".models.").append(modelName);
		return getJavaCode(customer, fullyQualifiedActionName.toString(), true);
	}

	@Override
	public  <T extends Bean> ListModel<T> getListModel(Customer customer, Document document, String modelName)
	throws MetaDataException {
		StringBuilder fullyQualifiedActionName = new StringBuilder(128);
		fullyQualifiedActionName.append(document.getOwningModuleName()).append('.').append(document.getName());
		fullyQualifiedActionName.append(".models.").append(modelName);
		return getJavaCode(customer, fullyQualifiedActionName.toString(), true);
	}

	@Override
	public ServerSideAction<Bean> getAction(Customer customer, Document document, String actionName) throws MetaDataException {
		StringBuilder fullyQualifiedActionName = new StringBuilder(128);
		fullyQualifiedActionName.append(document.getOwningModuleName()).append('.').append(document.getName());
		fullyQualifiedActionName.append(".actions.").append(actionName);
		return getJavaCode(customer, fullyQualifiedActionName.toString(), true);
	}

	@Override
	public BizExportAction getBizExportAction(Customer customer, Document document, String exportActionName) throws MetaDataException {
		StringBuilder fullyQualifiedActionName = new StringBuilder(128);
		fullyQualifiedActionName.append(document.getOwningModuleName()).append('.').append(document.getName());
		fullyQualifiedActionName.append(".actions.").append(exportActionName);
		return getJavaCode(customer, fullyQualifiedActionName.toString(), true);
	}

	@Override
	public BizImportAction getBizImportAction(Customer customer, Document document, String importActionName) throws MetaDataException {
		StringBuilder fullyQualifiedActionName = new StringBuilder(128);
		fullyQualifiedActionName.append(document.getOwningModuleName()).append('.').append(document.getName());
		fullyQualifiedActionName.append(".actions.").append(importActionName);
		return getJavaCode(customer, fullyQualifiedActionName.toString(), true);
	}

	@Override
	public DownloadAction<Bean> getDownloadAction(Customer customer, Document document, String uploadActionName) throws MetaDataException {
		StringBuilder fullyQualifiedActionName = new StringBuilder(128);
		fullyQualifiedActionName.append(document.getOwningModuleName()).append('.').append(document.getName());
		fullyQualifiedActionName.append(".actions.").append(uploadActionName);
		return getJavaCode(customer, fullyQualifiedActionName.toString(), true);
	}

	@Override
	public UploadAction<Bean> getUploadAction(Customer customer, Document document, String uploadActionName) throws MetaDataException {
		StringBuilder fullyQualifiedActionName = new StringBuilder(128);
		fullyQualifiedActionName.append(document.getOwningModuleName()).append('.').append(document.getName());
		fullyQualifiedActionName.append(".actions.").append(uploadActionName);
		return getJavaCode(customer, fullyQualifiedActionName.toString(), true);
	}

	@Override
	public User retrieveUser(String userPrincipal) throws MetaDataException {
		throw new UnsupportedOperationException();
	}

	@Override
	public final void resetMenus(User user) throws MetaDataException {
		UserImpl internalUser = (UserImpl) user;
		for (Module module : user.getCustomer().getModules()) {
			Menu menu = UtilImpl.cloneBySerialization(module.getMenu());
			removeInaccessibleItems(module.getName(), menu, user);
			internalUser.putModuleMenu(module.getName(), menu);
		}
	}

	private static void removeInaccessibleItems(String moduleName, Menu menu, User user) {
		// Check all the child items to see if we have access
		Iterator<MenuItem> i = menu.getItems().iterator();
		while (i.hasNext()) {
			MenuItem menuItem = i.next();

			// If we are dealing with a menu group, recurse the check to its menu items
			if (menuItem instanceof Menu) {
				Menu menuGroup = (Menu) menuItem;

				removeInaccessibleItems(moduleName, menuGroup, user);

				// If there are no menu items left, remove the menu group as well.
				if (menuGroup.getItems().isEmpty()) {
					i.remove();
				}
			}
			else { // we are dealing with menu items (not a group)
				// item is secured by at least 1 role
				boolean secureMenuItem = ( !menuItem.getRoleNames().isEmpty());

				// if not a secured item then it is automatically accessible
				boolean accessibleMenuItem = ( !secureMenuItem);

				if (! accessibleMenuItem) {
					// check for a role name in the menu item that the user has permissions to.
					for (String roleName : menuItem.getRoleNames()) {
						if (user.isInRole(moduleName, roleName)) {
							accessibleMenuItem = true;
							break;
						}
					}
				}

				// Remove the menu item if it is not accessible,
				if (! accessibleMenuItem) {
					i.remove();
				}
			}
		}
	}

	@Override
	public void validateCustomer(Customer customer) throws MetaDataException {
		populateVTable((CustomerImpl) customer);

		try {
			Module homeModule = customer.getHomeModule();
			if (homeModule == null) {
				throw new MetaDataException("Repository returned null for [homeModule] in customer " + customer.getName());
			}
		}
		catch (MetaDataException e) {
			throw new MetaDataException("Home Module reference does not reference a module in customer " + customer.getName(), e);
		}

		for (String moduleName : ((CustomerImpl) customer).getModuleNames()) {
			try {
				if (getModule(customer, moduleName) == null) {
					throw new MetaDataException("Repository returned null for " + moduleName + 
													" for customer " + customer.getName());
				}
			}
			catch (MetaDataException e) {
				throw new MetaDataException("Module reference " + moduleName + 
												" does not reference a module in customer " + customer.getName(), e);
			}
		}
		// TODO check the converter type corresponds to the type required.
	}

	@Override
	public void validateModule(Customer customer, Module module) throws MetaDataException {
		// if home document is transient then home ref had better be edit
		String homeDocumentName = module.getHomeDocumentName();
		if (homeDocumentName != null) {
			Document homeDocument = module.getDocument(customer, homeDocumentName);
			if ((homeDocument.getPersistent() == null) && (! ViewType.edit.equals(module.getHomeRef()))) { // is transient but not edit
				throw new MetaDataException("Home document " + homeDocumentName + 
												" for customer " + customer.getName() + 
												" in module " + module.getName() +
												" is transient and therefore the module requires a homeRef of 'edit'.");
			}
		}
		
		// check action privilege references an action in the given document view
		for (Role role : module.getRoles()) {
			for (Privilege privilege : ((RoleImpl) role).getPrivileges()) {
				if (privilege instanceof ActionPrivilege) {
					ActionPrivilege actionPrivilege = (ActionPrivilege) privilege;
					String actionPrivilegeName = actionPrivilege.getName();
					Document actionDocument = null;

					List<View> views = new ArrayList<>(2);
					try {
						actionDocument = module.getDocument(customer, actionPrivilege.getDocumentName());

						Router router = getRouter();
						for (UxUi uxui : router.getUxUis()) {
							views.add(actionDocument.getView(uxui.getName(), customer, ViewType.edit));
							views.add(actionDocument.getView(uxui.getName(), customer, ViewType.create));
						}
					}
					catch (MetaDataException e) {
						throw new MetaDataException("Could not get view for document " + actionPrivilege.getDocumentName(), e);
					}
					// if the action is not in any defined view, chuck a wobbly
					boolean found = false;
					for (View view : views) {
						if ((view != null) && (view.getAction(actionPrivilegeName) != null)) {
							found = true;
							break;
						}
					}
					if (! found) {
						throw new MetaDataException("Action privilege " + actionPrivilege.getName() + 
														" for customer " + customer.getName() + 
														" in module " + module.getName() +
														" for document " + actionDocument.getName() + 
														" for role " + role.getName() +
														" does not reference a valid action");
					}
				}
			}
		}
		
		// check query columns
		for (QueryDefinition query : module.getMetadataQueries()) {
			if (query instanceof DocumentQueryDefinition) {
				DocumentQueryDefinition documentQuery = (DocumentQueryDefinition) query;
				Module queryDocumentModule = documentQuery.getDocumentModule(customer);
				Document queryDocument = queryDocumentModule.getDocument(customer, documentQuery.getDocumentName());
				for (QueryColumn column : documentQuery.getColumns()) {
					String binding = column.getBinding();
					if (binding != null) {
						TargetMetaData target = null;
						try {
							target = BindUtil.getMetaDataForBinding(customer, 
																		queryDocumentModule,
																		queryDocument,
																		binding);
						}
						catch (MetaDataException e) {
							throw new MetaDataException("Query " + query.getName() + 
															" in module " + query.getOwningModule().getName() +
															" with column binding " + binding +
															" is not a valid binding.", e);
						}
	
						Document targetDocument = target.getDocument();
						Attribute targetAttribute = target.getAttribute();
						Persistent targetPersistent = targetDocument.getPersistent();
						if ((targetPersistent == null) || (targetPersistent.getName() == null) || // transient document
								((targetAttribute != null) && 
									(! BindUtil.isImplicit(targetAttribute.getName())) &&
									(! targetAttribute.isPersistent()))) { // transient non-implicit attribute
							if (column.isSortable() || column.isFilterable() || column.isEditable()) {
								throw new MetaDataException("Query " + query.getName() + 
															" in module " + query.getOwningModule().getName() +
															" with column binding " + binding +
															" references a transient (or mapped) attribute and should not be sortable, filterable or editable.");
							}
						}
						
						// Customer overridden documents that are used in metadata queries cause an error unless 
						// <association>.bizId is used as the binding.
						if ((targetAttribute != null) && AttributeType.association.equals(targetAttribute.getAttributeType()) &&
								(column.getFilterOperator() != null)) {
							throw new MetaDataException("Query " + query.getName() + 
															" in module " + query.getOwningModule().getName() +
															" with column binding " + binding +
															" references an association which has a column filter defined.  Use [" + 
															binding + ".bizId] as the binding for the column.");
						}
					}
				}
			}
		}
		
		// check menu items
		checkMenu(module.getMenu().getItems(), customer, module);
	}

	private void checkMenu(List<MenuItem> items, Customer customer, Module module)
	throws MetaDataException {
		for (MenuItem item : items) {
			if (item instanceof MenuGroup) {
				checkMenu(((MenuGroup) item).getItems(), customer, module);
			}
			else {
				if (item instanceof AbstractDocumentMenuItem) {
					String documentName = ((AbstractDocumentMenuItem) item).getDocumentName();
					Document document = null;
					if (documentName != null) {
						try {
							document = module.getDocument(customer, documentName);
						}
						catch (Exception e) {
							throw new MetaDataException("Menu [" + item.getName() + 
															"] in module " + module.getName() +
															" is for document " + documentName +
															" which does not exist.", e);
						}
						// NB EditItem can be to a transient document
						if ((! (item instanceof EditItem)) && (document.getPersistent() == null)) {
							throw new MetaDataException("Menu [" + item.getName() + 
															"] in module " + module.getName() +
															" is for document " + documentName +
															" which is not persistent.");
						}
					}

					if (item instanceof AbstractDocumentOrQueryOrModelMenuItem) {
						AbstractDocumentOrQueryOrModelMenuItem dataItem = (AbstractDocumentOrQueryOrModelMenuItem) item;
						String queryName = dataItem.getQueryName();
						DocumentQueryDefinition query = null;
						if (queryName != null) {
							query = module.getDocumentQuery(queryName);
							if (query == null) {
								throw new MetaDataException("Menu [" + item.getName() + 
																"] in module " + module.getName() +
																" is for query " + queryName +
																" which does not exist.");
							}
							documentName = query.getDocumentName();
							document = module.getDocument(customer, documentName);
						}
						
						// TODO check list/tree/calendar model names
						String modelName = ((AbstractDocumentOrQueryOrModelMenuItem) item).getModelName();
						if (modelName != null) {
							if (item instanceof MapItem) {
								try {
									getMapModel(customer, document, modelName);
								}
								catch (Exception e) {
									throw new MetaDataException("Menu [" + item.getName() + 
																	"] in module " + module.getName() +
																	" is for model " + modelName +
																	" which does not exist.");
								}
							}
						}
						
						if (item instanceof TreeItem) {
							// Not a model, then its a query or document so check the document is hierarchical
							if ((modelName == null) && (documentName != null) && (document != null)) {
								if (! documentName.equals(document.getParentDocumentName())) {
									throw new MetaDataException("Tree Menu [" + item.getName() + 
																	"] in module " + module.getName() + 
																	" is for document " + document.getName() + 
																	" which is not hierarchical.");
								}
							}
						}
						else if (item instanceof MapItem) {
							if (document != null) {
								String binding = ((MapItem) item).getGeometryBinding();
								try {
									TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
									Attribute attribute = target.getAttribute();
									if ((attribute == null) || 
											(! AttributeType.geometry.equals(attribute.getAttributeType()))) {
										throw new MetaDataException("Map Menu [" + item.getName() + 
																		"] in module " + module.getName() + 
																		" has a geometryBinding of " + binding + 
																		" which is not a geometry.");
									}
								}
								catch (Exception e) {
									throw new MetaDataException("Map Menu [" + item.getName() + 
																	"] in module " + module.getName() + 
																	" has a geometryBinding of " + binding + 
																	" which does not exist.");
								}
								
							}
						}
					}
				}
			}
		}
	}
	
	@Override
	public void validateDocument(Customer customer, Document document) throws MetaDataException {
		String documentIdentifier = document.getOwningModuleName() + '.' + document.getName();

		// Check that conditions do not start with is or not
		for (String conditionName : ((DocumentImpl) document).getConditionsCode().keySet()) {
			if (conditionName.startsWith("is")) {
				throw new MetaDataException("Condition " + conditionName + " in document " + documentIdentifier + " cannot start with 'is' - the 'is' prefix is generated in the bean method.");
			}
			else if (conditionName.startsWith("not")) {
				throw new MetaDataException("Condition " + conditionName + " in document " + documentIdentifier + " cannot start with 'not' - not conditions are automatically generated.  Switch the sense of the condition.");
			}
		}
		
		// Check the bizKey expression bindings, if defined
		String bizKeyExpression = ((DocumentImpl) document).getBizKeyExpression();
		if (bizKeyExpression != null) {
			Module module = getModule(customer, document.getOwningModuleName());
			if (! BindUtil.messageBindingsAreValid(customer, module, document, bizKeyExpression)) {
				throw new MetaDataException("The biz key [expression] defined contains malformed binding expressions in document " + documentIdentifier);
			}
		}
		
		// NOTE - Persistent etc is checked when generating documents as it is dependent on the hierarchy and persistence strategy etc
/*		
		Persistent persistent = document.getPersistent();
		if (persistent != null) {
			String persistentName = persistent.getName();
			ExtensionStrategy strategy = persistent.getStrategy();
			if ((strategy != null) && (persistentName == null)) {
				throw new MetaDataException("Document " + documentIdentifier + " employs extension strategy " + strategy + " but has no persistent name.");
			}
		}
*/		
		// TODO if document has no persistentName, ensure each field has persistent = false
		// TODO if document has a parentDocument defined, ensure that it exists in the document's module.

		// Check attributes
		for (Attribute attribute : document.getAttributes()) {
			// TODO for all fields that hasDomain is true, ensure that a bizlet exists and it returns domain values (collection length not zero)
			// TODO for all references ensure that the documentName document exists for the module.
			// TODO for all composition collections (ie reference a document that has a parentDocument = to this one) - no queryName is defined on the collection.
			// TODO for all aggregation collections (ie reference a document that has does not have a parentDocument = to this one {or parentDocument is not defined}) - a queryName must be defined on the collection.
			// TODO for all references with a query, ensure that the query's document is the same as the reference's document.

			if (attribute instanceof Inverse) {
				// Check that the document name and reference name point to a reference
				Inverse inverse = (Inverse) attribute;
				String targetDocumentName = inverse.getDocumentName();
				Module owningModule = getModule(customer, document.getOwningModuleName());
				DocumentRef inverseDocumentRef = owningModule.getDocumentRefs().get(targetDocumentName);
				if (inverseDocumentRef == null) {
					throw new MetaDataException("The target [documentName] of " + 
													targetDocumentName + " in Inverse " +
													inverse.getName() + " in document " + 
													documentIdentifier + " is not a valid document reference in this module.");
					
				}
				Module targetModule = owningModule;
				String targetModuleName = inverseDocumentRef.getReferencedModuleName();
				if (targetModuleName != null) {
					targetModule = getModule(customer, targetModuleName);
				}
				Document targetDocument = getDocument(customer, targetModule, targetDocumentName);

				String targetReferenceName = inverse.getReferenceName();
				Reference targetReference = targetDocument.getReferenceByName(targetReferenceName);
				if (targetReference == null) {
					throw new MetaDataException("The target [referenceName] of " + 
													targetReferenceName + " in Inverse " +
													inverse.getName() + " in document " + 
													documentIdentifier + " is not a valid reference within the document " + 
													targetModule.getName() + '.' + targetDocumentName);
					
				}
				inverse.setRelationship((targetReference instanceof Collection) ?
											InverseRelationship.manyToMany :
											InverseRelationship.oneToMany);
			}
		}
		
		// Check the message binding expressions, if present
		List<UniqueConstraint> constraints = document.getUniqueConstraints();
		if (constraints != null) {
			Module owningModule = getModule(customer, document.getOwningModuleName());
			for (UniqueConstraint constraint : constraints) {
				String message = constraint.getMessage();
				if (! BindUtil.messageBindingsAreValid(customer, owningModule, document, message)) {
					throw new MetaDataException("The unique constraint [message] contains malformed binding expressions in constraint " +
							constraint.getName() + " in document " + documentIdentifier);
				}
			}
		}
		// TODO check binding in uniqueConstraint.fieldReference.ref as above
	}

	@Override
	@SuppressWarnings("unused")
	public void validateView(Customer customer, Document document, View view, String uxui) throws MetaDataException {
		new ViewValidator((ViewImpl) view,
							(CustomerImpl) customer,
							(DocumentImpl) document,
							uxui);
	}
}
