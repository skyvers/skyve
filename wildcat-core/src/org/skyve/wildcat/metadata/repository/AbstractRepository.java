package org.skyve.wildcat.metadata.repository;

import java.io.File;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.domain.types.Enumeration;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.wildcat.metadata.customer.CustomerImpl;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.UtilImpl;

public abstract class AbstractRepository implements Repository {
	private static AbstractRepository repository;

	/**
	 * Prevent external instantiation.
	 */
	protected AbstractRepository() {
		// noop
	}

	public static AbstractRepository get() {
		return repository;
	}

	public static void set(AbstractRepository repository) {
		AbstractRepository.repository = repository;
	}

	public static final String SUBVERSION_DIRECTORY = ".svn";

	public final String ROUTER_NAME = "router";
	public final String ROUTER_NAMESPACE = ROUTER_NAME + '/';
	public final String CUSTOMERS_NAME = "customers";
	public final String CUSTOMERS_NAMESPACE = CUSTOMERS_NAME + '/';
	public final String RESOURCES_NAMESPACE = "resources/";
	public final String MODULES_NAME = "modules";
	public final String MODULES_NAMESPACE = MODULES_NAME + '/';
	protected final String CONVERTERS_NAMESPACE = "converters/";
	protected final String VIEWS_NAME = "views";
	protected final String VIEWS_NAMESPACE = VIEWS_NAME + '/';
	protected final String MODELS_NAME = "models";
	protected final String MODELS_NAMESPACE = MODELS_NAME + '/';
	protected final String ACTIONS_NAME = "actions";
	protected final String ACTIONS_NAMESPACE = ACTIONS_NAME + '/';
	protected final String IMAGES_NAME = "images";
	protected final String IMAGES_NAMESPACE = IMAGES_NAME + '/';
	protected final String REPORTS_NAME = "reports";
	protected final String REPORTS_NAMESPACE = REPORTS_NAME + '/';
	protected final String QUERIES_NAME = "queries";
	protected final String QUERIES_NAMESPACE = QUERIES_NAME + '/';
	public final String DOMAIN_NAME = "domain";
	protected final String DOMAIN_NAMESPACE = DOMAIN_NAME + '/';
	public final String CLIENT_NAME = "client";
	public final String CLIENT_NAMESPACE = CLIENT_NAME + '/';

	protected abstract <T extends MetaData> T get(String name) throws MetaDataException;

	protected abstract void put(String name, MetaData metaData) throws MetaDataException;

	/**
	 * 
	 * @param customer if <code>null</code>, the entire repository goes.
	 */
	@Override
	public void evictCachedMetaData(Customer customer) throws MetaDataException {
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
	}

	// class maps
	private Map<String, Class<?>> classes = new TreeMap<>();
	
	public Class<?> getJavaClass(Customer customer, String fullyQualifiedJavaCodeName) 
	throws MetaDataException {
		Class<?> result = null;
		
		String javaCodeLocation = null;
		if (customer == null) {
			javaCodeLocation = fullyQualifiedJavaCodeName;
		}
		else {
			javaCodeLocation = ((CustomerImpl) customer).getVTable().get(fullyQualifiedJavaCodeName);
		}
		if (javaCodeLocation != null) {
			result = classes.get(javaCodeLocation);
			if (result == null) {
				synchronized (this) {
					// check again in case this thread was stalled by another in the same spot
					result = classes.get(javaCodeLocation);
					if (result == null) {
						String className = javaCodeLocation.replace('/', '.');
						try {
							result = Class.forName(className, true, Thread.currentThread().getContextClassLoader());
						}
						catch (Exception e) {
							throw new MetaDataException("A problem was encountered loading class " + className, e);
						}
					}
				}
			}
		}
		
		return result;
	}
	
	/**
	 * If customer is null, we must be looking for a repository code that does not 
	 * rely on the customer's vtable - not overloaded by a customer.
	 * 
	 * @param <T> The type of the metadata.
	 * @param customer The customer to load the code for, or null
	 * @param fullyQualifiedJavaCodeName
	 * @param assertExistence
	 * @return
	 * @throws MetaDataException
	 */
	@SuppressWarnings("unchecked")
	public final <T extends MetaData> T getJavaCode(Customer customer, 
														String fullyQualifiedJavaCodeName,
														boolean assertExistence)
	throws MetaDataException {
		T result = null;
		Class<?> type = getJavaClass(customer, fullyQualifiedJavaCodeName);
		
		if (type == null) {
			if (assertExistence) {
				throw new MetaDataException(fullyQualifiedJavaCodeName + " does not exist in the customer's vtable");
			}
		}
		else {
			try {
				result = (T) type.newInstance();
			}
			catch (Exception e) {
				throw new MetaDataException("A problem was encountered loading class " + type, e);
			}
		}
		
		return result;
	}

	@Override
	public String getReportFileName(Customer customer, Document document, String reportName) {
		StringBuilder path = new StringBuilder(64);
		path.append(document.getOwningModuleName()).append('.').append(document.getName());
		path.append(".reports.").append(reportName);
		String key = path.toString();
		String result = ((CustomerImpl) customer).getVTable().get(key);
		if (result == null) {
			throw new IllegalArgumentException("Report " + reportName + " for document " + 
												document.getOwningModuleName() + '.' + document.getName() + " is not defined.");
		}

		path.setLength(0);
		path.append(UtilImpl.getAbsoluteBasePath()).append(result).append(".jasper");
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
				path.append(UtilImpl.getAbsoluteBasePath());
				path.append(CUSTOMERS_NAMESPACE);
				path.append(customerName);
				path.append('/');
				path.append(moduleName);
				path.append('/');
				path.append(RESOURCES_NAMESPACE);
				path.append(resourcePath);
				file = new File(path.toString());
				if (file.exists()) {
					return file;
				}
			}
			
			// Check module folder
			path.setLength(0);
			path.append(UtilImpl.getAbsoluteBasePath());
			path.append(MODULES_NAMESPACE);
			path.append(moduleName);
			path.append('/');
			path.append(RESOURCES_NAMESPACE);
			path.append(resourcePath);
			file = new File(path.toString());
			if (file.exists()) {
				return file;
			}
		}

		// Check customer folder, if we have a customer to play with
		if (customerName != null) {
			path.setLength(0);
			path.append(UtilImpl.getAbsoluteBasePath());
			path.append(CUSTOMERS_NAMESPACE);
			path.append(customerName);
			path.append('/');
			path.append(RESOURCES_NAMESPACE);
			path.append(resourcePath);
			file = new File(path.toString());
			if (file.exists()) {
				return file;
			}
		}
		
		path.setLength(0);
		path.append(UtilImpl.getAbsoluteBasePath());
		path.append(RESOURCES_NAMESPACE);
		path.append(resourcePath);
		return new File(path.toString());
	}

	public String getEncapsulatingClassNameForEnumeration(org.skyve.wildcat.metadata.model.document.field.Enumeration enumeration) {
		StringBuilder result = new StringBuilder(64);
		
		result.append(MODULES_NAME).append('.');
		String moduleName = enumeration.getModuleRef();
		if (moduleName == null) {
			moduleName = enumeration.getOwningDocument().getOwningModuleName();
		}
		result.append(moduleName).append('.');
		result.append(DOMAIN_NAME).append('.');
		String documentName = enumeration.getDocumentRef();
		if (documentName == null) {
			documentName = enumeration.getOwningDocument().getName();
		}
		result.append(documentName);

		return result.toString();
	}
	
	public Document findNearestPersistentUnmappedSuperDocument(Customer customer, Module module, Document document)
	throws MetaDataException {
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
				if (ExtensionStrategy.mapped.equals(inheritsPersistent.getStrategy())) {
					Extends baseInherits = result.getExtends();
					if (baseInherits != null) { // only recurse if we have a base document to recurse to
						Module baseModule = getModule(customer, result.getOwningModuleName());
						result = findNearestPersistentUnmappedSuperDocument(customer, baseModule, result);
					}
				}
			}
		}
		
		if (result != null) {
			Persistent persistent = result.getPersistent();
			if ((persistent == null) || 
					(persistent.getName() == null) ||
					ExtensionStrategy.mapped.equals(persistent.getStrategy())) {
				result = null;
			}
		}
		return result;
	}

	public abstract List<String> getAllCustomerNames() throws MetaDataException;

	/**
	 * Used to return all module names defined in the modules area (not customer overridden definitions).
	 * 
	 * @return
	 * @throws MetaDataException
	 */
	public abstract List<String> getAllVanillaModuleNames() throws MetaDataException;

	/**
	 * 
	 * @param customer Can be null, which means get the un-overridden module.
	 * @param moduleName
	 * @return
	 * @throws MetaDataException
	 */
	public abstract Module getModule(Customer customer, String moduleName) throws MetaDataException;

	/**
	 * 
	 * @param customer Can be null, which means get the un-overridden document.
	 * @param module
	 * @param documentName
	 * @return
	 * @throws MetaDataException
	 */
	public abstract Document getDocument(Customer customer, Module module, String documentName) throws MetaDataException;

	public abstract <T extends Bean> Bizlet<T> getBizlet(Customer customer, Document document) throws MetaDataException;

	public abstract Class<Enumeration> getEnum(org.skyve.wildcat.metadata.model.document.field.Enumeration enumeration) throws MetaDataException;

	public abstract void validateCustomer(Customer customer) throws MetaDataException;

	public abstract void validateModule(Customer customer, Module module) throws MetaDataException;

	public abstract void validateDocument(Customer customer, Document document) throws MetaDataException;

	public abstract void validateView(Customer customer, Document document, View view, String uxui) throws MetaDataException;
}
