package org.skyve.impl.metadata.repository;

import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

import org.apache.deltaspike.core.api.provider.BeanProvider;
import org.skyve.domain.messages.SkyveException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;

public abstract class FileSystemRepository extends CachedRepository {
	protected String absolutePath;
	private String canonicalBasePath;
	protected boolean loadClasses = true;
	// class maps
	private Map<String, Class<?>> classes = new TreeMap<>();
	
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
	public Class<?> getJavaClass(Customer customer, String fullyQualifiedJavaCodeName) {
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
						if (loadClasses) {
							String className = javaCodeLocation.replace('/', '.');
							try {
								result = Class.forName(className, true, Thread.currentThread().getContextClassLoader());
							}
							catch (Exception e) {
								throw new MetaDataException("A problem was encountered loading class " + className, e);
							}
						}
						else {
							// Not loading classes
							// check for a java file and return a MetaData implementation
							// NB WidgetReference is a pretty simple MetaData implementation
							if (new File(this.absolutePath + javaCodeLocation + ".java").exists()) {
								result = WidgetReference.class;
							}
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
	 * @param runtime	Are we really running or just generating etc.
	 * @return a new instance of the specified java class name or null if it does not exist in the customers vtable
	 */
	@SuppressWarnings("unchecked")
	public final <T extends MetaData> T getJavaMetaData(Customer customer, 
															String fullyQualifiedJavaCodeName,
															boolean assertExistence,
															boolean runtime) {
		T result = null;
		
		Class<?> type = getJavaClass(customer, fullyQualifiedJavaCodeName);
		if (type == null) {
			if (assertExistence) {
				throw new MetaDataException(fullyQualifiedJavaCodeName + " does not exist in the customer's vtable");
			}
		}
		else {
			try {
				result = (T) type.getConstructor().newInstance();
				if (runtime) {
					BeanProvider.injectFields(result);
				}
			}
			catch (SkyveException e) {
				throw e;
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
