package org.skyve.metadata.repository;

import java.io.File;

import org.skyve.domain.Bean;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.metadata.controller.BizImportAction;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.ComparisonModel;
import org.skyve.metadata.view.model.map.MapModel;
import org.skyve.wildcat.metadata.repository.router.Router;

/**
 * 
 */
public interface Repository {
	/**
	 * 
	 * @param customer if <code>null</code>, the entire repository goes.
	 */
	public void evictCachedMetaData(Customer customer) throws MetaDataException;
	
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
	public <T extends MetaData> T getJavaCode(Customer customer, 
												String fullyQualifiedJavaCodeName, 
												boolean assertExistence)
	throws MetaDataException;
	
	/**
	 * 
	 * @param customer
	 * @param document
	 * @param reportName
	 * @return
	 */
	public String getReportFileName(Customer customer, Document document, String reportName);
	
	/**
	 * Check in customer module folder, check in module folder, check in customer images folder, check in images folder.
	 * 
	 * @param imagePath The relative path to the image
	 * @param customerName The name of the customer.
	 * @param moduleName The name of the module.
	 * @return The resource file.
	 */
	public File findResourceFile(String resourcePath, String customerName, String moduleName);
	
	/**
	 * @return
	 * @throws MetaDataException
	 */
	public Router getRouter()
	throws MetaDataException;

	/**
	 * 
	 * @param customerName
	 * @return
	 * @throws MetaDataException
	 */
	public Customer getCustomer(String customerName)
	throws MetaDataException;

	/**
	 * 
	 * @param customer
	 * @param document
	 * @param imageName
	 * @return
	 * @throws MetaDataException
	 */
	public <T extends Bean> DynamicImage<T> getDynamicImage(Customer customer,
																Document document,
																String imageName)
	throws MetaDataException;

	/**
	 * 
	 * @param uxui
	 * @param customer
	 * @param document
	 * @param viewType
	 * @return
	 * @throws MetaDataException
	 */
	public View getView(String uxui, Customer customer, Document document, ViewType viewType)
	throws MetaDataException;

	
	public <T extends Bean> ComparisonModel<T> getComparisonModel(Customer customer, Document document, String modelName)
	throws MetaDataException;
	
	public <T extends Bean> MapModel<T> getMapModel(Customer customer, Document document, String modelName)
	throws MetaDataException;

	/**
	 * 
	 * @param customer
	 * @param document
	 * @param className
	 * @return
	 * @throws MetaDataException
	 */
	public ServerSideAction<Bean> getAction(Customer customer, Document document, String className)
	throws MetaDataException;

	/**
	 * 
	 * @param customer
	 * @param document
	 * @param className
	 * @return
	 * @throws MetaDataException
	 */
	public BizExportAction getBizExportAction(Customer customer, Document document, String className)
	throws MetaDataException;

	/**
	 * 
	 * @param customer
	 * @param document
	 * @param className
	 * @return
	 * @throws MetaDataException
	 */
	public BizImportAction getBizImportAction(Customer customer, Document document, String className)
	throws MetaDataException;

	/**
	 * 
	 * @param customer
	 * @param document
	 * @param className
	 * @return
	 * @throws MetaDataException
	 */
	public UploadAction<Bean> getUploadAction(Customer customer, Document document, String className)
	throws MetaDataException;

	/**
	 * 
	 * @param userName
	 * @return
	 * @throws MetaDataException
	 */
	public User retrieveUser(String userName)
	throws MetaDataException;

	/**
	 * 
	 * @param user
	 * @throws MetaDataException
	 */
	public void resetMenus(User user)
	throws MetaDataException;
}
