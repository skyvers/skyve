package org.skyve.metadata.repository;

import java.io.File;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.metadata.controller.BizImportAction;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.comparison.ComparisonModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.map.MapModel;

/**
 * 
 */
public interface Repository {
	/**
	 * 
	 * @param customer if <code>null</code>, the entire repository goes.
	 */
	public void evictCachedMetaData(Customer customer);
	
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
	 */
	public Router getRouter();

	/**
	 * 
	 * @param customerName
	 * @return
	 */
	public Customer getCustomer(String customerName);

	/**
	 * 
	 * @param customer
	 * @param document
	 * @param imageName
	 * @return
	 */
	public <T extends Bean> DynamicImage<T> getDynamicImage(Customer customer,
																Document document,
																String imageName);

	/**
	 * 
	 * @param uxui
	 * @param customer
	 * @param document
	 * @param viewType
	 * @return
	 */
	public View getView(String uxui, Customer customer, Document document, ViewType viewType);

	public <T extends Bean, C extends Bean> ComparisonModel<T, C> getComparisonModel(Customer customer, Document document, String modelName);
	
	public <T extends Bean> MapModel<T> getMapModel(Customer customer, Document document, String modelName);

	public <T extends Bean> ListModel<T> getListModel(Customer customer, Document document, String modelName);
	
	/**
	 * 
	 * @param customer
	 * @param document
	 * @param className
	 * @return
	 */
	public ServerSideAction<Bean> getAction(Customer customer, Document document, String className);

	/**
	 * 
	 * @param customer
	 * @param document
	 * @param className
	 * @return
	 */
	public BizExportAction getBizExportAction(Customer customer, Document document, String className);

	/**
	 * 
	 * @param customer
	 * @param document
	 * @param className
	 * @return
	 */
	public BizImportAction getBizImportAction(Customer customer, Document document, String className);

	/**
	 * 
	 * @param customer
	 * @param document
	 * @param className
	 * @return
	 */
	public DownloadAction<Bean> getDownloadAction(Customer customer, Document document, String className);

	/**
	 * 
	 * @param customer
	 * @param document
	 * @param className
	 * @return
	 */
	public UploadAction<Bean> getUploadAction(Customer customer, Document document, String className);

	/**
	 * 
	 * @param userName
	 * @return
	 */
	public User retrieveUser(String userName);

	/**
	 * 
	 * @param user
	 */
	public void resetMenus(User user);
}
