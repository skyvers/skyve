package org.skyve.impl.cdi;

import java.io.File;
import java.io.Serializable;

import javax.enterprise.inject.Alternative;

import org.skyve.CORE;
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
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.model.comparison.ComparisonModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.map.MapModel;

/**
 * A proxy that can be Serialized with no state and continue to work after deserialization.
 * 
 * @author mike
 */
@Alternative
public class RepositoryInjectable implements Repository, Serializable {
	private static final long serialVersionUID = -2932762525201765101L;

	@Override
	public void evictCachedMetaData(Customer customer) {
		CORE.getRepository().evictCachedMetaData(customer);
	}

	@Override
	public String getReportFileName(Customer customer, Document document, String reportName) {
		return CORE.getRepository().getReportFileName(customer, document, reportName);
	}

	@Override
	public File findResourceFile(String resourcePath, String customerName, String moduleName) {
		return CORE.getRepository().findResourceFile(resourcePath, customerName, moduleName);
	}

	@Override
	public Router getRouter() {
		return CORE.getRepository().getRouter();
	}

	@Override
	public Customer getCustomer(String customerName) {
		return CORE.getRepository().getCustomer(customerName);
	}

	@Override
	public <T extends Bean> DynamicImage<T> getDynamicImage(Customer customer,
																Document document,
																String imageName,
																boolean runtime) {
		return CORE.getRepository().getDynamicImage(customer, document, imageName, runtime);
	}

	@Override
	public View getView(String uxui, Customer customer, Document document, String name) {
		return CORE.getRepository().getView(uxui, customer, document, name);
	}

	@Override
	public <T extends Bean, C extends Bean> ComparisonModel<T, C> getComparisonModel(Customer customer,
																						Document document,
																						String modelName,
																						boolean runtime) {
		return CORE.getRepository().getComparisonModel(customer, document, modelName, runtime);
	}

	@Override
	public <T extends Bean> MapModel<T> getMapModel(Customer customer,
														Document document,
														String modelName,
														boolean runtime) {
		return CORE.getRepository().getMapModel(customer, document, modelName, runtime);
	}

	@Override
	public <T extends Bean> ListModel<T> getListModel(Customer customer,
														Document document,
														String modelName,
														boolean runtime) {
		return CORE.getRepository().getListModel(customer, document, modelName, runtime);
	}

	@Override
	public ServerSideAction<Bean> getServerSideAction(Customer customer,
														Document document,
														String className,
														boolean runtime) {
		return CORE.getRepository().getServerSideAction(customer, document, className, runtime);
	}

	@Override
	public BizExportAction getBizExportAction(Customer customer,
												Document document,
												String className,
												boolean runtime) {
		return CORE.getRepository().getBizExportAction(customer, document, className, runtime);
	}

	@Override
	public BizImportAction getBizImportAction(Customer customer,
												Document document,
												String className,
												boolean runtime) {
		return CORE.getRepository().getBizImportAction(customer, document, className, runtime);
	}

	@Override
	public DownloadAction<Bean> getDownloadAction(Customer customer,
													Document document,
													String className,
													boolean runtime) {
		return CORE.getRepository().getDownloadAction(customer, document, className, runtime);
	}

	@Override
	public UploadAction<Bean> getUploadAction(Customer customer,
												Document document,
												String className,
												boolean runtime) {
		return CORE.getRepository().getUploadAction(customer, document, className, runtime);
	}

	@Override
	public User retrieveUser(String userName) {
		return CORE.getRepository().retrieveUser(userName);
	}

	@Override
	public void resetMenus(User user) {
		CORE.getRepository().resetMenus(user);
	}

	@Override
	public void populatePermissions(User user) {
		CORE.getRepository().populatePermissions(user);
	}

	@Override
	public void resetUserPermissions(User user) {
		CORE.getRepository().resetUserPermissions(user);
	}
}
