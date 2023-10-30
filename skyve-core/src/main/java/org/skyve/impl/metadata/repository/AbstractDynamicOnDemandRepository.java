package org.skyve.impl.metadata.repository;

import java.io.File;
import java.sql.Connection;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.user.UserImpl;
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

public abstract class AbstractDynamicOnDemandRepository extends MutableCachedRepository {
	@Override
	public <T extends Bean> Bizlet<T> getBizlet(Customer customer, Document document, boolean runtime) {
		return null;
	}

	@Override
	public BizletMetaData getMetaDataBizlet(Customer customer, Document document) {
		return null;
	}

	@Override
	public <T extends Bean> DynamicImage<T> getDynamicImage(Customer customer, Document document, String imageName, boolean runtime) {
		return null;
	}

	@Override
	public <T extends Bean, C extends Bean> ComparisonModel<T, C> getComparisonModel(Customer customer, Document document, String modelName, boolean runtime) {
		return null;
	}

	@Override
	public <T extends Bean> MapModel<T> getMapModel(Customer customer, Document document, String modelName, boolean runtime) {
		return null;
	}

	@Override
	public <T extends Bean> ChartModel<T> getChartModel(Customer customer, Document document, String modelName, boolean runtime) {
		return null;
	}

	@Override
	public <T extends Bean> ListModel<T> getListModel(Customer customer, Document document, String modelName, boolean runtime) {
		return null;
	}

	@Override
	public ServerSideAction<Bean> getServerSideAction(Customer customer, Document document, String className, boolean runtime) {
		return null;
	}

	@Override
	public ActionMetaData getMetaDataAction(Customer customer, Document document, String actionName) {
		return null;
	}
	
	@Override
	public BizExportAction getBizExportAction(Customer customer, Document document, String className, boolean runtime) {
		return null;
	}

	@Override
	public BizImportAction getBizImportAction(Customer customer, Document document, String className, boolean runtime) {
		return null;
	}

	@Override
	public DownloadAction<Bean> getDownloadAction(Customer customer, Document document, String className, boolean runtime) {
		return null;
	}

	@Override
	public UploadAction<Bean> getUploadAction(Customer customer, Document document, String className, boolean runtime) {
		return null;
	}

	@Override
	public void validateCustomerForGenerateDomain(Customer customer) {
		// nothing to do
	}

	@Override
	public void validateModuleForGenerateDomain(Customer customer, Module module) {
		// nothing to do
	}

	@Override
	public void validateDocumentForGenerateDomain(Customer customer, Document document) {
		// nothing to do
	}

	@Override
	public void validateViewForGenerateDomain(Customer customer, Document document, View view, String uxui) {
		// nothing to do
	}

	@Override
	public Router getGlobalRouter() {
		return null;
	}

	@Override
	public List<Router> getModuleRouters() {
		return null;
	}

	@Override
	public String getReportFileName(Customer customer, Document document, String reportName) {
		return null;
	}

	@Override
	public Class<?> getJavaClass(Customer customer, String fullyQualifiedJavaCodeName) {
		return null;
	}

	@Override
	public UserImpl retrieveUser(String userPrincipal) {
		return null;
	}

	@Override
	public void resetMenus(User user) {
		// nothing to do
	}

	@Override
	public void populatePermissions(User user) {
		// nothing to do
	}

	@Override
	public void resetUserPermissions(User user) {
		// nothing to do
	}
	
	@Override
	public void populateUser(User user, Connection connection) {
		// nothing to do
	}

	@Override
	public List<Bean> retrieveAllJobSchedulesForAllCustomers() {
		return null;
	}

	@Override
	public List<Bean> retrieveAllReportSchedulesForAllCustomers() {
		return null;
	}

	@Override
	public String retrievePublicUserName(String customerName) {
		return null;
	}
	
	@Override
	public File findResourceFile(String resourcePath, String customerName, String moduleName) {
		return null;
	}

	@Override
	public Object getDataFactory(Customer customer, Document document) {
		return null;
	}
}
