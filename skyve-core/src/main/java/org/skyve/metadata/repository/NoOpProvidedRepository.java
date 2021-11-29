package org.skyve.metadata.repository;

import java.io.File;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.ProvidedRepositoryDelegate;
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

public class NoOpProvidedRepository extends ProvidedRepositoryDelegate {
	@Override
	public void evictCachedMetaData(Customer customer) {
		// do nothing
	}

	@Override
	public File findResourceFile(String resourcePath, String customerName, String moduleName) {
		return null;
	}

	@Override
	public Router getRouter() {
		return null;
	}

	@Override
	public Customer getCustomer(String customerName) {
		return null;
	}

	@Override
	public <T extends Bean> DynamicImage<T> getDynamicImage(Customer customer, Document document, String imageName, boolean runtime) {
		return null;
	}

	@Override
	public View getView(String uxui, Customer customer, Document document, String name) {
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
	public Object getDataFactory(Customer customer, String moduleName, String documentName) {
		return null;
	}

	@Override
	public UserImpl retrieveUser(String userName) {
		return null;
	}

	@Override
	public void resetMenus(User user) {
		// do nothing
	}

	@Override
	public void populatePermissions(User user) {
		// do nothing
	}

	@Override
	public void resetUserPermissions(User user) {
		// do nothing
	}

	@Override
	public List<String> getAllCustomerNames() {
		return null;
	}

	@Override
	public List<String> getAllVanillaModuleNames() {
		return null;
	}

	@Override
	public Module getModule(Customer customer, String moduleName) {
		return null;
	}

	@Override
	public Document getDocument(Customer customer, Module module, String documentName) {
		return null;
	}

	@Override
	public <T extends Bean> Bizlet<T> getBizlet(Customer customer, Document document, boolean runtime) {
		return null;
	}

	@Override
	public void validateCustomerForGenerateDomain(Customer customer) {
		// do nothing
	}

	@Override
	public void validateModuleForGenerateDomain(Customer customer, Module module) {
		// do nothing
	}

	@Override
	public void validateDocumentForGenerateDomain(Customer customer, Document document) {
		// do nothing
	}

	@Override
	public void validateViewForGenerateDomain(Customer customer, Document document, View view, String uxui) {
		// do nothing
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
	public String vtable(String customerName, String key) {
		return null;
	}
	
	@Override
	public boolean getUseScaffoldedViews() {
		return false;
	}
}
