package org.skyve.impl.metadata.repository;

import java.io.File;
import java.sql.Connection;
import java.util.Collections;
import java.util.List;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.job.UserJobSchedule;
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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * A MutableCachedRepository that uses a plain TreeMap as the cache and is not thread-safe.
 * It implements the Repository Java/Code methods as no-ops.
 * Any thread safe extension classes need to implement their own concurrency.
 */
public abstract class AbstractDynamicRepository extends MutableCachedRepository {
	/**
	 * Use a plain TreeMap as the cache.
	 */
	protected AbstractDynamicRepository() {
		super(new TreeMap<>());
	}

	@Override
	public @Nullable <T extends Bean> Bizlet<T> getBizlet(@Nullable Customer customer, @Nonnull Document document, boolean runtime) {
		return null;
	}

	@Override
	public @Nullable BizletMetaData getMetaDataBizlet(@Nullable Customer customer, @Nonnull Document document) {
		return null;
	}

	@Override
	public @Nullable <T extends Bean> DynamicImage<T> getDynamicImage(@Nullable Customer customer, @Nonnull Document document, @Nonnull String imageName, boolean runtime) {
		return null;
	}

	@Override
	public @Nullable <T extends Bean, C extends Bean> ComparisonModel<T, C> getComparisonModel(@Nullable Customer customer, @Nonnull Document document, @Nonnull String modelName, boolean runtime) {
		return null;
	}

	@Override
	public @Nullable <T extends Bean> MapModel<T> getMapModel(@Nullable Customer customer, @Nonnull Document document, @Nonnull String modelName, boolean runtime) {
		return null;
	}

	@Override
	public @Nullable <T extends Bean> ChartModel<T> getChartModel(@Nullable Customer customer, @Nonnull Document document, @Nonnull String modelName, boolean runtime) {
		return null;
	}

	@Override
	public @Nullable <T extends Bean> ListModel<T> getListModel(@Nullable Customer customer, @Nonnull Document document, @Nonnull String modelName, boolean runtime) {
		return null;
	}

	@Override
	public @Nullable ServerSideAction<Bean> getServerSideAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String className, boolean runtime) {
		return null;
	}

	@Override
	public @Nullable ActionMetaData getMetaDataAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String actionName) {
		return null;
	}
	
	@Override
	public @Nullable BizExportAction getBizExportAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String className, boolean runtime) {
		return null;
	}

	@Override
	public @Nullable BizImportAction getBizImportAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String className, boolean runtime) {
		return null;
	}

	@Override
	public @Nullable DownloadAction<Bean> getDownloadAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String className, boolean runtime) {
		return null;
	}

	@Override
	public @Nullable UploadAction<Bean> getUploadAction(@Nullable Customer customer, @Nonnull Document document, @Nonnull String className, boolean runtime) {
		return null;
	}

	@Override
	public void validateCustomerForGenerateDomain(@Nonnull Customer customer) {
		// nothing to do
	}

	@Override
	public void validateModuleForGenerateDomain(@Nonnull Customer customer, @Nonnull Module module) {
		// nothing to do
	}

	@Override
	public void validateDocumentForGenerateDomain(@Nonnull Customer customer, @Nonnull Document document) {
		// nothing to do
	}

	@Override
	public void validateViewForGenerateDomain(@Nonnull Customer customer, @Nonnull Document document, @Nonnull View view, @Nonnull String uxui) {
		// nothing to do
	}

	@Override
	public @Nullable Router getGlobalRouter() {
		return null;
	}

	@Override
	public @Nonnull List<Router> getModuleRouters() {
		return Collections.emptyList();
	}

	@Override
	public @Nullable String getReportFileName(@Nullable Customer customer, @Nonnull Document document, @Nonnull String reportName) {
		return null;
	}

	@Override
	public @Nullable Class<?> getJavaClass(@Nullable Customer customer, @Nonnull String fullyQualifiedJavaCodeName) {
		return null;
	}

	@Override
	public @Nullable UserImpl retrieveUser(@Nonnull String userPrincipal) {
		return null;
	}

	@Override
	public void resetMenus(@Nonnull User user) {
		// nothing to do
	}

	@Override
	public boolean populatePermissions(@Nonnull User user) {
		return false;
	}

	@Override
	public void resetUserPermissions(@Nonnull User user) {
		// nothing to do
	}
	
	@Override
	public boolean populateUser(@Nonnull User user, @Nonnull Connection connection) {
		return false;
	}

	@Override
	public @Nonnull List<UserJobSchedule> retrieveAllScheduledJobsForAllCustomers() {
		return Collections.emptyList();
	}

	@Override
	public @Nonnull List<UserJobSchedule> retrieveAllScheduledReportsForAllCustomers() {
		return Collections.emptyList();
	}

	@Override
	public @Nullable String retrievePublicUserName(@Nonnull String customerName) {
		return null;
	}
	
	@Override
	public @Nullable File findResourceFile(@Nonnull String resourcePath, @Nullable String customerName, @Nullable String moduleName) {
		return null;
	}

	@Override
	public @Nullable Object getDataFactory(@Nullable Customer customer, @Nonnull Document document) {
		return null;
	}
}
