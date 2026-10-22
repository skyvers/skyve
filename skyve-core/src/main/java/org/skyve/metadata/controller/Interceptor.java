package org.skyve.metadata.controller;

import java.util.List;

import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.web.WebContext;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Provides before/after interception hooks for every Skyve lifecycle event.
 *
 * <p>Extend this class and register the subclass in the customer XML
 * ({@code <interceptors>}) to intercept framework calls without modifying Bizlets.
 * Skyve chains all registered interceptors in declaration order.
 *
 * <p>Each {@code beforeXxx} method returns {@code boolean}. Returning {@code true}
 * short-circuits further interceptor chain processing and skips the underlying framework
 * operation. Returning {@code false} (the default) allows the chain to continue.
 *
 * <p>Threading: Interceptor instances are long-lived singletons shared across
 * all threads. Override methods must be thread-safe or must not hold mutable instance
 * state.
 *
 * @see Observer
 */
@SuppressWarnings("java:S112") // Hooks intentionally allow implementations to propagate application-specific failures.
public abstract class Interceptor implements MetaData {
	/**
	 * Executes beforeNewInstance.
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforeNewInstance(@Nonnull Bean bean)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterNewInstance.
	 * @param bean the bean
	 */
	public void afterNewInstance(@Nonnull Bean bean)
	throws Exception {
		// no-op
	}
	
	/**
	 * Executes beforeValidate.
	 * @param bean the bean
	 * @param e the e
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforeValidate(@Nonnull Bean bean, @Nonnull ValidationException e)
	throws Exception {
		return false;
	}

	/**
	 * Executes afterValidate.
	 * @param bean the bean
	 * @param e the e
	 */
	public void afterValidate(@Nonnull Bean bean, @Nonnull ValidationException e)
	throws Exception {
		// no-op
	}
	
	/**
	 * Executes beforeGetConstantDomainValues.
	 * @param attributeName the attributeName
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforeGetConstantDomainValues(@Nonnull String attributeName)
	throws Exception {
		return false;
	}

	/**
	 * Executes afterGetConstantDomainValues.
	 * @param attributeName the attributeName
	 * @param result the result
	 */
	public void afterGetConstantDomainValues(@Nonnull String attributeName, @Nullable List<DomainValue> result)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforeGetVariantDomainValues.
	 * @param attributeName the attributeName
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforeGetVariantDomainValues(@Nonnull String attributeName)
	throws Exception {
		return false;
	}

	/**
	 * Executes afterGetVariantDomainValues.
	 * @param attributeName the attributeName
	 * @param result the result
	 */
	public void afterGetVariantDomainValues(@Nonnull String attributeName, @Nullable List<DomainValue> result)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforeGetDynamicDomainValues.
	 * @param attributeName the attributeName
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforeGetDynamicDomainValues(@Nonnull String attributeName, @Nonnull Bean bean)
	throws Exception {
		return false;
	}

	/**
	 * Executes afterGetDynamicDomainValues.
	 * @param attributeName the attributeName
	 * @param bean the bean
	 * @param result the result
	 */
	public void afterGetDynamicDomainValues(@Nonnull String attributeName,
												@Nonnull Bean bean,
												@Nullable List<DomainValue> result)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforeComplete.
	 * @param attributeName the attributeName
	 * @param value the value
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforeComplete(@Nonnull String attributeName, @Nullable String value, @Nullable Bean bean)
	throws Exception {
		return false;
	}

	/**
	 * Executes afterComplete.
	 * @param attributeName the attributeName
	 * @param value the value
	 * @param bean the bean
	 * @param result the result
	 */
	public void afterComplete(@Nonnull String attributeName,
								@Nullable String value,
								@Nullable Bean bean,
								@Nullable List<String> result)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforeSave.
	 * @param document the document
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforeSave(@Nonnull Document document, @Nonnull PersistentBean bean)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterSave.
	 * @param document the document
	 * @param result the result
	 */
	public void afterSave(@Nonnull Document document, @Nonnull PersistentBean result)
	throws Exception {
		// no-op
	}
	
	/**
	 * Executes beforePreSave.
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforePreSave(@Nonnull Bean bean)
	throws Exception {
		return false;
	}

	/**
	 * Executes afterPreSave.
	 * @param bean the bean
	 */
	public void afterPreSave(@Nonnull Bean bean)
	throws Exception {
		// no-op
	}
	
	/**
	 * Executes beforePostSave.
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforePostSave(@Nonnull Bean bean)
	throws Exception {
		return false;
	}

	/**
	 * Executes afterPostSave.
	 * @param bean the bean
	 */
	public void afterPostSave(@Nonnull Bean bean)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforeDelete.
	 * @param document the document
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforeDelete(@Nonnull Document document, @Nonnull PersistentBean bean)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterDelete.
	 * @param document the document
	 * @param bean the bean
	 */
	public void afterDelete(@Nonnull Document document, @Nonnull PersistentBean bean)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforePreDelete.
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforePreDelete(@Nonnull PersistentBean bean)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterPreDelete.
	 * @param bean the bean
	 */
	public void afterPreDelete(@Nonnull PersistentBean bean)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforePostDelete.
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforePostDelete(@Nonnull PersistentBean bean)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterPostDelete.
	 * @param bean the bean
	 */
	public void afterPostDelete(@Nonnull PersistentBean bean)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforePostLoad.
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforePostLoad(@Nonnull PersistentBean bean)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterPostLoad.
	 * @param bean the bean
	 */
	public void afterPostLoad(@Nonnull PersistentBean bean)
	throws Exception {
		// no-op
	}
	
	/**
	 * Executes beforePreExecute.
	 * @param actionName the actionName
	 * @param bean the bean
	 * @param parentBean the parentBean
	 * @param webContext the webContext
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforePreExecute(@Nonnull ImplicitActionName actionName,
										@Nonnull Bean bean,
										@Nullable Bean parentBean,
										@Nonnull WebContext webContext)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterPreExecute.
	 * @param actionName the actionName
	 * @param result the result
	 * @param parentBean the parentBean
	 * @param webContext the webContext
	 */
	public void afterPreExecute(@Nonnull ImplicitActionName actionName,
									@Nonnull Bean result,
									@Nullable Bean parentBean,
									@Nonnull WebContext webContext)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforePreRerender.
	 * @param source the source
	 * @param bean the bean
	 * @param webContext the webContext
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforePreRerender(@Nonnull String source, @Nonnull Bean bean, @Nonnull WebContext webContext)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterPreRerender.
	 * @param source the source
	 * @param result the result
	 * @param webContext the webContext
	 */
	public void afterPreRerender(@Nonnull String source, @Nonnull Bean result, @Nonnull WebContext webContext)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforeServerSideAction.
	 * @param document the document
	 * @param actionName the actionName
	 * @param bean the bean
	 * @param webContext the webContext
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforeServerSideAction(@Nonnull Document document,
											@Nonnull String actionName,
											@Nonnull Bean bean,
											@Nullable WebContext webContext)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterServerSideAction.
	 * @param document the document
	 * @param actionName the actionName
	 * @param result the result
	 * @param webContext the webContext
	 */
	public void afterServerSideAction(@Nonnull Document document,
										@Nonnull String actionName,
										@Nonnull ServerSideActionResult<Bean> result,
										@Nullable WebContext webContext)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforeDownloadAction.
	 * @param document the document
	 * @param actionName the actionName
	 * @param bean the bean
	 * @param webContext the webContext
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforeDownloadAction(@Nonnull Document document,
											@Nonnull String actionName,
											@Nonnull Bean bean,
											@Nonnull WebContext webContext)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterDownloadAction.
	 * @param document the document
	 * @param actionName the actionName
	 * @param bean the bean
	 * @param download the download
	 * @param webContext the webContext
	 */
	public void afterDownloadAction(@Nonnull Document document,
										@Nonnull String actionName,
										@Nonnull Bean bean,
										@Nonnull Download download,
										@Nonnull WebContext webContext)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforeUploadAction.
	 * @param document the document
	 * @param actionName the actionName
	 * @param bean the bean
	 * @param upload the upload
	 * @param webContext the webContext
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforeUploadAction(@Nonnull Document document,
										@Nonnull String actionName,
										@Nonnull Bean bean,
										@Nonnull Upload upload,
										@Nonnull WebContext webContext)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterUploadAction.
	 * @param document the document
	 * @param actionName the actionName
	 * @param bean the bean
	 * @param upload the upload
	 * @param webContext the webContext
	 */
	public void afterUploadAction(@Nonnull Document document,
									@Nonnull String actionName,
									@Nonnull Bean bean,
									@Nonnull Upload upload,
									@Nonnull WebContext webContext)
	throws Exception {
		// no-op
	}
	
	/**
	 * Executes beforeBizImportAction.
	 * @param document the document
	 * @param actionName the actionName
	 * @param bizPortable the bizPortable
	 * @param problems the problems
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforeBizImportAction(@Nonnull Document document,
											@Nonnull String actionName,
											@Nonnull BizPortWorkbook bizPortable,
											@Nonnull UploadException problems)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterBizImportAction.
	 * @param document the document
	 * @param actionName the actionName
	 * @param bizPortable the bizPortable
	 * @param problems the problems
	 */
	public void afterBizImportAction(@Nonnull Document document,
										@Nonnull String actionName,
										@Nonnull BizPortWorkbook bizPortable,
										@Nonnull UploadException problems)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforeBizExportAction.
	 * @param document the document
	 * @param actionName the actionName
	 * @param webContext the webContext
	 * @return the result
	 */
	@SuppressWarnings("static-method")
	public boolean beforeBizExportAction(@Nonnull Document document,
											@Nonnull String actionName,
											@Nonnull WebContext webContext)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterBizExportAction.
	 * @param document the document
	 * @param actionName the actionName
	 * @param result the result
	 * @param webContext the webContext
	 */
	public void afterBizExportAction(@Nonnull Document document,
										@Nonnull String actionName,
										@Nonnull BizPortWorkbook result,
										@Nonnull WebContext webContext)
	throws Exception {
		// no-op
	}
	
	/**
	 * Note that this method should not throw any exceptions and should always succeed to enable correct state management after the render response.
	 */
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforePostRender(@Nonnull Bean bean, @Nonnull WebContext webContext) {
		return false;
	}
	
	/**
	 * Note that this method should not throw any exceptions and should always succeed to enable correct state management after the render response.
	 */
	@SuppressWarnings("unused")
	public void afterPostRender(@Nonnull Bean result, @Nonnull WebContext webContext) {
		// no-op
	}
}
