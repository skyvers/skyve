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
public abstract class Interceptor implements MetaData {
	/**
	 * Executes beforeNewInstance.
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforeNewInstance(Bean bean)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterNewInstance.
	 * @param bean the bean
	 */
	@SuppressWarnings("unused")
	public void afterNewInstance(Bean bean)
	throws Exception {
		// no-op
	}
	
	/**
	 * Executes beforeValidate.
	 * @param bean the bean
	 * @param e the e
	 * @return the result
	 */
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforeValidate(Bean bean, ValidationException e)
	throws Exception {
		return false;
	}

	/**
	 * Executes afterValidate.
	 * @param bean the bean
	 * @param e the e
	 */
	@SuppressWarnings("unused")
	public void afterValidate(Bean bean, ValidationException e)
	throws Exception {
		// no-op
	}
	
	/**
	 * Executes beforeGetConstantDomainValues.
	 * @param attributeName the attributeName
	 * @return the result
	 */
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforeGetConstantDomainValues(String attributeName)
	throws Exception {
		return false;
	}

	/**
	 * Executes afterGetConstantDomainValues.
	 * @param attributeName the attributeName
	 * @param result the result
	 */
	@SuppressWarnings("unused")
	public void afterGetConstantDomainValues(String attributeName, List<DomainValue> result)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforeGetVariantDomainValues.
	 * @param attributeName the attributeName
	 * @return the result
	 */
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforeGetVariantDomainValues(String attributeName)
	throws Exception {
		return false;
	}

	/**
	 * Executes afterGetVariantDomainValues.
	 * @param attributeName the attributeName
	 * @param result the result
	 */
	@SuppressWarnings("unused")
	public void afterGetVariantDomainValues(String attributeName, List<DomainValue> result)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforeGetDynamicDomainValues.
	 * @param attributeName the attributeName
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforeGetDynamicDomainValues(String attributeName, Bean bean)
	throws Exception {
		return false;
	}

	/**
	 * Executes afterGetDynamicDomainValues.
	 * @param attributeName the attributeName
	 * @param bean the bean
	 * @param result the result
	 */
	@SuppressWarnings("unused")
	public void afterGetDynamicDomainValues(String attributeName, Bean bean, List<DomainValue> result)
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
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforeComplete(String attributeName, String value, Bean bean)
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
	@SuppressWarnings("unused")
	public void afterComplete(String attributeName, String value, Bean bean, List<String> result)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforeSave.
	 * @param document the document
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforeSave(Document document, PersistentBean bean)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterSave.
	 * @param document the document
	 * @param result the result
	 */
	@SuppressWarnings("unused")
	public void afterSave(Document document, PersistentBean result)
	throws Exception {
		// no-op
	}
	
	/**
	 * Executes beforePreSave.
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforePreSave(Bean bean)
	throws Exception {
		return false;
	}

	/**
	 * Executes afterPreSave.
	 * @param bean the bean
	 */
	@SuppressWarnings("unused")
	public void afterPreSave(Bean bean)
	throws Exception {
		// no-op
	}
	
	/**
	 * Executes beforePostSave.
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforePostSave(Bean bean)
	throws Exception {
		return false;
	}

	/**
	 * Executes afterPostSave.
	 * @param bean the bean
	 */
	@SuppressWarnings("unused")
	public void afterPostSave(Bean bean)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforeDelete.
	 * @param document the document
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforeDelete(Document document, PersistentBean bean)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterDelete.
	 * @param document the document
	 * @param bean the bean
	 */
	@SuppressWarnings("unused")
	public void afterDelete(Document document, PersistentBean bean)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforePreDelete.
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforePreDelete(PersistentBean bean)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterPreDelete.
	 * @param bean the bean
	 */
	@SuppressWarnings("unused")
	public void afterPreDelete(PersistentBean bean)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforePostDelete.
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforePostDelete(PersistentBean bean)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterPostDelete.
	 * @param bean the bean
	 */
	@SuppressWarnings("unused")
	public void afterPostDelete(PersistentBean bean)
	throws Exception {
		// no-op
	}

	/**
	 * Executes beforePostLoad.
	 * @param bean the bean
	 * @return the result
	 */
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforePostLoad(PersistentBean bean)
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterPostLoad.
	 * @param bean the bean
	 */
	@SuppressWarnings("unused")
	public void afterPostLoad(PersistentBean bean)
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
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforePreExecute(ImplicitActionName actionName, Bean bean, Bean parentBean, WebContext webContext) 
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
	@SuppressWarnings("unused")
	public void afterPreExecute(ImplicitActionName actionName, Bean result, Bean parentBean, WebContext webContext)
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
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforePreRerender(String source, Bean bean, WebContext webContext) 
	throws Exception {
		return false;
	}
	
	/**
	 * Executes afterPreRerender.
	 * @param source the source
	 * @param result the result
	 * @param webContext the webContext
	 */
	@SuppressWarnings("unused")
	public void afterPreRerender(String source, Bean result, WebContext webContext)
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
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforeServerSideAction(Document document, 
											String actionName, 
											Bean bean, 
											WebContext webContext)
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
	@SuppressWarnings("unused")
	public void afterServerSideAction(Document document, 
										String actionName, 
										ServerSideActionResult<Bean> result, 
										WebContext webContext)
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
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforeDownloadAction(Document document, 
											String actionName,
											Bean bean, 
											WebContext webContext)
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
	@SuppressWarnings("unused")
	public void afterDownloadAction(Document document,
										String actionName,
										Bean bean, 
										Download download, 
										WebContext webContext)
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
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforeUploadAction(Document document, 
										String actionName,
										Bean bean, 
										Upload upload, 
										WebContext webContext)
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
	@SuppressWarnings("unused")
	public void afterUploadAction(Document document,
									String actionName,
									Bean bean, 
									Upload upload, 
									WebContext webContext)
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
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforeBizImportAction(Document document, 
											String actionName,
											BizPortWorkbook bizPortable, 
											UploadException problems)
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
	@SuppressWarnings("unused")
	public void afterBizImportAction(Document document,
										String actionName,
										BizPortWorkbook bizPortable,
										UploadException problems)
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
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforeBizExportAction(Document document,
											String actionName,
											WebContext webContext)
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
	@SuppressWarnings("unused")
	public void afterBizExportAction(Document document,
										String actionName,
										BizPortWorkbook result,
										WebContext webContext)
	throws Exception {
		// no-op
	}
	
	/**
	 * Note that this method should not throw any exceptions and should always succeed to enable correct state management after the render response.
	 */
	@SuppressWarnings({"unused", "static-method"})
	public boolean beforePostRender(Bean bean, WebContext webContext) {
		return false;
	}
	
	/**
	 * Note that this method should not throw any exceptions and should always succeed to enable correct state management after the render response.
	 */
	@SuppressWarnings("unused")
	public void afterPostRender(Bean result, WebContext webContext) {
		// no-op
	}
}
