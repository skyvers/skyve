package modules.admin;

import java.util.List;
import java.util.logging.Level;

import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.controller.Interceptor;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

public class LoggingInterceptor extends Interceptor {
	private boolean veto = false;
	
	@Override
	public boolean beforeNewInstance(Bean bean) {
		Util.LOGGER.log(Level.INFO, "beforeNewInstance - {0}", bean);
		return veto;
	}

	@Override
	public void afterNewInstance(Bean bean) {
		Util.LOGGER.log(Level.INFO, "afterNewInstance - {0}", bean);
	}

	@Override
	public boolean beforeValidate(Bean bean, ValidationException e) {
		Util.LOGGER.log(Level.INFO, "beforeValidate - {0}", bean);
		return veto;
	}

	@Override
	public void afterValidate(Bean bean, ValidationException e) {
		Util.LOGGER.log(Level.INFO, "afterValidate - {0}", bean);
	}

	@Override
	public boolean beforeGetConstantDomainValues(String attributeName) {
		Util.LOGGER.log(Level.INFO, "beforeGetConstantDomainValues - attribute = {0}", attributeName);
		return veto;
	}

	@Override
	public void afterGetConstantDomainValues(String attributeName, List<DomainValue> result) {
		Util.LOGGER.log(Level.INFO, "afterGetConstantDomainValues - attribute = {0}", attributeName);
	}

	@Override
	public boolean beforeGetVariantDomainValues(String attributeName) {
		Util.LOGGER.log(Level.INFO, "beforeGetVariantDomainValues - attribute = {0}", attributeName);
		return veto;
	}

	@Override
	public void afterGetVariantDomainValues(String attributeName, List<DomainValue> result) {
		Util.LOGGER.log(Level.INFO, "afterGetVariantDomainValues - attribute = {0}", attributeName);
	}

	@Override
	public boolean beforeGetDynamicDomainValues(String attributeName, Bean bean) {
		Util.LOGGER.log(Level.INFO, "beforeGetDynamicDomainValues - attribute = {0}", attributeName);
		return veto;
	}

	@Override
	public void afterGetDynamicDomainValues(String attributeName, Bean bean, List<DomainValue> result) {
		Util.LOGGER.log(Level.INFO, "afterGetDynamicDomainValues - attribute = {0}", attributeName);
	}

	@Override
	public boolean beforeSave(Document document, PersistentBean bean) throws Exception {
		Util.LOGGER.log(Level.INFO, "beforeSave - bean = {0}", bean);
		return veto;
	}

	@Override
	public void afterSave(Document document, final PersistentBean result) throws Exception {
		Util.LOGGER.log(Level.INFO, "afterSave - result = {0}", result);
	}

	@Override
	public boolean beforePreSave(Bean bean) {
		Util.LOGGER.log(Level.INFO, "beforePreSave - {0}", bean);
		return veto;
	}

	@Override
	public void afterPreSave(Bean bean) {
		Util.LOGGER.log(Level.INFO, "afterPreSave - {0}", bean);
	}

	@Override
	public boolean beforePostSave(Bean bean) {
		Util.LOGGER.log(Level.INFO, "beforePostSave - {0}", bean);
		return veto;
	}

	@Override
	public void afterPostSave(Bean bean) {
		Util.LOGGER.log(Level.INFO, "afterPostSave - {0}", bean);
	}

	@Override
	public boolean beforeDelete(Document document, PersistentBean bean) {
		Util.LOGGER.log(Level.INFO, "beforeDelete - {0}", bean);
		return veto;
	}

	@Override
	public void afterDelete(Document document, PersistentBean bean) {
		Util.LOGGER.log(Level.INFO, "afterDelete - {0}", bean);
	}

	@Override
	public boolean beforePreDelete(PersistentBean bean) {
		Util.LOGGER.log(Level.INFO, "beforePreDelete - {0}", bean);
		return veto;
	}

	@Override
	public void afterPreDelete(PersistentBean bean) {
		Util.LOGGER.log(Level.INFO, "afterPreDelete - {0}", bean);
	}

	@Override
	public boolean beforePostLoad(PersistentBean bean) {
		Util.LOGGER.log(Level.INFO, "beforePostLoad - {0}", bean);
		return veto;
	}

	@Override
	public void afterPostLoad(PersistentBean bean) {
		Util.LOGGER.log(Level.INFO, "afterPostLoad - {0}", bean);
	}

	@Override
	public boolean beforePreExecute(ImplicitActionName actionName, Bean bean, Bean parentBean, WebContext webContext) {
		Util.LOGGER.log(Level.INFO, 
							"beforePreExecute - action = {0}, bean = {1}, parent = {2}",
							new Object[] {actionName, bean, parentBean});
		return veto;
	}

	@Override
	public void afterPreExecute(ImplicitActionName actionName, Bean result, Bean parentBean, WebContext webContext) {
		Util.LOGGER.log(Level.INFO, 
							"afterPreExecute - action = {0}, bean = {1}, parent = {2}",
							new Object[] {actionName, result, parentBean});
	}

	@Override
	public boolean beforeServerSideAction(Document document, String actionName, Bean bean, WebContext webContext) {
		Util.LOGGER.log(Level.INFO, 
							"beforeServerSideAction - doc = {0}.{1}, action = {2}, bean = {3}",
							new Object[] {document.getOwningModuleName(), document.getName(), actionName, bean});
		return veto;
	}

	@Override
	public void afterServerSideAction(Document document, String actionName, ServerSideActionResult<Bean> result, WebContext webContext) {
		Util.LOGGER.log(Level.INFO, 
							"afterServerSideAction - doc = {0}.{1}, action = {2}, result = {3}",
							new Object[] {document.getOwningModuleName(), document.getName(), actionName, result});
	}

	@Override
	public boolean beforeUploadAction(Document document, String actionName, Bean bean, Upload upload, WebContext webContext) {
		Util.LOGGER.log(Level.INFO, 
							"beforeUploadAction - doc = {0}.{1}, action = {2}, bean = {3}, file = {4}",
							new Object[] {document.getOwningModuleName(), document.getName(), actionName, bean, upload});
		return veto;
	}

	@Override
	public void afterUploadAction(Document document, String actionName, Bean bean, Upload upload, WebContext webContext) {
		Util.LOGGER.log(Level.INFO, 
							"afterUploadAction - doc = {0}.{1}, action = {2}, bean = {3}, file = {4}",
							new Object[] {document.getOwningModuleName(), document.getName(), actionName, bean, upload});
	}

	@Override
	public boolean beforeBizImportAction(Document document, String actionName, BizPortWorkbook bizPortable, UploadException problems) {
		Util.LOGGER.log(Level.INFO, 
							"beforeBizImportAction - doc = {0}.{1}, action = {2}, bean = {3}, workbook = {4}",
							new Object[] {document.getOwningModuleName(), document.getName(), actionName, bizPortable});
		return veto;
	}

	@Override
	public void afterBizImportAction(Document document, String actionName, BizPortWorkbook bizPortable, UploadException problems) {
		Util.LOGGER.log(Level.INFO, 
							"afterBizImportAction - doc = {0}.{1}, action = {2}, bean = {3}, workbook = {4}",
							new Object[] {document.getOwningModuleName(), document.getName(), actionName, bizPortable});
	}

	@Override
	public boolean beforeBizExportAction(Document document, String actionName, WebContext webContext) {
		Util.LOGGER.log(Level.INFO, 
							"beforeBizExportAction - doc = {0}.{1}, action = {2}",
							new Object[] {document.getOwningModuleName(), document.getName(), actionName});
		return veto;
	}

	@Override
	public void afterBizExportAction(Document document, String actionName, BizPortWorkbook result, WebContext webContext) {
		Util.LOGGER.log(Level.INFO, 
							"beforeBizExportAction - doc = {0}.{1}, action = {2}, result = {3}",
							new Object[] {document.getOwningModuleName(), document.getName(), actionName, result});
	}
}
