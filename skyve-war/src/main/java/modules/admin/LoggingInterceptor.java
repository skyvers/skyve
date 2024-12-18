package modules.admin;

import java.util.List;

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
import org.skyve.web.WebContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggingInterceptor extends Interceptor {

    private static final Logger LOGGER = LoggerFactory.getLogger(LoggingInterceptor.class);

    private boolean veto = false;

	@Override
	public boolean beforeNewInstance(Bean bean) {
		LOGGER.info("beforeNewInstance - {}", bean);
		return veto;
	}

	@Override
	public void afterNewInstance(Bean bean) {
		LOGGER.info("afterNewInstance - {}", bean);
	}

	@Override
	public boolean beforeValidate(Bean bean, ValidationException e) {
		LOGGER.info("beforeValidate - {}", bean);
		return veto;
	}

	@Override
	public void afterValidate(Bean bean, ValidationException e) {
		LOGGER.info("afterValidate - {}", bean);
	}

	@Override
	public boolean beforeGetConstantDomainValues(String attributeName) {
		LOGGER.info("beforeGetConstantDomainValues - attribute = {}", attributeName);
		return veto;
	}

	@Override
	public void afterGetConstantDomainValues(String attributeName, List<DomainValue> result) {
		LOGGER.info("afterGetConstantDomainValues - attribute = {}", attributeName);
	}

	@Override
	public boolean beforeGetVariantDomainValues(String attributeName) {
		LOGGER.info("beforeGetVariantDomainValues - attribute = {}", attributeName);
		return veto;
	}

	@Override
	public void afterGetVariantDomainValues(String attributeName, List<DomainValue> result) {
		LOGGER.info("afterGetVariantDomainValues - attribute = {}", attributeName);
	}

	@Override
	public boolean beforeGetDynamicDomainValues(String attributeName, Bean bean) {
		LOGGER.info("beforeGetDynamicDomainValues - attribute = {}", attributeName);
		return veto;
	}

	@Override
	public void afterGetDynamicDomainValues(String attributeName, Bean bean, List<DomainValue> result) {
		LOGGER.info("afterGetDynamicDomainValues - attribute = {}", attributeName);
	}

	@Override
	public boolean beforeSave(Document document, PersistentBean bean) throws Exception {
		LOGGER.info("beforeSave - bean = {}", bean);
		return veto;
	}

	@Override
	public void afterSave(Document document, final PersistentBean result) throws Exception {
		LOGGER.info("afterSave - result = {}", result);
	}

	@Override
	public boolean beforePreSave(Bean bean) {
		LOGGER.info("beforePreSave - {}", bean);
		return veto;
	}

	@Override
	public void afterPreSave(Bean bean) {
		LOGGER.info("afterPreSave - {}", bean);
	}

	@Override
	public boolean beforePostSave(Bean bean) {
		LOGGER.info("beforePostSave - {}", bean);
		return veto;
	}

	@Override
	public void afterPostSave(Bean bean) {
		LOGGER.info("afterPostSave - {}", bean);
	}

	@Override
	public boolean beforeDelete(Document document, PersistentBean bean) {
		LOGGER.info("beforeDelete - {}", bean);
		return veto;
	}

	@Override
	public void afterDelete(Document document, PersistentBean bean) {
		LOGGER.info("afterDelete - {}", bean);
	}

	@Override
	public boolean beforePreDelete(PersistentBean bean) {
		LOGGER.info("beforePreDelete - {}", bean);
		return veto;
	}

	@Override
	public void afterPreDelete(PersistentBean bean) {
		LOGGER.info("afterPreDelete - {}", bean);
	}

	@Override
	public boolean beforePostLoad(PersistentBean bean) {
		LOGGER.info("beforePostLoad - {}", bean);
		return veto;
	}

	@Override
	public void afterPostLoad(PersistentBean bean) {
		LOGGER.info("afterPostLoad - {}", bean);
	}

	@Override
	public boolean beforePreExecute(ImplicitActionName actionName, Bean bean, Bean parentBean, WebContext webContext) {
		LOGGER.info("beforePreExecute - action = {}, bean = {}, parent = {}", actionName, bean, parentBean);
		return veto;
	}

	@Override
	public void afterPreExecute(ImplicitActionName actionName, Bean result, Bean parentBean, WebContext webContext) {
		LOGGER.info("afterPreExecute - action = {}, bean = {}, parent = {}", actionName, result, parentBean);
	}

	@Override
	public boolean beforeServerSideAction(Document document, String actionName, Bean bean, WebContext webContext) {
		LOGGER.info("beforeServerSideAction - doc = {}.{}, action = {}, bean = {}", document.getOwningModuleName(), document.getName(), actionName, bean);
		return veto;
	}

	@Override
	public void afterServerSideAction(Document document, String actionName, ServerSideActionResult<Bean> result, WebContext webContext) {
		LOGGER.info("afterServerSideAction - doc = {}.{}, action = {}, result = {}", document.getOwningModuleName(), document.getName(), actionName, result);
	}

	@Override
	public boolean beforeUploadAction(Document document, String actionName, Bean bean, Upload upload, WebContext webContext) {
		LOGGER.info("beforeUploadAction - doc = {}.{}, action = {}, bean = {}, file = {}", document.getOwningModuleName(), document.getName(), actionName, bean, upload);
		return veto;
	}

	@Override
	public void afterUploadAction(Document document, String actionName, Bean bean, Upload upload, WebContext webContext) {
		LOGGER.info("afterUploadAction - doc = {}.{}, action = {}, bean = {}, file = {}", document.getOwningModuleName(), document.getName(), actionName, bean, upload);
	}

	@Override
	public boolean beforeBizImportAction(Document document, String actionName, BizPortWorkbook bizPortable, UploadException problems) {
		LOGGER.info("beforeBizImportAction - doc = {}.{}, action = {}, bean = {}, workbook = {}", document.getOwningModuleName(), document.getName(), actionName, bizPortable);
		return veto;
	}

	@Override
	public void afterBizImportAction(Document document, String actionName, BizPortWorkbook bizPortable, UploadException problems) {
		LOGGER.info("afterBizImportAction - doc = {}.{}, action = {}, bean = {}, workbook = {}", document.getOwningModuleName(), document.getName(), actionName, bizPortable);
	}

	@Override
	public boolean beforeBizExportAction(Document document, String actionName, WebContext webContext) {
		LOGGER.info("beforeBizExportAction - doc = {}.{}, action = {}", document.getOwningModuleName(), document.getName(), actionName);
		return veto;
	}

	@Override
	public void afterBizExportAction(Document document, String actionName, BizPortWorkbook result, WebContext webContext) {
		LOGGER.info("beforeBizExportAction - doc = {}.{}, action = {}, result = {}", document.getOwningModuleName(), document.getName(), actionName, result);
	}
}
