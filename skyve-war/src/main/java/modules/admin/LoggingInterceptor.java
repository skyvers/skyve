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

/**
 * Simple {@link Interceptor} implementation that logs each lifecycle
 * callback. The {@code veto} flag can be toggled to prevent the operation
 * from completing.
 */
public class LoggingInterceptor extends Interceptor {

    private static final Logger LOGGER = LoggerFactory.getLogger(LoggingInterceptor.class);

    /**
     * Indicates whether intercepted operations should be vetoed.
     */
    private boolean veto = false;

        /**
         * Called before a new bean instance is created.
         * Logs the invocation and returns the {@code veto} flag.
         */
        @Override
        public boolean beforeNewInstance(Bean bean) {
                LOGGER.info("beforeNewInstance - {}", bean);
                return veto;
        }

        /**
         * Invoked after a new bean instance has been created.
         * Only logs the event.
         */
        @Override
        public void afterNewInstance(Bean bean) {
                LOGGER.info("afterNewInstance - {}", bean);
        }

        /**
         * Executed prior to validation.
         * Logs the bean being validated and returns the veto flag.
         */
        @Override
        public boolean beforeValidate(Bean bean, ValidationException e) {
                LOGGER.info("beforeValidate - {}", bean);
                return veto;
        }

        /**
         * Called after validation has completed.
         * Logs the bean instance.
         */
        @Override
        public void afterValidate(Bean bean, ValidationException e) {
                LOGGER.info("afterValidate - {}", bean);
        }

        /**
         * Invoked before constant domain values are retrieved.
         * @param attributeName the attribute name
         * @return {@code true} to veto value retrieval
         */
        @Override
        public boolean beforeGetConstantDomainValues(String attributeName) {
                LOGGER.info("beforeGetConstantDomainValues - attribute = {}", attributeName);
                return veto;
        }

        /**
         * Called after constant domain values have been retrieved.
         */
        @Override
        public void afterGetConstantDomainValues(String attributeName, List<DomainValue> result) {
                LOGGER.info("afterGetConstantDomainValues - attribute = {}", attributeName);
        }

        /**
         * Triggered before variant domain values are looked up.
         * @param attributeName the attribute name
         * @return {@code true} to stop retrieval
         */
        @Override
        public boolean beforeGetVariantDomainValues(String attributeName) {
                LOGGER.info("beforeGetVariantDomainValues - attribute = {}", attributeName);
                return veto;
        }

        /**
         * Logs completion of variant domain value retrieval.
         */
        @Override
        public void afterGetVariantDomainValues(String attributeName, List<DomainValue> result) {
                LOGGER.info("afterGetVariantDomainValues - attribute = {}", attributeName);
        }

        /**
         * Called before dynamic domain values are calculated.
         * @param attributeName the attribute requiring values
         * @param bean          the context bean
         * @return {@code true} to veto the call
         */
        @Override
        public boolean beforeGetDynamicDomainValues(String attributeName, Bean bean) {
                LOGGER.info("beforeGetDynamicDomainValues - attribute = {}", attributeName);
                return veto;
        }

        /**
         * Logs completion of dynamic domain value evaluation.
         */
        @Override
        public void afterGetDynamicDomainValues(String attributeName, Bean bean, List<DomainValue> result) {
                LOGGER.info("afterGetDynamicDomainValues - attribute = {}", attributeName);
        }

        /**
         * Before a bean is persisted this method logs the intent.
         *
         * @return {@code true} to veto the save
         */
        @Override
        public boolean beforeSave(Document document, PersistentBean bean) throws Exception {
                LOGGER.info("beforeSave - bean = {}", bean);
                return veto;
        }

        /**
         * Logs the bean returned from a save operation.
         */
        @Override
        public void afterSave(Document document, final PersistentBean result) throws Exception {
                LOGGER.info("afterSave - result = {}", result);
        }

        /**
         * Called before the pre-save lifecycle event.
         * @return {@code true} to veto further processing
         */
        @Override
        public boolean beforePreSave(Bean bean) {
                LOGGER.info("beforePreSave - {}", bean);
                return veto;
        }

        /**
         * Logs completion of the pre-save event.
         */
        @Override
        public void afterPreSave(Bean bean) {
                LOGGER.info("afterPreSave - {}", bean);
        }

        /**
         * Invoked before the post-save hook.
         * @return {@code true} to veto the hook
         */
        @Override
        public boolean beforePostSave(Bean bean) {
                LOGGER.info("beforePostSave - {}", bean);
                return veto;
        }

        /**
         * Logs completion of the post-save hook.
         */
        @Override
        public void afterPostSave(Bean bean) {
                LOGGER.info("afterPostSave - {}", bean);
        }

        /**
         * Called before a bean is deleted.
         *
         * @return {@code true} to veto deletion
         */
        @Override
        public boolean beforeDelete(Document document, PersistentBean bean) {
                LOGGER.info("beforeDelete - {}", bean);
                return veto;
        }

        /**
         * Logs after a bean has been deleted.
         */
        @Override
        public void afterDelete(Document document, PersistentBean bean) {
                LOGGER.info("afterDelete - {}", bean);
        }

        /**
         * Invoked before the pre-delete hook.
         * @return {@code true} to veto deletion
         */
        @Override
        public boolean beforePreDelete(PersistentBean bean) {
                LOGGER.info("beforePreDelete - {}", bean);
                return veto;
        }

        /**
         * Logs after the pre-delete hook has run.
         */
        @Override
        public void afterPreDelete(PersistentBean bean) {
                LOGGER.info("afterPreDelete - {}", bean);
        }

        /**
         * Called before the post-load event.
         * @return {@code true} to veto further processing
         */
        @Override
        public boolean beforePostLoad(PersistentBean bean) {
                LOGGER.info("beforePostLoad - {}", bean);
                return veto;
        }

        /**
         * Logs when a bean has been loaded.
         */
        @Override
        public void afterPostLoad(PersistentBean bean) {
                LOGGER.info("afterPostLoad - {}", bean);
        }

        /**
         * Logs before an implicit action executes.
         * @return {@code true} to veto execution
         */
        @Override
        public boolean beforePreExecute(ImplicitActionName actionName, Bean bean, Bean parentBean, WebContext webContext) {
                LOGGER.info("beforePreExecute - action = {}, bean = {}, parent = {}", actionName, bean, parentBean);
                return veto;
        }

        /**
         * Logs completion of an implicit action.
         */
        @Override
        public void afterPreExecute(ImplicitActionName actionName, Bean result, Bean parentBean, WebContext webContext) {
                LOGGER.info("afterPreExecute - action = {}, bean = {}, parent = {}", actionName, result, parentBean);
        }

        /**
         * Executed prior to a server side action.
         * @return {@code true} to veto the action
         */
        @Override
        public boolean beforeServerSideAction(Document document, String actionName, Bean bean, WebContext webContext) {
                LOGGER.info("beforeServerSideAction - doc = {}.{}, action = {}, bean = {}", document.getOwningModuleName(), document.getName(), actionName, bean);
                return veto;
        }

        /**
         * Logs the outcome of a server side action.
         */
        @Override
        public void afterServerSideAction(Document document, String actionName, ServerSideActionResult<Bean> result, WebContext webContext) {
                LOGGER.info("afterServerSideAction - doc = {}.{}, action = {}, result = {}", document.getOwningModuleName(), document.getName(), actionName, result);
        }

        /**
         * Called before an upload action is processed.
         * @return {@code true} to veto the action
         */
        @Override
        public boolean beforeUploadAction(Document document, String actionName, Bean bean, Upload upload, WebContext webContext) {
                LOGGER.info("beforeUploadAction - doc = {}.{}, action = {}, bean = {}, file = {}", document.getOwningModuleName(), document.getName(), actionName, bean, upload);
                return veto;
        }

        /**
         * Logs the uploaded file after the upload action has finished.
         */
        @Override
        public void afterUploadAction(Document document, String actionName, Bean bean, Upload upload, WebContext webContext) {
                LOGGER.info("afterUploadAction - doc = {}.{}, action = {}, bean = {}, file = {}", document.getOwningModuleName(), document.getName(), actionName, bean, upload);
        }

        /**
         * Executed prior to a BizPort import action.
         * @return {@code true} to veto the import
         */
        @Override
        public boolean beforeBizImportAction(Document document, String actionName, BizPortWorkbook bizPortable, UploadException problems) {
                LOGGER.info("beforeBizImportAction - doc = {}.{}, action = {}, bean = {}, workbook = {}", document.getOwningModuleName(), document.getName(), actionName, bizPortable);
                return veto;
        }

        /**
         * Logs completion of a BizPort import action.
         */
        @Override
        public void afterBizImportAction(Document document, String actionName, BizPortWorkbook bizPortable, UploadException problems) {
                LOGGER.info("afterBizImportAction - doc = {}.{}, action = {}, bean = {}, workbook = {}", document.getOwningModuleName(), document.getName(), actionName, bizPortable);
        }

        /**
         * Invoked before a BizPort export action is executed.
         * @return {@code true} to veto the export
         */
        @Override
        public boolean beforeBizExportAction(Document document, String actionName, WebContext webContext) {
                LOGGER.info("beforeBizExportAction - doc = {}.{}, action = {}", document.getOwningModuleName(), document.getName(), actionName);
                return veto;
        }

        /**
         * Logs completion of a BizPort export action.
         */
        @Override
        public void afterBizExportAction(Document document, String actionName, BizPortWorkbook result, WebContext webContext) {
                LOGGER.info("beforeBizExportAction - doc = {}.{}, action = {}, result = {}", document.getOwningModuleName(), document.getName(), actionName, result);
        }
}
