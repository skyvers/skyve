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

        @Override
        /**
         * Called before a new bean instance is created.
         * Logs the invocation and returns the {@code veto} flag.
         */
        public boolean beforeNewInstance(Bean bean) {
                LOGGER.info("beforeNewInstance - {}", bean);
                return veto;
        }

        @Override
        /**
         * Invoked after a new bean instance has been created.
         * Only logs the event.
         */
        public void afterNewInstance(Bean bean) {
                LOGGER.info("afterNewInstance - {}", bean);
        }

        @Override
        /**
         * Executed prior to validation.
         * Logs the bean being validated and returns the veto flag.
         */
        public boolean beforeValidate(Bean bean, ValidationException e) {
                LOGGER.info("beforeValidate - {}", bean);
                return veto;
        }

        @Override
        /**
         * Called after validation has completed.
         * Logs the bean instance.
         */
        public void afterValidate(Bean bean, ValidationException e) {
                LOGGER.info("afterValidate - {}", bean);
        }

        @Override
        /**
         * Invoked before constant domain values are retrieved.
         * @param attributeName the attribute name
         * @return {@code true} to veto value retrieval
         */
        public boolean beforeGetConstantDomainValues(String attributeName) {
                LOGGER.info("beforeGetConstantDomainValues - attribute = {}", attributeName);
                return veto;
        }

        @Override
        /**
         * Called after constant domain values have been retrieved.
         */
        public void afterGetConstantDomainValues(String attributeName, List<DomainValue> result) {
                LOGGER.info("afterGetConstantDomainValues - attribute = {}", attributeName);
        }

        @Override
        /**
         * Triggered before variant domain values are looked up.
         * @param attributeName the attribute name
         * @return {@code true} to stop retrieval
         */
        public boolean beforeGetVariantDomainValues(String attributeName) {
                LOGGER.info("beforeGetVariantDomainValues - attribute = {}", attributeName);
                return veto;
        }

        @Override
        /**
         * Logs completion of variant domain value retrieval.
         */
        public void afterGetVariantDomainValues(String attributeName, List<DomainValue> result) {
                LOGGER.info("afterGetVariantDomainValues - attribute = {}", attributeName);
        }

        @Override
        /**
         * Called before dynamic domain values are calculated.
         * @param attributeName the attribute requiring values
         * @param bean          the context bean
         * @return {@code true} to veto the call
         */
        public boolean beforeGetDynamicDomainValues(String attributeName, Bean bean) {
                LOGGER.info("beforeGetDynamicDomainValues - attribute = {}", attributeName);
                return veto;
        }

        @Override
        /**
         * Logs completion of dynamic domain value evaluation.
         */
        public void afterGetDynamicDomainValues(String attributeName, Bean bean, List<DomainValue> result) {
                LOGGER.info("afterGetDynamicDomainValues - attribute = {}", attributeName);
        }

        @Override
        /**
         * Before a bean is persisted this method logs the intent.
         *
         * @return {@code true} to veto the save
         */
        public boolean beforeSave(Document document, PersistentBean bean) throws Exception {
                LOGGER.info("beforeSave - bean = {}", bean);
                return veto;
        }

        @Override
        /**
         * Logs the bean returned from a save operation.
         */
        public void afterSave(Document document, final PersistentBean result) throws Exception {
                LOGGER.info("afterSave - result = {}", result);
        }

        @Override
        /**
         * Called before the pre-save lifecycle event.
         * @return {@code true} to veto further processing
         */
        public boolean beforePreSave(Bean bean) {
                LOGGER.info("beforePreSave - {}", bean);
                return veto;
        }

        @Override
        /**
         * Logs completion of the pre-save event.
         */
        public void afterPreSave(Bean bean) {
                LOGGER.info("afterPreSave - {}", bean);
        }

        @Override
        /**
         * Invoked before the post-save hook.
         * @return {@code true} to veto the hook
         */
        public boolean beforePostSave(Bean bean) {
                LOGGER.info("beforePostSave - {}", bean);
                return veto;
        }

        @Override
        /**
         * Logs completion of the post-save hook.
         */
        public void afterPostSave(Bean bean) {
                LOGGER.info("afterPostSave - {}", bean);
        }

        @Override
        /**
         * Called before a bean is deleted.
         *
         * @return {@code true} to veto deletion
         */
        public boolean beforeDelete(Document document, PersistentBean bean) {
                LOGGER.info("beforeDelete - {}", bean);
                return veto;
        }

        @Override
        /**
         * Logs after a bean has been deleted.
         */
        public void afterDelete(Document document, PersistentBean bean) {
                LOGGER.info("afterDelete - {}", bean);
        }

        @Override
        /**
         * Invoked before the pre-delete hook.
         * @return {@code true} to veto deletion
         */
        public boolean beforePreDelete(PersistentBean bean) {
                LOGGER.info("beforePreDelete - {}", bean);
                return veto;
        }

        @Override
        /**
         * Logs after the pre-delete hook has run.
         */
        public void afterPreDelete(PersistentBean bean) {
                LOGGER.info("afterPreDelete - {}", bean);
        }

        @Override
        /**
         * Called before the post-load event.
         * @return {@code true} to veto further processing
         */
        public boolean beforePostLoad(PersistentBean bean) {
                LOGGER.info("beforePostLoad - {}", bean);
                return veto;
        }

        @Override
        /**
         * Logs when a bean has been loaded.
         */
        public void afterPostLoad(PersistentBean bean) {
                LOGGER.info("afterPostLoad - {}", bean);
        }

        @Override
        /**
         * Logs before an implicit action executes.
         * @return {@code true} to veto execution
         */
        public boolean beforePreExecute(ImplicitActionName actionName, Bean bean, Bean parentBean, WebContext webContext) {
                LOGGER.info("beforePreExecute - action = {}, bean = {}, parent = {}", actionName, bean, parentBean);
                return veto;
        }

        @Override
        /**
         * Logs completion of an implicit action.
         */
        public void afterPreExecute(ImplicitActionName actionName, Bean result, Bean parentBean, WebContext webContext) {
                LOGGER.info("afterPreExecute - action = {}, bean = {}, parent = {}", actionName, result, parentBean);
        }

        @Override
        /**
         * Executed prior to a server side action.
         * @return {@code true} to veto the action
         */
        public boolean beforeServerSideAction(Document document, String actionName, Bean bean, WebContext webContext) {
                LOGGER.info("beforeServerSideAction - doc = {}.{}, action = {}, bean = {}", document.getOwningModuleName(), document.getName(), actionName, bean);
                return veto;
        }

        @Override
        /**
         * Logs the outcome of a server side action.
         */
        public void afterServerSideAction(Document document, String actionName, ServerSideActionResult<Bean> result, WebContext webContext) {
                LOGGER.info("afterServerSideAction - doc = {}.{}, action = {}, result = {}", document.getOwningModuleName(), document.getName(), actionName, result);
        }

        @Override
        /**
         * Called before an upload action is processed.
         * @return {@code true} to veto the action
         */
        public boolean beforeUploadAction(Document document, String actionName, Bean bean, Upload upload, WebContext webContext) {
                LOGGER.info("beforeUploadAction - doc = {}.{}, action = {}, bean = {}, file = {}", document.getOwningModuleName(), document.getName(), actionName, bean, upload);
                return veto;
        }

        @Override
        /**
         * Logs the uploaded file after the upload action has finished.
         */
        public void afterUploadAction(Document document, String actionName, Bean bean, Upload upload, WebContext webContext) {
                LOGGER.info("afterUploadAction - doc = {}.{}, action = {}, bean = {}, file = {}", document.getOwningModuleName(), document.getName(), actionName, bean, upload);
        }

        @Override
        /**
         * Executed prior to a BizPort import action.
         * @return {@code true} to veto the import
         */
        public boolean beforeBizImportAction(Document document, String actionName, BizPortWorkbook bizPortable, UploadException problems) {
                LOGGER.info("beforeBizImportAction - doc = {}.{}, action = {}, bean = {}, workbook = {}", document.getOwningModuleName(), document.getName(), actionName, bizPortable);
                return veto;
        }

        @Override
        /**
         * Logs completion of a BizPort import action.
         */
        public void afterBizImportAction(Document document, String actionName, BizPortWorkbook bizPortable, UploadException problems) {
                LOGGER.info("afterBizImportAction - doc = {}.{}, action = {}, bean = {}, workbook = {}", document.getOwningModuleName(), document.getName(), actionName, bizPortable);
        }

        @Override
        /**
         * Invoked before a BizPort export action is executed.
         * @return {@code true} to veto the export
         */
        public boolean beforeBizExportAction(Document document, String actionName, WebContext webContext) {
                LOGGER.info("beforeBizExportAction - doc = {}.{}, action = {}", document.getOwningModuleName(), document.getName(), actionName);
                return veto;
        }

        @Override
        /**
         * Logs completion of a BizPort export action.
         */
        public void afterBizExportAction(Document document, String actionName, BizPortWorkbook result, WebContext webContext) {
                LOGGER.info("beforeBizExportAction - doc = {}.{}, action = {}, result = {}", document.getOwningModuleName(), document.getName(), actionName, result);
        }
}
