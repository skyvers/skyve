package modules.admin.Audit;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.domain.Audit;

public class AuditBizlet extends Bizlet<Audit> {

    @Inject
    private transient AuditService auditService;

    @Override
    public Audit preExecute(ImplicitActionName actionName, Audit bean, Bean parentBean, WebContext webContext) {
        if (ImplicitActionName.Edit.equals(actionName)) {
            bean.setSourceVersion(bean);
            auditService.sourceVersionChanged(bean);
        }

        return bean;
    }

    @Override
    public void preRerender(String source, Audit bean, WebContext webContext) throws Exception {
        if (Audit.sourceVersionPropertyName.equals(source)) {
            auditService.sourceVersionChanged(bean);
        }

        super.preRerender(source, bean, webContext);
    }

    @Override
    public List<DomainValue> getDynamicDomainValues(String attributeName, Audit bean) {
        if (Audit.sourceVersionPropertyName.equals(attributeName)) {
            bean.originalValues()
                .clear();
            return auditService.getVersions(bean, false);
        } else if (Audit.comparisonVersionPropertyName.equals(attributeName) &&
                (bean.getSourceVersion() != null)) {
            return auditService.getVersions(bean.getSourceVersion(), true);
        }

        return null;
    }

    @Override
    public Audit resolve(String bizId, Bean conversationBean, WebContext webContext) {
    	// Load the requested Audit instance from the archives, if available
        Audit result = auditService.retrieveFromArchives(bizId);

        if (result == null) {
            LOGGER.trace("Unabled to resolve Audit {} from archives", bizId);
        } else {
            LOGGER.trace("Resolved {} for {} from archives", result, bizId);
        }

        return result;
    }
}
