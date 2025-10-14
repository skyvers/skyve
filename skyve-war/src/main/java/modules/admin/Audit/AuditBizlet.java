package modules.admin.Audit;

import java.util.List;
import java.util.Optional;

import org.skyve.domain.Bean;
import org.skyve.impl.archive.support.ArchiveRetriever;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.domain.Audit;

public class AuditBizlet extends Bizlet<Audit> {

    private ArchiveRetriever retriever = ArchiveRetriever.getInstance();

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

        Optional<ArchiveDocConfig> config = auditService.auditDocConfig();
        if (config.isEmpty()) {
            LOGGER.debug("Audit archiving not configured");
            return null;
        }

        // Load the requested Audit instance from the archives, if available, falling back to the DB otherwise
        Audit result = retriever.<Audit> retrieveByBizId(config.get(), bizId)
                                .orElse(null);

        if (result == null) {
            LOGGER.trace("Unabled to resolve Audit {}", bizId);
        } else {
            LOGGER.trace("Resolved {} for {}", result, bizId);
        }

        return result;
    }
}
