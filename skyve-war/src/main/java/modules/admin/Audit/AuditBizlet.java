package modules.admin.Audit;

import static java.util.Comparator.comparing;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.Audit.job.support.ArchiveRetriever;
import modules.admin.Audit.models.LuceneFilter;
import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;

public class AuditBizlet extends Bizlet<Audit> {

    @Inject
    private ArchiveRetriever retriever;

    @Override
    public Audit preExecute(ImplicitActionName actionName,
            Audit bean,
            Bean parentBean,
            WebContext webContext)
            throws Exception {
        if (ImplicitActionName.Edit.equals(actionName)) {
            bean.setSourceVersion(bean);
            sourceVersionChanged(bean);
        }

        return bean;
    }

    public void sourceVersionChanged(Audit bean) throws Exception {
        Audit source = bean.getSourceVersion();
        bean.setMe(bean);

        if (Operation.update.equals(source.getOperation())) {
            List<DomainValue> lesserVersions = getVersions(source, true);
            if (lesserVersions.isEmpty()) {
                bean.setComparisonVersion(null);
            } else {
                String bizId = lesserVersions.get(0)
                                             .getCode();

                Optional<Audit> comparison = retriever.retrieveByBizId(bizId);
                bean.setComparisonVersion(comparison.orElse(null));
            }
        } else {
            bean.setComparisonVersion(null);
        }
    }

    @Override
    public List<DomainValue> getDynamicDomainValues(String attributeName, Audit bean)
            throws Exception {
        if (Audit.sourceVersionPropertyName.equals(attributeName)) {
            bean.originalValues()
                .clear();
            return getVersions(bean, false);
        } else if (Audit.comparisonVersionPropertyName.equals(attributeName) &&
                (bean.getSourceVersion() != null)) {
            return getVersions(bean.getSourceVersion(), true);
        }

        return null;
    }

    public List<DomainValue> getVersions(Audit audit, boolean forComparison)
            throws Exception {

        if (forComparison && (!Operation.update.equals(audit.getOperation()))) {
            return new ArrayList<>();
        }

        LuceneFilter filter = new LuceneFilter();
        filter.addEquals(Audit.auditBizIdPropertyName, audit.getAuditBizId());

        List<Audit> audits = new ArrayList<>(retriever.retrieveAll(filter, 500));

        if (forComparison) {
            // f.addLessThan(Audit.millisPropertyName, audit.getMillis());
            audits.removeIf(a -> a.getMillis() >= audit.getMillis());
        }

        // q.addBoundOrdering(Audit.millisPropertyName, SortDirection.descending);
        Collections.sort(audits, comparing(Audit::getMillis).reversed());

        // Delete spurious 'insert' records ()
        Audit oldestInsert = audits.get(audits.size() - 1);
        audits.removeIf(a -> a.getOperation() == Operation.insert && a != oldestInsert);

        List<DomainValue> result = new ArrayList<>(audits.size());
        for (Audit version : audits) {
            result.add(new DomainValue(version.getBizId(), version.getBizKey()));
        }

        return result;
    }

    @Override
    public Audit resolve(String bizId, Bean conversationBean, WebContext webContext) throws Exception {
        return retriever.<Audit> retrieveByBizId(bizId)
                        .orElse(null);
    }
}
