package modules.admin.Audit;

import static java.util.Collections.emptyList;
import static java.util.Comparator.comparing;
import static java.util.stream.Collectors.toSet;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.archive.list.LuceneFilter;
import org.skyve.impl.archive.support.ArchiveRetriever;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;

public class AuditBizlet extends Bizlet<Audit> {

    private ArchiveRetriever retriever = ArchiveRetriever.getInstance();

    @Inject
    private Persistence persistence;

    @Override
    public Audit preExecute(ImplicitActionName actionName, Audit bean, Bean parentBean, WebContext webContext) {
        if (ImplicitActionName.Edit.equals(actionName)) {
            bean.setSourceVersion(bean);
            sourceVersionChanged(bean);
        }

        return bean;
    }

    @Override
    public void preRerender(String source, Audit bean, WebContext webContext) throws Exception {
        if (Audit.sourceVersionPropertyName.equals(source)) {
            sourceVersionChanged(bean);
        }

        super.preRerender(source, bean, webContext);
    }

    /**
     * The "Source Version To Compare" selection has changed, update the available
     * comparison versions options ("Other Version To Compare"), picking the first option
     * as a default.
     * 
     * @param bean
     * @throws Exception
     */
    private void sourceVersionChanged(Audit bean) {
        Audit source = bean.getSourceVersion();
        bean.setMe(bean);

        if (Operation.update.equals(source.getOperation())) {
            List<DomainValue> lesserVersions = getVersions(source, true);
            if (lesserVersions.isEmpty()) {
                bean.setComparisonVersion(null);
            } else {
                // Set the next Audit in the versions list to compare to
                String bizId = lesserVersions.get(0)
                                             .getCode();

                // Try searching the RDBMS first
                Audit found = retrieveFromRdbms(bizId);

                // Then try the archives
                if (found == null) {
                    found = retrieveFromArchives(bizId);
                }

                bean.setComparisonVersion(found);
            }
        } else {
            bean.setComparisonVersion(null);
        }
    }

    protected Audit retrieveFromRdbms(String bizId) {
        return persistence.retrieve(Audit.MODULE_NAME, Audit.DOCUMENT_NAME, bizId);
    }

    /**
     * Retrieve the request Audit from the archives (if Audit archiving is
     * enabled).
     * 
     * @return The requested bean, or null if it isn't found or if Audit
     *         archiving isn't configured.
     */
    protected Audit retrieveFromArchives(String bizId) {

        Optional<ArchiveDocConfig> config = auditDocConfig();
        if (config.isEmpty()) {
            return null;
        }

        return retriever.<Audit> retrieveByBizId(config.get(), bizId)
                        .orElse(null);
    }

    @Override
    public List<DomainValue> getDynamicDomainValues(String attributeName, Audit bean) {
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

    private List<DomainValue> getVersions(Audit bean, boolean forComparison) {

        LOGGER.trace("Getting versions for {} [forComparison:{}]", bean, forComparison);

        List<Audit> luceneVersions = getLuceneVersions(bean, forComparison);
        List<Bean> rdbmsVersions = getRdbmsVersions(bean, forComparison);
        List<Bean> merged = merge(luceneVersions, rdbmsVersions);

        // Sort & delete spurious 'insert' records
        cleanSortAuditList(merged);

        return convertToDomainValues(merged);
    }

    /**
     * Merge the two lists of Audits (one proper docs), the other
     * DynamicBeans. Discarding RDBMS duplicates (by bizId).
     * 
     * @param luceneVersions
     * @param rdbmsVersions
     * @return
     */
    private List<Bean> merge(List<Audit> luceneVersions, List<Bean> rdbmsVersions) {

        List<Bean> mergedResult = new ArrayList<>(luceneVersions);
        Set<String> luceneAuditIds = luceneVersions.stream()
                                                   .map(Audit::getBizId)
                                                   .collect(toSet());

        for (Bean dbAudit : rdbmsVersions) {

            String bizId = (String) Binder.get(dbAudit, Bean.DOCUMENT_ID);
            if (!luceneAuditIds.contains(bizId)) {
                mergedResult.add(dbAudit);
            }
        }

        return mergedResult;
    }

    /**
     * Convert the provided list of Beans to DomainValue objects to use
     * in the drop down list.
     */
    private List<DomainValue> convertToDomainValues(List<Bean> versions) {

        List<DomainValue> result = new ArrayList<>(versions.size());

        for (Bean version : versions) {
            String bizId = (String) Binder.get(version, Bean.DOCUMENT_ID);
            String bizKey = (String) Binder.get(version, Bean.BIZ_KEY);

            result.add(new DomainValue(bizId, bizKey));
        }

        return result;
    }

    public List<Bean> getRdbmsVersions(Audit audit, boolean forComparison) {

        if (forComparison && (!Operation.update.equals(audit.getOperation()))) {
            return new ArrayList<>();
        }

        Persistence p = CORE.getPersistence();

        DocumentQuery q = p.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
        q.addBoundProjection(Bean.DOCUMENT_ID);
        q.addBoundProjection(Bean.BIZ_KEY);
        q.addBoundProjection(Audit.millisPropertyName);
        q.addBoundProjection(Audit.operationPropertyName);

        DocumentFilter f = q.getFilter();
        f.addEquals(Audit.auditBizIdPropertyName, audit.getAuditBizId());
        if (forComparison) {
            f.addLessThan(Audit.millisPropertyName, audit.getMillis());
        }
        q.addBoundOrdering(Audit.millisPropertyName, SortDirection.descending);

        q.setFirstResult(0)
         .setMaxResults(100);
        List<Bean> versions = q.projectedResults();

        return versions;
    }

    public List<Audit> getLuceneVersions(Audit audit, boolean forComparison) {
        Optional<ArchiveDocConfig> config = auditDocConfig();
        if (config.isEmpty()) {
            LOGGER.debug("Audit archiving not configured");
            return emptyList();
        }

        if (forComparison && (!Operation.update.equals(audit.getOperation()))) {
            return new ArrayList<>();
        }

        LuceneFilter filter = new LuceneFilter();
        filter.addEquals(Audit.auditBizIdPropertyName, audit.getAuditBizId());

        List<Audit> audits = new ArrayList<>(retriever.retrieveAll(config.get(), filter, 500));

        if (forComparison) {
            // f.addLessThan(Audit.millisPropertyName, audit.getMillis());
            audits.removeIf(a -> a.getMillis() >= audit.getMillis());
        }

        return audits;
    }

    /**
     * Remove all but the oldest Insert operations and sort the
     * provided list of audits newest to oldest.
     * 
     * @param audits The list of audits, must be sorted so the older audit
     *        is at the end of the list
     */
    protected void cleanSortAuditList(List<Bean> audits) {

        if (audits.size() <= 1) {
            return;
        }

        // Sort the supplied list, with oldest items (ie smallest millis value)
        // at the end
        Function<Bean, Long> millisFn = bean -> {
            if (bean instanceof Audit a) {
                return a.getMillis();
            }

            return (Long) Binder.get(bean, Audit.millisPropertyName);
        };

        Collections.sort(audits, comparing(millisFn).reversed());

        // This will be the
        Bean lastElement = audits.get(audits.size() - 1);

        // Remove all 'insert' Audits, except the first
        for (Iterator<Bean> it = audits.iterator(); it.hasNext();) {
            Bean bean = it.next();

            // Always leave the last element alone
            // This should be true insert record
            if (bean == lastElement) {
                continue;
            }

            Operation op = (Operation) Binder.get(bean, Audit.operationPropertyName);
            if (op == Operation.insert) {
                it.remove();
            }
        }
    }

    /**
     * Find the Audit document archiving configuration (if it exists).
     * 
     * @return
     */
    private Optional<ArchiveDocConfig> auditDocConfig() {

        return Util.getArchiveConfig()
                   .findArchiveDocConfig(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
    }

    @Override
    public Audit resolve(String bizId, Bean conversationBean, WebContext webContext) {

        Optional<ArchiveDocConfig> config = auditDocConfig();
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
