package modules.admin.Audit.models;

import static java.util.stream.Collectors.toCollection;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Stream;

import org.apache.lucene.document.Document;
import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.apache.lucene.search.SortField.Type;
import org.skyve.archive.support.DocumentConverter;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.view.model.list.ArchivedDocumentListModel;

import modules.admin.domain.Audit;

public class ArchivedAuditListModel<U extends Bean> extends ArchivedDocumentListModel<U> {

    private List<String> auditBindings = List.of(Audit.timestampPropertyName,
            Audit.userNamePropertyName,
            Audit.operationPropertyName,
            Audit.auditModuleNamePropertyName,
            Audit.auditDocumentNamePropertyName,
            Audit.auditBizKeyPropertyName,
            Audit.auditBizIdPropertyName);

    @Override
    public String getDescription() {
        return "The list of all Audits.";
    }

    @Override
    protected String toSortBinding(String binding) {
        return DocumentConverter.toSortBinding(binding);
    }

    @Override
    protected Bean convertToBean(Document luceneDoc) {
        Map<String, Object> props = new HashMap<>();

        Consumer<String> putStringField = binding -> {
            props.put(binding, luceneDoc.get(binding));
        };

        Stream.of(
                Bean.DOCUMENT_ID,
                Audit.userNamePropertyName,
                Bean.USER_ID,
                Audit.operationPropertyName,
                Audit.auditModuleNamePropertyName,
                Audit.auditDocumentNamePropertyName,
                Audit.auditBizIdPropertyName,
                Audit.auditBizKeyPropertyName)
              .forEach(putStringField);

        String dateStr = luceneDoc.get(Audit.timestampPropertyName);
        props.put(Audit.timestampPropertyName, new Timestamp(DocumentConverter.stringToDate(dateStr)));

        return new DynamicBean(getModule(), getDocument(), props);
    }

    @Override
    protected String getModule() {
        return Audit.MODULE_NAME;
    }

    @Override
    protected String getDocument() {
        return Audit.DOCUMENT_NAME;
    }

    @Override
    public List<MetaDataQueryColumn> getColumns() {

        return auditBindings.stream()
                            .map(this::createColumn)
                            .collect(toCollection(ArrayList::new));
    }

    private MetaDataQueryColumn createColumn(String binding) {
        MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
        column.setBinding(binding);
        column.setSortable(true);

        if (Audit.auditBizIdPropertyName.equals(binding)) {
            column.setHidden(true);
        }

        return column;
    }

    @Override
    public Set<String> getProjections() {

        Set<String> projections = new LinkedHashSet<>();

        projections.add(Bean.DOCUMENT_ID);
        projections.add(PersistentBean.LOCK_NAME);
        projections.add(PersistentBean.TAGGED_NAME);
        projections.add(PersistentBean.FLAG_COMMENT_NAME);
        projections.add(Bean.BIZ_KEY);

        projections.addAll(auditBindings);

        return projections;
    }

    @Override
    protected Sort getDefaultSort() {
        // Default to sorting by timestamp, descending
        return new Sort(new SortField(toSortBinding(Audit.timestampPropertyName), Type.STRING, true));
    }
}
