package modules.admin.Audit.job.support;

import java.util.Optional;

import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.SortedDocValuesField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.util.BytesRef;
import org.skyve.archive.support.DocumentConverter;
import org.skyve.domain.Bean;

import modules.admin.domain.Audit;

/**
 * Converts archived audit beans into Lucene documents for full-text search and sorting.
 *
 * <p>The converter indexes the audit's core metadata plus the audited document identifiers
 * so the archive list can filter and sort without rehydrating the original bean.
 */
public class AuditDocumentConverter implements DocumentConverter {
	
    private static SortedDocValuesField sortField(String binding, String value) {
        return new SortedDocValuesField(DocumentConverter.toSortBinding(binding), new BytesRef(value));
    }

    /**
     * Converts an audited bean into a Lucene document suitable for archive indexing.
     *
     * @param bean the bean to index; must be an {@link Audit}
     * @return the Lucene document containing searchable and sortable audit fields
     */
    @Override
    public Document convert(Bean bean) {

        Audit audit = (Audit) bean;

        Document doc = new Document();

        // TextField: Reader or String indexed for full-text search
        // StringField: String indexed verbatim as a single token

        // timestamp
        String timestampStr = DocumentConverter.dateToString(audit.getTimestamp());
        doc.add(new TextField(Audit.timestampPropertyName, timestampStr, Store.YES));
        doc.add(sortField(Audit.timestampPropertyName, timestampStr));

        // user
        doc.add(new TextField(Audit.userNamePropertyName, audit.getUserName(), Store.YES));
        doc.add(sortField(Audit.userNamePropertyName, audit.getUserName()));

        doc.add(new TextField(Bean.USER_ID, audit.getBizUserId(), Store.YES));

        // the audit record's bizId
        doc.add(new TextField(Bean.DOCUMENT_ID, audit.getBizId(), Store.YES));

        // operation
        String opName = audit.getOperation()
                             .name();
        doc.add(new TextField(Audit.operationPropertyName, opName, Store.YES));
        doc.add(sortField(Audit.operationPropertyName, opName));

        // module+document
        doc.add(new TextField(Audit.auditModuleNamePropertyName, audit.getAuditModuleName(), Store.YES));
        doc.add(sortField(Audit.auditModuleNamePropertyName, audit.getAuditModuleName()));

        doc.add(new TextField(Audit.auditDocumentNamePropertyName, audit.getAuditDocumentName(), Store.YES));
        doc.add(sortField(Audit.auditDocumentNamePropertyName, audit.getAuditDocumentName()));

        // audited doc's bizkey/description
        Optional.ofNullable(audit.getAuditBizKey())
                .ifPresent(bk -> {
                    doc.add(new TextField(Audit.auditBizKeyPropertyName, bk, Store.YES));
                    doc.add(sortField(Audit.auditBizKeyPropertyName, bk));
                });

        // bizId of the document being audited
        doc.add(new TextField(Audit.auditBizIdPropertyName, audit.getAuditBizId(), Store.YES));
        doc.add(sortField(Audit.auditBizIdPropertyName, audit.getAuditBizId()));

        return doc;
    }

    /**
     * Returns whether this converter handles the supplied module/document pair.
     *
     * @param module the module name being indexed; never {@code null}
     * @param document the document name being indexed; never {@code null}
     * @return {@code true} only for the admin audit document
     */
    @Override
    public boolean handles(String module, String document) {
        return Audit.MODULE_NAME.equals(module) && Audit.DOCUMENT_NAME.equals(document);
    }
}
