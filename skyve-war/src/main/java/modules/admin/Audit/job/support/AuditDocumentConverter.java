package modules.admin.Audit.job.support;

import java.text.ParseException;
import java.util.Date;
import java.util.Optional;

import org.apache.lucene.document.DateTools;
import org.apache.lucene.document.DateTools.Resolution;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.SortedDocValuesField;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.util.BytesRef;
import org.skyve.domain.Bean;

import modules.admin.domain.Audit;

public class AuditDocumentConverter implements DocumentConverter {

    /**
     * If we use the same field name for the SortedDocValuesField as we do for
     * the stored field we run into this error:
     * 
     * <pre>
     * Term [4f 66 66 69 63 65] exists in doc values but not in the terms index
     * </pre>
     * 
     * I believe this is because the terms in the stored field have gone through the
     * analyzer's filter (lowercase, most likely). Storing the sort field under
     * a different name sidesteps the issues.
     * 
     * @see https://github.com/apache/lucene/issues/12067
     * 
     * @param binding
     * @return
     */
    public static String toSortBinding(String binding) {
        return binding + "_sort";
    }

    private static SortedDocValuesField sortField(String binding, String value) {

        return new SortedDocValuesField(toSortBinding(binding), new BytesRef(value));
    }

    @Override
    public Document convert(Bean bean) {

        Audit audit = (Audit) bean;

        Document doc = new Document();

        // TextField: Reader or String indexed for full-text search
        // StringField: String indexed verbatim as a single token

        // timestamp
        String timestampStr = dateToString(audit.getTimestamp());
        doc.add(new StringField(Audit.timestampPropertyName, timestampStr, Store.YES));
        doc.add(sortField(Audit.timestampPropertyName, timestampStr));

        // user
        doc.add(new TextField(Audit.userNamePropertyName, audit.getUserName(), Store.YES));
        doc.add(sortField(Audit.userNamePropertyName, audit.getUserName()));

        doc.add(new StringField(Bean.USER_ID, audit.getBizUserId(), Store.YES));

        // the audit record's bizId
        doc.add(new StringField(Bean.DOCUMENT_ID, audit.getBizId(), Store.YES));

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
        doc.add(new StringField(Audit.auditBizIdPropertyName, audit.getAuditBizId(), Store.YES));
        doc.add(sortField(Audit.auditBizIdPropertyName, audit.getAuditBizId()));

        return doc;
    }

    public static String dateToString(Date date) {
        return DateTools.dateToString(date, Resolution.MILLISECOND);
    }

    public static Date stringToDate(String dateStr) {
        try {
            return DateTools.stringToDate(dateStr);
        } catch (ParseException e) {
            throw new RuntimeException("Unable to parse date string: " + dateStr, e);
        }
    }

    @Override
    public boolean handles(String module, String document) {

        return Audit.MODULE_NAME.equals(module) && Audit.DOCUMENT_NAME.equals(document);
    }

}
