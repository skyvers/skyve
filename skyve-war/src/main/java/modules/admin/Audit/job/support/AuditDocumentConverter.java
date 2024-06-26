package modules.admin.Audit.job.support;

import java.text.ParseException;
import java.util.Date;
import java.util.Optional;

import org.apache.lucene.document.DateTools;
import org.apache.lucene.document.DateTools.Resolution;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.skyve.domain.Bean;

import modules.admin.domain.Audit;

public class AuditDocumentConverter {

    public Document convert(Audit audit) {

        Document doc = new Document();

        // TextField: Reader or String indexed for full-text search
        // StringField: String indexed verbatim as a single token

        // timestamp
        doc.add(new StringField(Audit.timestampPropertyName,
                dateToString(audit.getTimestamp()), Store.YES));

        // user
        doc.add(new TextField(Audit.userNamePropertyName, audit.getUserName(), Store.YES));
        doc.add(new StringField(Bean.USER_ID, audit.getBizUserId(), Store.YES));

        // the audit record's bizId
        doc.add(new StringField(Bean.DOCUMENT_ID, audit.getBizId(), Store.YES));

        // operation
        // FIXME probably shouldn't be the localised description...
        doc.add(new TextField(Audit.operationPropertyName, audit.getOperation()
                                                                .toLocalisedDescription(),
                Store.YES));

        // module+document
        doc.add(new TextField(Audit.auditModuleNamePropertyName, audit.getAuditModuleName(), Store.YES));
        doc.add(new TextField(Audit.auditDocumentNamePropertyName, audit.getAuditDocumentName(), Store.YES));

        // audited doc's bizkey/description
        Optional.ofNullable(audit.getAuditBizKey())
                .ifPresent(bk -> doc.add(new TextField(Audit.auditBizKeyPropertyName, bk, Store.YES)));

        // bizId of the document being audited
        doc.add(new StringField(Audit.auditBizIdPropertyName, audit.getAuditBizId(), Store.YES));

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
}
