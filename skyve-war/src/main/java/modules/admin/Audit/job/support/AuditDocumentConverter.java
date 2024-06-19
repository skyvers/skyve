package modules.admin.Audit.job.support;

import java.util.Optional;

import org.apache.lucene.document.DateTools;
import org.apache.lucene.document.DateTools.Resolution;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.StringField;

import modules.admin.domain.Audit;

public class AuditDocumentConverter {

    public Document convert(Audit audit) {

        Document doc = new Document();

        // timestamp
        doc.add(new StringField("timestamp",
                DateTools.dateToString(audit.getTimestamp(), Resolution.MILLISECOND), Store.YES));

        // user
        doc.add(new StringField("userName", audit.getUserName(), Store.YES));
        doc.add(new StringField("bizUserId", audit.getBizUserId(), Store.YES));

        // the audit record's bizId
        doc.add(new StringField("bizId", audit.getBizId(), Store.YES));

        // operation
        doc.add(new StringField("operation", audit.getOperation()
                                                  .toCode(),
                Store.YES));

        // module+document
        doc.add(new StringField("module", audit.getAuditModuleName(), Store.YES));
        doc.add(new StringField("document", audit.getAuditDocumentName(), Store.YES));

        // audited doc's bizkey/description
        Optional.ofNullable(audit.getAuditBizKey())
                .ifPresent(bk -> doc.add(new StringField("auditBizKey", bk, Store.YES)));

        // bizId of the document being audited
        doc.add(new StringField("auditBizId", audit.getAuditBizId(), Store.YES));

        return doc;
    }
}
