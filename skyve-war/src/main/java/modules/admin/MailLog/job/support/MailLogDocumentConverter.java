package modules.admin.MailLog.job.support;

import java.util.Optional;

import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.SortedDocValuesField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.util.BytesRef;
import org.skyve.archive.support.DocumentConverter;
import org.skyve.domain.Bean;

import modules.admin.domain.MailLog;

public class MailLogDocumentConverter implements DocumentConverter {

	private static SortedDocValuesField sortField(String binding, String value) {
		return new SortedDocValuesField(DocumentConverter.toSortBinding(binding), new BytesRef(value));
	}

	private static void addTextField(Document doc, String binding, String value) {
		doc.add(new TextField(binding, value, Store.YES));
		doc.add(sortField(binding, value));
	}

	@Override
	public Document convert(Bean bean) {
		MailLog mailLog = (MailLog) bean;
		Document doc = new Document();

		String timestampStr = DocumentConverter.dateToString(mailLog.getTimestamp());
		addTextField(doc, MailLog.timestampPropertyName, timestampStr);

		doc.add(new TextField(Bean.DOCUMENT_ID, mailLog.getBizId(), Store.YES));
		doc.add(new TextField(Bean.USER_ID, mailLog.getBizUserId(), Store.YES));

		Optional.ofNullable(mailLog.getDispatchStatus())
				.ifPresent(value -> addTextField(doc, MailLog.dispatchStatusPropertyName, value));
		Optional.ofNullable(mailLog.getProvider())
				.ifPresent(value -> addTextField(doc, MailLog.providerPropertyName, value));
		Optional.ofNullable(mailLog.getToRecipients())
				.ifPresent(value -> addTextField(doc, MailLog.toRecipientsPropertyName, value));
		Optional.ofNullable(mailLog.getSubject())
				.ifPresent(value -> addTextField(doc, MailLog.subjectPropertyName, value));
		Optional.ofNullable(mailLog.getIsBulk())
				.ifPresent(value -> addTextField(doc, MailLog.isBulkPropertyName, value.toString()));
		Optional.ofNullable(mailLog.getMailCount())
				.ifPresent(value -> addTextField(doc, MailLog.mailCountPropertyName, value.toString()));
		Optional.ofNullable(mailLog.getRecipientCount())
				.ifPresent(value -> addTextField(doc, MailLog.recipientCountPropertyName, value.toString()));

		return doc;
	}

	@Override
	public boolean handles(String module, String document) {
		return MailLog.MODULE_NAME.equals(module) && MailLog.DOCUMENT_NAME.equals(document);
	}
}
