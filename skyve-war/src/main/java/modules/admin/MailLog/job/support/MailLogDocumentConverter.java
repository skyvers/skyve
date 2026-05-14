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

	/** Padding width for numeric sort fields — enough to represent {@link Long#MAX_VALUE} (19 digits) plus one. */
	private static final int NUMERIC_SORT_PAD_WIDTH = 20;

	private static SortedDocValuesField sortField(String binding, String value) {
		return new SortedDocValuesField(DocumentConverter.toSortBinding(binding), new BytesRef(value));
	}

	private static void addTextField(Document doc, String binding, String value) {
		doc.add(new TextField(binding, value, Store.YES));
		doc.add(sortField(binding, value));
	}

	/**
	 * Add an integer field with zero-padded sort value so lexicographic ordering
	 * matches numeric ordering (e.g. "00000000000000000010" sorts after "00000000000000000002").
	 */
	private static void addIntField(Document doc, String binding, long value) {
		String stored = Long.toString(value);
		String sortValue = String.format("%0" + NUMERIC_SORT_PAD_WIDTH + "d", value);
		doc.add(new TextField(binding, stored, Store.YES));
		doc.add(sortField(binding, sortValue));
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
				.ifPresent(value -> addIntField(doc, MailLog.mailCountPropertyName, value));
		Optional.ofNullable(mailLog.getRecipientCount())
				.ifPresent(value -> addIntField(doc, MailLog.recipientCountPropertyName, value));

		return doc;
	}

	@Override
	public boolean handles(String module, String document) {
		return MailLog.MODULE_NAME.equals(module) && MailLog.DOCUMENT_NAME.equals(document);
	}
}
