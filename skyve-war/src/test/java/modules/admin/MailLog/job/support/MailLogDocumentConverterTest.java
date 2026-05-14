package modules.admin.MailLog.job.support;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.apache.lucene.document.Document;
import org.junit.jupiter.api.Test;
import org.skyve.archive.support.DocumentConverter;
import org.skyve.domain.Bean;
import org.skyve.domain.types.Timestamp;

import modules.admin.domain.MailLog;

class MailLogDocumentConverterTest {

	@Test
	void testHandlesAdminMailLogOnly() {
		MailLogDocumentConverter converter = new MailLogDocumentConverter();

		assertThat(converter.handles("admin", "MailLog"), is(true));
		assertThat(converter.handles("admin", "Audit"), is(false));
		assertThat(converter.handles("crm", "MailLog"), is(false));
	}

	@Test
	void testConvertIndexesExpectedFields() {
		MailLogDocumentConverter converter = new MailLogDocumentConverter();
		MailLog mailLog = new MailLog();
		mailLog.setBizId("mail-log-1");
		mailLog.setBizUserId("user-1");
		mailLog.setTimestamp(new Timestamp());
		mailLog.setDispatchStatus("SENT");
		mailLog.setProvider("smtp");
		mailLog.setToRecipients("to@skyve.org");
		mailLog.setSubject("Subject");
		mailLog.setIsBulk(Boolean.TRUE);
		mailLog.setMailCount(Long.valueOf(2));
		mailLog.setRecipientCount(Long.valueOf(3));

		Document doc = converter.convert(mailLog);

		String timestamp = DocumentConverter.dateToString(mailLog.getTimestamp());
		assertThat(doc.get(MailLog.timestampPropertyName), is(timestamp));
		assertThat(doc.get(Bean.DOCUMENT_ID), is("mail-log-1"));
		assertThat(doc.get(Bean.USER_ID), is("user-1"));
		assertThat(doc.get(MailLog.dispatchStatusPropertyName), is("SENT"));
		assertThat(doc.get(MailLog.providerPropertyName), is("smtp"));
		assertThat(doc.get(MailLog.toRecipientsPropertyName), is("to@skyve.org"));
		assertThat(doc.get(MailLog.subjectPropertyName), is("Subject"));
		assertThat(doc.get(MailLog.isBulkPropertyName), is("true"));
		assertThat(doc.get(MailLog.mailCountPropertyName), is("2"));
		assertThat(doc.get(MailLog.recipientCountPropertyName), is("3"));

		assertThat(doc.getField(DocumentConverter.toSortBinding(MailLog.timestampPropertyName)), notNullValue());
		assertThat(doc.getField(DocumentConverter.toSortBinding(MailLog.dispatchStatusPropertyName)), notNullValue());
		assertThat(doc.getField(DocumentConverter.toSortBinding(MailLog.providerPropertyName)), notNullValue());
		assertThat(doc.getField(DocumentConverter.toSortBinding(MailLog.toRecipientsPropertyName)), notNullValue());
		assertThat(doc.getField(DocumentConverter.toSortBinding(MailLog.subjectPropertyName)), notNullValue());
		assertThat(doc.getField(DocumentConverter.toSortBinding(MailLog.isBulkPropertyName)), notNullValue());
		assertThat(doc.getField(DocumentConverter.toSortBinding(MailLog.mailCountPropertyName)), notNullValue());
		assertThat(doc.getField(DocumentConverter.toSortBinding(MailLog.recipientCountPropertyName)), notNullValue());
	}
}
