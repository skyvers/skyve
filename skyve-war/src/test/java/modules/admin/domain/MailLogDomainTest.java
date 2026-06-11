package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

/**
 * Tests for the {@link MailLog} admin domain bean (persistent, concrete).
 * Exercises getter/setter coverage via {@link DataBuilder} and targeted set/get calls.
 */
@SuppressWarnings("static-method")
class MailLogDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderPopulatesMailLogBean() {
		MailLog bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(MailLog.MODULE_NAME, MailLog.DOCUMENT_NAME);
		assertNotNull(bean);
		assertNotNull(bean.getBizId());
	}

	@Test
	void bizModuleAndDocumentAreCorrect() {
		MailLog bean = new MailLog();
		assertEquals(MailLog.MODULE_NAME, bean.getBizModule());
		assertEquals(MailLog.DOCUMENT_NAME, bean.getBizDocument());
	}

	@Test
	void recipientFieldsSetAndGet() {
		MailLog bean = new MailLog();
		bean.setToRecipients("to@example.com");
		bean.setCcRecipients("cc@example.com");
		bean.setBccRecipients("bcc@example.com");
		assertEquals("to@example.com", bean.getToRecipients());
		assertEquals("cc@example.com", bean.getCcRecipients());
		assertEquals("bcc@example.com", bean.getBccRecipients());
	}

	@Test
	void subjectAndBodyExcerptSetAndGet() {
		MailLog bean = new MailLog();
		bean.setSubject("Hello World");
		bean.setBodyExcerpt("Hello, this is a test email...");
		assertEquals("Hello World", bean.getSubject());
		assertEquals("Hello, this is a test email...", bean.getBodyExcerpt());
	}

	@Test
	void dispatchStatusAndProviderSetAndGet() {
		MailLog bean = new MailLog();
		bean.setDispatchStatus("SENT");
		bean.setProvider("SMTP");
		bean.setProviderMessageId("msg-123");
		assertEquals("SENT", bean.getDispatchStatus());
		assertEquals("SMTP", bean.getProvider());
		assertEquals("msg-123", bean.getProviderMessageId());
	}

	@Test
	void relayStatusAndDetailSetAndGet() {
		MailLog bean = new MailLog();
		bean.setRelayStatus("OK");
		bean.setRelayDetail("Delivered");
		assertEquals("OK", bean.getRelayStatus());
		assertEquals("Delivered", bean.getRelayDetail());
	}

	@Test
	void errorDetailSetAndGet() {
		MailLog bean = new MailLog();
		bean.setErrorDetail("Connection refused");
		assertEquals("Connection refused", bean.getErrorDetail());
	}

	@Test
	void timestampSetAndGet() {
		MailLog bean = new MailLog();
		Timestamp ts = new Timestamp();
		bean.setTimestamp(ts);
		assertNotNull(bean.getTimestamp());
	}

	@Test
	void bulkAndCounterFieldsSetAndGet() {
		MailLog bean = new MailLog();
		bean.setIsBulk(Boolean.TRUE);
		bean.setMailCount(Long.valueOf(100L));
		bean.setRecipientCount(Long.valueOf(50L));
		bean.setHasMultipleSubjects(Boolean.TRUE);
		bean.setSubjectVariantCount(Long.valueOf(3L));
		bean.setHasMultipleBodies(Boolean.FALSE);
		bean.setBodyVariantCount(Long.valueOf(0L));
		assertEquals(Boolean.TRUE, bean.getIsBulk());
		assertEquals(Long.valueOf(100L), bean.getMailCount());
		assertEquals(Long.valueOf(50L), bean.getRecipientCount());
		assertEquals(Boolean.TRUE, bean.getHasMultipleSubjects());
		assertEquals(Long.valueOf(3L), bean.getSubjectVariantCount());
		assertEquals(Boolean.FALSE, bean.getHasMultipleBodies());
		assertEquals(Long.valueOf(0L), bean.getBodyVariantCount());
	}

	@Test
	void attachmentFileNamesSetAndGet() {
		MailLog bean = new MailLog();
		bean.setAttachmentFileNames("file1.pdf,file2.xlsx");
		assertEquals("file1.pdf,file2.xlsx", bean.getAttachmentFileNames());
	}
}
