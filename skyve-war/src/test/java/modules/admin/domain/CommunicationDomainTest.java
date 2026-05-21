package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.app.admin.Communication.ActionType;
import org.skyve.domain.app.admin.Communication.FormatType;
import org.skyve.domain.types.DateTime;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

/**
 * Tests for the {@link Communication} admin domain bean (persistent).
 * Exercises getter/setter coverage via {@link DataBuilder} and targeted set/get calls.
 */
@SuppressWarnings("static-method")
class CommunicationDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderPopulatesCommunicationBean() throws Exception {
		Communication bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
		assertNotNull(bean);
		assertNotNull(bean.getBizId());
	}

	@Test
	void bizModuleAndDocumentAreCorrect() throws Exception {
		Communication bean = Communication.newInstance();
		assertEquals(Communication.MODULE_NAME, bean.getBizModule());
		assertEquals(Communication.DOCUMENT_NAME, bean.getBizDocument());
	}

	@Test
	void descriptionPropertySetAndGet() throws Exception {
		Communication bean = Communication.newInstance();
		bean.setDescription("test description");
		assertEquals("test description", bean.getDescription());
	}

	@Test
	void moduleNameAndDocumentNameSetAndGet() throws Exception {
		Communication bean = Communication.newInstance();
		bean.setModuleName("testModule");
		bean.setDocumentName("testDocument");
		assertEquals("testModule", bean.getModuleName());
		assertEquals("testDocument", bean.getDocumentName());
	}

	@Test
	void subjectAndBodySetAndGet() throws Exception {
		Communication bean = Communication.newInstance();
		bean.setSubject("Test Subject");
		bean.setBody("<p>Test body</p>");
		assertEquals("Test Subject", bean.getSubject());
		assertEquals("<p>Test body</p>", bean.getBody());
	}

	@Test
	void actionTypeSetAndGet() throws Exception {
		Communication bean = Communication.newInstance();
		bean.setActionType(ActionType.saveForBulkSend);
		assertEquals(ActionType.saveForBulkSend, bean.getActionType());
	}

	@Test
	void formatTypeSetAndGet() throws Exception {
		Communication bean = Communication.newInstance();
		bean.setFormatType(FormatType.email);
		assertEquals(FormatType.email, bean.getFormatType());
	}

	@Test
	void sendToAndCcToSetAndGet() throws Exception {
		Communication bean = Communication.newInstance();
		bean.setSendTo("user@example.com");
		bean.setCcTo("cc@example.com");
		assertEquals("user@example.com", bean.getSendTo());
		assertEquals("cc@example.com", bean.getCcTo());
	}

	@Test
	void toBindingSetAndGet() throws Exception {
		Communication bean = Communication.newInstance();
		bean.setToBinding("contact.email");
		assertEquals("contact.email", bean.getToBinding());
	}

	@Test
	void sendFromSetAndGet() throws Exception {
		Communication bean = Communication.newInstance();
		bean.setSendFrom("noreply@example.com");
		assertEquals("noreply@example.com", bean.getSendFrom());
	}

	@Test
	void notificationAndSystemUseSetAndGet() throws Exception {
		Communication bean = Communication.newInstance();
		bean.setNotification(Boolean.TRUE);
		bean.setSystemUse(Boolean.FALSE);
		assertEquals(Boolean.TRUE, bean.getNotification());
		assertEquals(Boolean.FALSE, bean.getSystemUse());
	}

	@Test
	void attachment1And2SetAndGet() throws Exception {
		Communication bean = Communication.newInstance();
		bean.setAttachmentFileName1("file1.pdf");
		bean.setAttachmentFileName2("file2.xlsx");
		bean.setAttachmentFileName3("file3.docx");
		assertEquals("file1.pdf", bean.getAttachmentFileName1());
		assertEquals("file2.xlsx", bean.getAttachmentFileName2());
		assertEquals("file3.docx", bean.getAttachmentFileName3());
	}

	@Test
	void calendarFieldsSetAndGet() throws Exception {
		Communication bean = Communication.newInstance();
		bean.setIncludeCalendar(Boolean.TRUE);
		bean.setCalendarTitleExpression("{title}");
		bean.setCalendarDescriptionExpression("{description}");
		DateTime start = new DateTime();
		DateTime end = new DateTime();
		bean.setCalendarStartTime(start);
		bean.setCalendarEndTime(end);
		assertEquals(Boolean.TRUE, bean.getIncludeCalendar());
		assertNotNull(bean.getCalendarTitleExpression());
		assertNotNull(bean.getCalendarStartTime());
		assertNotNull(bean.getCalendarEndTime());
	}

	@Test
	void resultsAndUnsubscribeUrlSetAndGet() throws Exception {
		Communication bean = Communication.newInstance();
		bean.setResults("sent: 5, failed: 0");
		bean.setUnsubscribeUrl("https://example.com/unsubscribe");
		assertEquals("sent: 5, failed: 0", bean.getResults());
		assertEquals("https://example.com/unsubscribe", bean.getUnsubscribeUrl());
	}
}
