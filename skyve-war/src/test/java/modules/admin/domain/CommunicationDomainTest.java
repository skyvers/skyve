package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
	void dataBuilderPopulatesCommunicationBean() {
		Communication bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
		assertNotNull(bean);
		assertNotNull(bean.getBizId());
	}

	@Test
	void bizModuleAndDocumentAreCorrect() {
		Communication bean = Communication.newInstance();
		assertEquals(Communication.MODULE_NAME, bean.getBizModule());
		assertEquals(Communication.DOCUMENT_NAME, bean.getBizDocument());
	}

	@Test
	void descriptionPropertySetAndGet() {
		Communication bean = Communication.newInstance();
		bean.setDescription("test description");
		assertEquals("test description", bean.getDescription());
	}

	@Test
	void moduleNameAndDocumentNameSetAndGet() {
		Communication bean = Communication.newInstance();
		bean.setModuleName("testModule");
		bean.setDocumentName("testDocument");
		assertEquals("testModule", bean.getModuleName());
		assertEquals("testDocument", bean.getDocumentName());
	}

	@Test
	void subjectAndBodySetAndGet() {
		Communication bean = Communication.newInstance();
		bean.setSubject("Test Subject");
		bean.setBody("<p>Test body</p>");
		assertEquals("Test Subject", bean.getSubject());
		assertEquals("<p>Test body</p>", bean.getBody());
	}

	@Test
	void actionTypeSetAndGet() {
		Communication bean = Communication.newInstance();
		bean.setActionType(ActionType.saveForBulkSend);
		assertEquals(ActionType.saveForBulkSend, bean.getActionType());
	}

	@Test
	void formatTypeSetAndGet() {
		Communication bean = Communication.newInstance();
		bean.setFormatType(FormatType.email);
		assertEquals(FormatType.email, bean.getFormatType());
	}

	@Test
	void sendToAndCcToSetAndGet() {
		Communication bean = Communication.newInstance();
		bean.setSendTo("user@example.com");
		bean.setCcTo("cc@example.com");
		assertEquals("user@example.com", bean.getSendTo());
		assertEquals("cc@example.com", bean.getCcTo());
	}

	@Test
	void toBindingSetAndGet() {
		Communication bean = Communication.newInstance();
		bean.setToBinding("contact.email");
		assertEquals("contact.email", bean.getToBinding());
	}

	@Test
	void sendFromSetAndGet() {
		Communication bean = Communication.newInstance();
		bean.setSendFrom("noreply@example.com");
		assertEquals("noreply@example.com", bean.getSendFrom());
	}

	@Test
	void notificationAndSystemUseSetAndGet() {
		Communication bean = Communication.newInstance();
		bean.setNotification(Boolean.TRUE);
		bean.setSystemUse(Boolean.FALSE);
		assertEquals(Boolean.TRUE, bean.getNotification());
		assertEquals(Boolean.FALSE, bean.getSystemUse());
	}

	@Test
	void attachment1And2SetAndGet() {
		Communication bean = Communication.newInstance();
		bean.setAttachmentFileName1("file1.pdf");
		bean.setAttachmentFileName2("file2.xlsx");
		bean.setAttachmentFileName3("file3.docx");
		assertEquals("file1.pdf", bean.getAttachmentFileName1());
		assertEquals("file2.xlsx", bean.getAttachmentFileName2());
		assertEquals("file3.docx", bean.getAttachmentFileName3());
	}

	@Test
	void calendarFieldsSetAndGet() {
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
	void resultsAndUnsubscribeUrlSetAndGet() {
		Communication bean = Communication.newInstance();
		bean.setResults("sent: 5, failed: 0");
		bean.setUnsubscribeUrl("https://example.com/unsubscribe");
		assertEquals("sent: 5, failed: 0", bean.getResults());
		assertEquals("https://example.com/unsubscribe", bean.getUnsubscribeUrl());
	}

	@Test
	void sendToOverrideAndCcToOverrideAndMonitorBccSetAndGet() {
		Communication bean = Communication.newInstance();
		bean.setSendToOverride("override@example.com");
		bean.setCcToOverride("ccoverride@example.com");
		bean.setMonitorBcc(Boolean.TRUE);
		assertEquals("override@example.com", bean.getSendToOverride());
		assertEquals("ccoverride@example.com", bean.getCcToOverride());
		assertEquals(Boolean.TRUE, bean.getMonitorBcc());
	}

	@Test
	void selectedBatchTimestampFolderNameAndRefreshBatchesAndUnTagSuccessfulSetAndGet() {
		Communication bean = Communication.newInstance();
		bean.setSelectedBatchTimestampFolderName("2024-01-01_12-00");
		bean.setRefreshBatches(Boolean.TRUE);
		bean.setUnTagSuccessful(Boolean.FALSE);
		assertEquals("2024-01-01_12-00", bean.getSelectedBatchTimestampFolderName());
		assertEquals(Boolean.TRUE, bean.getRefreshBatches());
		assertEquals(Boolean.FALSE, bean.getUnTagSuccessful());
	}

	@Test
	void mailImageSetAndGet() {
		Communication bean = Communication.newInstance();
		bean.setMailImage("logo.png");
		assertEquals("logo.png", bean.getMailImage());
	}

	@Test
	void conditionBatchSelectedIsTrueWhenFolderNameIsSet() {
		Communication bean = Communication.newInstance();
		bean.setSelectedBatchTimestampFolderName("some-folder");
		assertTrue(bean.isBatchSelected());
		assertFalse(bean.isNotBatchSelected());
	}

	@Test
	void conditionBatchSelectedIsFalseWhenFolderNameIsNull() {
		Communication bean = Communication.newInstance();
		bean.setSelectedBatchTimestampFolderName(null);
		assertFalse(bean.isBatchSelected());
		assertTrue(bean.isNotBatchSelected());
	}

	@Test
	void conditionBatchesRefreshRequired() {
		Communication bean = Communication.newInstance();
		bean.setRefreshBatches(Boolean.TRUE);
		assertTrue(bean.isBatchesRefreshRequired());
		assertFalse(bean.isNotBatchesRefreshRequired());
		bean.setRefreshBatches(Boolean.FALSE);
		assertFalse(bean.isBatchesRefreshRequired());
		assertTrue(bean.isNotBatchesRefreshRequired());
	}

	@Test
	void conditionEmailConfigured() {
		Communication bean = Communication.newInstance();
		// just exercise the method; actual result depends on UtilImpl.SMTP config
		boolean result = bean.isEmailConfigured();
		assertNotEquals(Boolean.valueOf(result), Boolean.valueOf(bean.isNotEmailConfigured()));
	}

	@Test
	void conditionEmailType() {
		Communication bean = Communication.newInstance();
		bean.setFormatType(FormatType.email);
		assertTrue(bean.isEmailType());
		assertFalse(bean.isNotEmailType());
		bean.setFormatType(null);
		assertFalse(bean.isEmailType());
		assertTrue(bean.isNotEmailType());
	}

	@Test
	void conditionIncludesCalendar() {
		Communication bean = Communication.newInstance();
		bean.setIncludeCalendar(Boolean.TRUE);
		assertTrue(bean.isIncludesCalendar());
		assertFalse(bean.isNotIncludesCalendar());
		bean.setIncludeCalendar(Boolean.FALSE);
		assertFalse(bean.isIncludesCalendar());
		assertTrue(bean.isNotIncludesCalendar());
	}

	@Test
	void conditionLockedWhenPersistedAndSystemUse() {
		// A new (unpersisted) bean should not be locked
		Communication bean = Communication.newInstance();
		bean.setSystemUse(Boolean.TRUE);
		assertFalse(bean.isLocked()); // not persisted
		assertTrue(bean.isNotLocked());
	}

	@Test
	void conditionSaveAction() {
		Communication bean = Communication.newInstance();
		bean.setActionType(ActionType.saveForBulkSend);
		assertTrue(bean.isSaveAction());
		assertFalse(bean.isNotSaveAction());
		bean.setActionType(ActionType.sendImmediately);
		assertFalse(bean.isSaveAction());
		assertTrue(bean.isNotSaveAction());
	}

	@Test
	void conditionShowBatches() {
		Communication bean = Communication.newInstance();
		bean.setDescription("Test Comm");
		assertTrue(bean.isShowBatches());
		assertFalse(bean.isNotShowBatches());
		bean.setDescription(null);
		assertFalse(bean.isShowBatches());
		assertTrue(bean.isNotShowBatches());
	}
}
