package org.skyve.impl.mail;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.content.AttachmentContent;
import org.skyve.content.MimeType;
import org.skyve.domain.app.admin.MailLog;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.user.User;
import org.skyve.util.Mail;
import org.skyve.util.MailAttachment;
import org.skyve.util.MailDispatchOutcome;

class MailLogUtilTest {
	private final List<MailLogUtil.MailLogEntry> entries = new ArrayList<>();

	private boolean originalLogBodyExcerpt;
	private boolean originalLogAttachments;
	private long originalLogAttachmentsMaxBytes;
	private Class<? extends AbstractContentManager> originalContentManagerClass;

	@BeforeEach
	void beforeEach() {
		originalLogBodyExcerpt = UtilImpl.SMTP_LOG_BODY_EXCERPT;
		originalLogAttachments = UtilImpl.SMTP_LOG_ATTACHMENTS;
		originalLogAttachmentsMaxBytes = UtilImpl.SMTP_LOG_ATTACHMENTS_MAX_BYTES;
		originalContentManagerClass = AbstractContentManager.IMPLEMENTATION_CLASS;
		UtilImpl.SMTP_LOG_BODY_EXCERPT = false;
		UtilImpl.SMTP_LOG_ATTACHMENTS = false;
		UtilImpl.SMTP_LOG_ATTACHMENTS_MAX_BYTES = UtilImpl.DEFAULT_SMTP_LOG_ATTACHMENTS_MAX_BYTES;
		RecordingContentManager.puts.clear();
		MailLogUtil.setRecorderForTesting(entries::add);
	}

	@AfterEach
	void afterEach() {
		MailLogUtil.clearRecorderForTesting();
		UtilImpl.SMTP_LOG_BODY_EXCERPT = originalLogBodyExcerpt;
		UtilImpl.SMTP_LOG_ATTACHMENTS = originalLogAttachments;
		UtilImpl.SMTP_LOG_ATTACHMENTS_MAX_BYTES = originalLogAttachmentsMaxBytes;
		AbstractContentManager.IMPLEMENTATION_CLASS = originalContentManagerClass;
	}

	@Test
	void testLogMailCapturesMetadataAndRedactsBody() {
		Mail mail = new Mail().from("sender@skyve.org")
								.addTo("to2@skyve.org")
								.addTo("to1@skyve.org")
								.addCC("cc@skyve.org")
								.addBCC("bcc@skyve.org")
								.subject("Subject")
								.body("<p>Hello<br/>Your verification code is 123456</p>");
		mail.attach(new MailAttachment("report.txt", new byte[] { 1, 2, 3 }, MimeType.plain));
		mail.attach(new MailAttachment(null, new byte[] { 9 }, MimeType.plain));
		mail.getAttachments().add(null);

		MailDispatchOutcome outcome = MailDispatchOutcome.sent("smtp", "message-1", "accepted", "queued");
		MailLogUtil.logMail(mail, outcome);

		assertEquals(1, entries.size());
		MailLogUtil.MailLogEntry entry = entries.get(0);
		assertThat(entry.getToRecipients(), is("to1@skyve.org, to2@skyve.org"));
		assertThat(entry.getCcRecipients(), is("cc@skyve.org"));
		assertThat(entry.getBccRecipients(), is("bcc@skyve.org"));
		assertThat(entry.getSubject(), is("Subject"));
		assertThat(entry.getBodyExcerpt(), is("[REDACTED]"));
		assertThat(entry.getAttachmentFileNames(), is("report.txt"));
		assertThat(entry.getDispatchStatus(), is("SENT"));
		assertThat(entry.getProvider(), is("smtp"));
		assertThat(entry.getProviderMessageId(), is("message-1"));
		assertThat(entry.getRelayStatus(), is("accepted"));
		assertThat(entry.getRelayDetail(), is("queued"));
		assertThat(entry.getErrorDetail(), is((String) null));
		assertThat(entry.getIsBulk(), is(Boolean.FALSE));
		assertThat(entry.getMailCount(), is(Long.valueOf(1)));
		assertThat(entry.getRecipientCount(), is(Long.valueOf(4)));
		assertThat(entry.getHasMultipleSubjects(), is(Boolean.FALSE));
		assertThat(entry.getSubjectVariantCount(), is(Long.valueOf(1)));
		assertThat(entry.getHasMultipleBodies(), is(Boolean.FALSE));
		assertThat(entry.getBodyVariantCount(), is(Long.valueOf(1)));
	}

	@Test
	void testLogBulkMailTracksVariantsAndHandlesEmptyRecipientJoins() {
		Mail first = new Mail().subject("Subject 1").body("<div>One</div>");
		first.attach(new MailAttachment("first.txt", new byte[] { 1 }, MimeType.plain));
		first.getAttachments().add(null);

		Mail second = new Mail().subject("Subject 2").body("Security code is 123456");

		MailDispatchOutcome outcome = MailDispatchOutcome.failed("provider", "relay failed");
		MailLogUtil.logBulkMail(Arrays.asList(first, second), outcome);

		assertEquals(1, entries.size());
		MailLogUtil.MailLogEntry entry = entries.get(0);
		assertThat(entry.getToRecipients(), is((String) null));
		assertThat(entry.getCcRecipients(), is((String) null));
		assertThat(entry.getBccRecipients(), is((String) null));
		assertThat(entry.getSubject(), is("Subject 1"));
		assertThat(entry.getBodyExcerpt(), is("[REDACTED]"));
		assertThat(entry.getAttachmentFileNames(), is("first.txt"));
		assertThat(entry.getDispatchStatus(), is("FAILED"));
		assertThat(entry.getProvider(), is("provider"));
		assertThat(entry.getErrorDetail(), is("relay failed"));
		assertThat(entry.getIsBulk(), is(Boolean.TRUE));
		assertThat(entry.getMailCount(), is(Long.valueOf(2)));
		assertThat(entry.getRecipientCount(), is(Long.valueOf(0)));
		assertThat(entry.getHasMultipleSubjects(), is(Boolean.TRUE));
		assertThat(entry.getSubjectVariantCount(), is(Long.valueOf(2)));
		assertThat(entry.getHasMultipleBodies(), is(Boolean.TRUE));
		assertThat(entry.getBodyVariantCount(), is(Long.valueOf(2)));
	}

	@Test
	void testLogMailStoresMaskedPlainTextExcerptWhenEnabled() {
		UtilImpl.SMTP_LOG_BODY_EXCERPT = true;
		Mail mail = new Mail().subject("Subject").body("<p>Hello<br/>Your verification code is 123456</p>");

		MailLogUtil.logMail(mail, MailDispatchOutcome.sent("smtp", "message-1", "accepted", "queued"));

		assertEquals(1, entries.size());
		assertThat(entries.get(0).getBodyExcerpt(), is("Hello\nYour verification code is ******"));
	}

	@Test
	void testLogBulkMailStoresFirstMailExcerptWhenEnabled() {
		UtilImpl.SMTP_LOG_BODY_EXCERPT = true;
		Mail first = new Mail().subject("Subject 1").body("<div>One</div>");
		Mail second = new Mail().subject("Subject 2").body("Two");

		MailLogUtil.logBulkMail(Arrays.asList(first, second), MailDispatchOutcome.sent("smtp", null, null, null));

		assertEquals(1, entries.size());
		assertThat(entries.get(0).getBodyExcerpt(), is("One"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testBodyExcerptTruncatesLongBodyWhenEnabled() {
		UtilImpl.SMTP_LOG_BODY_EXCERPT = true;
		String body = "a".repeat(600);

		String excerpt = MailLogUtil.bodyExcerpt(body);

		assertNotNull(excerpt);
		assertEquals(500, excerpt.length());
		assertThat(excerpt, is("a".repeat(497) + "..."));
	}

	@Test
	@SuppressWarnings("static-method")
	void testBodyExcerptKeepsBodyAtMaxLengthWhenEnabled() {
		UtilImpl.SMTP_LOG_BODY_EXCERPT = true;
		String body = "b".repeat(500);

		assertThat(MailLogUtil.bodyExcerpt(body), is(body));
	}

	@Test
	@SuppressWarnings("static-method")
	void testBodyExcerptReturnsNullForBlankOrMarkupOnlyBodyWhenEnabled() {
		UtilImpl.SMTP_LOG_BODY_EXCERPT = true;

		assertThat(MailLogUtil.bodyExcerpt(null), is((String) null));
		assertThat(MailLogUtil.bodyExcerpt("   "), is((String) null));
		assertThat(MailLogUtil.bodyExcerpt("<br/>"), is((String) null));
	}

	@Test
	@SuppressWarnings("static-method")
	void testBodyExcerptIsRedactedByDefault() {
		assertThat(MailLogUtil.bodyExcerpt("Hello"), is("[REDACTED]"));
		assertThat(MailLogUtil.bodyExcerpt(null), is((String) null));
	}

	@Test
	void testLogMailDoesNotZipAttachmentsByDefault() {
		Mail mail = new Mail().subject("Subject").body("Body");
		mail.attach(new MailAttachment("report.txt", bytes("report"), MimeType.plain));

		MailLogUtil.logMail(mail, MailDispatchOutcome.sent("smtp", null, null, null));

		assertEquals(1, entries.size());
		assertThat(entries.get(0).getAttachmentsZip(), is((byte[]) null));
		assertThat(entries.get(0).getAttachmentFileNames(), is("report.txt"));
	}

	@Test
	void testLogMailZipsAttachmentsWhenEnabled() throws Exception {
		UtilImpl.SMTP_LOG_ATTACHMENTS = true;
		Mail mail = new Mail().subject("Subject").body("Body");
		mail.attach(new MailAttachment("report.txt", bytes("report"), MimeType.plain));
		mail.attach(new MailAttachment("invoice.pdf", bytes("invoice"), MimeType.pdf));

		MailLogUtil.logMail(mail, MailDispatchOutcome.sent("smtp", null, null, null));

		assertEquals(1, entries.size());
		Map<String, String> zip = unzip(entries.get(0).getAttachmentsZip());
		assertEquals(2, zip.size());
		assertThat(zip.get("report.txt"), is("report"));
		assertThat(zip.get("invoice.pdf"), is("invoice"));
	}

	@Test
	void testLogBulkMailStoresIdenticalAttachmentOnceWhenEnabled() throws Exception {
		UtilImpl.SMTP_LOG_ATTACHMENTS = true;
		List<Mail> mails = new ArrayList<>();
		for (int i = 0; i < 3; i++) {
			Mail mail = new Mail().subject("Subject").body("Body");
			mail.attach(new MailAttachment("terms.pdf", bytes("terms"), MimeType.pdf));
			mails.add(mail);
		}

		MailLogUtil.logBulkMail(mails, MailDispatchOutcome.sent("smtp", null, null, null));

		Map<String, String> zip = unzip(entries.get(0).getAttachmentsZip());
		assertEquals(1, zip.size());
		assertThat(zip.get("terms.pdf"), is("terms"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testZipAttachmentsSuffixesNameClashWithDifferentBytes() throws Exception {
		Mail first = new Mail();
		first.attach(new MailAttachment("statement.pdf", bytes("one"), MimeType.pdf));
		Mail second = new Mail();
		second.attach(new MailAttachment("statement.pdf", bytes("two"), MimeType.pdf));
		Mail third = new Mail();
		third.attach(new MailAttachment("statement.pdf", bytes("three"), MimeType.pdf));

		Map<String, String> zip = unzip(MailLogUtil.zipAttachments(List.of(first, second, third), Long.MAX_VALUE));

		assertEquals(3, zip.size());
		assertThat(zip.get("statement.pdf"), is("one"));
		assertThat(zip.get("statement (2).pdf"), is("two"));
		assertThat(zip.get("statement (3).pdf"), is("three"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testZipAttachmentsSkipsAttachmentsOverLimitAndListsThem() throws Exception {
		Mail mail = new Mail();
		mail.attach(new MailAttachment("small.txt", bytes("1234"), MimeType.plain));
		mail.attach(new MailAttachment("large.txt", bytes("1234567890"), MimeType.plain));
		mail.attach(new MailAttachment("fits.txt", bytes("12"), MimeType.plain));

		Map<String, String> zip = unzip(MailLogUtil.zipAttachments(List.of(mail), 6));

		assertEquals(3, zip.size());
		assertThat(zip.get("small.txt"), is("1234"));
		assertThat(zip.get("fits.txt"), is("12"));
		assertThat(zip.get(MailLogUtil.SKIPPED_ATTACHMENTS_FILE_NAME),
					is("large.txt (10 bytes) exceeds the remaining limit of 2 bytes"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testZipAttachmentsNamesUnnamedAndIgnoresEmptyAttachments() throws Exception {
		Mail mail = new Mail();
		mail.attach(new MailAttachment(null, bytes("first"), MimeType.plain));
		mail.attach(new MailAttachment("  ", bytes("second"), MimeType.plain));
		mail.attach(new MailAttachment("empty.txt", new byte[0], MimeType.plain));
		mail.getAttachments().add(null);

		Map<String, String> zip = unzip(MailLogUtil.zipAttachments(List.of(mail), Long.MAX_VALUE));

		assertEquals(2, zip.size());
		assertThat(zip.get("attachment-1"), is("first"));
		assertThat(zip.get("attachment-2"), is("second"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testZipAttachmentsReturnsNullWhenThereAreNoAttachments() {
		Mail mail = new Mail();
		mail.attach(new MailAttachment("empty.txt", new byte[0], MimeType.plain));

		assertThat(MailLogUtil.zipAttachments(List.of(new Mail(), mail), Long.MAX_VALUE), is((byte[]) null));
		assertThat(MailLogUtil.zipAttachments(List.of(), Long.MAX_VALUE), is((byte[]) null));
	}

	@Test
	@SuppressWarnings("static-method")
	void testPutAttachmentsStoresZipWithoutIndexing() {
		AbstractContentManager.IMPLEMENTATION_CLASS = RecordingContentManager.class;
		MailLog log = mockMailLog();
		byte[] zip = bytes("zip");

		String contentId = MailLogUtil.putAttachments(log, zip);

		assertThat(contentId, is("content-1"));
		assertEquals(1, RecordingContentManager.puts.size());
		AttachmentContent content = RecordingContentManager.puts.get(0);
		assertThat(content.getBizCustomer(), is("demo"));
		assertThat(content.getBizModule(), is("admin"));
		assertThat(content.getBizDocument(), is("MailLog"));
		assertThat(content.getBizId(), is("log-1"));
		assertThat(content.getAttributeName(), is(MailLogUtil.ATTACHMENTS_ATTRIBUTE_NAME));
		assertThat(content.getFileName(), is(MailLogUtil.ATTACHMENTS_FILE_NAME));
		assertThat(content.getMimeType(), is(MimeType.zip));
		assertFalse(RecordingContentManager.indexed);
	}

	@Test
	@SuppressWarnings("static-method")
	void testPutAttachmentsReturnsNullWhenContentManagerFails() {
		AbstractContentManager.IMPLEMENTATION_CLASS = FailingContentManager.class;

		assertThat(MailLogUtil.putAttachments(mockMailLog(), bytes("zip")), is((String) null));
	}

	@Test
	void testLogBulkMailWithEmptyListProducesEmptyEntry() {
		MailLogUtil.logBulkMail(List.of(), MailDispatchOutcome.skipped("provider", "empty"));

		assertEquals(1, entries.size());
		MailLogUtil.MailLogEntry entry = entries.get(0);
		assertThat(entry.getToRecipients(), is((String) null));
		assertThat(entry.getCcRecipients(), is((String) null));
		assertThat(entry.getBccRecipients(), is((String) null));
		assertThat(entry.getSubject(), is((String) null));
		assertThat(entry.getBodyExcerpt(), is((String) null));
		assertThat(entry.getAttachmentFileNames(), is((String) null));
		assertThat(entry.getDispatchStatus(), is("SKIPPED"));
		assertThat(entry.getProvider(), is("provider"));
		assertThat(entry.getRelayDetail(), is("empty"));
		assertThat(entry.getIsBulk(), is(Boolean.TRUE));
		assertThat(entry.getMailCount(), is(Long.valueOf(0)));
		assertThat(entry.getRecipientCount(), is(Long.valueOf(0)));
		assertThat(entry.getHasMultipleSubjects(), is(Boolean.FALSE));
		assertThat(entry.getSubjectVariantCount(), is(Long.valueOf(0)));
		assertThat(entry.getHasMultipleBodies(), is(Boolean.FALSE));
		assertThat(entry.getBodyVariantCount(), is(Long.valueOf(0)));
	}

	@Test
	void testLogMailWithoutRecorderSkipsPersistenceWhenNoPersistenceConfigured() {
		Class<? extends AbstractPersistence> originalImplementationClass = AbstractPersistence.IMPLEMENTATION_CLASS;
		try {
			AbstractPersistence.IMPLEMENTATION_CLASS = null;
			MailLogUtil.clearRecorderForTesting();
			MailLogUtil.logMail(new Mail().subject("Subject").body("Body"), MailDispatchOutcome.sent("smtp"));
		}
		finally {
			AbstractPersistence.IMPLEMENTATION_CLASS = originalImplementationClass;
		}
		// Verify no entry was recorded since the recorder was cleared.
		assertTrue(entries.isEmpty());
	}

	@SuppressWarnings("static-method")
	@Test
	void testCreateMailLogUserUsesCurrentUserId() throws Exception {
		User user = Mockito.mock(User.class);
		Mockito.when(user.getId()).thenReturn("user-1");
		Mockito.when(user.getCustomerName()).thenReturn("customer-1");

		SuperUser result = (SuperUser) invokePrivate("createMailLogUser", new Class<?>[] { User.class }, user);

		assertThat(result, instanceOf(SuperUser.class));
		assertThat(result.getId(), is("user-1"));
		assertThat(result.getCustomerName(), is("customer-1"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testCreateMailLogUserFallsBackToAnonymousCustomer() throws Exception {
		String originalCustomer = UtilImpl.CUSTOMER;
		try {
			UtilImpl.CUSTOMER = "fallback-customer";
			User user = Mockito.mock(User.class);
			Mockito.when(user.getId()).thenReturn(null);
			Mockito.when(user.getCustomerName()).thenReturn(null);

			SuperUser result = (SuperUser) invokePrivate("createMailLogUser", new Class<?>[] { User.class }, user);

			assertThat(result, instanceOf(SuperUser.class));
			assertThat(result.getId(), is("mailLogUser"));
			assertThat(result.getCustomerName(), is("fallback-customer"));
		}
		finally {
			UtilImpl.CUSTOMER = originalCustomer;
		}
	}

	@SuppressWarnings("static-method")
	@Test
	void testCreateMailLogUserReturnsNullWhenCustomerCannotBeResolved() throws Exception {
		String originalCustomer = UtilImpl.CUSTOMER;
		try {
			UtilImpl.CUSTOMER = null;
			User user = Mockito.mock(User.class);
			Mockito.when(user.getId()).thenReturn(null);
			Mockito.when(user.getCustomerName()).thenReturn(null);

			Object result = invokePrivate("createMailLogUser", new Class<?>[] { User.class }, user);

			assertThat(result, is((Object) null));
		}
		finally {
			UtilImpl.CUSTOMER = originalCustomer;
		}
	}

	@SuppressWarnings("static-method")
	@Test
	void testCreateMailLogUserUsesDefaultCustomerWhenCurrentUserIsNull() throws Exception {
		String originalCustomer = UtilImpl.CUSTOMER;
		try {
			UtilImpl.CUSTOMER = "default-customer";

			SuperUser result = (SuperUser) invokePrivate("createMailLogUser", new Class<?>[] { User.class }, new Object[] { null });

			assertThat(result.getId(), is("mailLogUser"));
			assertThat(result.getCustomerName(), is("default-customer"));
		}
		finally {
			UtilImpl.CUSTOMER = originalCustomer;
		}
	}

	@Test
	void testLogMailTimestampIsPopulated() {
		MailLogUtil.logMail(new Mail().addTo("to@skyve.org").subject("Subject").body("Body"),
				MailDispatchOutcome.sent("smtp"));

		assertEquals(1, entries.size());
		assertNotNull(entries.get(0).getTimestamp());
	}

	@Test
	void testLogMailBodyEdgeCasesProduceEntries() {
		String[] bodies = {
				"</><unclosed",
				"<b></b>",
				"<br><br><br><br>",
				"security code: 12345678",
				"verification code is= 12345678",
				"verification codeis 123456789",
				null,
				"<br class=\"x\">text",
				"before</span>after",
				"before</div extra>after"
		};
		for (String body : bodies) {
			entries.clear();
			Mail mail = new Mail().addTo("to@skyve.org").subject("Subject").body(body);
			MailLogUtil.logMail(mail, MailDispatchOutcome.sent("smtp"));
			assertEquals(1, entries.size());
		}
	}

	@SuppressWarnings("static-method")
	@Test
	void testPlainTextBodyNormalisesHtmlWhitespaceAndMasksSensitiveCode() throws Exception {
		String result = (String) invokePrivate("plainTextBody",
				new Class<?>[] { String.class },
				"<p>Hello\t there</p><br/>security code: 123456\n\n\nDone");

		assertEquals("Hello there\n\nsecurity code: ******\n\nDone", result);
	}

	@SuppressWarnings("static-method")
	@Test
	void testPlainTextBodyReturnsNullAfterFinalTrim() throws Exception {
		String result = (String) invokePrivate("plainTextBody",
				new Class<?>[] { String.class },
				"<br/> <br/>");

		assertEquals(null, result);
	}

	// ---- maskSensitiveCodes: digit-length < 4 (L323) ----

	@SuppressWarnings("static-method")
	@Test
	void testMaskSensitiveCodesTooFewDigitsNoMasking() throws Exception {
		// "security code i" — prefix matches, 'i' is not a digit so digitLength=0 < 4 → continue
		String result = (String) invokePrivate("maskSensitiveCodes",
				new Class<?>[] { String.class },
				"security code i");
		assertEquals("security code i", result);
	}

	// ---- maskSensitiveCodes: digits followed by word char (L326) ----

	@SuppressWarnings("static-method")
	@Test
	void testMaskSensitiveCodesDigitsFollowedByWordCharNoMasking() throws Exception {
		// "security code: 12345a" — 5 digits followed by 'a' (word char) → continue
		String result = (String) invokePrivate("maskSensitiveCodes",
				new Class<?>[] { String.class },
				"security code: 12345a");
		assertEquals("security code: 12345a", result);
	}

	@SuppressWarnings("static-method")
	@Test
	void testMaskSensitiveCodesIgnoresEmbeddedPrefixAndOversizedCode() throws Exception {
		String result = (String) invokePrivate("maskSensitiveCodes",
				new Class<?>[] { String.class },
				"mysecurity code: 123456 and security code: 12345678901");

		assertEquals("mysecurity code: 123456 and security code: 12345678901", result);
	}

	@SuppressWarnings("static-method")
	@Test
	void testMaskSensitiveCodesMasksCodeAtEndOfString() throws Exception {
		String result = (String) invokePrivate("maskSensitiveCodes",
				new Class<?>[] { String.class },
				"pin code=9876");

		assertEquals("pin code=****", result);
	}

	// ---- startsWithWord: end exceeds length (L343) ----

	@SuppressWarnings({ "static-method", "boxing" })
	@Test
	void testStartsWithWordEndExceedsLengthReturnsFalse() throws Exception {
		// "x" has length 1; checking for "is" (length 2) → end=2 > 1 → return false at L343
		boolean result = (boolean) invokePrivate("startsWithWord",
				new Class<?>[] { String.class, int.class, String.class },
				"x", Integer.valueOf(0), "is");
		assertFalse(result);
	}

	// ---- startsWithWord: preceded by word char (L349) ----

	@SuppressWarnings({ "static-method", "boxing" })
	@Test
	void testStartsWithWordPrecededByWordCharReturnsFalse() throws Exception {
		// start=1 is 'i' in "eis", start-1=0 is 'e' (word char) → return false at L349
		boolean result = (boolean) invokePrivate("startsWithWord",
				new Class<?>[] { String.class, int.class, String.class },
				"eis", Integer.valueOf(1), "is");
		assertFalse(result);
	}

	private static Object invokePrivate(String methodName, Class<?>[] parameterTypes, Object... args) throws Exception {
		Method method = MailLogUtil.class.getDeclaredMethod(methodName, parameterTypes);
		method.setAccessible(true);
		return method.invoke(null, args);
	}

	@SuppressWarnings("static-method")
	@Test
	void testTrimSpacesAroundNewlinesRemovesSpaceAfterNewline() throws Exception {
		String result = (String) invokePrivate("trimSpacesAroundNewlines",
				new Class<?>[] { String.class },
				"one\n two");

		assertEquals("one\ntwo", result);
	}

	@SuppressWarnings("static-method")
	@Test
	void testTrimSpacesAroundNewlinesRemovesSpaceBeforeNewline() throws Exception {
		String result = (String) invokePrivate("trimSpacesAroundNewlines",
				new Class<?>[] { String.class },
				"one \ntwo");

		assertEquals("one\ntwo", result);
	}

	@SuppressWarnings({ "static-method", "boxing" })
	@Test
	void testStartsWithWordReturnsTrueWhenFollowedByPunctuation() throws Exception {
		boolean result = (boolean) invokePrivate("startsWithWord",
				new Class<?>[] { String.class, int.class, String.class },
				"is:", Integer.valueOf(0), "is");

		assertTrue(result);
	}

	@SuppressWarnings("static-method")
	@Test
	void testCloseQuietlyIgnoresNullPersistence() {
		assertDoesNotThrow(() -> invokePrivate("closeQuietly",
				new Class<?>[] { AbstractHibernatePersistence.class },
				new Object[] { null }));
	}

	private static byte[] bytes(String value) {
		return value.getBytes(StandardCharsets.UTF_8);
	}

	private static Map<String, String> unzip(byte[] zip) throws IOException {
		assertNotNull(zip);
		Map<String, String> result = new LinkedHashMap<>();
		try (ZipInputStream in = new ZipInputStream(new ByteArrayInputStream(zip))) {
			ZipEntry entry = in.getNextEntry();
			while (entry != null) {
				result.put(entry.getName(), new String(in.readAllBytes(), StandardCharsets.UTF_8));
				entry = in.getNextEntry();
			}
		}
		return result;
	}

	private static MailLog mockMailLog() {
		MailLog log = Mockito.mock(MailLog.class);
		Mockito.when(log.getBizCustomer()).thenReturn("demo");
		Mockito.when(log.getBizModule()).thenReturn("admin");
		Mockito.when(log.getBizDocument()).thenReturn("MailLog");
		Mockito.when(log.getBizUserId()).thenReturn("user-1");
		Mockito.when(log.getBizId()).thenReturn("log-1");
		return log;
	}

	/**
	 * Records attachment puts and assigns a content ID, standing in for the configured content manager.
	 */
	public static class RecordingContentManager extends NoOpContentManager {
		static final List<AttachmentContent> puts = new ArrayList<>();
		static boolean indexed;

		@Override
		public void put(AttachmentContent content, boolean index) throws Exception {
			puts.add(content);
			indexed = index;
			content.setContentId("content-" + puts.size());
		}
	}

	/**
	 * Fails every attachment put, standing in for an unavailable content store.
	 */
	public static class FailingContentManager extends NoOpContentManager {
		@Override
		public void put(AttachmentContent content, boolean index) throws Exception {
			throw new IllegalStateException("content store unavailable");
		}
	}
}
