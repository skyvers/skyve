package org.skyve.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;

class MailTest {

	// ---- addTo(Collection) ----

	@Test
	@SuppressWarnings("static-method")
	void testAddToCollectionAddsAddresses() {
		Mail mail = new Mail();
		List<String> recipients = Arrays.asList("a@example.com", "b@example.com");
		Mail result = mail.addTo(recipients);
		assertSame(mail, result);
		Set<String> addresses = mail.getRecipientEmailAddresses();
		assertEquals(2, addresses.size());
		assertTrue(addresses.contains("a@example.com"));
		assertTrue(addresses.contains("b@example.com"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testAddToCollectionNullIsNoOp() {
		Mail mail = new Mail();
		Mail result = mail.addTo((java.util.Collection<String>) null);
		assertSame(mail, result);
		assertTrue(mail.getRecipientEmailAddresses().isEmpty());
	}

	// ---- addTo(String...) ----

	@Test
	@SuppressWarnings("static-method")
	void testAddToVarargsAddsAddresses() {
		Mail mail = new Mail();
		Mail result = mail.addTo("x@example.com", "y@example.com");
		assertSame(mail, result);
		Set<String> addresses = mail.getRecipientEmailAddresses();
		assertEquals(2, addresses.size());
		assertTrue(addresses.contains("x@example.com"));
		assertTrue(addresses.contains("y@example.com"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testAddToVarargsNullArrayIsNoOp() {
		Mail mail = new Mail();
		Mail result = mail.addTo((String[]) null);
		assertSame(mail, result);
		assertTrue(mail.getRecipientEmailAddresses().isEmpty());
	}

	// ---- addCC(Collection) ----

	@Test
	@SuppressWarnings("static-method")
	void testAddCCCollectionAddsAddresses() {
		Mail mail = new Mail();
		List<String> ccs = Arrays.asList("cc1@example.com", "cc2@example.com");
		Mail result = mail.addCC(ccs);
		assertSame(mail, result);
		Set<String> addresses = mail.getCcEmailAddresses();
		assertEquals(2, addresses.size());
		assertTrue(addresses.contains("cc1@example.com"));
		assertTrue(addresses.contains("cc2@example.com"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testAddCCCollectionNullIsNoOp() {
		Mail mail = new Mail();
		Mail result = mail.addCC((java.util.Collection<String>) null);
		assertSame(mail, result);
		assertTrue(mail.getCcEmailAddresses().isEmpty());
	}

	// ---- addCC(String...) ----

	@Test
	@SuppressWarnings("static-method")
	void testAddCCVarargsAddsAddresses() {
		Mail mail = new Mail();
		Mail result = mail.addCC("cc3@example.com", "cc4@example.com");
		assertSame(mail, result);
		Set<String> addresses = mail.getCcEmailAddresses();
		assertEquals(2, addresses.size());
		assertTrue(addresses.contains("cc3@example.com"));
		assertTrue(addresses.contains("cc4@example.com"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testAddCCVarargsNullArrayIsNoOp() {
		Mail mail = new Mail();
		Mail result = mail.addCC((String[]) null);
		assertSame(mail, result);
		assertTrue(mail.getCcEmailAddresses().isEmpty());
	}

	// ---- addBCC(Collection) ----

	@Test
	@SuppressWarnings("static-method")
	void testAddBCCCollectionAddsAddresses() {
		Mail mail = new Mail();
		List<String> bccs = Arrays.asList("bcc1@example.com", "bcc2@example.com");
		Mail result = mail.addBCC(bccs);
		assertSame(mail, result);
		Set<String> addresses = mail.getBccEmailAddresses();
		assertEquals(2, addresses.size());
		assertTrue(addresses.contains("bcc1@example.com"));
		assertTrue(addresses.contains("bcc2@example.com"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testAddBCCCollectionNullIsNoOp() {
		Mail mail = new Mail();
		Mail result = mail.addBCC((java.util.Collection<String>) null);
		assertSame(mail, result);
		assertTrue(mail.getBccEmailAddresses().isEmpty());
	}

	// ---- addBCC(String...) ----

	@Test
	@SuppressWarnings("static-method")
	void testAddBCCVarargsAddsAddresses() {
		Mail mail = new Mail();
		Mail result = mail.addBCC("bcc3@example.com", "bcc4@example.com");
		assertSame(mail, result);
		Set<String> addresses = mail.getBccEmailAddresses();
		assertEquals(2, addresses.size());
		assertTrue(addresses.contains("bcc3@example.com"));
		assertTrue(addresses.contains("bcc4@example.com"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testAddBCCVarargsNullArrayIsNoOp() {
		Mail mail = new Mail();
		Mail result = mail.addBCC((String[]) null);
		assertSame(mail, result);
		assertTrue(mail.getBccEmailAddresses().isEmpty());
	}

	// ---- html() ----

	@Test
	@SuppressWarnings("static-method")
	void testHtmlSetsContentTypeToHtml() {
		Mail mail = new Mail().textPlain();
		assertEquals(MimeType.plain, mail.getContentType());
		Mail result = mail.html();
		assertSame(mail, result);
		assertEquals(MimeType.html, mail.getContentType());
	}

	// ---- attach(Collection<MailAttachment>) ----

	@Test
	@SuppressWarnings("static-method")
	void testAttachCollectionAddsAttachments() {
		Mail mail = new Mail();
		MailAttachment a1 = new MailAttachment("file1.txt", new byte[] {1}, "text/plain");
		MailAttachment a2 = new MailAttachment("file2.txt", new byte[] {2}, "text/plain");
		Mail result = mail.attach(Arrays.asList(a1, a2));
		assertSame(mail, result);
		List<MailAttachment> attachments = mail.getAttachments();
		assertEquals(2, attachments.size());
		assertTrue(attachments.contains(a1));
		assertTrue(attachments.contains(a2));
	}

	@Test
	@SuppressWarnings("static-method")
	void testAttachCollectionNullIsNoOp() {
		Mail mail = new Mail();
		Mail result = mail.attach((java.util.Collection<MailAttachment>) null);
		assertSame(mail, result);
		assertTrue(mail.getAttachments().isEmpty());
	}

	// ---- attach(String, byte[], MimeType) ----

	@Test
	@SuppressWarnings("static-method")
	void testAttachByteArrayWithMimeType() {
		Mail mail = new Mail();
		byte[] data = {10, 20, 30};
		Mail result = mail.attach("report.pdf", data, MimeType.pdf);
		assertSame(mail, result);
		List<MailAttachment> attachments = mail.getAttachments();
		assertEquals(1, attachments.size());
		MailAttachment att = attachments.get(0);
		assertEquals("report.pdf", att.getAttachmentFileName());
		assertEquals(MimeType.pdf.toString(), att.getAttachmentContentType());
	}

	// ---- header(String, String) ----

	@Test
	@SuppressWarnings("static-method")
	void testHeaderSingleKeyValueAddsToHeaders() {
		Mail mail = new Mail();
		Mail result = mail.header("X-Custom", "value1");
		assertSame(mail, result);
		Map<String, String> headers = mail.getHeaders();
		assertEquals("value1", headers.get("X-Custom"));
	}

	// ---- unsent() ----

	@Test
	@SuppressWarnings("static-method")
	void testUnsentAddsXUnsentHeader() {
		Mail mail = new Mail();
		Mail result = mail.unsent();
		assertSame(mail, result);
		Map<String, String> headers = mail.getHeaders();
		assertEquals("1", headers.get("X-Unsent"));
	}

	// ---- attach(MailAttachment...) varargs ----

	@Test
	@SuppressWarnings("static-method")
	void testAttachVarargsAddsAttachments() {
		Mail mail = new Mail();
		MailAttachment a1 = new MailAttachment("v1.txt", new byte[] {1}, "text/plain");
		MailAttachment a2 = new MailAttachment("v2.txt", new byte[] {2}, "text/plain");
		Mail result = mail.attach(a1, a2);
		assertSame(mail, result);
		List<MailAttachment> attachments = mail.getAttachments();
		assertEquals(2, attachments.size());
		assertTrue(attachments.contains(a1));
		assertTrue(attachments.contains(a2));
	}

	@Test
	@SuppressWarnings("static-method")
	void testAttachVarargsNullArrayIsNoOp() {
		Mail mail = new Mail();
		Mail result = mail.attach((MailAttachment[]) null);
		assertSame(mail, result);
		assertTrue(mail.getAttachments().isEmpty());
	}

	// ---- header(Map) ----

	@Test
	@SuppressWarnings("static-method")
	void testHeaderMapAddsMultipleHeaders() {
		Mail mail = new Mail();
		Map<String, String> customHeaders = new LinkedHashMap<>();
		customHeaders.put("H1", "V1");
		customHeaders.put("H2", "V2");
		Mail result = mail.header(customHeaders);
		assertSame(mail, result);
		Map<String, String> headers = mail.getHeaders();
		assertEquals("V1", headers.get("H1"));
		assertEquals("V2", headers.get("H2"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testHeaderMapNullIsNoOp() {
		Mail mail = new Mail();
		Mail result = mail.header((Map<String, String>) null);
		assertSame(mail, result);
		assertTrue(mail.getHeaders().isEmpty());
	}
}
