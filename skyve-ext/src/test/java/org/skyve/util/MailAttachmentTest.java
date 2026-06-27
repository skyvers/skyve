package org.skyve.util;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;

@SuppressWarnings("static-method")
public class MailAttachmentTest {
	private Class<? extends AbstractContentManager> originalContentManagerClass;

	@Before
	public void setup() {
		originalContentManagerClass = AbstractContentManager.IMPLEMENTATION_CLASS;
		RecordingContentManager.attachments.clear();
		RecordingContentManager.throwOnGet = false;
	}

	@After
	public void teardown() {
		AbstractContentManager.IMPLEMENTATION_CLASS = originalContentManagerClass;
		RecordingContentManager.attachments.clear();
		RecordingContentManager.throwOnGet = false;
	}

	@Test
	public void testDefaultConstructorCreatesEmptyAttachment() {
		MailAttachment a = new MailAttachment();
		assertNull(a.getAttachmentFileName());
		assertNull(a.getAttachment());
		assertNull(a.getAttachmentContentType());
	}

	@Test
	public void testThreeArgConstructorWithStringContentType() {
		byte[] data = "hello".getBytes();
		MailAttachment a = new MailAttachment("report.pdf", data, "application/pdf");
		assertEquals("report.pdf", a.getAttachmentFileName());
		assertArrayEquals(data, a.getAttachment());
		assertEquals("application/pdf", a.getAttachmentContentType());
	}

	@Test
	public void testThreeArgConstructorWithMimeType() {
		byte[] data = "pngdata".getBytes();
		MailAttachment a = new MailAttachment("image.png", data, MimeType.png);
		assertEquals("image.png", a.getAttachmentFileName());
		assertArrayEquals(data, a.getAttachment());
		assertEquals(MimeType.png.toString(), a.getAttachmentContentType());
	}

	@Test
	public void testSetAttachmentFileName() {
		MailAttachment a = new MailAttachment();
		a.setAttachmentFileName("doc.pdf");
		assertEquals("doc.pdf", a.getAttachmentFileName());
	}

	@Test
	public void testSetAttachment() {
		MailAttachment a = new MailAttachment();
		byte[] data = "data".getBytes();
		a.setAttachment(data);
		assertArrayEquals(data, a.getAttachment());
	}

	@Test
	public void testSetAttachmentContentType() {
		MailAttachment a = new MailAttachment();
		a.setAttachmentContentType("text/plain");
		assertEquals("text/plain", a.getAttachmentContentType());
	}

	@Test
	public void testSetAttachmentMimeType() {
		MailAttachment a = new MailAttachment();
		a.setAttachmentMimeType(MimeType.pdf);
		assertEquals(MimeType.pdf.toString(), a.getAttachmentContentType());
	}

	@Test
	public void testSetAttachmentMimeTypeOverwritesStringContentType() {
		MailAttachment a = new MailAttachment("f.txt", "data".getBytes(), "text/plain");
		a.setAttachmentMimeType(MimeType.pdf);
		assertEquals(MimeType.pdf.toString(), a.getAttachmentContentType());
		assertNotNull(a.getAttachmentFileName());
	}

	@Test
	public void testContentConstructorLoadsAttachment() {
		AbstractContentManager.IMPLEMENTATION_CLASS = RecordingContentManager.class;
		byte[] bytes = "content".getBytes(StandardCharsets.UTF_8);
		RecordingContentManager.attachments.put("content-1", attachment("report.txt", MimeType.plain, bytes));

		MailAttachment a = new MailAttachment("content-1");

		assertEquals("report.txt", a.getAttachmentFileName());
		assertArrayEquals(bytes, a.getAttachment());
		assertEquals(MimeType.plain.toString(), a.getAttachmentContentType());
	}

	@Test
	public void testNamedContentConstructorOverridesAttachmentFileName() {
		AbstractContentManager.IMPLEMENTATION_CLASS = RecordingContentManager.class;
		byte[] bytes = "pdf".getBytes(StandardCharsets.UTF_8);
		RecordingContentManager.attachments.put("content-2", attachment("original.pdf", MimeType.pdf, bytes));

		MailAttachment a = new MailAttachment("invoice.pdf", "content-2");

		assertEquals("invoice.pdf", a.getAttachmentFileName());
		assertArrayEquals(bytes, a.getAttachment());
		assertEquals(MimeType.pdf.toString(), a.getAttachmentContentType());
	}

	@Test
	public void testContentConstructorThrowsDomainExceptionWhenAttachmentMissing() {
		AbstractContentManager.IMPLEMENTATION_CLASS = RecordingContentManager.class;

		DomainException e = assertThrows(DomainException.class, () -> new MailAttachment("missing"));

		assertTrue(e.getMessage().contains("can't be retrieved"));
	}

	@Test
	public void testContentConstructorWrapsUnexpectedContentManagerFailure() {
		AbstractContentManager.IMPLEMENTATION_CLASS = RecordingContentManager.class;
		RecordingContentManager.throwOnGet = true;

		DomainException e = assertThrows(DomainException.class, () -> new MailAttachment("content-3"));

		assertEquals("Could not get the content to attach", e.getMessage());
		assertSame(IllegalStateException.class, e.getCause().getClass());
	}

	private static AttachmentContent attachment(String fileName, MimeType mimeType, byte[] bytes) {
		return new AttachmentContent("demo", "admin", "Communication", null, "user", "biz", "attachment")
				.attachment(fileName, mimeType, bytes);
	}

	public static class RecordingContentManager extends NoOpContentManager {
		static final Map<String, AttachmentContent> attachments = new HashMap<>();
		static boolean throwOnGet;

		@Override
		public AttachmentContent getAttachment(String contentId) throws Exception {
			if (throwOnGet) {
				throw new IllegalStateException("content lookup failed");
			}
			return attachments.get(contentId);
		}
	}
}
