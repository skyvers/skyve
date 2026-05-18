package org.skyve.util;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.skyve.content.MimeType;

@SuppressWarnings("static-method")
public class MailAttachmentTest {

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
}
