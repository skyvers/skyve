package org.skyve.content;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.util.Date;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

@SuppressWarnings({ "static-method", "java:S8692" }) // system clock OK
public class AttachmentContentTest {

	@Rule
	public TemporaryFolder tmp = new TemporaryFolder();

	private static AttachmentContent newAC(String attributeName) {
		return new AttachmentContent("demo", "admin", "User", null, "user1", "biz1", attributeName);
	}

	// ---- Constructor ----

	@Test
	public void testConstructorStoresFields() {
		AttachmentContent ac = new AttachmentContent("cust", "mod", "doc", "dg", "uid", "bid", "photo");
		assertEquals("cust", ac.getBizCustomer());
		assertEquals("mod", ac.getBizModule());
		assertEquals("doc", ac.getBizDocument());
		assertEquals("dg", ac.getBizDataGroupId());
		assertEquals("uid", ac.getBizUserId());
		assertEquals("bid", ac.getBizId());
		assertEquals("photo", ac.getAttributeName());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testConstructorRejectsCompoundAttributeName() {
		newAC("parent.photo");
	}

	// ---- attachment(fileName, MimeType, bytes) ----

	@Test
	public void testAttachmentMimeTypeAndBytesFluentReturn() {
		AttachmentContent ac = newAC("doc");
		AttachmentContent returned = ac.attachment("report.pdf", MimeType.pdf, "hello".getBytes());
		assertSame("attachment() must return this for fluent chaining", ac, returned);
	}

	@Test
	public void testAttachmentMimeTypeAndBytesSetsFields() {
		AttachmentContent ac = newAC("doc");
		ac.attachment("report.pdf", MimeType.pdf, "hello".getBytes());
		assertEquals("report.pdf", ac.getFileName());
		assertEquals(MimeType.pdf.toString(), ac.getContentType());
		assertEquals(MimeType.pdf, ac.getMimeType());
	}

	@Test
	public void testAttachmentNullFileNameDerivesFromMimeType() {
		AttachmentContent ac = newAC("doc");
		ac.attachment(null, MimeType.pdf, "data".getBytes());
		assertEquals("content.pdf", ac.getFileName());
		assertEquals(MimeType.pdf.toString(), ac.getContentType());
	}

	// ---- attachment(fileName, MimeType, file) ----

	@Test
	public void testAttachmentMimeTypeAndFile() throws Exception {
		File f = tmp.newFile("test.pdf");
		AttachmentContent ac = newAC("doc");
		ac.attachment("report.pdf", MimeType.pdf, f);
		assertEquals("report.pdf", ac.getFileName());
		assertEquals(MimeType.pdf.toString(), ac.getContentType());
	}

	// ---- attachment(fileName, String contentType, bytes) ----

	@Test
	public void testAttachmentStringContentTypeAndBytes() {
		AttachmentContent ac = newAC("doc");
		ac.attachment("report.pdf", "application/pdf", "data".getBytes());
		assertEquals("report.pdf", ac.getFileName());
		assertEquals("application/pdf", ac.getContentType());
	}

        @Test
        public void testAttachmentNullFileNameUnrecognisedContentTypeDefaultsToContent() {
                // covers L158: fileName==null, contentType not null but not a known MimeType
                AttachmentContent ac = newAC("doc");
                ac.attachment(null, "application/x-completely-unknown-fake-type", "data".getBytes());
                assertEquals("content", ac.getFileName());
        }

	// ---- attachment(fileName, String contentType, file) ----

	@Test
	public void testAttachmentStringContentTypeAndFile() throws Exception {
		File f = tmp.newFile("report.pdf");
		AttachmentContent ac = newAC("doc");
		ac.attachment("report.pdf", "application/pdf", f);
		assertEquals("report.pdf", ac.getFileName());
		assertEquals("application/pdf", ac.getContentType());
	}

	// ---- attachment(fileNameWithStandardSuffix, bytes) ----

	@Test
	public void testAttachmentFileNameSuffixDerivesContentType() {
		AttachmentContent ac = newAC("img");
		ac.attachment("image.png", "pngdata".getBytes());
		assertEquals("image.png", ac.getFileName());
		assertEquals(MimeType.png.toString(), ac.getContentType());
		assertEquals(MimeType.png, ac.getMimeType());
	}

	// ---- attachment(fileNameWithStandardSuffix, file) ----

	@Test
	public void testAttachmentFileWithSuffixDerivesContentType() throws Exception {
		File f = tmp.newFile("img.png");
		AttachmentContent ac = newAC("img");
		ac.attachment("img.png", f);
		assertEquals("img.png", ac.getFileName());
		assertEquals(MimeType.png.toString(), ac.getContentType());
	}

	// ---- Filename sanitisation ----

	@Test
	public void testAttachmentFilenamePrefixPathIsStripped() {
		AttachmentContent ac = newAC("doc");
		ac.attachment("/tmp/report.pdf", "data".getBytes());
		assertEquals("report.pdf", ac.getFileName());
	}

	@Test
	public void testAttachmentFilenameInvalidCharsAreStripped() {
		AttachmentContent ac = newAC("doc");
		ac.attachment("report<>.pdf", "data".getBytes());
		assertEquals("report.pdf", ac.getFileName());
	}

	// ---- getContentStream ----

	@Test
	public void testGetContentStreamFromBytes() throws Exception {
		AttachmentContent ac = newAC("doc");
		byte[] data = "hello world".getBytes();
		ac.attachment("test.txt", "text/plain", data);
		try (InputStream is = ac.getContentStream()) {
			assertArrayEquals(data, is.readAllBytes());
		}
	}

	@Test
	public void testGetContentStreamFromFile() throws Exception {
		File f = tmp.newFile("test.txt");
		try (FileOutputStream out = new FileOutputStream(f)) {
			out.write("file content".getBytes());
		}
		AttachmentContent ac = newAC("doc");
		ac.attachment("test.txt", "text/plain", f);
		try (InputStream is = ac.getContentStream()) {
			assertArrayEquals("file content".getBytes(), is.readAllBytes());
		}
	}

	@Test
	public void testGetContentStreamFromMissingFileReturnsEmpty() throws Exception {
		File f = new File("/nonexistent_path_8392/missing_9281.txt");
		AttachmentContent ac = newAC("doc");
		ac.attachment("missing.txt", "text/plain", f);
		try (InputStream is = ac.getContentStream()) {
			assertEquals(0, is.available());
		}
	}

	// ---- getContentBytes ----

	@Test
	public void testGetContentBytesFromBytes() throws Exception {
		AttachmentContent ac = newAC("doc");
		byte[] data = "hello".getBytes();
		ac.attachment("test.txt", "text/plain", data);
		assertArrayEquals(data, ac.getContentBytes());
	}

	@Test
	public void testGetContentBytesFromFile() throws Exception {
		File f = tmp.newFile("test.txt");
		try (FileOutputStream out = new FileOutputStream(f)) {
			out.write("fromfile".getBytes());
		}
		AttachmentContent ac = newAC("doc");
		ac.attachment("test.txt", "text/plain", f);
		assertArrayEquals("fromfile".getBytes(), ac.getContentBytes());
	}

	// ---- Cloning ----

	@Test
	public void testCloneForRemoteUpdate() throws Exception {
		AttachmentContent ac = newAC("doc");
		ac.attachment("test.pdf", MimeType.pdf, "content".getBytes());
		ac.setContentId("cid-123");
		ac.markup("<svg/>");
		AttachmentContent clone = ac.cloneForRemoteUpdate();

		assertEquals("test.pdf", clone.getFileName());
		assertEquals(MimeType.pdf.toString(), clone.getContentType());
		assertEquals("cid-123", clone.getContentId());
		assertEquals("<svg/>", clone.getMarkup());
		assertNotNull(clone.getContentBytes());
		assertEquals(0, clone.getContentBytes().length);
	}

	@Test
	public void testCloneNewForPut() {
		AttachmentContent ac = newAC("doc");
		ac.attachment("test.pdf", MimeType.pdf, "content".getBytes());
		ac.setContentId("cid-123");
		AttachmentContent clone = ac.cloneNewForPut();

		assertEquals("test.pdf", clone.getFileName());
		assertEquals(MimeType.pdf.toString(), clone.getContentType());
		assertNull("cloneNewForPut must not copy contentId", clone.getContentId());
	}

	// ---- markup() fluent ----

	@Test
	public void testMarkupFluentAndGetter() {
		AttachmentContent ac = newAC("img");
		AttachmentContent returned = ac.markup("<svg/>");
		assertSame(ac, returned);
		assertEquals("<svg/>", ac.getMarkup());
	}

	// ---- Setters ----

	@Test
	public void testSetters() {
		AttachmentContent ac = newAC("doc");

		ac.setAttributeName("newAttr");
		assertEquals("newAttr", ac.getAttributeName());

		ac.setContentId("cid-999");
		assertEquals("cid-999", ac.getContentId());

		ac.setFileName("new.pdf");
		assertEquals("new.pdf", ac.getFileName());

		ac.setContentType("application/pdf");
		assertEquals("application/pdf", ac.getContentType());

		Date d = new Date();
		ac.setLastModified(d);
		assertEquals(d, ac.getLastModified());

		ac.setMarkup("<path/>");
		assertEquals("<path/>", ac.getMarkup());

		ac.setBizCustomer("newCust");
		assertEquals("newCust", ac.getBizCustomer());

		ac.setBizModule("newMod");
		assertEquals("newMod", ac.getBizModule());

		ac.setBizDocument("newDoc");
		assertEquals("newDoc", ac.getBizDocument());

		ac.setBizDataGroupId("dgid");
		assertEquals("dgid", ac.getBizDataGroupId());

		ac.setBizUserId("uid");
		assertEquals("uid", ac.getBizUserId());

		ac.setExternalAbsoluteFilePath("/tmp/test.pdf");
		assertEquals("/tmp/test.pdf", ac.getExternalAbsoluteFilePath());
	}

	// ---- externalAbsoluteFilePath() fluent ----

	@Test(expected = IllegalArgumentException.class)
	public void testExternalAbsoluteFilePathFluentThrowsIfNotExists() {
		AttachmentContent ac = newAC("doc");
		ac.externalAbsoluteFilePath("/nonexistent_path_8392/missing_file_9281.pdf");
	}

	@Test
	public void testExternalAbsoluteFilePathFluentWithExistingFile() throws Exception {
		File f = tmp.newFile("test.pdf");
		AttachmentContent ac = newAC("doc");
		ac.externalAbsoluteFilePath(f.getAbsolutePath());
		assertEquals(f.getAbsolutePath(), ac.getExternalAbsoluteFilePath());
		assertEquals("test.pdf", ac.getFileName());
	}

	// ---- getMimeType() null-safety ----

	@Test
	public void testGetMimeTypeNullWhenNoContentType() {
		AttachmentContent ac = newAC("doc");
		assertNull(ac.getMimeType());
	}

        @Test
        public void testAttachmentNullFileNameAndNullContentTypeDefaultsToContent() throws Exception {
                AttachmentContent ac = newAC("doc");
                // Call attachment(String, String, byte[]) with null fileName, null contentType
                java.lang.reflect.Method m = AttachmentContent.class.getDeclaredMethod(
                        "internalAttachment", String.class, String.class, byte[].class, java.io.File.class);
                m.setAccessible(true);
                m.invoke(ac, null, null, new byte[]{1}, null);
                assertEquals("content", ac.getFileName());
        }

        @Test
        public void testSerializationRoundTrip() throws Exception {
                AttachmentContent ac = newAC("doc");
                ac.attachment("hello.txt", MimeType.plain, "text content".getBytes());
                // Serialise then deserialise — exercises writeReplace
                java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
                try (java.io.ObjectOutputStream oos = new java.io.ObjectOutputStream(baos)) {
                        oos.writeObject(ac);
                }
                byte[] serialised = baos.toByteArray();
                try (java.io.ObjectInputStream ois = new java.io.ObjectInputStream(
                        new java.io.ByteArrayInputStream(serialised))) {
                        AttachmentContent restored = (AttachmentContent) ois.readObject();
                        assertEquals("hello.txt", restored.getFileName());
                }
        }
}
