package org.skyve.content;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Date;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@SuppressWarnings({ "static-method", "java:S8692" }) // system clock OK
class AttachmentContentTest {

	@TempDir
	Path tmp;

	private static AttachmentContent newAC(String attributeName) {
		return new AttachmentContent("demo", "admin", "User", null, "user1", "biz1", attributeName);
	}

	// ---- Constructor ----

	@Test
	void testConstructorStoresFields() {
		AttachmentContent ac = new AttachmentContent("cust", "mod", "doc", "dg", "uid", "bid", "photo");
		assertEquals("cust", ac.getBizCustomer());
		assertEquals("mod", ac.getBizModule());
		assertEquals("doc", ac.getBizDocument());
		assertEquals("dg", ac.getBizDataGroupId());
		assertEquals("uid", ac.getBizUserId());
		assertEquals("bid", ac.getBizId());
		assertEquals("photo", ac.getAttributeName());
	}

	@Test
	void testConstructorRejectsCompoundAttributeName() {
		assertThrows(IllegalArgumentException.class, () -> newAC("parent.photo"));
	}

	// ---- attachment(fileName, MimeType, bytes) ----

	@Test
	void testAttachmentMimeTypeAndBytesFluentReturn() {
		AttachmentContent ac = newAC("doc");
		AttachmentContent returned = ac.attachment("report.pdf", MimeType.pdf, "hello".getBytes());
		assertSame(ac, returned, "attachment() must return this for fluent chaining");
	}

	@Test
	void testAttachmentMimeTypeAndBytesSetsFields() {
		AttachmentContent ac = newAC("doc");
		ac.attachment("report.pdf", MimeType.pdf, "hello".getBytes());
		assertEquals("report.pdf", ac.getFileName());
		assertEquals(MimeType.pdf.toString(), ac.getContentType());
		assertEquals(MimeType.pdf, ac.getMimeType());
	}

	@Test
	void testAttachmentNullFileNameDerivesFromMimeType() {
		AttachmentContent ac = newAC("doc");
		ac.attachment(null, MimeType.pdf, "data".getBytes());
		assertEquals("content.pdf", ac.getFileName());
		assertEquals(MimeType.pdf.toString(), ac.getContentType());
	}

	// ---- attachment(fileName, MimeType, file) ----

	@Test
	void testAttachmentMimeTypeAndFile() throws Exception {
		File f = Files.createFile(tmp.resolve("test.pdf")).toFile();
		AttachmentContent ac = newAC("doc");
		ac.attachment("report.pdf", MimeType.pdf, f);
		assertEquals("report.pdf", ac.getFileName());
		assertEquals(MimeType.pdf.toString(), ac.getContentType());
	}

	// ---- attachment(fileName, String contentType, bytes) ----

	@Test
	void testAttachmentStringContentTypeAndBytes() {
		AttachmentContent ac = newAC("doc");
		ac.attachment("report.pdf", "application/pdf", "data".getBytes());
		assertEquals("report.pdf", ac.getFileName());
		assertEquals("application/pdf", ac.getContentType());
	}

        @Test
        void testAttachmentNullFileNameUnrecognisedContentTypeDefaultsToContent() {
                // covers L158: fileName==null, contentType not null but not a known MimeType
                AttachmentContent ac = newAC("doc");
                ac.attachment(null, "application/x-completely-unknown-fake-type", "data".getBytes());
                assertEquals("content", ac.getFileName());
        }

	// ---- attachment(fileName, String contentType, file) ----

	@Test
	void testAttachmentStringContentTypeAndFile() throws Exception {
		File f = Files.createFile(tmp.resolve("report.pdf")).toFile();
		AttachmentContent ac = newAC("doc");
		ac.attachment("report.pdf", "application/pdf", f);
		assertEquals("report.pdf", ac.getFileName());
		assertEquals("application/pdf", ac.getContentType());
	}

	// ---- attachment(fileNameWithStandardSuffix, bytes) ----

	@Test
	void testAttachmentFileNameSuffixDerivesContentType() {
		AttachmentContent ac = newAC("img");
		ac.attachment("image.png", "pngdata".getBytes());
		assertEquals("image.png", ac.getFileName());
		assertEquals(MimeType.png.toString(), ac.getContentType());
		assertEquals(MimeType.png, ac.getMimeType());
	}

	// ---- attachment(fileNameWithStandardSuffix, file) ----

	@Test
	void testAttachmentFileWithSuffixDerivesContentType() throws Exception {
		File f = Files.createFile(tmp.resolve("img.png")).toFile();
		AttachmentContent ac = newAC("img");
		ac.attachment("img.png", f);
		assertEquals("img.png", ac.getFileName());
		assertEquals(MimeType.png.toString(), ac.getContentType());
	}

	// ---- Filename sanitisation ----

	@Test
	void testAttachmentFilenamePrefixPathIsStripped() {
		AttachmentContent ac = newAC("doc");
		ac.attachment("/tmp/report.pdf", "data".getBytes());
		assertEquals("report.pdf", ac.getFileName());
	}

	@Test
	void testAttachmentFilenameInvalidCharsAreStripped() {
		AttachmentContent ac = newAC("doc");
		ac.attachment("report<>.pdf", "data".getBytes());
		assertEquals("report.pdf", ac.getFileName());
	}

	// ---- getContentStream ----

	@Test
	void testGetContentStreamFromBytes() throws Exception {
		AttachmentContent ac = newAC("doc");
		byte[] data = "hello world".getBytes();
		ac.attachment("test.txt", "text/plain", data);
		try (InputStream is = ac.getContentStream()) {
			assertArrayEquals(data, is.readAllBytes());
		}
	}

	@Test
	void testGetContentLengthFromBytes() {
		AttachmentContent ac = newAC("doc");
		ac.attachment("test.txt", "text/plain", "hello world".getBytes());

		assertEquals(11L, ac.getContentLength());
	}

	@Test
	void testGetContentStreamFromFile() throws Exception {
		File f = Files.createFile(tmp.resolve("test.txt")).toFile();
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
	void testGetContentLengthFromFile() throws Exception {
		File f = Files.createFile(tmp.resolve("length.txt")).toFile();
		try (FileOutputStream out = new FileOutputStream(f)) {
			out.write("file content".getBytes());
		}
		AttachmentContent ac = newAC("doc");
		ac.attachment("length.txt", "text/plain", f);

		assertEquals(f.length(), ac.getContentLength());
	}

	@Test
	void testGetContentLengthWithoutAttachment() {
		AttachmentContent ac = newAC("doc");

		assertEquals(-1L, ac.getContentLength());
	}

	@Test
	void testGetContentStreamFromMissingFileReturnsEmpty() throws Exception {
		File f = new File("/nonexistent_path_8392/missing_9281.txt");
		AttachmentContent ac = newAC("doc");
		ac.attachment("missing.txt", "text/plain", f);
		try (InputStream is = ac.getContentStream()) {
			assertEquals(0, is.available());
		}
	}

	// ---- getContentBytes ----

	@Test
	void testGetContentBytesFromBytes() throws Exception {
		AttachmentContent ac = newAC("doc");
		byte[] data = "hello".getBytes();
		ac.attachment("test.txt", "text/plain", data);
		assertArrayEquals(data, ac.getContentBytes());
	}

	@Test
	void testGetContentBytesFromFile() throws Exception {
		File f = Files.createFile(tmp.resolve("test.txt")).toFile();
		try (FileOutputStream out = new FileOutputStream(f)) {
			out.write("fromfile".getBytes());
		}
		AttachmentContent ac = newAC("doc");
		ac.attachment("test.txt", "text/plain", f);
		assertArrayEquals("fromfile".getBytes(), ac.getContentBytes());
	}

	// ---- Cloning ----

	@Test
	void testCloneForRemoteUpdate() throws Exception {
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
	void testCloneNewForPut() {
		AttachmentContent ac = newAC("doc");
		ac.attachment("test.pdf", MimeType.pdf, "content".getBytes());
		ac.setContentId("cid-123");
		AttachmentContent clone = ac.cloneNewForPut();

		assertEquals("test.pdf", clone.getFileName());
		assertEquals(MimeType.pdf.toString(), clone.getContentType());
		assertNull(clone.getContentId(), "cloneNewForPut must not copy contentId");
	}

	// ---- markup() fluent ----

	@Test
	void testMarkupFluentAndGetter() {
		AttachmentContent ac = newAC("img");
		AttachmentContent returned = ac.markup("<svg/>");
		assertSame(ac, returned);
		assertEquals("<svg/>", ac.getMarkup());
	}

	// ---- Setters ----

	@Test
	void testSetters() {
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

	@Test
	void testExternalAbsoluteFilePathFluentThrowsIfNotExists() {
		AttachmentContent ac = newAC("doc");
		assertThrows(IllegalArgumentException.class, () -> ac.externalAbsoluteFilePath("/nonexistent_path_8392/missing_file_9281.pdf"));
	}

	@Test
	void testExternalAbsoluteFilePathFluentWithExistingFile() throws Exception {
		File f = Files.createFile(tmp.resolve("test.pdf")).toFile();
		AttachmentContent ac = newAC("doc");
		ac.externalAbsoluteFilePath(f.getAbsolutePath());
		assertEquals(f.getAbsolutePath(), ac.getExternalAbsoluteFilePath());
		assertEquals("test.pdf", ac.getFileName());
	}

	// ---- getMimeType() null-safety ----

	@Test
	void testGetMimeTypeNullWhenNoContentType() {
		AttachmentContent ac = newAC("doc");
		assertNull(ac.getMimeType());
	}

        @Test
        void testAttachmentNullFileNameAndNullContentTypeDefaultsToContent() throws Exception {
                AttachmentContent ac = newAC("doc");
                // Call attachment(String, String, byte[]) with null fileName, null contentType
                java.lang.reflect.Method m = AttachmentContent.class.getDeclaredMethod(
                        "internalAttachment", String.class, String.class, byte[].class, java.io.File.class);
                m.setAccessible(true);
                m.invoke(ac, null, null, new byte[]{1}, null);
                assertEquals("content", ac.getFileName());
        }

        @Test
        void testSerializationRoundTrip() throws Exception {
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
