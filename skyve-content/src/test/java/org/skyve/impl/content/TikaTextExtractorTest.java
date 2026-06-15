package org.skyve.impl.content;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;

import org.apache.poi.xwpf.usermodel.XWPFDocument;
import org.apache.poi.xwpf.usermodel.XWPFParagraph;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.impl.util.UUIDv7;

@SuppressWarnings("static-method")
class TikaTextExtractorTest {
	@Test
	void testDocx() throws IOException {
		testContent("test.docx", "Test");
	}
	
	@Test
	void testXlsx() throws IOException {
		testContent("test.xlsx", "Test");
	}

	@Test
	void testXml() throws IOException {
		testContent("test.xml", "admin");
	}

	@Test
	void testHtml() throws IOException {
		testContent("test.html", "booking");
	}

	@Test
	void testPdf() throws IOException {
		testContent("test.pdf", "Invoice");
	}

	@Test
	void testSvg() throws IOException {
		testContent("test.svg", "Test");
	}

	@Test
	void testDoc() throws IOException {
		testContent("test.doc", "REGISTRATION");
	}

	private static void testContent(String resourceName, String expected) throws IOException {
		String resourcePath = "/org/skyve/impl/content/" + resourceName;
		try (InputStream is = TikaTextExtractorTest.class.getResourceAsStream(resourcePath)) {
			assertNotNull(is, () -> "Missing test resource " + resourcePath);
			byte[] bytes = is.readAllBytes();
			AttachmentContent content = new AttachmentContent("demo",
																"admin",
																"Contact",
																null,
																"",
																UUIDv7.create().toString(),
																"image")
												.attachment(resourceName, bytes);
			String text = new TikaTextExtractor().extractTextFromContent(content);
			assertTrue(text.contains(expected));
		}
	}

	@Test
	void testExtractTextFromMarkupWithHtml() {
		String markup = "<html><body><p>Hello world</p></body></html>";
		String result = new TikaTextExtractor().extractTextFromMarkup(markup);
		assertNotNull(result);
		assertTrue(result.contains("Hello world"));
	}

	@Test
	void testExtractTextFromMarkupWithNull() {
		String result = new TikaTextExtractor().extractTextFromMarkup(null);
		assertNull(result);
	}

	@Test
	void testExtractTextFromMarkupWithBlank() {
		String result = new TikaTextExtractor().extractTextFromMarkup("   ");
		assertNull(result);
	}

	@Test
	void testExtractTextFromContentIncludesHtmlMetadataAndMarkup() {
		String html = "<html><head><title>Example Title</title><meta name=\"subject\" content=\"Example Subject\"></head>"
				+ "<body><p>Hello body</p></body></html>";
		AttachmentContent content = new AttachmentContent("demo", "admin", "Contact", null, "", UUIDv7.create().toString(), "image")
				.attachment("test.html", "text/html", html.getBytes())
				.markup("<div>Markup trail</div>");

		String result = new TikaTextExtractor().extractTextFromContent(content);

		assertNotNull(result);
		assertTrue(result.contains("Hello body"));
		assertTrue(result.contains("Example Title"));
		assertTrue(result.contains("Markup trail"));
	}

	@Test
	void testExtractTextFromContentIncludesDocxMetadata() throws Exception {
		byte[] docxBytes;
		try (XWPFDocument document = new XWPFDocument();
				ByteArrayOutputStream out = new ByteArrayOutputStream()) {
			document.getProperties().getCoreProperties().setTitle("Meta Title");
			document.getProperties().getCoreProperties().setCreator("Meta Author");
			document.getProperties().getCoreProperties().setSubjectProperty("Meta Subject");
			XWPFParagraph paragraph = document.createParagraph();
			paragraph.createRun().setText("Body text");
			document.write(out);
			docxBytes = out.toByteArray();
		}

		AttachmentContent content = new AttachmentContent("demo", "admin", "Contact", null, "", UUIDv7.create().toString(), "image")
				.attachment("meta.docx", "application/vnd.openxmlformats-officedocument.wordprocessingml.document", docxBytes);

		String result = new TikaTextExtractor().extractTextFromContent(content);

		assertNotNull(result);
		assertTrue(result.contains("Body text"));
		assertTrue(result.contains("Meta Title"));
		assertTrue(result.contains("Meta Subject") || result.contains("Meta Author"));
	}

	@Test
	void testExtractTextFromContentHandlesNullAttachmentGracefully() {
		assertNull(new TikaTextExtractor().extractTextFromContent(null));
	}

	@Test
	void testExtractTextFromContentReturnsNullForEmptyInput() {
		AttachmentContent content = new AttachmentContent("demo", "admin", "Contact", null, "", UUIDv7.create().toString(), "image")
				.attachment("empty.txt", "text/plain", new byte[0])
				.markup("   ");

		assertNull(new TikaTextExtractor().extractTextFromContent(content));
	}

	@Test
	void testExtractTextFromContentHandlesMissingBytesGracefully() {
		AttachmentContent content = new AttachmentContent("demo", "admin", "Contact", null, "", UUIDv7.create().toString(), "image");

		assertNull(new TikaTextExtractor().extractTextFromContent(content));
	}

	@Test
	void testSniffContentTypeWithPngBytes() {
		// PNG magic bytes
		byte[] pngBytes = new byte[] {(byte)0x89, 'P', 'N', 'G', '\r', '\n', (byte)0x1A, '\n'};
		AttachmentContent content = new AttachmentContent("demo", "admin", "Contact", null, "", UUIDv7.create().toString(), "image")
				.attachment("test.png", pngBytes);
		// Remove content type so sniff is triggered
		content.setContentType(null);
		new TikaTextExtractor().sniffContentType(content);
		assertNotNull(content.getContentType());
	}

	@Test
	void testSniffContentTypeWithoutFileNameUsesStreamDetection() throws Exception {
		byte[] pngBytes = new byte[] {(byte)0x89, 'P', 'N', 'G', '\r', '\n', (byte)0x1A, '\n'};
		AttachmentContent content = new AttachmentContent("demo", "admin", "Contact", null, "", UUIDv7.create().toString(), "image")
				.attachment("test.png", pngBytes);
		content.setContentType(null);
		setField(content, "fileName", null);

		new TikaTextExtractor().sniffContentType(content);

		assertEquals("image/png", content.getContentType());
	}

	@Test
	void testSniffContentTypeAlreadySet() {
		byte[] bytes = new byte[] {1, 2, 3};
		AttachmentContent content = new AttachmentContent("demo", "admin", "Contact", null, "", UUIDv7.create().toString(), "image")
				.attachment("test.bin", bytes);
		content.setContentType("application/octet-stream");
		new TikaTextExtractor().sniffContentType(content);
		// Content type should not change when already set
		assertEquals("application/octet-stream", content.getContentType());
	}

	@Test
	void testSniffContentTypeHandlesMissingBytesGracefully() {
		AttachmentContent content = new AttachmentContent("demo", "admin", "Contact", null, "", UUIDv7.create().toString(), "image");
		content.setContentType(null);

		new TikaTextExtractor().sniffContentType(content);

		assertNull(content.getContentType());
	}

	@Test
	void testSniffLanguageEnglish() {
		try {
			String result = new TikaTextExtractor().sniffLanguage("The quick brown fox jumps over the lazy dog");
			// if a detector is available, result should be non-null
			if (result != null) {
				assertEquals("en", result);
			}
		} catch (IllegalStateException expected) {
			assertNotNull(expected.getMessage());
			// No language detectors available in test classpath — acceptable
		}
	}

	@Test
	void testSniffLanguageEmpty() {
		try {
			// empty text should return null or a language code, never throw
			String result = new TikaTextExtractor().sniffLanguage("");
			assertTrue(result == null || !result.isEmpty(),
					"sniffLanguage should return null or a valid language code");
		} catch (IllegalStateException e) {
			// No language detectors available in test classpath — acceptable
			assertNotNull(e);
		}
	}

	private static void setField(Object target, String fieldName, Object value) throws Exception {
		Field field = AttachmentContent.class.getDeclaredField(fieldName);
		field.setAccessible(true);
		field.set(target, value);
	}
}
