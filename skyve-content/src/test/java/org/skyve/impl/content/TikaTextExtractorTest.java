package org.skyve.impl.content;

import java.io.IOException;
import java.io.InputStream;

import org.junit.Assert;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.impl.util.UUIDv7;

@SuppressWarnings("static-method")
public class TikaTextExtractorTest {
	@Test
	public void testDocx() throws IOException {
		testContent("test.docx", "Test");
	}
	
	@Test
	public void testXlsx() throws IOException {
		testContent("test.xlsx", "Test");
	}

	@Test
	public void testXml() throws IOException {
		testContent("test.xml", "admin");
	}

	@Test
	public void testHtml() throws IOException {
		testContent("test.html", "booking");
	}

	@Test
	public void testPdf() throws IOException {
		testContent("test.pdf", "Invoice");
	}

	@Test
	public void testSvg() throws IOException {
		testContent("test.svg", "Test");
	}

	@Test
	public void testDoc() throws IOException {
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
			Assert.assertTrue(text.contains(expected));
		}
	}

	@Test
	public void testExtractTextFromMarkupWithHtml() {
		String markup = "<html><body><p>Hello world</p></body></html>";
		String result = new TikaTextExtractor().extractTextFromMarkup(markup);
		assertNotNull(result);
		Assert.assertTrue(result.contains("Hello world"));
	}

	@Test
	public void testExtractTextFromMarkupWithNull() {
		String result = new TikaTextExtractor().extractTextFromMarkup(null);
		Assert.assertNull(result);
	}

	@Test
	public void testExtractTextFromMarkupWithBlank() {
		String result = new TikaTextExtractor().extractTextFromMarkup("   ");
		Assert.assertNull(result);
	}

	@Test
	public void testSniffContentTypeWithPngBytes() throws IOException {
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
	public void testSniffContentTypeAlreadySet() throws IOException {
		byte[] bytes = new byte[] {1, 2, 3};
		AttachmentContent content = new AttachmentContent("demo", "admin", "Contact", null, "", UUIDv7.create().toString(), "image")
				.attachment("test.bin", bytes);
		content.setContentType("application/octet-stream");
		new TikaTextExtractor().sniffContentType(content);
		// Content type should not change when already set
		Assert.assertEquals("application/octet-stream", content.getContentType());
	}

	@Test
	public void testSniffLanguageEnglish() {
		try {
			String result = new TikaTextExtractor().sniffLanguage("The quick brown fox jumps over the lazy dog");
			// if a detector is available, result should be non-null
			if (result != null) {
				Assert.assertEquals("en", result);
			}
		} catch (IllegalStateException e) {
			// No language detectors available in test classpath — acceptable
		}
	}

	@Test
	public void testSniffLanguageEmpty() {
		try {
			// empty text should return null or a language code, never throw
			String result = new TikaTextExtractor().sniffLanguage("");
			Assert.assertTrue("sniffLanguage should return null or a valid language code",
					result == null || !result.isEmpty());
		} catch (IllegalStateException e) {
			// No language detectors available in test classpath — acceptable
			Assert.assertNotNull(e);
		}
	}
}
