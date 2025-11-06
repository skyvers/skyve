package org.skyve.impl.content;

import java.io.IOException;
import java.io.InputStream;

import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.impl.util.UUIDv7;

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

	private void testContent(String resourceName, String expected) throws IOException {
		try (InputStream is = getClass().getResourceAsStream(resourceName)) {
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
}