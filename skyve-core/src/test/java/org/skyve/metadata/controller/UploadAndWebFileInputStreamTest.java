package org.skyve.metadata.controller;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.io.ByteArrayInputStream;

import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;

class UploadAndWebFileInputStreamTest {

	@Test
	@SuppressWarnings("static-method")
	void uploadConstructorSetsAllFields() throws Exception {
		try (WebFileInputStream stream = new WebFileInputStream(new ByteArrayInputStream(new byte[0]))) {
			Upload upload = new Upload("test.pdf", stream, MimeType.pdf);
			assertThat(upload.getFileName(), is("test.pdf"));
			assertThat(upload.getMimeType(), is(MimeType.pdf));
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void uploadWithNullStreamAndMimeType() {
		Upload upload = new Upload("file.txt", null, null);
		assertThat(upload.getFileName(), is("file.txt"));
	}

	@Test
	@SuppressWarnings({"static-method", "resource"})
	void uploadGetInputStreamReturnsStream() throws Exception {
		try (WebFileInputStream stream = new WebFileInputStream(new ByteArrayInputStream(new byte[0]))) {
			Upload upload = new Upload("test.pdf", stream, MimeType.pdf);
			assertSame(stream, upload.getInputStream());
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void webFileInputStreamProcessedAndClose() throws Exception {
		ByteArrayInputStream bais = new ByteArrayInputStream("hello".getBytes());
		try (WebFileInputStream wfis = new WebFileInputStream(bais)) {
			// Verify the stream is readable
			assertEquals(104, wfis.read()); // 'h'
			// Before processed, close does nothing to the underlying stream
			wfis.close();
			// After processed, close calls super.close()
			wfis.processed();
		}
	}
}
