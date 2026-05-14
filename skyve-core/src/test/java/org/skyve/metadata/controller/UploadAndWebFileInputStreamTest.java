package org.skyve.metadata.controller;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.io.ByteArrayInputStream;

import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;

public class UploadAndWebFileInputStreamTest {

	@Test
	@SuppressWarnings("static-method")
	public void uploadConstructorSetsAllFields() throws Exception {
		try (WebFileInputStream stream = new WebFileInputStream(new ByteArrayInputStream(new byte[0]))) {
			Upload upload = new Upload("test.pdf", stream, MimeType.pdf);
			assertThat(upload.getFileName(), is("test.pdf"));
			assertThat(upload.getMimeType(), is(MimeType.pdf));
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void uploadWithNullStreamAndMimeType() {
		Upload upload = new Upload("file.txt", null, null);
		assertThat(upload.getFileName(), is("file.txt"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void webFileInputStreamProcessedAndClose() throws Exception {
		ByteArrayInputStream bais = new ByteArrayInputStream("hello".getBytes());
		try (WebFileInputStream wfis = new WebFileInputStream(bais)) {
			// Before processed, close does nothing to the underlying stream
			wfis.close();
			// After processed, close calls super.close()
			wfis.processed();
		}
	}
}
