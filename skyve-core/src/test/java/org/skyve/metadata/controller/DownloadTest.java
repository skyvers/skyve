package org.skyve.metadata.controller;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.io.File;

import org.junit.jupiter.api.Test;
import org.skyve.content.Disposition;
import org.skyve.content.MimeType;

public class DownloadTest {

	@Test
	@SuppressWarnings("static-method")
	public void byteArrayConstructorSetsFields() {
		byte[] data = {1, 2, 3};
		Download d = new Download("file.txt", data, MimeType.plain);
		assertThat(d.getFileName(), is("file.txt"));
		assertArrayEquals(data, d.getBytes());
		assertThat(d.getMimeType(), is(MimeType.plain));
		assertThat(d.getDisposition(), is(Disposition.attachment));
		assertNull(d.getFile());
		// Note: getInputStream() returns a WebFileInputStream resource - just verify no file/stream was set
		assertThat(d.getDisposition(), is(Disposition.attachment));
	}

	@Test
	@SuppressWarnings("static-method")
	public void byteArrayConstructorWithDispositionSetsDisposition() {
		byte[] data = {1, 2, 3};
		Download d = new Download("file.txt", data, MimeType.plain, Disposition.inline);
		assertThat(d.getDisposition(), is(Disposition.inline));
	}

	@Test
	@SuppressWarnings("static-method")
	public void stringContentConstructorConvertsToBytes() {
		Download d = new Download("text.txt", "hello world", MimeType.plain);
		assertThat(d.getFileName(), is("text.txt"));
		assertThat(new String(d.getBytes()), is("hello world"));
		assertThat(d.getDisposition(), is(Disposition.attachment));
	}

	@Test
	@SuppressWarnings("static-method")
	public void stringContentConstructorWithDispositionSetsDisposition() {
		Download d = new Download("text.txt", "content", MimeType.plain, Disposition.inline);
		assertThat(d.getDisposition(), is(Disposition.inline));
	}

	@Test
	@SuppressWarnings("static-method")
	public void fileConstructorSetsFile() {
		File f = new File("/tmp/test.csv");
		Download d = new Download("test.csv", f, MimeType.csv);
		assertThat(d.getFile(), is(f));
		assertThat(d.getMimeType(), is(MimeType.csv));
		assertThat(d.getDisposition(), is(Disposition.attachment));
		assertNull(d.getBytes());
	}

	@Test
	@SuppressWarnings("static-method")
	public void fileConstructorWithDispositionSetsDisposition() {
		File f = new File("/tmp/test.csv");
		Download d = new Download("test.csv", f, MimeType.csv, Disposition.inline);
		assertThat(d.getDisposition(), is(Disposition.inline));
	}
}
