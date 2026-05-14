package org.skyve.metadata.model.document;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.metadata.model.document.DynamicImage.ImageFormat;

public class DynamicImageImageFormatTest {

	@Test
	@SuppressWarnings("static-method")
	public void pngHasPngMimeType() {
		assertThat(ImageFormat.png.getMimeType(), is(MimeType.png));
	}

	@Test
	@SuppressWarnings("static-method")
	public void jpegHasJpegMimeType() {
		assertThat(ImageFormat.jpeg.getMimeType(), is(MimeType.jpeg));
	}

	@Test
	@SuppressWarnings("static-method")
	public void gifHasGifMimeType() {
		assertThat(ImageFormat.gif.getMimeType(), is(MimeType.gif));
	}

	@Test
	@SuppressWarnings("static-method")
	public void valuesContainsThreeFormats() {
		assertEquals(3, ImageFormat.values().length);
	}

	@Test
	@SuppressWarnings("static-method")
	public void valueOfPng() {
		assertNotNull(ImageFormat.valueOf("png"));
	}
}
