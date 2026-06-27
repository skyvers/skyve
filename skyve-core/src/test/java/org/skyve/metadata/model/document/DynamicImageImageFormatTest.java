package org.skyve.metadata.model.document;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.awt.image.BufferedImage;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.content.MimeType;
import org.skyve.metadata.model.document.DynamicImage.ImageFormat;
import org.skyve.metadata.user.User;

class DynamicImageImageFormatTest {
	private static class TestDynamicImage implements DynamicImage<Bean> {
		@Override
		public BufferedImage getImage(Bean bean, int width, int height, User user) {
			return new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void pngHasPngMimeType() {
		assertThat(ImageFormat.png.getMimeType(), is(MimeType.png));
	}

	@Test
	@SuppressWarnings("static-method")
	void jpegHasJpegMimeType() {
		assertThat(ImageFormat.jpeg.getMimeType(), is(MimeType.jpeg));
	}

	@Test
	@SuppressWarnings("static-method")
	void gifHasGifMimeType() {
		assertThat(ImageFormat.gif.getMimeType(), is(MimeType.gif));
	}

	@Test
	@SuppressWarnings("static-method")
	void valuesContainsThreeFormats() {
		assertEquals(3, ImageFormat.values().length);
	}

	@Test
	@SuppressWarnings("static-method")
	void valueOfPng() {
		assertNotNull(ImageFormat.valueOf("png"));
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultMethodsReturnNullByDefault() {
		DynamicImage<Bean> image = new TestDynamicImage();
		assertNull(image.getFormat());
		assertNull(image.getCompressionQuality());
		assertNull(image.getCacheTime());
	}
}
