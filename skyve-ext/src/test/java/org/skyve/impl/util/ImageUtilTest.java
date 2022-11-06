package org.skyve.impl.util;

import static org.junit.Assert.assertTrue;

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;

import javax.imageio.ImageIO;

import org.jfree.chart.encoders.ImageFormat;
import org.junit.Test;
import org.skyve.content.MimeType;

public class ImageUtilTest {

	@SuppressWarnings("static-method")
	@Test
	public void testReduceImageSize() throws Exception {
		// Close to largest sized BufferedImage that can be made
		// Throws NegativeArraySizeException at larger width and height
		int width = 25000;
		int height = 25000;
		BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);

		// Write image to a byte array
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		ImageIO.write(image, ImageFormat.JPEG.toString(), baos);
		byte[] bytes = baos.toByteArray();

		// Default max image upload size of 10MB is too large for the created BufferedImage
		// To test, use 1MB as the max image upload size
		UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB = 1;

		byte[] reducedBytes = ImageUtil.reduceImageSize(MimeType.jpeg, bytes);

		int maxImageSizeBytes = UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB * 1048576;
		assertTrue(reducedBytes.length < maxImageSizeBytes);
	}
}
