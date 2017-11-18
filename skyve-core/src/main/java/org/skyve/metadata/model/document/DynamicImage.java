package org.skyve.metadata.model.document;

import java.awt.image.BufferedImage;

import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.user.User;

/**
 * Creates a dynamic image programmatically.
 * @param <T>
 */
public interface DynamicImage<T extends Bean> extends MetaData {
	/**
	 * 
	 * @param bean
	 * @param width
	 * @param height
	 * @param user
	 * @return
	 * @throws Exception
	 */
	public BufferedImage getImage(T bean, int width, int height, User user) throws Exception;

	public static enum ImageFormat {
		png(MimeType.png) , jpeg(MimeType.jpeg), gif(MimeType.gif);

		private MimeType mimeType;
		
		private ImageFormat(MimeType mimeType) {
			this.mimeType = mimeType;
		}
		
		public MimeType getMimeType() {
			return mimeType;
		}
	}

	/**
	 * Select a format.
	 * 
	 * png - has alpha channel, good for lines and text, has lossless compression.
	 * jpeg - good for photos - lossy compression, not so good for lines and text, no transparency.
	 * gif - can be animated, transparent colour, good for lines and text, 256 colour palette.
	 * 
	 * @return null for default (png)
	 */
	public ImageFormat getFormat();

	/**
	 * Set to a number between 0.0 and 1.0.
	 * 
	 * @return null for default quality
	 */
	public Float getCompressionQuality();
}
