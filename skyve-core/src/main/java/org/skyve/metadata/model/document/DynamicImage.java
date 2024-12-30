package org.skyve.metadata.model.document;

import java.awt.image.BufferedImage;
import java.time.Duration;

import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Creates a dynamic image programmatically.
 * @param <T>
 */
public interface DynamicImage<T extends Bean> extends MetaData {
	/**
	 * Produce a BufferedImage to send to the client.
	 * @param bean
	 * @param width
	 * @param height
	 * @param user
	 * @return
	 * @throws Exception
	 */
	public @Nonnull BufferedImage getImage(@Nonnull T bean, int width, int height, @Nonnull User user)
	throws Exception;

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
	default @Nullable ImageFormat getFormat() {
		return null;
	}

	/**
	 * Set to a number between 0.0 and 1.0.
	 * 
	 * @return null for default quality
	 */
	default @Nullable Float getCompressionQuality() {
		return null;
	}
	
	/**
	 * The duration of time to cache the image for or null for no caching.
	 * The duration value is used to generate the number of millis to add to current time
	 * for the cache header so certain Duration values are incompatible with this usage.
	 * @return null for no caching
	 */
	default Duration getCacheTime() {
		return null;
	}
}
