package org.skyve.util;

import java.awt.image.BufferedImage;
import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.commons.codec.binary.Base32;
import org.skyve.content.AttachmentContent;
import org.skyve.content.MimeType;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.util.ImageUtil;
import org.skyve.impl.util.UtilImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import net.coobird.thumbnailator.Thumbnails;

/**
 * Creates thumb nails from file or content images.
 *
 * @author mike
 */
public class Thumbnail {

    private static final Logger LOGGER = LoggerFactory.getLogger(Thumbnail.class);

	/**
	 * This is a marker file to indicate that this piece of content is not an image.
	 * A file type SVG will be returned in this case.
	 */
	private static final String NOT_AN_IMAGE_FILE_NAME = "NOT_AN_IMAGE.txt";
	
	/**
	 * Used to constrain the number of threads that can generate/serve thumb nails.
	 */
	private static final ConcurrentMap<String, String> CONCURRENT = new ConcurrentHashMap<>(UtilImpl.THUMBNAIL_CONCURRENT_THREADS);

	/**
	 * The thumb nail image bytes.
	 */
	private byte[] bytes;
	
	/**
	 * The thumb nail mime type - png or svg or null if unknown
	 */
	private MimeType mimeType;
	
	/**
	 *  The content ID or the SHA-1 hash of the requested file canonical name
	 */
	private String cacheKey;
		
	/**
	 * File constructor
	 * @param file	The image file.
	 */
	public Thumbnail(File file) throws IOException {
		bytes = FileUtil.bytes(file);
		mimeType = MimeType.fromFileName(file.getName());
	}

	/**
	 * Content constructor
	 * @param content	The attachment.
	 */
	public Thumbnail(AttachmentContent content) throws InterruptedException, IOException {
		this(content, 0, 0);
	}

	/**
	 * Thumbnail File constructor.
	 * This generates a thumb nail of the given width and height from an image file.
	 * This method is thread safe.
	 * The memory usage is constrained by the UtilImpl.THUMBNAIL_CONCURRENT_THREADS setting and
	 * thumb nail file creation and access is hobbled to one thread per thumb nail file at a time.
	 * 
	 * @param file	The image file
	 * @param width	The required width
	 * @param height	The required height
	 * @throws InterruptedException
	 * @throws IOException
	 * @throws NoSuchAlgorithmException
	 */
	public Thumbnail(File file, int width, int height) throws InterruptedException, IOException, NoSuchAlgorithmException {
		// Wait until we are below max concurrency
		while (CONCURRENT.size() > UtilImpl.THUMBNAIL_CONCURRENT_THREADS) {
			Thread.sleep(10);
		}

		// Create a cache key based on a "SHA-1" hash of the canonical file name.
		// It is unlikely that there will be hash collisions (git uses this method for commit hashes).
		// If there is a collision, the wrong thumb nail image will be served which is not the end of the world.
		MessageDigest md = MessageDigest.getInstance("SHA1"); // SHA-1 base 32 is 32 chars
		cacheKey = new Base32().encodeAsString(md.digest(file.getCanonicalPath().getBytes()));

		// This happens atomically and serves as a critical section for file requests
		// Note that it is blocking ALL thumb nail requests to the image in case it needs to write the NOT_AN_IMAGE file
		while (CONCURRENT.putIfAbsent(cacheKey, cacheKey) != null) {
			Thread.sleep(10);
		}
	
		// Get the thumb nail and finally remove our lock on this file.
		try {
			try (FileInputStream fis = new FileInputStream(file); BufferedInputStream bis = new BufferedInputStream(fis)) {
				process(file.getName(), bis, null, width, height);
			}
		}
		finally {
			CONCURRENT.remove(cacheKey);
		}
	}
	
	/**
	 * Thumbnail Content constructor.
	 * This generates a thumb nail of the given width and height from an attachment content.
	 * This method is thread safe.
	 * The memory usage is constrained by the UtilImpl.THUMBNAIL_CONCURRENT_THREADS setting and
	 * thumb nail file creation and access is hobbled to one thread per attachment at a time.
	 * 
	 * @param content	The attachment
	 * @param width	The required width (if <= 0 this will not be processed)
	 * @param height	The required height	(if <=0 this will not be processed)
	 * @throws InterruptedException
	 * @throws IOException
	 */
	public Thumbnail(AttachmentContent content, int width, int height) throws InterruptedException, IOException {
		// No processing required - ie no resize and no SVG burning, just return content bytes and mime type.
		String markup = content.getMarkup();
		if (((width <= 0) || (height <= 0)) && (markup == null)) {
			bytes = content.getContentBytes();
			mimeType = content.getMimeType();
			return;
		}

		// Wait until we are below max concurrency
		while (CONCURRENT.size() > UtilImpl.THUMBNAIL_CONCURRENT_THREADS) {
			Thread.sleep(10);
		}

		// Lower case to suit all file systems
		cacheKey = content.getContentId().toLowerCase();
		
		// This happens atomically and serves as a critical section for content requests
		// Note that it is blocking ALL thumb nail requests to the attachment in case it needs to write the NOT_AN_IMAGE file
		while (CONCURRENT.putIfAbsent(cacheKey, cacheKey) != null) {
			Thread.sleep(10);
		}
		
		// Get the thumb nail and finally remove our lock on this file.
		try {
			try (InputStream is = content.getContentStream()) {
				process(content.getFileName(), is, markup, width, height);
			}
		}
		finally {
			CONCURRENT.remove(cacheKey);
		}
	}
	
	/**
	 * Get the bytes for this thumb nail generated in the constructors.
	 * @return	the bytes
	 */
	public byte[] getBytes() {
		return bytes;
	}
	
	/**
	 * Get the mime type for this thumb nail.
	 * @return	MimeType.png, MimeType.svg or null for unknown
	 */
	public MimeType getMimeType() {
		return mimeType;
	}
	
	private void process(@Nonnull String fileName,
							@Nonnull InputStream is,
							@Nullable String markup,
							int width,
							int height) throws IOException {
		// Return an SVG file directly, no need to make a Thumbnail
		// SVG files in an <img/> or as a background image in CSS do not run script tags in browsers.
		if (MimeType.svg == MimeType.fromFileName(fileName)) {
			bytes = FileUtil.bytes(is);
			mimeType = MimeType.svg;
			return;
		}

		File folder = null;
		File pngFile = null;
		// Look for the thumb nail file if we are using file storage
		if (UtilImpl.THUMBNAIL_FILE_STORAGE) {
			StringBuilder path = new StringBuilder(128).append(Util.getThumbnnailDirectory());
			AbstractContentManager.appendBalancedFolderPathFromContentId(cacheKey, path, false);
			folder = new File(path.toString());
			folder.mkdirs();

			// If NOT_AN_IMAGE file exists return an SVG image as we've tried to process a thumb nail before
			File notAnImage = new File(folder, NOT_AN_IMAGE_FILE_NAME);
			if (notAnImage.exists()) {
				mimeType = MimeType.svg;
				bytes = ImageUtil.svg(fileName, width, height);
				return;
			}

			// Check if a png thumb nail file exists and use that
			pngFile = new File(folder, new StringBuilder(13).append(width).append('x').append(height).append(".png").toString());
			if (pngFile.exists()) {
				bytes = FileUtil.bytes(pngFile);
				mimeType = MimeType.png;
				return;
			}
		}

		BufferedImage image = ImageUtil.read(is, UtilImpl.THUMBNAIL_SUBSAMPLING_MINIMUM_TARGET_SIZE);
		if (image == null) { // not an image or can't be read
			// return an SVG based on the file type
			mimeType = MimeType.svg;
			bytes = ImageUtil.svg(fileName, width, height);

			// Write the NOT_AN_IMAGE file if we are using file storage
			if (UtilImpl.THUMBNAIL_FILE_STORAGE) {
				new File(folder, NOT_AN_IMAGE_FILE_NAME).createNewFile();
			}
		}
		else {
			// Burn the markup into the image
			if (markup != null) {
				try {
					ImageUtil.burnSvg(image, markup);
				}
				catch (Exception e) {
					LOGGER.warn("Could not burn markup onto image - markis = {}", markup, e);
				}
			}

			// Resize after burning
			if ((width > 0) && (height > 0)) {
				image = Thumbnails.of(image).size(width, height).keepAspectRatio(true).asBufferedImage();
			}
			
			try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
				// Create the thumb nail
				Thumbnails.of(image).scale(1.0).outputFormat("png").toOutputStream(baos);
				mimeType = MimeType.png;
				bytes = baos.toByteArray();
				
				// Write the thumb nail file if we are using file storage
				if (UtilImpl.THUMBNAIL_FILE_STORAGE) {
					try (FileOutputStream out = new FileOutputStream(pngFile)) {
						out.write(bytes);
						out.flush();
					}
				}
			}
			image = null; // encourage garbage collection
		}
	}
}
