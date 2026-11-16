package org.skyve.util;

import java.awt.image.BufferedImage;
import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InterruptedIOException;
import java.nio.file.DirectoryNotEmptyException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

import org.apache.commons.codec.binary.Base32;
import org.apache.commons.io.function.IOSupplier;
import org.apache.commons.io.input.CloseShieldInputStream;
import org.skyve.content.AttachmentContent;
import org.skyve.content.MimeType;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.util.ImageUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import net.coobird.thumbnailator.Thumbnails;

/**
 * Creates thumb nails from file or content images.
 * <p>
 * A thumb nail of a given size is made once and, with file storage on, kept on the file system and served from there.
 * Only a few thumb nails are made at once - see {@link #decodeThreads(int, int, long)} - and the same image is
 * worked on by one thread at a time. A request that can't start within 60 seconds gets a {@link BusyException}.
 *
 * @author mike
 */
public class Thumbnail {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(Thumbnail.class);

	/**
	 * This is a marker file to indicate that this piece of content is not an image.
	 * A file type SVG will be returned in this case.
	 */
	private static final String NOT_AN_IMAGE_FILE_NAME = "NOT_AN_IMAGE.txt";

	/**
	 * Thumb nails are not stored when the thumb nail drive has less than this percentage of its space free,
	 * so the thumb nail store can never fill the drive. They are still made and served.
	 */
	private static final long MINIMUM_FREE_SPACE_PERCENT = 10;

	/**
	 * The longest a request waits to start making a thumb nail - a safety limit that normal use never reaches.
	 */
	private static final long WAIT_SECONDS = 60;

	/**
	 * Locks so that one thread at a time works on an image - a thumb nail being made, or the image's
	 * thumb nails being deleted. An image's lock is chosen by its cache key, so unrelated images rarely share one.
	 * Fair, so waiting requests are served in the order they arrived.
	 */
	private static final ReentrantLock[] IMAGE_LOCKS = new ReentrantLock[256];
	static {
		for (int i = 0, l = IMAGE_LOCKS.length; i < l; i++) {
			IMAGE_LOCKS[i] = new ReentrantLock(true);
		}
	}

	/**
	 * Limits the thumb nails being made at once, which limits the CPU and memory they use.
	 * Fair, so waiting requests are served in the order they arrived.
	 * Package-private so tests can take every permit.
	 */
	static final Semaphore DECODERS = new Semaphore(decodeThreads(UtilImpl.THUMBNAIL_CONCURRENT_THREADS,
																			Runtime.getRuntime().availableProcessors(),
																			Runtime.getRuntime().maxMemory()),
																true);

	/**
	 * Thrown when a thumb nail could not be started within the wait limit because the server is busy making others.
	 * The servlets respond with 503 (Service Unavailable).
	 */
	public static class BusyException extends InterruptedIOException {
		private static final long serialVersionUID = 4236571209434868467L;

		/**
		 * @param message	What was being waited for.
		 */
		public BusyException(String message) {
			super(message);
		}
	}

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
	 * When the original file was last changed, so a thumb nail stored before then is made again.
	 * 0 when the original never changes - content ids and stream urls always give the same bytes.
	 */
	private long sourceLastModified;

	/**
	 * For {@link #stored(String, String, int, int)}.
	 */
	private Thumbnail() {
		// nothing to make
	}

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
	public Thumbnail(AttachmentContent content) throws IOException {
		this(content, 0, 0);
	}

	/**
	 * Thumbnail File constructor.
	 * This generates a thumb nail that fits inside the given width and height from an image file.
	 * This method is thread safe. See the class comment for how many thumb nails are made at once.
	 * 
	 * @param file	The image file
	 * @param width	The required width
	 * @param height	The required height
	 * @throws IOException
	 * @throws BusyException	if the thumb nail could not be started within the wait limit because the server is busy.
	 */
	public Thumbnail(File file, int width, int height) throws IOException {
		determineCacheKey(file.getCanonicalPath());
		// A file can change, keeping its name, when an app is redeployed
		sourceLastModified = file.lastModified();
		make(file.getName(), null, width, height, () -> new BufferedInputStream(new FileInputStream(file)));
	}
	
	/**
	 * Thumbnail Content constructor.
	 * This generates a thumb nail that fits inside the given width and height from an attachment content.
	 * This method is thread safe. See the class comment for how many thumb nails are made at once.
	 * 
	 * @param content	The attachment
	 * @param width	The required width (if <= 0 this will not be processed)
	 * @param height	The required height	(if <=0 this will not be processed)
	 * @throws IOException
	 * @throws BusyException	if the thumb nail could not be started within the wait limit because the server is busy.
	 */
	public Thumbnail(AttachmentContent content, int width, int height) throws IOException {
		// No processing required - ie no resize and no SVG burning, just return content bytes and mime type.
		String markup = content.getMarkup();
		if (((width <= 0) || (height <= 0)) && (markup == null)) {
			bytes = content.getContentBytes();
			mimeType = content.getMimeType();
			return;
		}

		// Lower case to suit all file systems
		cacheKey = content.getContentId().toLowerCase();
		// No file name gives the blank file icon if it is not an image
		String contentFileName = content.getFileName();
		String fileName = (contentFileName == null) ? "" : contentFileName;
		make(fileName, markup, width, height, content::getContentStream);
	}

	/**
	 * Thumbnail InputStream constructor.
	 * This generates a thumb nail that fits inside the given width and height from an arbitrary input stream.
	 * The url is used to compute a stable SHA-1 cache key (same approach as {@link #Thumbnail(File, int, int)}),
	 * so generated thumbnails are written to and served from the file system cache when
	 * {@code UtilImpl.THUMBNAIL_FILE_STORAGE} is enabled.
	 * The contentType is used to detect SVG content and derive the correct file extension; for
	 * non-image streams a file-type SVG placeholder is produced by {@code Thumbnail.process()}.
	 * This method is thread safe. See the class comment for how many thumb nails are made at once.
	 *
	 * @param inputStream	The image data
	 * @param url	A stable identifier for the resource (e.g. the servlet request URI) used to key the disk cache
	 * @param contentType	The MIME content type of the input stream (e.g. "image/jpeg", "image/svg+xml")
	 * @param width	The required width
	 * @param height	The required height
	 * @throws IOException
	 * @throws BusyException	if the thumb nail could not be started within the wait limit because the server is busy.
	 */
	@SuppressWarnings("resource") // the caller closes its stream
	public Thumbnail(InputStream inputStream, String url, String contentType, int width, int height)
	throws IOException {
		determineCacheKey(url);
		make(streamFileName(contentType), null, width, height, () -> CloseShieldInputStream.wrap(inputStream));
	}

	/**
	 * Makes the thumb nail, or serves the stored one. An SVG original is served as it is, without waiting.
	 * Otherwise the stored thumb nail is served if there is one, else the image's lock is taken and it is made.
	 *
	 * @param fileName	The original's file name, for its type.
	 * @param markup	Markup to burn into the image, or null.
	 * @param source	Opens the original. The stream is closed when done.
	 */
	private void make(@Nonnull String fileName,
						@Nullable String markup,
						int width,
						int height,
						@Nonnull IOSupplier<InputStream> source) throws IOException {
		// SVG files in an <img/> or as a background image in CSS do not run script tags in browsers.
		if (MimeType.svg == MimeType.fromFileName(fileName)) {
			try (InputStream is = source.get()) {
				bytes = FileUtil.bytes(is);
			}
			mimeType = MimeType.svg;
			return;
		}
		if (serveStored(fileName, width, height)) {
			return;
		}
		long deadline = deadline();
		ReentrantLock lock = imageLock(cacheKey);
		acquire(lock, deadline);
		try {
			// Check again now the image's lock is held, in case another thread has just made it
			if (! serveStored(fileName, width, height)) {
				try (InputStream is = source.get()) {
					process(fileName, is, markup, width, height, deadline);
				}
			}
		}
		finally {
			lock.unlock();
		}
	}

	/**
	 * The stored thumb nail that the InputStream constructor would serve for a url, found without the stream,
	 * so a caller can avoid reading or copying the original - see {@link #Thumbnail(InputStream, String, String, int, int)}.
	 * This does not wait.
	 *
	 * @param url	The stable identifier the thumb nail was made with
	 * @param contentType	The MIME content type of the original
	 * @param width	The required width
	 * @param height	The required height
	 * @return	the stored thumb nail, or null if there isn't one (or file storage is off), so it has to be made.
	 * @throws IOException	if the url can't be hashed.
	 */
	public static @Nullable Thumbnail stored(@Nonnull String url, @Nonnull String contentType, int width, int height)
	throws IOException {
		Thumbnail result = new Thumbnail();
		result.determineCacheKey(url);
		return result.serveStored(streamFileName(contentType), width, height) ? result : null;
	}

	/**
	 * A file name for a stream's content type, so a stream that isn't an image gets the right file type icon.
	 */
	private static @Nonnull String streamFileName(@Nonnull String contentType) {
		MimeType mt = MimeType.fromContentType(contentType);
		return (mt != null) ? "file." + mt.getStandardFileSuffix() : "file.bin";
	}

	/**
	 * Deletes stored thumb nails that were made more than the given number of days ago, then any empty folders,
	 * so the thumb nails of images that are gone - or that were removed without deleting them - don't stay on the
	 * disk forever. A thumb nail still in use is made again the next time it is asked for.
	 * This takes no locks: a thumb nail being read when it is deleted is made again, one being made is too new,
	 * and a folder is only made just before a thumb nail is written into it.
	 *
	 * @param days	How old a thumb nail has to be to be deleted.
	 * @return	the number of files deleted.
	 * @throws IOException	if the thumb nail directory could not be walked.
	 */
	public static int deleteStoredThumbnailsOlderThan(int days) throws IOException {
		File directory = new File(Util.getThumbnnailDirectory());
		if (! directory.isDirectory()) {
			return 0;
		}
		OldThumbnailSweeper sweeper = new OldThumbnailSweeper(directory.toPath(),
																System.currentTimeMillis() - TimeUnit.DAYS.toMillis(days));
		Files.walkFileTree(directory.toPath(), sweeper);
		return sweeper.deleted;
	}

	/**
	 * Deletes the files written before a cutoff, then the folders left empty.
	 */
	private static final class OldThumbnailSweeper extends SimpleFileVisitor<Path> {
		private final Path root;
		private final long cutoff;
		private int deleted;

		private OldThumbnailSweeper(@Nonnull Path root, long cutoff) {
			this.root = root;
			this.cutoff = cutoff;
		}

		@Override
		public FileVisitResult visitFile(Path file, BasicFileAttributes attributes) {
			if (attributes.isRegularFile() && (attributes.lastModifiedTime().toMillis() < cutoff)) {
				try {
					if (Files.deleteIfExists(file)) {
						deleted++;
					}
				}
				catch (IOException e) {
					LOGGER.warn("Could not delete the old thumbnail {}", file, e);
				}
			}
			return FileVisitResult.CONTINUE;
		}

		@Override
		public FileVisitResult visitFileFailed(Path file, IOException e) {
			LOGGER.warn("Could not check the thumbnail {}", file, e);
			return FileVisitResult.CONTINUE;
		}

		@Override
		public FileVisitResult postVisitDirectory(Path dir, IOException e) {
			if (! dir.equals(root)) {
				try {
					Files.deleteIfExists(dir);
				}
				catch (@SuppressWarnings("unused") DirectoryNotEmptyException ex) {
					// still has thumb nails
				}
				catch (IOException ex) {
					LOGGER.warn("Could not delete the old thumbnail folder {}", dir, ex);
				}
			}
			return FileVisitResult.CONTINUE;
		}
	}

	/**
	 * Deletes the stored thumb nails of an attachment, if there are any.
	 * Call this when the attachment is deleted, so its thumb nails don't stay on the disk forever.
	 *
	 * @param contentId	The attachment's content ID.
	 * @throws IOException	if the thumb nails could not be deleted.
	 * @throws BusyException	if a thumb nail of the attachment was still being made at the wait limit.
	 */
	public static void deleteStoredThumbnails(@Nonnull String contentId) throws IOException {
		// Lower case to match the content constructor
		String key = contentId.toLowerCase();
		File folder = thumbnailFolder(key);
		if (folder.exists()) {
			ReentrantLock lock = imageLock(key);
			acquire(lock, deadline());
			try {
				FileUtil.delete(folder);
			}
			finally {
				lock.unlock();
			}
		}
	}

	/**
	 * How many thumb nails can be made at once: thumbnail.concurrentThreads, or half the processors when that
	 * is 0 or less, but no more than fit in a quarter of the heap at 32 x maximumDimension squared bytes each
	 * (128MB at 2048).
	 *
	 * @param configured	thumbnail.concurrentThreads
	 * @param processors	The number of processors.
	 * @param maximumHeap	The maximum heap in bytes.
	 * @return	The number of thumb nails that can be made at once - at least 1.
	 */
	static int decodeThreads(int configured, int processors, long maximumHeap) {
		long maximumDimension = UtilImpl.THUMBNAIL_MAXIMUM_DIMENSION;
		// (2 x maximumDimension) squared pixels, at 4 bytes each, twice as a photo taken on its side is turned upright in a copy
		long largestDecodeBytes = 32L * maximumDimension * maximumDimension;
		long heapLimit = Math.max(1L, maximumHeap / 4L / largestDecodeBytes);
		int wanted = (configured > 0) ? configured : Math.max(1, processors / 2);
		return (int) Math.min(wanted, heapLimit);
	}

	/**
	 * The lock for an image's cache key.
	 */
	private static @Nonnull ReentrantLock imageLock(@Nonnull String key) {
		return IMAGE_LOCKS[Math.floorMod(key.hashCode(), IMAGE_LOCKS.length)];
	}

	/**
	 * The {@link System#nanoTime()} to stop waiting at, {@link #WAIT_SECONDS} from now.
	 */
	private static long deadline() {
		return System.nanoTime() + TimeUnit.SECONDS.toNanos(WAIT_SECONDS);
	}

	/**
	 * Takes a lock, waiting until the deadline. The caller must unlock it in a finally block.
	 *
	 * @throws BusyException	if the deadline passed first.
	 * @throws InterruptedIOException	if the thread was interrupted.
	 */
	@SuppressWarnings("java:S2222") // the caller unlocks it
	static void acquire(@Nonnull ReentrantLock lock, long deadline) throws InterruptedIOException {
		try {
			if (lock.tryLock(deadline - System.nanoTime(), TimeUnit.NANOSECONDS)) {
				return;
			}
		}
		catch (@SuppressWarnings("unused") InterruptedException e) {
			Thread.currentThread().interrupt();
			throw new InterruptedIOException("Interrupted whilst waiting to make a thumbnail");
		}
		throw new BusyException("Timed out waiting for another thumbnail of the same image");
	}

	/**
	 * Takes a decode permit, waiting until the deadline. The caller must release it in a finally block.
	 *
	 * @throws BusyException	if the deadline passed first.
	 * @throws InterruptedIOException	if the thread was interrupted.
	 */
	static void acquireDecoder(long deadline) throws InterruptedIOException {
		try {
			if (DECODERS.tryAcquire(deadline - System.nanoTime(), TimeUnit.NANOSECONDS)) {
				return;
			}
		}
		catch (@SuppressWarnings("unused") InterruptedException e) {
			Thread.currentThread().interrupt();
			throw new InterruptedIOException("Interrupted whilst waiting to make a thumbnail");
		}
		throw new BusyException("Timed out waiting for other thumbnails to be made");
	}

	/**
	 * Create a cache key based on a "SHA-1" hash of the canonical file name.
	 * It is unlikely that there will be hash collisions (git uses this method for commit hashes).
	 * If there is a collision, the wrong thumb nail image will be served which is not the end of the world.
	 * 
	 * @param bytes
	 * @throws IOException
	 */
	private void determineCacheKey(String uri) throws IOException{
		try {
	    	@SuppressWarnings("java:S4790") // Just to make a unique name within the character limit - not for security purposes
			MessageDigest md = MessageDigest.getInstance("SHA1"); // SHA-1 base 32 is 32 chars
			cacheKey = new Base32().encodeAsString(md.digest(uri.getBytes()));
		}
		catch (NoSuchAlgorithmException e) {
			throw new IOException("SHA-1 algorithm not found for thumbnail cache key generation", e);
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

	/**
	 * The folder that holds the stored thumb nails for a cache key.
	 */
	private static @Nonnull File thumbnailFolder(@Nonnull String key) {
		StringBuilder path = new StringBuilder(128).append(Util.getThumbnnailDirectory());
		AbstractContentManager.appendBalancedFolderPathFromContentId(key, path);
		return new File(path.toString());
	}

	/**
	 * The stored thumb nail file for a size in a thumb nail folder.
	 */
	private static @Nonnull File sizeFile(@Nonnull File folder, int width, int height) {
		return new File(folder, new StringBuilder(13).append(width).append('x').append(height).append(".png").toString());
	}

	/**
	 * Whether a stored file exists and was written no earlier than the original was last changed.
	 */
	private boolean isCurrent(@Nonnull File stored) {
		long lastModified = stored.lastModified(); // 0 if it doesn't exist
		return (lastModified > 0L) && (lastModified >= sourceLastModified);
	}

	/**
	 * Serves the stored thumb nail, if file storage is on and it has been made before.
	 * This needs no lock, as thumb nails are stored by renaming a complete file into place,
	 * so stored thumb nails are served without waiting for thumb nails being made.
	 *
	 * @param fileName	The original's file name, for its file type icon if it is not an image.
	 * @return	true if the stored thumb nail (or the file type icon of a stored non-image) is now in bytes.
	 */
	private boolean serveStored(@Nonnull String fileName, int width, int height) throws IOException {
		if (! UtilImpl.THUMBNAIL_FILE_STORAGE) {
			return false;
		}
		File folder = thumbnailFolder(cacheKey);
		// If NOT_AN_IMAGE file exists return an SVG image as we've tried to process a thumb nail before
		if (isCurrent(new File(folder, NOT_AN_IMAGE_FILE_NAME))) {
			mimeType = MimeType.svg;
			bytes = ImageUtil.svg(fileName, width, height);
			return true;
		}
		File pngFile = sizeFile(folder, width, height);
		if (isCurrent(pngFile)) {
			try {
				bytes = FileUtil.bytes(pngFile);
				mimeType = MimeType.png;
				return true;
			}
			catch (@SuppressWarnings("unused") IOException e) {
				// deleted since, with the attachment or by the sweep, so make it again
			}
		}
		return false;
	}

	/**
	 * Whether the thumb nail drive has enough free space to store more.
	 * A thumb nail directory that doesn't exist yet measures 0 of 0, so has space.
	 */
	private static boolean hasSpace() {
		File directory = new File(Util.getThumbnnailDirectory());
		return directory.getUsableSpace() >= (directory.getTotalSpace() / 100 * MINIMUM_FREE_SPACE_PERCENT);
	}

	/**
	 * Stores thumb nail bytes, if the drive has space. The file is written under a temporary name and then
	 * renamed, so a crash part way through never leaves a broken thumb nail to be served.
	 * A problem is logged rather than thrown, as the thumb nail can still be served.
	 */
	private void store(@Nonnull File folder, @Nonnull File file, @Nonnull byte[] content) {
		if (! hasSpace()) {
			LOGGER.debug("Not storing a thumbnail for cacheKey = {} - the thumbnail drive is low on space", cacheKey);
			return;
		}
		Path temp = null;
		try {
			Files.createDirectories(folder.toPath());
			temp = Files.createTempFile(folder.toPath(), "making-", ".tmp");
			Files.write(temp, content);
			// The temporary file is in the same folder, so this is a rename, which replaces an out of date thumb nail
			Files.move(temp, file.toPath(), StandardCopyOption.ATOMIC_MOVE);
			temp = null;
		}
		catch (IOException e) {
			LOGGER.warn("Could not store the thumbnail {} for cacheKey = {} - it is still served", file, cacheKey, e);
		}
		finally {
			if (temp != null) {
				try {
					Files.deleteIfExists(temp);
				}
				catch (IOException e) {
					LOGGER.warn("Could not delete the temporary thumbnail {}", temp, e);
				}
			}
		}
	}

	/**
	 * Shrinks an image to fit inside a box, keeping its shape, but never enlarges it.
	 * The browser scales a small image up just as well.
	 * A box without both sides leaves the image as it is.
	 */
	private static @Nonnull BufferedImage shrinkToFit(@Nonnull BufferedImage image, int width, int height)
	throws IOException {
		if ((width > 0) && (height > 0) && ((image.getWidth() > width) || (image.getHeight() > height))) {
			return Thumbnails.of(image).size(width, height).keepAspectRatio(true).asBufferedImage();
		}
		return image;
	}

	/**
	 * Decodes the original image for a box, burns in the markup and shrinks it to fit.
	 *
	 * @return	the image, or null if the original is not an image that can be read.
	 */
	private @Nullable BufferedImage decode(@Nonnull InputStream is, @Nullable String markup, int width, int height)
	throws IOException {
		BufferedImage result = ImageUtil.read(is, UtilImpl.THUMBNAIL_SUBSAMPLING_MINIMUM_TARGET_SIZE, width, height);
		if (result != null) {
			// Burn the markup into the image
			if (markup != null) {
				try {
					ImageUtil.burnSvg(result, markup);
				}
				catch (Exception e) {
					LOGGER.warn("Could not burn markup onto image - markup = {}, cacheKey = {}", markup, cacheKey, e);
				}
			}
			// Shrink after burning
			result = shrinkToFit(result, width, height);
		}
		return result;
	}

	/**
	 * Makes the thumb nail. The thread waits for a decode permit - see {@link #decodeThreads(int, int, long)} -
	 * makes the thumb nail, and stores it if file storage is on.
	 * The caller holds the image's lock.
	 */
	private void process(@Nonnull String fileName,
							@Nonnull InputStream is,
							@Nullable String markup,
							int width,
							int height,
							long deadline) throws IOException {
		File folder = UtilImpl.THUMBNAIL_FILE_STORAGE ? thumbnailFolder(cacheKey) : null;

		acquireDecoder(deadline);
		try {
			BufferedImage image = decode(is, markup, width, height);
			if (image == null) { // not an image or can't be read
				// return an SVG based on the file type
				mimeType = MimeType.svg;
				bytes = ImageUtil.svg(fileName, width, height);

				// Write the NOT_AN_IMAGE file if we are using file storage
				if ((folder != null) && hasSpace()) {
					// Written rather than created, so an out of date one is brought up to date
					try {
						Files.createDirectories(folder.toPath());
						Files.write(new File(folder, NOT_AN_IMAGE_FILE_NAME).toPath(), new byte[0]);
					}
					catch (IOException e) {
						LOGGER.warn("Could not write the NOT_AN_IMAGE file in folder = {}, cacheKey = {}", folder, cacheKey, e);
					}
				}
			}
			else {
				try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
					Thumbnails.of(image).scale(1.0).outputFormat("png").toOutputStream(baos);
					bytes = baos.toByteArray();
				}
				mimeType = MimeType.png;
			}
		}
		finally {
			DECODERS.release();
		}

		if ((folder != null) && (MimeType.png == mimeType)) {
			store(folder, sizeFile(folder, width, height), bytes);
		}
	}
}
