package util.sail;

import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Duration;
import java.util.Base64;
import java.util.Iterator;
import java.util.Random;

import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.stream.ImageInputStream;
import javax.imageio.stream.ImageOutputStream;

import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriver.Timeouts;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Files for a SAIL test to upload, made on first use under {@link #FOLDER} rather than kept in the repository:
 * <ul>
 * <li>{@link #photo(long)}: a JPEG of exactly a given number of bytes, for a test of an upload limit;</li>
 * <li>{@link #photo(int, int)}: a JPEG of given pixel dimensions;</li>
 * <li>{@link #oversizePhoto()}: a JPEG that is too large both in pixels and in bytes, which a page must redraw smaller
 * 		before upload;</li>
 * <li>{@link #file(String, long)}: a file of a given name and exactly a given number of bytes, all zeros, which is not
 * 		an image, so a page does not redraw it smaller before upload;</li>
 * <li>{@link #video(WebDriver, long)}: a playable WebM video of at least a given number of bytes, recorded by the
 * 		browser itself, because no video encoder (such as ffmpeg) is assumed outside it.</li>
 * </ul>
 * Each file is reused while it is there with the right size, so a run makes it once. The photos are the same every time
 * (fixed seeds), and the file is all zeros; the video is noise from the browser's own random numbers. A file is written under a temporary name and
 * then moved into place, so a file that was only partly written (a run stopped part way) is never reused.
 * <p>
 * Threading: thread-safe; the methods that make a file are synchronized on the class. JVMs sharing the folder (parallel
 * lanes) may each make the same file, under their own temporary names: the last move wins, and every copy is complete.
 * <p>
 * These are a test's inputs. {@link SailMedia} is a run's evidence (snapshots and screencast), which is separate.
 */
public final class SailTestFiles {
	/** The folder the files are made in. */
	public static final Path FOLDER = Path.of("target", "sail-test-files").toAbsolutePath();
	/** The width of {@link #oversizePhoto()}, in pixels. */
	public static final int OVERSIZE_WIDTH = 9000;
	/** The height of {@link #oversizePhoto()}, in pixels. */
	public static final int OVERSIZE_HEIGHT = 7000;
	/** The smallest size of {@link #oversizePhoto()}: over 10 MB, the default image upload limit of Skyve's {@code UtilImpl}. */
	public static final long OVERSIZE_MINIMUM_BYTES = 10L * 1024 * 1024 + 1;
	/** The width of the recorded video, in pixels. */
	public static final int VIDEO_WIDTH = 640;
	/** The height of the recorded video, in pixels. */
	public static final int VIDEO_HEIGHT = 360;

	/** The JPEG quality of {@link #photo(long)}: high, so noise stays large. */
	private static final float NOISE_QUALITY = 0.95f;
	/** The JPEG quality of {@link #photo(int, int)}. */
	private static final float GRADIENT_QUALITY = 0.85f;
	/** The JPEG quality of {@link #oversizePhoto()}. */
	private static final float OVERSIZE_QUALITY = 0.9f;
	/** The noise in {@link #photo(long)} stops at 12 megapixels, a phone camera's photo; comments pad the rest. */
	private static final long MAXIMUM_NOISE_PIXELS = 4000L * 3000L;
	/** The smallest side of the noise in {@link #photo(long)}. */
	private static final int MINIMUM_SIDE = 8;
	/** A JPEG side can't be larger than this. */
	private static final int MAXIMUM_SIDE = 65_500;
	/** A comment segment's marker (2 bytes) and length (2 bytes): the least a segment of padding can add. */
	private static final int SEGMENT_HEADER = 4;
	/** The most a comment segment can add: its marker, and a length of at most 65,535 that counts itself. */
	private static final int SEGMENT_MAXIMUM = 2 + 65_535;
	/** The video is read back from the browser in pieces of this many bytes. */
	private static final int VIDEO_PIECE = 1 << 20;
	/** How much longer WebDriver waits for the recording than the page's own deadline, so the page's message comes back. */
	private static final Duration SCRIPT_MARGIN = Duration.ofSeconds(10);
	/** The zeros of {@link #file(String, long)} are written in pieces of this many bytes. */
	private static final int ZEROS_PIECE = 64 * 1024;
	/** The ending of the temporary names {@link #make(Path, Content)} uses, which no file's own name may have. */
	private static final String PARTIAL = ".partial";
	private static final long SEED = 42L;
	private static final long OVERSIZE_SEED = 7L;

	/**
	 * Records a canvas of noise with {@code MediaRecorder} until it has {@code arguments[0]} bytes, keeps the video as
	 * {@code window.__sailTestVideo} and gives its size; the page's deadline is {@code arguments[1]} milliseconds.
	 * The recording is event driven: frames are drawn on the browser's frame clock ({@code requestAnimationFrame}), the
	 * recorder hands its data over every half second ({@code dataavailable}), and the promise settles on its
	 * {@code stop} event, which follows the last {@code dataavailable}, so the video has every part.
	 */
	private static final String RECORD = """
			const target = arguments[0];
			const limit = arguments[1];
			return new Promise((resolve, reject) => {
			    const width = %d;
			    const height = %d;
			    const canvas = document.createElement('canvas');
			    canvas.width = width;
			    canvas.height = height;
			    const g = canvas.getContext('2d');
			    const image = g.createImageData(width, height);
			    const stream = canvas.captureStream(15);
			    const type = ['video/webm;codecs=vp8', 'video/webm'].find((t) => MediaRecorder.isTypeSupported(t));
			    if (! type) {
			        reject(new Error('this browser cannot record a WebM video'));
			        return;
			    }
			    // a high bit rate, so noise (which can't be compressed) makes a large file quickly
			    const recorder = new MediaRecorder(stream, { mimeType: type, videoBitsPerSecond: 24000000 });
			    const parts = [];
			    let size = 0;
			    let drawing = true;
			    const draw = () => {
			        if (drawing) {
			            const d = image.data;
			            for (let i = 0; i < d.length; i += 4) {
			                d[i] = Math.random() * 256;
			                d[i + 1] = Math.random() * 256;
			                d[i + 2] = Math.random() * 256;
			                d[i + 3] = 255;
			            }
			            g.putImageData(image, 0, 0);
			            requestAnimationFrame(draw);
			        }
			    };
			    const finish = () => {
			        drawing = false;
			        clearTimeout(deadline);
			        stream.getTracks().forEach((t) => t.stop());
			    };
			    recorder.ondataavailable = (e) => {
			        if (e.data.size) {
			            parts.push(e.data);
			            size += e.data.size;
			        }
			        if ((size >= target) && (recorder.state === 'recording')) {
			            recorder.stop();
			        }
			    };
			    recorder.onstop = () => {
			        finish();
			        if (size < target) {
			            reject(new Error('the browser recorded only ' + size + ' of ' + target + ' bytes in ' + limit + ' ms'));
			            return;
			        }
			        window.__sailTestVideo = new Blob(parts, { type: 'video/webm' });
			        resolve(window.__sailTestVideo.size);
			    };
			    recorder.onerror = (e) => {
			        finish();
			        reject(new Error('MediaRecorder failed: ' + (e.error || e)));
			    };
			    // the bound on the recording: stopping makes the stop event settle the promise, short of the target
			    const deadline = setTimeout(() => {
			        if (recorder.state === 'recording') {
			            recorder.stop();
			        }
			    }, limit);
			    recorder.start(500);
			    draw();
			});
			""".formatted(Integer.valueOf(VIDEO_WIDTH), Integer.valueOf(VIDEO_HEIGHT));
	/** Gives {@code arguments[1]} bytes of {@code window.__sailTestVideo} from {@code arguments[0]}, in Base64, when the {@code FileReader}'s {@code load} event comes. */
	private static final String READ_PIECE = """
			const slice = window.__sailTestVideo.slice(arguments[0], arguments[0] + arguments[1]);
			return new Promise((resolve, reject) => {
			    const reader = new FileReader();
			    reader.onload = () => resolve(String(reader.result).split(',')[1]);
			    reader.onerror = () => reject(reader.error);
			    reader.readAsDataURL(slice);
			});
			""";

	/** Writes a new file's content to a path. */
	@FunctionalInterface
	private interface Content {
		void write(@Nonnull Path path) throws IOException;
	}

	private SailTestFiles() {
		// static only
	}

	/**
	 * Returns a JPEG photo of exactly the given number of bytes, making it if it isn't there with that size. It is
	 * seeded noise, which a JPEG can't compress, of up to 12 megapixels, so a page that redraws it before upload still
	 * gets a large photo. The noise fills most of the size, and comment segments pad it to the exact number of bytes;
	 * the padding is seeded random bytes too, so compressing the upload doesn't shrink it.
	 *
	 * @param bytes	its size; one byte over a limit tests the limit
	 * @return its absolute path, {@code photo-<bytes>.jpg}
	 * @throws IllegalArgumentException	if a photo can't be that small (a few hundred bytes is the least), or is over 2 GB
	 */
	public static synchronized @Nonnull Path photo(long bytes) {
		if ((bytes <= 0) || (bytes > Integer.MAX_VALUE - 8)) {
			throw new IllegalArgumentException("A SAIL test photo of " + bytes + " bytes can't be made: use 1 to " + (Integer.MAX_VALUE - 8));
		}
		Path result = FOLDER.resolve("photo-" + bytes + ".jpg");
		if (size(result) == bytes) {
			return result;
		}
		byte[] content = noisePhoto((int) bytes);
		return make(result, path -> Files.write(path, content));
	}

	/**
	 * Returns a JPEG photo of the given pixel dimensions, making it if it isn't there with those dimensions. It is a
	 * gradient labelled with its size, so it is small (an 800 by 600 photo is a few tens of KB).
	 *
	 * @param width	its width in pixels
	 * @param height	its height in pixels
	 * @return its absolute path, {@code photo-<width>x<height>.jpg}
	 * @throws IllegalArgumentException	if a side is under 1 or over 65,500 pixels
	 */
	public static synchronized @Nonnull Path photo(int width, int height) {
		if ((width < 1) || (height < 1) || (width > MAXIMUM_SIDE) || (height > MAXIMUM_SIDE)) {
			throw new IllegalArgumentException("A SAIL test photo of " + width + " x " + height + " can't be made: each side must be 1 to " + MAXIMUM_SIDE);
		}
		Path result = FOLDER.resolve("photo-" + width + "x" + height + ".jpg");
		int[] dimensions = dimensions(result);
		if ((dimensions != null) && (dimensions[0] == width) && (dimensions[1] == height)) {
			return result;
		}
		return make(result, path -> Files.write(path, jpeg(gradient(width, height), GRADIENT_QUALITY)));
	}

	/**
	 * Returns a photo that is too large in pixels and in bytes, making it if it isn't there with the size: 9000 by 7000
	 * pixels ({@link #OVERSIZE_WIDTH} and {@link #OVERSIZE_HEIGHT}), a gradient with a 4500 by 3500 block of noise so the
	 * file is over 10 MB ({@link #OVERSIZE_MINIMUM_BYTES}; about 14 MB). A page must redraw it smaller before upload.
	 * Making it takes a few seconds and about 250 MB of memory.
	 *
	 * @return its absolute path, {@code photo-oversize-9000x7000.jpg}
	 * @throws IllegalStateException	if the photo made isn't over 10 MB
	 */
	public static synchronized @Nonnull Path oversizePhoto() {
		Path result = FOLDER.resolve("photo-oversize-" + OVERSIZE_WIDTH + "x" + OVERSIZE_HEIGHT + ".jpg");
		if (size(result) >= OVERSIZE_MINIMUM_BYTES) {
			return result;
		}
		return make(result, path -> {
			byte[] content = jpeg(noisy(gradient(OVERSIZE_WIDTH, OVERSIZE_HEIGHT), OVERSIZE_WIDTH / 2, OVERSIZE_HEIGHT / 2, OVERSIZE_SEED), OVERSIZE_QUALITY);
			if (content.length < OVERSIZE_MINIMUM_BYTES) {
				throw new IllegalStateException("The oversize SAIL test photo is only " + content.length + " bytes, not over " + (OVERSIZE_MINIMUM_BYTES - 1));
			}
			Files.write(path, content);
		});
	}

	/**
	 * Returns a file of the given name and exactly the given number of bytes, all zeros, making it if it isn't there with
	 * that size. It is not an image, so a page that redraws a large image smaller before upload does not change it:
	 * use it to test an upload limit with a file the page must refuse (for example {@code too-large-12582912.bin}).
	 * A file of 0 bytes is allowed (an empty file).
	 *
	 * @param name	its file name, chosen by the caller: a plain name with no path separator ({@code /} or {@code \}), not
	 * 				empty, not {@code .} or {@code ..}, not ending in {@code .partial} (the temporary names), and not starting
	 * 				with {@code photo-} or {@code video-} (the names of the other methods' files)
	 * @param bytes	its size, 0 or more
	 * @return its absolute path, under {@link #FOLDER}, with the given name
	 * @throws IllegalArgumentException	if the name isn't one of those, or the size is negative
	 * @throws UncheckedIOException	if the file can't be written
	 */
	public static synchronized @Nonnull Path file(@Nonnull String name, long bytes) {
		if (name.isEmpty() || name.equals(".") || name.equals("..") || (name.indexOf('/') >= 0) || (name.indexOf('\\') >= 0)
				|| name.indexOf('\0') >= 0 || name.endsWith(PARTIAL) || name.startsWith("photo-") || name.startsWith("video-")) {
			throw new IllegalArgumentException("A SAIL test file can't be named '" + name + "': use a plain file name, not ending in "
												+ PARTIAL + " or starting with photo- or video-");
		}
		if (bytes < 0) {
			throw new IllegalArgumentException("A SAIL test file of " + bytes + " bytes can't be made: use 0 or more");
		}
		Path result = FOLDER.resolve(name);
		if (size(result) == bytes) {
			return result;
		}
		return make(result, path -> {
			byte[] zeros = new byte[(int) Math.min(ZEROS_PIECE, Math.max(bytes, 1L))];
			try (OutputStream out = Files.newOutputStream(path)) {
				for (long left = bytes; left > 0; left -= zeros.length) {
					out.write(zeros, 0, (int) Math.min(zeros.length, left));
				}
			}
		});
	}

	/**
	 * Returns a playable WebM video of at least the given number of bytes, recording it in the browser if it isn't there
	 * with that size, within the SAIL timeout ({@value SailSettings#TIMEOUT_SECONDS}). The browser records about
	 * 3 MB a second, so a large video may need {@link #video(WebDriver, long, Duration)} with a longer timeout.
	 *
	 * @param driver	the leg's browser, which records the video in the page in front (any page will do; it isn't navigated)
	 * @param bytes	its least size; the recording stops at the first half second past it
	 * @return its absolute path, {@code video-<bytes>.webm}
	 * @throws IllegalArgumentException	if the size isn't positive
	 * @throws IllegalStateException	if the browser can't record a WebM video, or doesn't reach the size within the timeout
	 * @see #video(WebDriver, long, Duration)
	 */
	public static @Nonnull Path video(@Nonnull WebDriver driver, long bytes) {
		return video(driver, bytes, SailSettings.resolve().timeout());
	}

	/**
	 * Returns a playable WebM video of at least the given number of bytes, recording it in the browser if it isn't there
	 * with that size. The browser draws a {@value #VIDEO_WIDTH} by {@value #VIDEO_HEIGHT} canvas of noise, records it with
	 * {@code captureStream} and {@code MediaRecorder} (VP8 at 24 Mbit/s, so about 3 MB a second), and gives the video
	 * back in pieces. Like any {@code MediaRecorder} WebM, the file has no duration in its header: a {@code <video>}
	 * reports an infinite duration until it has seen the end (seeking past the end finds it).
	 * <p>
	 * Side effects: while it records, the page holds the video as {@code window.__sailTestVideo}, deleted once it is
	 * read back (left until the page goes if reading fails); the driver's script timeout is the timeout (plus a margin)
	 * meanwhile, and is put back afterwards.
	 *
	 * @param driver	the leg's browser, which records the video in the page in front (any page will do; it isn't navigated)
	 * @param bytes	its least size; the recording stops at the first half second past it
	 * @param timeout	how long the recording may take
	 * @return its absolute path, {@code video-<bytes>.webm}
	 * @throws IllegalArgumentException	if the size isn't positive
	 * @throws IllegalStateException	if the browser can't record a WebM video, or doesn't reach the size within the timeout
	 */
	public static synchronized @Nonnull Path video(@Nonnull WebDriver driver, long bytes, @Nonnull Duration timeout) {
		if (bytes <= 0) {
			throw new IllegalArgumentException("A SAIL test video of " + bytes + " bytes can't be made: use more than 0");
		}
		Path result = FOLDER.resolve("video-" + bytes + ".webm");
		if (size(result) >= bytes) {
			return result;
		}
		JavascriptExecutor js = (JavascriptExecutor) driver;
		Timeouts timeouts = driver.manage().timeouts();
		Duration before = timeouts.getScriptTimeout();
		timeouts.scriptTimeout(timeout.plus(SCRIPT_MARGIN));
		try {
			Object recorded;
			try {
				recorded = js.executeScript(RECORD, Long.valueOf(bytes), Long.valueOf(timeout.toMillis()));
			}
			catch (org.openqa.selenium.JavascriptException e) {
				throw new IllegalStateException("The browser could not record a SAIL test video of " + bytes + " bytes: " + e.getMessage(), e);
			}
			if (! (recorded instanceof Number total) || (total.longValue() < bytes)) {
				throw new IllegalStateException("The browser could not record a SAIL test video of " + bytes + " bytes: it gave " + recorded);
			}
			Path made = make(result, path -> {
				try (OutputStream out = Files.newOutputStream(path)) {
					for (long at = 0; at < total.longValue(); at += VIDEO_PIECE) {
						Object piece = js.executeScript(READ_PIECE, Long.valueOf(at), Long.valueOf(VIDEO_PIECE));
						out.write(Base64.getDecoder().decode(String.valueOf(piece)));
					}
				}
			});
			js.executeScript("delete window.__sailTestVideo;");
			return made;
		}
		finally {
			timeouts.scriptTimeout(before);
		}
	}

	/**
	 * Writes a file under a temporary name in {@link #FOLDER}, then moves it into place, so a partly written file is
	 * never there under the file's own name; the temporary name is unique, so JVMs sharing the folder don't collide.
	 */
	private static @Nonnull Path make(@Nonnull Path result, @Nonnull Content content) {
		Path partial = null;
		try {
			Files.createDirectories(FOLDER);
			partial = Files.createTempFile(FOLDER, result.getFileName() + ".", PARTIAL);
			content.write(partial);
			Files.move(partial, result, StandardCopyOption.REPLACE_EXISTING);
			return result;
		}
		catch (IOException e) {
			throw new UncheckedIOException("Could not make " + result, e);
		}
		finally {
			if (partial != null) {
				try {
					Files.deleteIfExists(partial);
				}
				catch (IOException e) {
					System.out.println("SAIL: could not delete " + partial + ": " + e);
				}
			}
		}
	}

	/** Returns the size of a file, or -1 if it isn't a regular file. */
	private static long size(@Nonnull Path path) {
		try {
			return Files.isRegularFile(path) ? Files.size(path) : -1L;
		}
		catch (@SuppressWarnings("unused") IOException e) { // gone or unreadable: made again
			return -1L;
		}
	}

	/** Returns a JPEG's width and height from its header, without decoding it, or null if it isn't a readable JPEG. */
	static @Nullable int[] dimensions(@Nonnull Path path) {
		if (! Files.isRegularFile(path)) {
			return null;
		}
		try (ImageInputStream in = ImageIO.createImageInputStream(path.toFile())) {
			Iterator<ImageReader> readers = (in == null) ? null : ImageIO.getImageReaders(in);
			if ((readers == null) || (! readers.hasNext())) {
				return null;
			}
			ImageReader reader = readers.next();
			try {
				reader.setInput(in, true, true);
				return new int[] {reader.getWidth(0), reader.getHeight(0)};
			}
			finally {
				reader.dispose();
			}
		}
		catch (@SuppressWarnings("unused") IOException e) { // not a readable JPEG: made again
			return null;
		}
	}

	/**
	 * Makes seeded noise of up to 12 megapixels that encodes to no more than the size, then pads it with comment
	 * segments to exactly the size.
	 */
	private static @Nonnull byte[] noisePhoto(int bytes) {
		try {
			// how many bytes a pixel of noise takes at this quality, measured on a sample rather than assumed
			int sampleWidth = 400;
			int sampleHeight = 300;
			double perPixel = (double) jpeg(noise(sampleWidth, sampleHeight, SEED), NOISE_QUALITY).length / (sampleWidth * sampleHeight);
			long pixels = Math.min(MAXIMUM_NOISE_PIXELS, (long) (bytes * 0.9 / perPixel));
			while (true) {
				int height = Math.max(MINIMUM_SIDE, (int) Math.sqrt(pixels * 3.0 / 4.0));
				int width = Math.max(MINIMUM_SIDE, height * 4 / 3);
				byte[] result = jpeg(noise(width, height, SEED), NOISE_QUALITY);
				int padding = bytes - result.length;
				if ((padding == 0) || (padding >= SEGMENT_HEADER)) {
					return pad(result, padding);
				}
				if (height == MINIMUM_SIDE) { // the smallest noise made, whatever the width
					throw new IllegalArgumentException("A SAIL test photo of " + bytes + " bytes can't be made: the smallest is "
														+ result.length + " bytes, and padding adds " + SEGMENT_HEADER + " or more");
				}
				// over the size (or too close under it to pad): try a fifth fewer pixels than its share
				pixels = (long) (((long) width * height) * 0.8 * Math.min(1.0, (double) bytes / result.length));
			}
		}
		catch (IOException e) {
			throw new UncheckedIOException("Could not make a SAIL test photo of " + bytes + " bytes", e);
		}
	}

	/**
	 * Inserts comment segments ({@code FF FE}, a length, then seeded random bytes) of the given total size after the
	 * JPEG's JFIF segment (which must come first), or after its start of image marker if it has none. Every reader skips
	 * a comment, so the photo is the same.
	 */
	static @Nonnull byte[] pad(@Nonnull byte[] jpeg, int padding) {
		if (padding == 0) {
			return jpeg;
		}
		int at = 2; // after the start of image marker, FF D8
		if (((jpeg[2] & 0xFF) == 0xFF) && ((jpeg[3] & 0xFF) == 0xE0)) {
			at = 4 + (((jpeg[4] & 0xFF) << 8) | (jpeg[5] & 0xFF));
		}
		byte[] result = new byte[jpeg.length + padding];
		System.arraycopy(jpeg, 0, result, 0, at);
		Random random = new Random(SEED);
		int to = at;
		int left = padding;
		while (left > 0) {
			int segment = Math.min(left, SEGMENT_MAXIMUM);
			int after = left - segment;
			if ((after > 0) && (after < SEGMENT_HEADER)) {
				segment -= SEGMENT_HEADER; // leave enough for the last segment's marker and length
			}
			int length = segment - 2;
			result[to] = (byte) 0xFF;
			result[to + 1] = (byte) 0xFE;
			result[to + 2] = (byte) (length >> 8);
			result[to + 3] = (byte) length;
			byte[] data = new byte[segment - SEGMENT_HEADER];
			random.nextBytes(data);
			System.arraycopy(data, 0, result, to + SEGMENT_HEADER, data.length);
			to += segment;
			left -= segment;
		}
		System.arraycopy(jpeg, at, result, to, jpeg.length - at);
		return result;
	}

	private static @Nonnull BufferedImage gradient(int width, int height) {
		BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
		Graphics2D g = image.createGraphics();
		try {
			g.setPaint(new GradientPaint(0, 0, new Color(40, 90, 160), width, height, new Color(230, 200, 120)));
			g.fillRect(0, 0, width, height);
			g.setColor(Color.WHITE);
			g.drawString("SAIL " + width + "x" + height, 20, 40);
		}
		finally {
			g.dispose();
		}
		return image;
	}

	private static @Nonnull BufferedImage noise(int width, int height, long seed) {
		return noisy(new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB), width, height, seed);
	}

	/** Fills the image's top left width × height with seeded noise, which a JPEG can't compress. */
	private static @Nonnull BufferedImage noisy(@Nonnull BufferedImage image, int width, int height, long seed) {
		Random random = new Random(seed);
		for (int y = 0; y < height; y++) {
			for (int x = 0; x < width; x++) {
				image.setRGB(x, y, random.nextInt(0x1000000));
			}
		}
		return image;
	}

	private static @Nonnull byte[] jpeg(@Nonnull BufferedImage image, float quality) throws IOException {
		ImageWriter writer = ImageIO.getImageWritersByFormatName("jpeg").next();
		ByteArrayOutputStream result = new ByteArrayOutputStream();
		try (ImageOutputStream out = ImageIO.createImageOutputStream(result)) {
			writer.setOutput(out);
			ImageWriteParam param = writer.getDefaultWriteParam();
			param.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);
			param.setCompressionQuality(quality);
			writer.write(null, new IIOImage(image, null, null), param);
		}
		finally {
			writer.dispose();
		}
		return result.toByteArray();
	}
}
