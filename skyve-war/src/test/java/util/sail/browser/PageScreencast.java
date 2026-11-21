package util.sail.browser;

import java.util.function.Consumer;

import jakarta.annotation.Nonnull;

/**
 * Records a browser window as a stream of images, as Chrome's DevTools screencast does: each image is a frame of
 * what the window showed, with the time it was drawn, and the browser draws another only when something changed.
 * <p>
 * It is a capability of the browser's control rather than a connection of its own because a second DevTools session on
 * a page changes the page: while one is attached (even if it sends nothing but {@code Target.attachToTarget}), a
 * document loaded after the browser goes offline ({@link NetworkConditions#offline()}) reports
 * {@code navigator.onLine === true}, though its requests fail. The control's own session for the window carries the
 * screencast instead, so recording a run with video does not change what the page sees.
 * <p>
 * The capability {@link Capability#SCREENCAST}.
 * <p>
 * Threading: a screencast belongs to the control, which has one at a time. The frames are handed to the sink on the
 * control's own thread, in the order the control receives them (a browser's events can reach it out of order, so
 * the order is the browser's to within that), and the control acknowledges each one so that the browser sends the
 * next.
 */
public interface PageScreencast {
	/**
	 * How to record: the image format and quality, and the largest image.
	 *
	 * @param format	the image format, {@code jpeg} or {@code png}
	 * @param quality	the compression quality, 0 to 100, for {@code jpeg}
	 * @param maxWidth	the widest image in pixels; a wider window is scaled down
	 * @param maxHeight	the tallest image in pixels; a taller window is scaled down
	 */
	record Options(@Nonnull String format, int quality, int maxWidth, int maxHeight) {
		// a plain value
	}

	/**
	 * One frame: what a window showed when the browser drew it.
	 *
	 * @param base64	the image, Base64 encoded, as the browser sends it; decode it off the control's thread
	 * @param millis	when the browser drew it, in milliseconds since the epoch
	 */
	record Image(@Nonnull String base64, long millis) {
		// a plain value
	}

	/**
	 * Starts recording a window, after stopping the recording that is on. Returns once the browser has started.
	 *
	 * @param windowHandle	the window's WebDriver window handle
	 * @param options	how to record
	 * @param sink	called with each frame, on the control's thread: it must be quick and must not call the control,
	 * 				so hand the frame to a thread of its own to write
	 * @throws IllegalStateException	if the browser has no such window, or cannot start recording it
	 */
	void startScreencast(@Nonnull String windowHandle, @Nonnull Options options, @Nonnull Consumer<Image> sink);

	/**
	 * Stops the recording, if one is on. When it returns the sink is not called again, though a frame the browser
	 * had already sent is dropped. A window that has closed has stopped its recording already.
	 */
	void stopScreencast();
}
