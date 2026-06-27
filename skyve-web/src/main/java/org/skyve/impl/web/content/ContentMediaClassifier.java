package org.skyve.impl.web.content;

import java.util.Locale;
import java.util.Set;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Classifies stored managed-content metadata into the compact media kind sent
 * with web client payloads.
 *
 * <p>The classifier is deliberately side-effect free: callers supply stored
 * MIME/content type and file-name metadata, and this class never opens the
 * content stream or performs repository lookups.
 */
public final class ContentMediaClassifier {
	private static final Set<String> IMAGE_EXTENSIONS = Set.of("jpg", "jpeg", "png", "gif", "webp", "bmp", "svg");
	private static final Set<String> VIDEO_EXTENSIONS = Set.of("mp4", "webm", "mov", "m4v", "ogv", "avi");

	private ContentMediaClassifier() {
		// utility
	}

	/**
	 * Server-classified presentation kind for managed content in web client
	 * payload companion fields.
	 */
	@SuppressWarnings("java:S115") // enum values are more readable in lower-case
	public enum ContentMediaKind {
		/**
		 * Image-like content that renderers may preview through the protected content
		 * serving path.
		 */
		image,

		/**
		 * Video-like content that renderers may preview with browser-native video
		 * controls.
		 */
		video,

		/**
		 * Content that should be presented as a normal managed-content link.
		 */
		link
	}

	/**
	 * Classifies content metadata without opening the content stream.
	 *
	 * @param contentType stored MIME/content type, optionally with parameters
	 * @param fileName stored file name used as a fallback when the content type is
	 *        missing or unrecognised
	 * @return media kind, defaulting to {@link ContentMediaKind#link}
	 */
	public static @Nonnull ContentMediaKind classify(@Nullable String contentType, @Nullable String fileName) {
		String type = normaliseContentType(contentType);
		if (type != null) {
			if (type.startsWith("image/")) {
				return ContentMediaKind.image;
			}
			if (type.startsWith("video/")) {
				return ContentMediaKind.video;
			}
		}

		String extension = extension(fileName);
		if (extension != null) {
			if (IMAGE_EXTENSIONS.contains(extension)) {
				return ContentMediaKind.image;
			}
			if (VIDEO_EXTENSIONS.contains(extension)) {
				return ContentMediaKind.video;
			}
		}

		return ContentMediaKind.link;
	}

	private static @Nullable String normaliseContentType(@Nullable String contentType) {
		if (contentType == null) {
			return null;
		}
		int parameterIndex = contentType.indexOf(';');
		String result = ((parameterIndex < 0) ? contentType : contentType.substring(0, parameterIndex)).trim();
		return result.isEmpty() ? null : result.toLowerCase(Locale.ROOT);
	}

	private static @Nullable String extension(@Nullable String fileName) {
		if (fileName == null) {
			return null;
		}
		String trimmed = fileName.trim();
		int dotIndex = trimmed.lastIndexOf('.');
		if ((dotIndex < 0) || (dotIndex == trimmed.length() - 1)) {
			return null;
		}
		return trimmed.substring(dotIndex + 1).toLowerCase(Locale.ROOT);
	}
}
