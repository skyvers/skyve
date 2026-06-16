package org.skyve.impl.web.content;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.skyve.impl.web.content.ContentMediaClassifier.ContentMediaKind;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ContentMediaClassifierTest {
	@Test
	void classifiesMimeTypesCaseInsensitivelyAndStripsParameters() {
		assertEquals(ContentMediaKind.image, ContentMediaClassifier.classify("IMAGE/PNG; charset=binary", "file.bin"));
		assertEquals(ContentMediaKind.video, ContentMediaClassifier.classify(" video/mp4 ; codecs=h264", "file.bin"));
	}

	@Test
	void fallsBackToFileExtensionWhenMimeTypeIsMissingOrUnknown() {
		assertEquals(ContentMediaKind.image, ContentMediaClassifier.classify(null, "photo.SVG"));
		assertEquals(ContentMediaKind.video, ContentMediaClassifier.classify("", "clip.WebM"));
		assertEquals(ContentMediaKind.video, ContentMediaClassifier.classify("application/octet-stream", "movie.mov"));
	}

	@Test
	void returnsLinkForUnknownOrMissingMetadata() {
		assertEquals(ContentMediaKind.link, ContentMediaClassifier.classify("application/pdf", "report.pdf"));
		assertEquals(ContentMediaKind.link, ContentMediaClassifier.classify(null, "archive.unknown"));
		assertEquals(ContentMediaKind.link, ContentMediaClassifier.classify(null, null));
	}
}
