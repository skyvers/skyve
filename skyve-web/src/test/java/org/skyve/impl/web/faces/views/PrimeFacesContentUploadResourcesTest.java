package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class PrimeFacesContentUploadResourcesTest {
	private static final Path UPLOAD_CSS = Path.of("src/css/upload.css");

	@Test
	void uploadWidgetConsumesTheAvailableIframeHeight() throws IOException {
		String css = Files.readString(UPLOAD_CSS);

		assertTrue(css.contains("#upload {\n\tdisplay: flex;\n\tflex: 1 1 auto;\n\tflex-direction: column;\n\tmin-height: 0;\n}"), css);
		assertTrue(css.contains("#upload .ui-fileupload-buttonbar {\n\tflex: 0 0 auto;\n}"), css);
		assertTrue(css.contains("#upload .ui-fileupload-content {\n\tflex: 1 1 auto;\n\tmin-height: 0;\n\toverflow: auto;\n}"), css);
	}
}
