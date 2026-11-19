package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;

/**
 * The SmartClient home page loads the SAIL recorder only through {@code #{desktop.sailRecording}}, and adds nothing
 * when the gate fails: the tag sits inside a {@code c:if} straight after {@code skyve-min.js}, with no text of its own
 * around it, so a page whose session is not recording is byte for byte what it was without the recorder.
 * {@code DesktopViewTest} tests the gate; {@code SmartClientRecorderRoundTripSailIT} compares the rendered pages.
 */
@SuppressWarnings("static-method")
class DesktopHomeSailRecorderTest {
	static final String SKYVE_MIN = "<script type=\"text/javascript\" src=\"skyve/desktop/skyve-min.js?v=#{desktop.webResourceFileVersion}\"></script>";
	static final String RECORDER = "<c:if test=\"#{desktop.sailRecording}\"><script type=\"text/javascript\" src=\"skyve/sail/recorder-sc-min.js?v=#{desktop.webResourceFileVersion}\"></script></c:if>";

	@Test
	void theRecorderIsOnlyLoadedThroughTheGate() throws IOException {
		String home = Files.readString(Path.of("src/main/webapp/desktop/home.xhtml"));
		String without = home.replace(RECORDER, "");
		assertAll(() -> assertTrue(home.contains(SKYVE_MIN + RECORDER + "\n"), "the recorder tag follows skyve-min.js on its line"),
					() -> assertEquals(1, home.split("recorder-sc", -1).length - 1, "the recorder is referenced once"),
					() -> assertFalse(without.contains("sail"), "nothing else refers to SAIL"),
					() -> assertTrue(without.contains(SKYVE_MIN + "\n"), "without the tag, the line is as before"));
	}
}
