package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

/**
 * Each PrimeFaces theme's head resources load the SAIL recorder only through {@code #{bean.sailRecording}}, and add
 * nothing when the gate fails: the tag sits inside a {@code c:if} straight after {@code skyve/prime/skyve-min.js}, with
 * no text of its own around it, so a page whose session is not recording is byte for byte what it was without the
 * recorder. {@code DesktopViewTest} tests the gate, which every page bean inherits from {@link LocalisableView}.
 */
@SuppressWarnings("static-method")
class PrimeFacesHeadSailRecorderTest {
	private static final String[] THEMES = {"editorial", "ultima", "ecuador", "diamond", "iframe"};
	private static final String SKYVE_MIN = "<script type=\"text/javascript\" src=\"skyve/prime/skyve-min.js?v=#{bean.webResourceFileVersion}\"></script>";
	static final String RECORDER = "<c:if test=\"#{bean.sailRecording}\"><script type=\"text/javascript\" src=\"skyve/sail/recorder-pf-min.js?v=#{bean.webResourceFileVersion}\"></script></c:if>";
	private static final String JSTL_NAMESPACE = "\n\t\txmlns:c=\"http://java.sun.com/jsp/jstl/core\"";

	@Test
	void eachThemeLoadsTheRecorderOnlyThroughTheGate() throws IOException {
		List<Executable> checks = new ArrayList<>(THEMES.length * 4);
		for (String theme : THEMES) {
			String head = Files.readString(Path.of("src/main/webapp/WEB-INF/pages/templates/" + theme + "/head-resources.xhtml"));
			String without = head.replace(RECORDER, "").replace(JSTL_NAMESPACE, "");
			checks.add(() -> assertTrue(head.contains(SKYVE_MIN + RECORDER + "\n"), theme + ": the recorder tag follows skyve-min.js on its line"));
			checks.add(() -> assertEquals(1, head.split("recorder-pf", -1).length - 1, theme + ": the recorder is referenced once"));
			checks.add(() -> assertFalse(without.contains("sail") || without.contains("c:"), theme + ": nothing else refers to SAIL or JSTL"));
			checks.add(() -> assertTrue(without.contains(SKYVE_MIN + "\n"), theme + ": without the tag, the line is as before"));
		}
		assertAll(checks.stream());
	}
}
