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
 * The PrimeFaces edit and list pages render the SAIL recorder's client-id map and its inline state only through
 * {@code #{skyve.sailRecording}}, and add nothing when the gate fails: each span sits inside a {@code c:if} glued to the
 * end of the tag before it, with no text of its own around it, so a page whose session is not recording is byte for
 * byte what it was without the recorder. The map is inside the form (an ajax update of the form renders it again with
 * what the view shows); the state is outside it (it is read once, when the page loads).
 * {@code PrimeFacesRecorderRoundTripSailIT} compares the rendered pages.
 */
@SuppressWarnings("static-method")
class PrimeFacesPageSailRecorderTest {
	private static final String JSTL_NAMESPACE = "\n\t\txmlns:c=\"http://java.sun.com/jsp/jstl/core\"";
	private static final String INCLUDE_END = "</ui:include>";
	private static final String FORM_END = "</s:csrfForm>";
	private static final String STATE = "<c:if test=\"#{skyve.sailRecording}\"><span id=\"sailRecorderState\" hidden=\"hidden\" data-state=\"#{skyve.sailRecordingState}\"></span></c:if>";

	private static String map(String getter) {
		return "<c:if test=\"#{skyve.sailRecording}\"><span id=\"sailRecorderMap\" hidden=\"hidden\" data-map=\"#{skyve." + getter + "}\"></span></c:if>";
	}

	@Test
	void editRendersTheMapAndStateOnlyThroughTheGate() throws IOException {
		check("edit", map("sailRecordingEditMap"));
	}

	@Test
	void listRendersTheMapAndStateOnlyThroughTheGate() throws IOException {
		check("list", map("sailRecordingListMap"));
	}

	private static void check(String page, String map) throws IOException {
		String xhtml = Files.readString(Path.of("src/main/webapp/external/" + page + ".xhtml"));
		String without = xhtml.replace(map, "").replace(STATE, "").replace(JSTL_NAMESPACE, "");
		assertAll(() -> assertTrue(xhtml.contains(INCLUDE_END + map + "\n"), page + ": the map follows the focus include on its line"),
					() -> assertTrue(xhtml.contains(FORM_END + STATE + "\n"), page + ": the state follows the form on its line"),
					() -> assertTrue(xhtml.indexOf(map) < xhtml.indexOf(FORM_END), page + ": the map is inside the form"),
					() -> assertEquals(2, xhtml.split("#\\{skyve.sailRecording}", -1).length - 1, page + ": two gated spans"),
					() -> assertFalse(without.contains("sail") || without.contains("c:"), page + ": nothing else refers to SAIL or JSTL"),
					() -> assertTrue(without.contains(INCLUDE_END + "\n") && without.contains(FORM_END + "\n"),
										page + ": without the spans, the lines are as before"));
	}
}
