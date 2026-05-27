package org.skyve.impl.generate.client.react;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;

class ReactSimpleViewsTest {
	@Test
	@SuppressWarnings("static-method")
	void calendarViewCreateWritesCalendarPlaceholder() throws Exception {
		Path projectDir = Files.createTempDirectory("react-calendar-view");
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		generator.srcSkyveViewsPath.mkdirs();

		ReactCalendarView view = new ReactCalendarView(generator, "admin", "UserCalendar");
		view.create();

		String content = Files.readString(projectDir.resolve("src/skyve/views/admin/UserCalendar.js"));
		assertTrue(content.contains("export class adminUserCalendar extends View"));
		assertTrue(content.contains("<h1>Calendar</h1>"));
	}

	@Test
	@SuppressWarnings("static-method")
	void mapViewCreateWritesMapPlaceholder() throws Exception {
		Path projectDir = Files.createTempDirectory("react-map-view");
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		generator.srcSkyveViewsPath.mkdirs();

		ReactMapView view = new ReactMapView(generator, "admin", "UserMap");
		view.create();

		String content = Files.readString(projectDir.resolve("src/skyve/views/admin/UserMap.js"));
		assertTrue(content.contains("export class adminUserMap extends View"));
		assertTrue(content.contains("<h1>Map</h1>"));
	}

	@Test
	@SuppressWarnings("static-method")
	void treeViewCreateWritesTreePlaceholder() throws Exception {
		Path projectDir = Files.createTempDirectory("react-tree-view");
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		generator.srcSkyveViewsPath.mkdirs();

		ReactTreeView view = new ReactTreeView(generator, "admin", "UserTree");
		view.create();

		String content = Files.readString(projectDir.resolve("src/skyve/views/admin/UserTree.js"));
		assertTrue(content.contains("export class adminUserTree extends View"));
		assertTrue(content.contains("<h1>Tree</h1>"));
	}
}