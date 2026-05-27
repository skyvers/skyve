package org.skyve.impl.generate.client.flutter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.FileWriter;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.client.flutter.FlutterGenerator.GeneratorConfig;

class FlutterSimpleViewsTest {
	@Test
	@SuppressWarnings("static-method")
	void calendarCreateIsNoOp() throws Exception {
		FlutterCalendarView view = new FlutterCalendarView(mockGenerator("my_app"), "admin", "UserCal");
		Path file = Files.createTempFile("flutter-calendar", ".dart");

		try (FileWriter writer = new FileWriter(file.toFile())) {
			view.create(writer);
		}

		assertEquals("", Files.readString(file));
	}

	@Test
	@SuppressWarnings("static-method")
	void mapCreateIsNoOp() throws Exception {
		FlutterMapView view = new FlutterMapView(mockGenerator("my_app"), "admin", "UserMap");
		Path file = Files.createTempFile("flutter-map", ".dart");

		try (FileWriter writer = new FileWriter(file.toFile())) {
			view.create(writer);
		}

		assertEquals("", Files.readString(file));
	}

	@Test
	@SuppressWarnings("static-method")
	void treeCreateIsNoOp() throws Exception {
		FlutterTreeView view = new FlutterTreeView(mockGenerator("my_app"), "admin", "UserTree");
		Path file = Files.createTempFile("flutter-tree", ".dart");

		try (FileWriter writer = new FileWriter(file.toFile())) {
			view.create(writer);
		}

		assertEquals("", Files.readString(file));
	}

	private static FlutterGenerator mockGenerator(String projectName) {
		GeneratorConfig config = new GeneratorConfig();
		config.setProjectName(projectName);

		FlutterGenerator generator = mock(FlutterGenerator.class);
		when(generator.getConfig()).thenReturn(config);
		return generator;
	}
}