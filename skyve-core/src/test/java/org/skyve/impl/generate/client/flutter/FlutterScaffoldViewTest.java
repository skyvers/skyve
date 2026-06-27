package org.skyve.impl.generate.client.flutter;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.FileWriter;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;

class FlutterScaffoldViewTest {
	@Test
	@SuppressWarnings("static-method")
	void calendarCreateIsNoOpWithoutThrowing() throws Exception {
		Path file = Files.createTempFile("flutter-calendar", ".dart");

		try (FileWriter fw = new FileWriter(file.toFile())) {
			new FlutterCalendarView(null, "admin", "Calendar").create(fw);
		}

		assertEquals("", Files.readString(file));
	}

	@Test
	@SuppressWarnings("static-method")
	void mapCreateIsNoOpWithoutThrowing() throws Exception {
		Path file = Files.createTempFile("flutter-map", ".dart");

		try (FileWriter fw = new FileWriter(file.toFile())) {
			new FlutterMapView(null, "admin", "Map").create(fw);
		}

		assertEquals("", Files.readString(file));
	}

	@Test
	@SuppressWarnings("static-method")
	void treeCreateIsNoOpWithoutThrowing() throws Exception {
		Path file = Files.createTempFile("flutter-tree", ".dart");

		try (FileWriter fw = new FileWriter(file.toFile())) {
			new FlutterTreeView(null, "admin", "Tree").create(fw);
		}

		assertEquals("", Files.readString(file));
	}

}