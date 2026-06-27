package org.skyve.impl.generate.client.flutter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import org.junit.jupiter.api.Test;

class FlutterViewTest {
	private static final String GENERATED = "class Generated {}\n";

	@Test
	@SuppressWarnings("static-method")
	void compareToUsesGeneratedClassNameOrdering() {
		FlutterGenerator generator = mock(FlutterGenerator.class);
		FlutterView a = new StubFlutterView(generator, "admin", "User");
		FlutterView b = new StubFlutterView(generator, "admin", "UserList");

		assertTrue(a.compareTo(b) < 0);
		assertTrue(b.compareTo(a) > 0);
		assertEquals(0, a.compareTo(new StubFlutterView(generator, "admin", "User")));
	}

	@Test
	@SuppressWarnings("static-method")
	void createWritesDartFileToModuleFolder() throws Exception {
		Path root = Files.createTempDirectory("flutter-view-create");
		File libViewsPath = root.resolve("lib/views").toFile();
		libViewsPath.mkdirs();

		FlutterGenerator generator = mockGenerator(libViewsPath);
		StubFlutterView view = new StubFlutterView(generator, "admin", "UserEdit");

		view.create();

		File file = new File(new File(libViewsPath, "admin"), view.fileName);
		assertTrue(file.exists());
		assertEquals(GENERATED, Files.readString(file.toPath()));
	}

	@Test
	@SuppressWarnings("static-method")
	void createOverwritesExistingDartFile() throws Exception {
		Path root = Files.createTempDirectory("flutter-view-overwrite");
		File libViewsPath = root.resolve("lib/views").toFile();
		File modulePath = new File(libViewsPath, "admin");
		modulePath.mkdirs();
		StubFlutterView view = new StubFlutterView(mockGenerator(libViewsPath), "admin", "UserEdit");

		File existing = new File(modulePath, view.fileName);
		try (FileWriter fw = new FileWriter(existing)) {
			fw.write("old\n");
		}

		view.create();

		assertEquals(GENERATED, Files.readString(existing.toPath()));
	}

	@Test
	@SuppressWarnings("static-method")
	void substituteLoadsClasspathTemplateAndReplacesTokens() throws Exception {
		FlutterGenerator generator = mock(FlutterGenerator.class);
		FlutterView view = new StubFlutterView(generator, "admin", "UserEdit");

		String result = view.substitute("/org/skyve/impl/generate/client/flutter/test-template.txt",
				Map.of("##CLASS##", "MyWidget", "##VALUE##", "abc"));

		assertTrue(result.contains("class MyWidget"));
		assertTrue(result.contains("\"abc\""));
	}

	private static FlutterGenerator mockGenerator(File libViewsPath) {
		FlutterGenerator generator = mock(FlutterGenerator.class);
		FlutterGenerator.GeneratorConfig config = new FlutterGenerator.GeneratorConfig();
		config.setLibViewsPath(libViewsPath);
		when(generator.getConfig()).thenReturn(config);
		return generator;
	}

	private static final class StubFlutterView extends FlutterView {
		private StubFlutterView(FlutterGenerator generator, String moduleName, String viewName) {
			super(generator, moduleName, viewName);
		}

		@Override
		protected void create(FileWriter fw) throws IOException {
			fw.write(GENERATED);
		}
	}
}