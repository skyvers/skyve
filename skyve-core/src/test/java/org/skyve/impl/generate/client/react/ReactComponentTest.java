package org.skyve.impl.generate.client.react;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;

class ReactComponentTest {
	private static final String PAYLOAD = "export const value = 1;\n";

	@Test
	@SuppressWarnings("static-method")
	void compareToUsesModuleAndComponentKeyOrdering() throws Exception {
		Path projectDir = Files.createTempDirectory("react-component-test");
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());

		ReactComponent a = new StubReactComponent(generator, "admin", "User");
		ReactComponent b = new StubReactComponent(generator, "admin", "UserList");

		assertTrue(a.compareTo(b) < 0);
		assertTrue(b.compareTo(a) > 0);
		assertEquals(0, a.compareTo(new StubReactComponent(generator, "admin", "User")));
	}

	@Test
	@SuppressWarnings("static-method")
	void createCreatesModuleFolderAndWritesComponentFile() throws Exception {
		Path projectDir = Files.createTempDirectory("react-component-create");
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		generator.srcSkyveViewsPath.mkdirs();

		ReactComponent component = new StubReactComponent(generator, "admin", "UserEdit");
		component.create();

		File created = new File(generator.srcSkyveViewsPath, "admin/UserEdit.js");
		assertTrue(created.exists());
		assertEquals(PAYLOAD, Files.readString(created.toPath()));
	}

	@Test
	@SuppressWarnings("static-method")
	void createOverwritesExistingComponentFile() throws Exception {
		Path projectDir = Files.createTempDirectory("react-component-overwrite");
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		generator.srcSkyveViewsPath.mkdirs();

		File moduleDir = new File(generator.srcSkyveViewsPath, "admin");
		moduleDir.mkdirs();
		File existing = new File(moduleDir, "UserEdit.js");
		try (FileWriter fw = new FileWriter(existing)) {
			fw.write("stale\n");
		}

		ReactComponent component = new StubReactComponent(generator, "admin", "UserEdit");
		component.create();

		assertEquals(PAYLOAD, Files.readString(existing.toPath()));
	}

	private static final class StubReactComponent extends ReactComponent {
		private StubReactComponent(ReactGenerator generator, String moduleName, String componentName) {
			super(generator, moduleName, componentName);
		}

		@Override
		protected void create(FileWriter fw) throws IOException {
			fw.write(PAYLOAD);
		}
	}
}