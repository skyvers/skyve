package org.skyve.impl.web.faces.components;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class SkyveTaglibStructureTest {
	@Test
	void setUxUiComponentAndTagAreAbsent() throws IOException {
		assertNull(classOrNull("org.skyve.impl.web.faces.components.SetUxUi"));
		@SuppressWarnings("resource") // The immediately following try-with-resources owns this stream.
		InputStream resource = java.util.Objects.requireNonNull(
				SkyveTaglibStructureTest.class.getResourceAsStream("/META-INF/skyve.taglib.xml"));
		try (InputStream stream = resource) {
			String taglib = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
			assertFalse(taglib.contains("<tag-name>setUxUi</tag-name>"));
			assertFalse(taglib.contains("org.skyve.impl.web.faces.components.SetUxUi"));
		}
	}

	private static Class<?> classOrNull(String name) {
		try {
			return Class.forName(name);
		}
		catch (@SuppressWarnings("unused") ClassNotFoundException e) {
			return null;
		}
	}
}
