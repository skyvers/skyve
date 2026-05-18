package org.skyve.impl.metadata.view;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

class DownloadAreaTypeTest {

	@Test
	@SuppressWarnings("static-method")
	void valuesContainsTwoTypes() {
		assertEquals(2, DownloadAreaType.values().length);
	}

	@Test
	@SuppressWarnings("static-method")
	void valueOfResources() {
		assertNotNull(DownloadAreaType.valueOf("resources"));
	}

	@Test
	@SuppressWarnings("static-method")
	void valueOfContent() {
		assertNotNull(DownloadAreaType.valueOf("content"));
	}
}
