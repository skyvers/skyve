package org.skyve.impl.metadata.view;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

public class DownloadAreaTypeTest {

	@Test
	@SuppressWarnings("static-method")
	public void valuesContainsTwoTypes() {
		assertEquals(2, DownloadAreaType.values().length);
	}

	@Test
	@SuppressWarnings("static-method")
	public void valueOfResources() {
		assertNotNull(DownloadAreaType.valueOf("resources"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void valueOfContent() {
		assertNotNull(DownloadAreaType.valueOf("content"));
	}
}
