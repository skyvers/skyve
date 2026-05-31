package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;

@SuppressWarnings("static-method")
class LocalisableViewTest {
	private static final class TestLocalisableView extends LocalisableView {
		private static final long serialVersionUID = 1L;
	}

	@Test
	void basicGettersExposeEncodingAndEnvironmentValues() {
		TestLocalisableView view = new TestLocalisableView();

		assertEquals("UTF-8", view.getEncoding());
		assertEquals(UtilImpl.ENVIRONMENT_IDENTIFIER, view.getEnvironmentIdentifier());
		assertEquals(UtilImpl.WEB_RESOURCE_FILE_VERSION, view.getWebResourceFileVersion());
		assertNotNull(view.getI18n());
	}

	@Test
	void i18nMapAdapterUnsupportedOperationsThrow() {
		LocalisableView.I18nMapAdapter map = new LocalisableView.I18nMapAdapter();

		assertThrows(UnsupportedOperationException.class, map::size);
		assertThrows(UnsupportedOperationException.class, map::isEmpty);
		assertThrows(UnsupportedOperationException.class, () -> map.containsKey("k"));
		assertThrows(UnsupportedOperationException.class, () -> map.containsValue("v"));
		assertThrows(UnsupportedOperationException.class, () -> map.put("k", "v"));
		assertThrows(UnsupportedOperationException.class, () -> map.remove("k"));
		assertThrows(UnsupportedOperationException.class, () -> map.putAll(java.util.Map.of("k", "v")));
		assertThrows(UnsupportedOperationException.class, map::clear);
		assertThrows(UnsupportedOperationException.class, map::keySet);
		assertThrows(UnsupportedOperationException.class, map::values);
		assertThrows(UnsupportedOperationException.class, map::entrySet);
	}
}
