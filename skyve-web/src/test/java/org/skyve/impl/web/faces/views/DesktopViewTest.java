package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Icons;

@SuppressWarnings("static-method")
class DesktopViewTest {
	@Test
	void gettersReturnNullBeforePreRenderInitialisesScripts() {
		DesktopView view = new DesktopView();

		assertNull(view.getLocaleScript());
		assertNull(view.getMenuScript());
		assertNull(view.getDataSourceScript());
		assertNull(view.getUiScript());
		assertNull(view.getBannerScript());
		assertNull(view.getSkin());
	}

	@Test
	void getSmartClientDirReturnsConfiguredDirectory() {
		DesktopView view = new DesktopView();

		assertEquals(UtilImpl.SMART_CLIENT_DIR, view.getSmartClientDir());
	}

	@Test
	void headerTemplateIncludesSearchButtonWhenTextSearchIsEnabled() {
		DesktopView view = new DesktopView() {
			@Override
			public boolean isCanTextSearch() {
				return true;
			}
		};

		String template = view.getHeaderTemplate();

		assertTrue(template.contains("popupSearch"));
		assertTrue(template.contains(Icons.FONT_SEARCH));
		assertTrue(template.contains(Icons.FONT_HELP));
		assertTrue(template.contains(Icons.FONT_DASHBOARD));
		assertTrue(template.contains("skyve-thick-grey.png"));
	}

	@Test
	void headerTemplateOmitsSearchButtonWhenTextSearchIsDisabled() {
		DesktopView view = new DesktopView() {
			@Override
			public boolean isCanTextSearch() {
				return false;
			}
		};

		String template = view.getHeaderTemplate();

		assertFalse(template.contains("popupSearch"));
		assertFalse(template.contains(Icons.FONT_SEARCH));
		assertTrue(template.contains(Icons.FONT_HELP));
		assertTrue(template.contains(Icons.FONT_DASHBOARD));
		assertTrue(template.startsWith("<table"));
	}
}