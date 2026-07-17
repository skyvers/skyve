package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Locale;

import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Icons;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	void localeScriptUsesDefaultBundlesForEnglish() {
		DesktopView view = new ResourceAwareDesktopView(null);
		
		view.createLocaleScripts(Locale.ENGLISH);
		
		assertEquals(script("isomorphic130/locales/frameworkMessages.properties") + "\n" + script("desktop/skyveMessages.js"),
						view.getLocaleScript());
	}
	
	@Test
	void localeScriptUsesLocalizedSkyveMessagesWhenWebappResourceExists() {
		DesktopView view = new ResourceAwareDesktopView("/desktop/skyveMessages_de.js");
		
		view.createLocaleScripts(Locale.GERMAN);
		
		String localeScript = view.getLocaleScript();
		assertTrue(localeScript.contains("isomorphic130/locales/frameworkMessages_de.properties"));
		assertTrue(localeScript.contains("desktop/skyveMessages_de.js"));
		assertFalse(localeScript.contains("skyveMessages_de.properties"));
	}
	
	@Test
	void localeScriptFallsBackToEnglishSkyveMessagesWhenLocalizedWebappResourceDoesNotExist() {
		DesktopView view = new ResourceAwareDesktopView(null);
		
		view.createLocaleScripts(Locale.GERMAN);
		
		String localeScript = view.getLocaleScript();
		assertTrue(localeScript.contains("isomorphic130/locales/frameworkMessages_de.properties"));
		assertTrue(localeScript.contains("desktop/skyveMessages.js"));
		assertFalse(localeScript.contains("desktop/skyveMessages_de.js"));
	}
	
	@Test
	void localeScriptUsesCorrectNorwegianFrameworkMessagesPath() {
		DesktopView view = new ResourceAwareDesktopView(null);
		
		view.createLocaleScripts(new Locale("nb", "NO"));
		
		String localeScript = view.getLocaleScript();
		assertTrue(localeScript.contains("isomorphic130/locales/frameworkMessages_nb_NO.properties"));
		assertFalse(localeScript.contains("frameworkMessages_.nb_NOproperties"));
		assertTrue(localeScript.contains("desktop/skyveMessages.js"));
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

	@Test
	void headerTemplateDefersHeightToSmartClientHeader() {
		DesktopView view = new DesktopView() {
			@Override
			public boolean isCanTextSearch() {
				return false;
			}
		};

		String template = view.getHeaderTemplate();

		assertTrue(template.startsWith("<table class=\"skyveHeaderTable\"><tr>"));
		assertFalse(template.contains("height=\"46px\""));
	}
	
	private static String script(String src) {
		return "<script type=\"text/javascript\" src=\"" + src + "\"></script>";
	}
	
	private static final class ResourceAwareDesktopView extends DesktopView {
		private static final long serialVersionUID = 1L;
		
		@Nullable
		private final String existingResourcePath;
		
		private ResourceAwareDesktopView(@Nullable String existingResourcePath) {
			this.existingResourcePath = existingResourcePath;
		}
		
		@Override
		boolean webResourceExists(@Nonnull String resourcePath) {
			return resourcePath.equals(existingResourcePath);
		}
	}
}
