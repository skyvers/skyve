package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.net.MalformedURLException;
import java.net.URI;

import org.junit.jupiter.api.Test;
import org.skyve.impl.web.RequestUxUiSelectionTestUtil;
import org.skyve.metadata.router.UxUi;
import org.skyve.web.UserAgentType;

import jakarta.servlet.ServletContext;
import jakarta.servlet.http.HttpServletRequest;

@SuppressWarnings({ "static-method", "java:S1192", "java:S5960" }) // Repeated literals and assertions are test-only.
class FacesThemeViewTest {
	@Test
	void resolvesEditorialStandardCustomAndForcedDarkPalettes() {
		FacesThemeView standard = viewFor(UxUi.newPrimeFaces("tablet", "editorial", "skyve", "blue"));
		FacesThemeView custom = viewFor(UxUi.newPrimeFaces("tablet", "editorial", "customer-theme", "brand-ocean"));
		FacesThemeView dark = viewFor(UxUi.newPrimeFaces("tablet", "editorial", "skyve-dark", "dark"));

		assertAll(
				() -> assertEquals("blue", standard.getBaseColour()),
				() -> assertNull(standard.getScheme()),
				() -> assertFalse(standard.isForcedDark()),
				() -> assertEquals("brand-ocean", custom.getBaseColour()),
				() -> assertNull(custom.getScheme()),
				() -> assertNull(dark.getScheme()),
				() -> assertTrue(dark.isForcedDark()));
	}

	@Test
	void resolvesTemplateAndColourFallbacks() {
		FacesThemeView missing = viewFor(UxUi.newPrimeFaces("tablet", null, "exact-theme", null));
		FacesThemeView padded = viewFor(
				UxUi.newPrimeFaces("tablet", "  customer-template  ", "exact-theme", "  brand-ocean  "));
		FacesThemeView blank = viewFor(UxUi.newPrimeFaces("tablet", "external", "exact-theme", "   "));

		assertAll(
				() -> assertEquals("external", missing.getTemplateName()),
				() -> assertEquals("blue", missing.getBaseColour()),
				() -> assertEquals("customer-template", padded.getTemplateName()),
				() -> assertEquals("brand-ocean", padded.getBaseColour()),
				() -> assertEquals("blue", blank.getBaseColour()));
	}

	@Test
	void derivesDiamondAndUltimaLayoutValues() {
		FacesThemeView diamond = viewFor(
				UxUi.newPrimeFaces("tablet", "diamond", "diamond-indigo-light", "indigo-light"));
		FacesThemeView ultima = viewFor(UxUi.newPrimeFaces("tablet", "ultima", "ultima-green", "green"));

		assertAll(
				() -> assertEquals("indigo", diamond.getBaseColour()),
				() -> assertEquals("light", diamond.getScheme()),
				() -> assertEquals("layout-sidebar-indigo", diamond.getLayoutStyleClass()),
				() -> assertEquals("layout-menu-green layout-topbar-green", ultima.getLayoutStyleClass()));
	}

	@Test
	void readsConfiguredComponentThemeDirectlyFromSelectedUxUi() {
		FacesThemeView exact = viewFor(
				UxUi.newPrimeFaces("tablet", "external", " vendor-theme-padded ", "blue"));
		FacesThemeView absent = viewFor(UxUi.newPrimeFaces("tablet", "external", null, "blue"));
		FacesThemeView smartClient = viewFor(
				UxUi.newSmartClient("desktop", "Tahoe", "casablanca", "smartclient"));

		assertAll(
				() -> assertEquals(" vendor-theme-padded ", exact.getComponentThemeName()),
				() -> assertFalse(exact.isSmartClient()),
				() -> assertNull(absent.getComponentThemeName()),
				() -> assertEquals("casablanca", smartClient.getComponentThemeName()),
				() -> assertTrue(smartClient.isSmartClient()),
				() -> assertEquals("external", smartClient.getTemplateName()));
	}

	@Test
	void usesCompleteIframeResourcePairAndResolvesOnlyOnce() throws Exception {
		ThemeRequest request = requestFor(
				UxUi.newPrimeFaces("tablet", "customer-template", "customer-theme", "brand"),
				UserAgentType.tablet,
				false);
		when(request.servletContext.getResource("/WEB-INF/pages/templates/customer-template/head-resources.xhtml"))
				.thenReturn(URI.create("file:/customer-template/head-resources.xhtml").toURL());
		when(request.servletContext.getResource("/WEB-INF/pages/templates/customer-template/body-resources.xhtml"))
				.thenReturn(URI.create("file:/customer-template/body-resources.xhtml").toURL());
		FacesThemeView view = new FacesThemeView(request.request);

		assertEquals("customer-template", view.getIframeResourceTemplateName());
		assertEquals("brand", view.getBaseColour());
		assertEquals("customer-template", view.getIframeResourceTemplateName());
		verify(request.servletContext, times(2)).getResource(any());
	}

	@Test
	void fallsBackToIframeResourcesForPartialOrMalformedFamily() throws Exception {
		ThemeRequest partial = requestFor(
				UxUi.newPrimeFaces("tablet", "customer-template", "customer-theme", "brand"),
				UserAgentType.tablet,
				false);
		when(partial.servletContext.getResource("/WEB-INF/pages/templates/customer-template/head-resources.xhtml"))
				.thenReturn(URI.create("file:/customer-template/head-resources.xhtml").toURL());
		ThemeRequest malformed = requestFor(
				UxUi.newPrimeFaces("tablet", "broken-template", "customer-theme", "brand"),
				UserAgentType.tablet,
				false);
		when(malformed.servletContext.getResource(any())).thenThrow(new MalformedURLException("broken"));

		assertEquals("iframe", new FacesThemeView(partial.request).getIframeResourceTemplateName());
		assertEquals("iframe", new FacesThemeView(malformed.request).getIframeResourceTemplateName());
	}

	@Test
	void phoneStatusUsesEffectiveRequestSelection() {
		FacesThemeView phone = new FacesThemeView(requestFor(
				UxUi.newPrimeFaces("phone", "editorial", "skyve", "blue"),
				UserAgentType.phone,
				false).request);
		FacesThemeView tablet = new FacesThemeView(requestFor(
				UxUi.newPrimeFaces("tablet", "external", "exact", "blue"),
				UserAgentType.tablet,
				true).request);

		assertTrue(phone.isPhone());
		assertFalse(tablet.isPhone());
	}

	private static FacesThemeView viewFor(UxUi uxui) {
		return new FacesThemeView(requestFor(uxui, UserAgentType.tablet, false).request);
	}

	private static ThemeRequest requestFor(UxUi uxui, UserAgentType type, boolean emulated) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		ServletContext servletContext = mock(ServletContext.class);
		when(request.getServletContext()).thenReturn(servletContext);
		RequestUxUiSelectionTestUtil.install(request, type, emulated, uxui);
		return new ThemeRequest(request, servletContext);
	}

	private record ThemeRequest(HttpServletRequest request, ServletContext servletContext) {
		// Test fixture.
	}
}
