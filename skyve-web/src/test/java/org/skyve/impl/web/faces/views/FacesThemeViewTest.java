package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.router.UxUi;

@SuppressWarnings("static-method")
class FacesThemeViewTest {
	@Test
	void primeFacesProfileExposesConfiguredTemplateAndColour() throws Exception {
		FacesThemeView view = viewFor(UxUi.newPrimeFaces("tablet", "ultima", "saga", "green"));

		assertEquals("ultima", view.getTemplateName());
		assertEquals("green", view.getThemeColour());
		assertFalse(view.isDark());
	}

	@Test
	void smartClientProfileUsesExternalPrimeFacesShell() throws Exception {
		FacesThemeView view = viewFor(UxUi.newSmartClient("desktop", "Tahoe", "casablanca"));

		assertEquals("external", view.getTemplateName());
		assertEquals("blue", view.getThemeColour());
	}

	@Test
	void templateFamiliesUseTheirEstablishedDefaultColours() throws Exception {
		assertEquals("blue", viewFor(UxUi.newPrimeFaces("tablet", "editorial", "saga")).getThemeColour());
		assertEquals("indigo", viewFor(UxUi.newPrimeFaces("tablet", "ecuador", "saga")).getThemeColour());
		assertEquals("indigo", viewFor(UxUi.newPrimeFaces("tablet", "ultima", "saga")).getThemeColour());
		assertEquals("indigo-light", viewFor(UxUi.newPrimeFaces("tablet", "diamond", "saga")).getThemeColour());
	}

	@Test
	void darkColourForcesDarkPresentation() throws Exception {
		FacesThemeView view = viewFor(UxUi.newPrimeFaces("tablet", "editorial", "skyve-dark", "dark"));

		assertTrue(view.isDark());
	}

	private static FacesThemeView viewFor(UxUi uxui) throws Exception {
		FacesThemeView result = new FacesThemeView();
		Field field = FacesThemeView.class.getDeclaredField("uxui");
		field.setAccessible(true);
		field.set(result, uxui);
		return result;
	}
}
