package org.skyve.metadata.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class UxUiTest {

	@Test
	void newPrimeFacesWithoutColourSetsProperties() {
		UxUi ui = UxUi.newPrimeFaces("desktop", "ace", "omega");
		assertEquals("desktop", ui.getName());
		assertEquals("ace", ui.getPfTemplateName());
		assertEquals("omega", ui.getPfThemeName());
		assertNull(ui.getPfThemeColour());
		assertNull(ui.getScSkin());
	}

	@Test
	void newPrimeFacesWithColourSetsPfThemeColour() {
		UxUi ui = UxUi.newPrimeFaces("desktop", "ace", "omega", "blue");
		assertEquals("blue", ui.getPfThemeColour());
	}

	@Test
	void getPfThemeNameWithColourReturnsExactTheme() {
		UxUi ui = UxUi.newPrimeFaces("desktop", "ace", "omega", "blue");
		assertEquals("omega", ui.getPfThemeName());
	}

	@Test
	void getPfThemeNameWithoutColourReturnsThemeName() {
		UxUi ui = UxUi.newPrimeFaces("desktop", "ace", "omega");
		assertEquals("omega", ui.getPfThemeName());
	}

	@Test
	void newSmartClientWithoutColourSetsProperties() {
		UxUi ui = UxUi.newSmartClient("desktop", "Enterprise", "omega");
		assertEquals("desktop", ui.getName());
		assertEquals("Enterprise", ui.getScSkin());
		assertEquals("omega", ui.getPfThemeName());
		assertNull(ui.getPfThemeColour());
		assertNull(ui.getPfTemplateName());
	}

	@Test
	void newSmartClientWithColourSetsPfThemeColour() {
		UxUi ui = UxUi.newSmartClient("desktop", "Enterprise", "omega", "green");
		assertEquals("green", ui.getPfThemeColour());
		assertEquals("omega", ui.getPfThemeName());
	}

	@Test
	void setNameUpdatesName() {
		UxUi ui = UxUi.newPrimeFaces("original", "ace", "omega");
		ui.setName("updated");
		assertEquals("updated", ui.getName());
	}

	@Test
	void setPfTemplateNameUpdates() {
		UxUi ui = UxUi.newPrimeFaces("desktop", "ace", "omega");
		ui.setPfTemplateName("sigma");
		assertEquals("sigma", ui.getPfTemplateName());
	}

	@Test
	void setPfThemeNameUpdates() {
		UxUi ui = UxUi.newPrimeFaces("desktop", "ace", "omega");
		ui.setPfThemeName("nova");
		assertEquals("nova", ui.getPfThemeName());
	}

	@Test
	void setPfThemeColourUpdates() {
		UxUi ui = UxUi.newPrimeFaces("desktop", "ace", "omega");
		ui.setPfThemeColour("red");
		assertEquals("red", ui.getPfThemeColour());
	}

	@Test
	void setScSkinUpdates() {
		UxUi ui = UxUi.newSmartClient("desktop", "Enterprise", "omega");
		ui.setScSkin("Graphite");
		assertEquals("Graphite", ui.getScSkin());
	}

	@Test
	void toStringIsNotNull() {
		UxUi ui = UxUi.newPrimeFaces("desktop", "ace", "omega", "blue");
		assertNotNull(ui.toString());
	}

	@Test
	void desktopNameConstantIsDesktop() {
		assertEquals("desktop", UxUi.DESKTOP_NAME);
	}
}
