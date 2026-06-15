package org.skyve.metadata.view;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.View.ViewParameter;

/**
 * Tests for the {@link View.ViewParameter} inner class and {@link View.ViewType} enum
 * in the {@code metadata/view} package.
 */
@SuppressWarnings("static-method")
class ViewParameterTest {

	// ---- ViewParameter ----

	@Test
	void fromBindingRoundTrip() {
		ViewParameter vp = new ViewParameter();
		vp.setFromBinding("contact.name");
		assertEquals("contact.name", vp.getFromBinding());
	}

	@Test
	void fromBindingBlankBecomesNull() {
		ViewParameter vp = new ViewParameter();
		vp.setFromBinding("   ");
		assertNull(vp.getFromBinding());
	}

	@Test
	void fromBindingNullBecomesNull() {
		ViewParameter vp = new ViewParameter();
		vp.setFromBinding(null);
		assertNull(vp.getFromBinding());
	}

	@Test
	void boundToRoundTrip() {
		ViewParameter vp = new ViewParameter();
		vp.setBoundTo("invoice.contactId");
		assertEquals("invoice.contactId", vp.getBoundTo());
	}

	@Test
	void boundToBlankBecomesNull() {
		ViewParameter vp = new ViewParameter();
		vp.setBoundTo("  ");
		assertNull(vp.getBoundTo());
	}

	@Test
	void defaultsAreNull() {
		ViewParameter vp = new ViewParameter();
		assertNull(vp.getFromBinding());
		assertNull(vp.getBoundTo());
	}

	// ---- ViewType enum ----

	@Test
	void viewTypeValuesContainExpectedConstants() {
		View.ViewType[] types = View.ViewType.values();
		assertEquals(5, types.length);
	}

	@Test
	void viewTypeEditValueOf() {
		assertEquals(View.ViewType.edit, View.ViewType.valueOf("edit"));
	}

	@Test
	void viewTypeListValueOf() {
		assertEquals(View.ViewType.list, View.ViewType.valueOf("list"));
	}

	// ---- Action.ActionShow enum ----

	@Test
	void actionShowValuesContainThreeConstants() {
		Action.ActionShow[] shows = Action.ActionShow.values();
		assertEquals(3, shows.length);
	}

	@Test
	void actionShowIconValueOf() {
		assertEquals(Action.ActionShow.icon, Action.ActionShow.valueOf("icon"));
	}

	@Test
	void actionShowTextValueOf() {
		assertEquals(Action.ActionShow.text, Action.ActionShow.valueOf("text"));
	}

	@Test
	void actionShowBothValueOf() {
		assertEquals(Action.ActionShow.both, Action.ActionShow.valueOf("both"));
	}

	// ---- View.getLocalisedTitle default method ----

	@Test
	void viewImplLocalisedTitleIsNotNull() {
		org.skyve.impl.metadata.view.ViewImpl vi = new org.skyve.impl.metadata.view.ViewImpl();
		vi.setTitle("Test View");
		// Util.i18n() returns the key when no bundle is loaded, so result is non-null
		assertNotNull(vi.getLocalisedTitle());
	}
}
