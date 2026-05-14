package org.skyve.impl.metadata.view.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ComponentTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new Component());
	}

	@Test
	void moduleNameNullByDefault() {
		assertNull(new Component().getModuleName());
	}

	@Test
	void moduleNameRoundTrip() {
		Component c = new Component();
		c.setModuleName("admin");
		assertEquals("admin", c.getModuleName());
	}

	@Test
	void documentNameNullByDefault() {
		assertNull(new Component().getDocumentName());
	}

	@Test
	void documentNameRoundTrip() {
		Component c = new Component();
		c.setDocumentName("UserDashboard");
		assertEquals("UserDashboard", c.getDocumentName());
	}

	@Test
	void nameNullByDefault() {
		assertNull(new Component().getName());
	}

	@Test
	void nameRoundTrip() {
		Component c = new Component();
		c.setName("editView");
		assertEquals("editView", c.getName());
	}

	@Test
	void widgetIdNullByDefault() {
		assertNull(new Component().getWidgetId());
	}

	@Test
	void widgetIdRoundTrip() {
		Component c = new Component();
		c.setWidgetId("widget123");
		assertEquals("widget123", c.getWidgetId());
	}

	@Test
	void invisibleConditionNameNullByDefault() {
		assertNull(new Component().getInvisibleConditionName());
	}

	@Test
	void invisibleConditionNameRoundTrip() {
		Component c = new Component();
		c.setInvisibleConditionName("hideIt");
		assertEquals("hideIt", c.getInvisibleConditionName());
	}

	@Test
	void invisibleConditionNameBlankBecomesNull() {
		Component c = new Component();
		c.setInvisibleConditionName("  ");
		assertNull(c.getInvisibleConditionName());
	}

	@Test
	void visibleConditionNameNegatesAndSetsInvisible() {
		Component c = new Component();
		c.setVisibleConditionName("showIt");
		assertEquals("notShowIt", c.getInvisibleConditionName());
	}

	@Test
	void visibleConditionNameBlankNegatesToNull() {
		Component c = new Component();
		c.setVisibleConditionName("  ");
		assertNull(c.getInvisibleConditionName());
	}

	@Test
	void propertiesMapIsNotNull() {
		assertNotNull(new Component().getProperties());
	}

	@Test
	void propertiesMapAcceptsEntries() {
		Component c = new Component();
		c.getProperties().put("key", "value");
		assertEquals("value", c.getProperties().get("key"));
	}

	@Test
	void namesListIsNotNull() {
		assertNotNull(new Component().getNames());
	}
}
