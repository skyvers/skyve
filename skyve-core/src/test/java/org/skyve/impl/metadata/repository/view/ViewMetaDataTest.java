package org.skyve.impl.metadata.repository.view;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ViewMetaDataTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new ViewMetaData());
	}

	@Test
	void nameRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setName("edit");
		assertEquals("edit", v.getName());
	}

	@Test
	void nameNullByDefault() {
		assertNull(new ViewMetaData().getName());
	}

	@Test
	void titleRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setTitle("My View");
		assertEquals("My View", v.getTitle());
	}

	@Test
	void titleNullByDefault() {
		assertNull(new ViewMetaData().getTitle());
	}

	@Test
	void icon32x32RelativeFileNameRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setIcon32x32RelativeFileName("icon.png");
		assertEquals("icon.png", v.getIcon32x32RelativeFileName());
	}

	@Test
	void iconStyleClassRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setIconStyleClass("fa-edit");
		assertEquals("fa-edit", v.getIconStyleClass());
	}

	@Test
	void helpRelativeFileNameRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setHelpRelativeFileName("help.html");
		assertEquals("help.html", v.getHelpRelativeFileName());
	}

	@Test
	void helpURLRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setHelpURL("https://example.com/help");
		assertEquals("https://example.com/help", v.getHelpURL());
	}

	@Test
	void refreshTimeInSecondsRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setRefreshTimeInSeconds(Integer.valueOf(30));
		assertEquals(Integer.valueOf(30), v.getRefreshTimeInSeconds());
	}

	@Test
	void refreshTimeInSecondsNullByDefault() {
		assertNull(new ViewMetaData().getRefreshTimeInSeconds());
	}

	@Test
	void refreshConditionNameRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setRefreshConditionName("canRefresh");
		assertEquals("canRefresh", v.getRefreshConditionName());
	}

	@Test
	void refreshActionNameRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setRefreshActionName("MyAction");
		assertEquals("MyAction", v.getRefreshActionName());
	}

	@Test
	void parametersListNonNull() {
		assertNotNull(new ViewMetaData().getParameters());
	}

	@Test
	void documentationRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setDocumentation("Some docs");
		assertEquals("Some docs", v.getDocumentation());
	}

	@Test
	void documentationNullByDefault() {
		assertNull(new ViewMetaData().getDocumentation());
	}

	@Test
	void lastModifiedMillisDefaultIsMaxValue() {
		assertEquals(Long.MAX_VALUE, new ViewMetaData().getLastModifiedMillis());
	}

	@Test
	void lastModifiedMillisRoundTrip() {
		ViewMetaData v = new ViewMetaData();
		v.setLastModifiedMillis(1234567890L);
		assertEquals(1234567890L, v.getLastModifiedMillis());
	}

	@Test
	void propertiesMapNonNull() {
		assertNotNull(new ViewMetaData().getProperties());
	}

	@Test
	void sidebarNullByDefault() {
		assertNull(new ViewMetaData().getSidebar());
	}

	@Test
	void actionsNullByDefault() {
		assertNull(new ViewMetaData().getActions());
	}

	@Test
	void accessesNullByDefault() {
		assertNull(new ViewMetaData().getAccesses());
	}
}
