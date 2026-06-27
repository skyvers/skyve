package org.skyve.impl.metadata.view;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collection;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.container.Sidebar;

/**
 * Tests for {@link ViewImpl}, covering the default method on the {@link org.skyve.metadata.view.View} interface.
 */
@SuppressWarnings("static-method")
class ViewImplTest {

	@Test
	void localisedTitleReturnsValueWhenTitleSet() {
		ViewImpl v = new ViewImpl();
		v.setTitle("My View");
		assertNotNull(v.getLocalisedTitle());
	}

	@Test
	void localisedTitleReturnsNullWhenTitleNull() {
		ViewImpl v = new ViewImpl();
		assertNull(v.getLocalisedTitle());
	}

	@Test
	void setRefreshConditionNameRoundtrips() {
		ViewImpl v = new ViewImpl();
		v.setRefreshConditionName("myCondition");
		assertEquals("myCondition", v.getRefreshConditionName());
	}

	@Test
	void setRefreshActionNameRoundtrips() {
		ViewImpl v = new ViewImpl();
		v.setRefreshActionName("myAction");
		assertEquals("myAction", v.getRefreshActionName());
	}

	@Test
	void setLastCheckedMillisRoundtrips() {
		ViewImpl v = new ViewImpl();
		v.setLastCheckedMillis(12345L);
		assertEquals(12345L, v.getLastCheckedMillis());
	}

	@Test
	void getInlineModelReturnsNullForUnknownName() {
		ViewImpl v = new ViewImpl();
		assertNull(v.getInlineModel("unknown"));
	}

	@Test
	void getOverriddenCustomerNameRoundtrips() {
		ViewImpl v = new ViewImpl();
		v.setOverriddenCustomerName("acme");
		assertEquals("acme", v.getOverriddenCustomerName());
	}

	@Test
	void setIcon32x32RelativeFileNameRoundtrips() {
		ViewImpl v = new ViewImpl();
		v.setIcon32x32RelativeFileName("icon.png");
		assertEquals("icon.png", v.getIcon32x32RelativeFileName());
	}

	@Test
	void setIconStyleClassRoundtrips() {
		ViewImpl v = new ViewImpl();
		v.setIconStyleClass("fa-user");
		assertEquals("fa-user", v.getIconStyleClass());
	}

	@Test
	void setHelpRelativeFileNameRoundtrips() {
		ViewImpl v = new ViewImpl();
		v.setHelpRelativeFileName("help.html");
		assertEquals("help.html", v.getHelpRelativeFileName());
	}

	@Test
	void setHelpURLRoundtrips() {
		ViewImpl v = new ViewImpl();
		v.setHelpURL("https://example.com/help");
		assertEquals("https://example.com/help", v.getHelpURL());
	}

	@Test
	void setTitleRoundtrips() {
		ViewImpl v = new ViewImpl();
		v.setTitle("My Title");
		assertEquals("My Title", v.getTitle());
	}

	@Test
	void setActionsWidgetIdRoundtrips() {
		ViewImpl v = new ViewImpl();
		v.setActionsWidgetId("actionsWidget");
		assertEquals("actionsWidget", v.getActionsWidgetId());
	}

	@Test
	void setDocumentationRoundtrips() {
		ViewImpl v = new ViewImpl();
		v.setDocumentation("Some docs");
		assertEquals("Some docs", v.getDocumentation());
	}

	@Test
	void setOverriddenUxUiNameRoundtrips() {
		ViewImpl v = new ViewImpl();
		v.setOverriddenUxUiName("desktop");
		assertEquals("desktop", v.getOverriddenUxUiName());
	}

	@Test
	void setRefreshTimeInSecondsRoundtrips() {
		ViewImpl v = new ViewImpl();
		v.setRefreshTimeInSeconds(Integer.valueOf(30));
		assertEquals(Integer.valueOf(30), v.getRefreshTimeInSeconds());
	}

	@Test
	void setNameRoundtrips() {
		ViewImpl v = new ViewImpl();
		v.setName("edit");
		assertEquals("edit", v.getName());
	}

	@Test
	void setLastModifiedMillisRoundtrips() {
		ViewImpl v = new ViewImpl();
		v.setLastModifiedMillis(12345L);
		assertEquals(12345L, v.getLastModifiedMillis());
	}

	@Test
	void putAndGetActionRoundtrips() {
		ViewImpl v = new ViewImpl();
		ActionImpl action = new ActionImpl();
		action.setName("myAction");
		v.putAction(action);
		assertEquals(action, v.getAction("myAction"));
	}

	@Test
	void getActionReturnsNullForUnknownName() {
		ViewImpl v = new ViewImpl();
		assertNull(v.getAction("unknown"));
	}

	@Test
	void getActionsReturnsEmptyWhenNoneAdded() {
		ViewImpl v = new ViewImpl();
		Collection<org.skyve.metadata.view.Action> actions = v.getActions();
		assertNotNull(actions);
		assertTrue(actions.isEmpty());
	}

	@Test
	void setAndGetSidebarRoundtrips() {
		ViewImpl v = new ViewImpl();
		assertNull(v.getSidebar());
		Sidebar sidebar = new Sidebar();
		v.setSidebar(sidebar);
		assertEquals(sidebar, v.getSidebar());
	}

	@Test
	void getParametersReturnsEmptyByDefault() {
		ViewImpl v = new ViewImpl();
		assertNotNull(v.getParameters());
		assertTrue(v.getParameters().isEmpty());
	}

	@Test
	void getPropertiesReturnsEmptyMapByDefault() {
		ViewImpl v = new ViewImpl();
		assertNotNull(v.getProperties());
		assertTrue(v.getProperties().isEmpty());
	}

	@Test
	void getAccessesReturnsNullWhenAccessesHaveNotBeenConverted() {
		ViewImpl v = new ViewImpl();
		assertNull(v.getAccesses(null, null, "desktop"));
	}
}
