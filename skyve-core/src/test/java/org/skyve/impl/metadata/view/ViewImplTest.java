package org.skyve.impl.metadata.view;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.component.Component;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.UserAccess;

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
	void localisedTitleThrowsWhenTitleNull() {
		// a title is required metadata; the localised title is non-null
		ViewImpl v = new ViewImpl();
		assertThrows(NullPointerException.class, v::getLocalisedTitle);
	}

	@Test
	void setRefreshIfConditionRoundtrips() {
		ViewImpl v = new ViewImpl();
		v.setRefreshIfCondition("myCondition");
		assertEquals("myCondition", v.getRefreshIfCondition());
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

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void getAccessesReturnsNullWhenAccessesHaveNotBeenConverted() {
		ViewImpl v = new ViewImpl();
		assertNull(v.getAccesses(null, null, "desktop"));
	}

	@Test
	void getAccessesSkipsAComponentFragmentWithoutAccesses() throws Exception {
		ViewImpl v = new ViewImpl();
		Set<UserAccess> accesses = new TreeSet<>();
		accesses.add(UserAccess.singular("admin", "User"));
		Field accessesField = ViewImpl.class.getDeclaredField("accesses");
		accessesField.setAccessible(true);
		accessesField.set(v, accesses);
		CustomerImpl customer = mock(CustomerImpl.class);
		Component component = mock(Component.class);
		when(component.getFragment(customer, "desktop")).thenReturn(new ViewImpl()); // accesses neither declared nor generated
		Field componentsField = ViewImpl.class.getDeclaredField("components");
		componentsField.setAccessible(true);
		@SuppressWarnings("unchecked")
		Set<Component> components = (Set<Component>) componentsField.get(v);
		components.add(component);

		Set<UserAccess> result = v.getAccesses(customer, mock(Document.class), "desktop");

		assertEquals(Set.of(UserAccess.singular("admin", "User")), result);
	}

	@Test
	void getAccessesTranslatesComponentAggregatesIntoTheOwningDocument() throws Exception {
		Set<UserAccess> fragmentAccesses = new TreeSet<>();
		fragmentAccesses.add(UserAccess.queryAggregate("other", "qContacts"));
		fragmentAccesses.add(UserAccess.documentAggregate("other", "Contact"));
		fragmentAccesses.add(UserAccess.modelAggregate("other", "Contact", "contactModel"));
		fragmentAccesses.add(UserAccess.dynamicImage("other", "Contact", "contactImage"));

		Set<UserAccess> result = accessesWithComponentFragment(fragmentAccesses);

		assertEquals(Set.of(UserAccess.singular("admin", "User"),
								UserAccess.queryAggregate("admin", "qContacts"),
								UserAccess.documentAggregate("admin", "Contact"),
								UserAccess.modelAggregate("admin", "User", "contactModel"),
								UserAccess.dynamicImage("admin", "User", "contactImage")),
						result);
	}

	@Test
	void getAccessesThrowsWhenAComponentAggregateHasNoComponent() {
		Set<UserAccess> fragmentAccesses = new TreeSet<>();
		@SuppressWarnings("null") // passes null on purpose
		UserAccess componentless = UserAccess.queryAggregate("other", null);
		fragmentAccesses.add(componentless);

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> accessesWithComponentFragment(fragmentAccesses));
		assertTrue(e.getMessage().endsWith("has no component"), e.getMessage());
	}

	/**
	 * Gets the accesses of a view in document admin.User with a singular access and one component
	 * whose fragment has the given accesses.
	 */
	private static Set<UserAccess> accessesWithComponentFragment(Set<UserAccess> fragmentAccesses) throws Exception {
		Field accessesField = ViewImpl.class.getDeclaredField("accesses");
		accessesField.setAccessible(true);
		ViewImpl v = new ViewImpl();
		Set<UserAccess> accesses = new TreeSet<>();
		accesses.add(UserAccess.singular("admin", "User"));
		accessesField.set(v, accesses);
		ViewImpl fragment = new ViewImpl();
		accessesField.set(fragment, fragmentAccesses);
		CustomerImpl customer = mock(CustomerImpl.class);
		Component component = mock(Component.class);
		when(component.getFragment(customer, "desktop")).thenReturn(fragment);
		Field componentsField = ViewImpl.class.getDeclaredField("components");
		componentsField.setAccessible(true);
		@SuppressWarnings("unchecked")
		Set<Component> components = (Set<Component>) componentsField.get(v);
		components.add(component);
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("admin");
		when(document.getName()).thenReturn("User");

		return v.getAccesses(customer, document, "desktop");
	}

	@Test
	void resolveThrowsWhenAPreviousCompleteTextFieldHasNoBinding() {
		ViewImpl v = new ViewImpl();
		v.setName("edit");
		TextField text = new TextField(); // no binding
		text.setComplete(CompleteType.previous);
		v.getContained().add(text);
		ModuleImpl module = mock(ModuleImpl.class);
		when(module.getName()).thenReturn("admin");
		DocumentImpl document = new DocumentImpl();
		document.setName("User");
		CustomerImpl customer = mock(CustomerImpl.class);

		boolean accessControl = UtilImpl.ACCESS_CONTROL;
		UtilImpl.ACCESS_CONTROL = true;
		try {
			MetaDataException e = assertThrows(MetaDataException.class, () -> v.resolve("desktop", customer, module, document, true));
			assertEquals("A text field binding is required in view edit for document admin.User", e.getMessage());
		}
		finally {
			UtilImpl.ACCESS_CONTROL = accessControl;
		}
	}

	@Test
	void resolveAddsAPreviousCompleteAccessForATextFieldWithABinding() {
		ViewImpl v = new ViewImpl();
		v.setName("edit");
		TextField text = new TextField();
		text.setBinding("name");
		text.setComplete(CompleteType.previous);
		v.getContained().add(text);
		ModuleImpl module = mock(ModuleImpl.class);
		when(module.getName()).thenReturn("admin");
		DocumentImpl document = new DocumentImpl();
		document.setName("User");
		CustomerImpl customer = mock(CustomerImpl.class);

		boolean accessControl = UtilImpl.ACCESS_CONTROL;
		UtilImpl.ACCESS_CONTROL = true;
		try {
			v.resolve("desktop", customer, module, document, true);
			Set<UserAccess> accesses = v.getAccesses(customer, document, "desktop");
			assertNotNull(accesses);
			assertTrue(accesses.contains(UserAccess.previousComplete("admin", "User", "name")), String.valueOf(accesses));
		}
		finally {
			UtilImpl.ACCESS_CONTROL = accessControl;
		}
	}
}
