package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.primefaces.model.menu.MenuModel;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.RequestUxUiSelectionTestUtil;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.router.UxUi;
import org.skyve.web.UserAgentType;

import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;

// Reflection and mutable JSF doubles exercise thread-local test infrastructure.
@SuppressWarnings({ "static-method", "boxing", "java:S1192", "java:S1948", "java:S3011", "java:S5960" })
class MenuViewTest {
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	@BeforeEach
	void bindPersistenceToThread() {
		threadPersistence().set(mock(AbstractPersistence.class));
	}

	@AfterEach
	void clearRequestState() {
		FacesContextBridge.setCurrent(null);
		threadPersistence().remove();
	}

	@Test
	void rebuildsTransientMenuOnPostbackWithoutReinitialisingHarnessState() {
		TestMenuView view = new TestMenuView();
		view.setBizModuleParameter("sales");
		installRequest("tablet", UserAgentType.tablet, true, null);

		MenuModel result = view.getMenu();

		assertSame(view.lastMenu, result);
		assertEquals(1, view.createCount);
		assertEquals(0, view.initialiseCount);
		assertEquals("sales", view.lastBizModule);
		assertEquals("tablet", view.lastUxUiName);
	}

	@Test
	void cachesMenuByResolvedUxUiName() {
		TestMenuView view = new TestMenuView();
		installRequest("tablet", UserAgentType.tablet, false, "admin");
		MenuModel tabletMenu = view.getMenu();

		installRequest("tablet", UserAgentType.phone, true, null);
		assertSame(tabletMenu, view.getMenu());
		assertEquals(1, view.createCount);

		installRequest("phone", UserAgentType.phone, true, null);
		MenuModel phoneMenu = view.getMenu();
		assertSame(view.lastMenu, phoneMenu);
		assertEquals(2, view.createCount);
		assertEquals("phone", view.lastUxUiName);
	}

	@Test
	void createMenuHrefUsesAbsoluteHrefWhenProvidedAndEscapesSingleQuotes() {
		MenuItem item = mock(MenuItem.class);

		String href = MenuView.createMenuHref(module("menu"), module("item"), item, "ignored", "/external/it's");

		assertEquals("javascript:SKYVE.PF.startHistory('/external/it\\'s')", href);
	}

	@Test
	void createMenuHrefBuildsListHrefForNamedModel() {
		ListItem item = new ListItem();
		item.setDocumentName("Contact");
		item.setModelName("activeContacts");

		String href = MenuView.createMenuHref(module("sales"), null, item, "ignoredQuery", null);

		assertHasStartHistoryWrapper(href);
		assertTrue(href.contains("/?a=l&m=sales&d=Contact&q=activeContacts"));
	}

	@Test
	void createMenuHrefBuildsListHrefForResolvedQuery() {
		ListItem item = new ListItem();

		String href = MenuView.createMenuHref(module("sales"), null, item, "allContacts", null);

		assertHasStartHistoryWrapper(href);
		assertTrue(href.contains("/?a=l&m=sales&q=allContacts"));
	}

	@Test
	void createMenuHrefBuildsEditHrefForTargetModuleAndDocument() {
		EditItem item = new EditItem();
		item.setDocumentName("Contact");

		String href = MenuView.createMenuHref(module("menu"), module("crm"), item, null, null);

		assertHasStartHistoryWrapper(href);
		assertTrue(href.contains("/?a=e&m=crm&d=Contact"));
	}

	@Test
	void createMenuHrefBuildsCalendarAndMapHrefs() {
		CalendarItem calendar = new CalendarItem();
		MapItem map = new MapItem();
		map.setDocumentName("Location");
		map.setModelName("siteMap");
		map.setGeometryBinding("location");

		String calendarHref = MenuView.createMenuHref(module("sales"), null, calendar, "events", null);
		String mapHref = MenuView.createMenuHref(module("sales"), null, map, "ignored", null);

		assertTrue(calendarHref.contains("/?a=c&m=sales&q=events"));
		assertTrue(mapHref.contains("/?a=m&m=sales&d=Location&q=siteMap&b=location"));
	}

	private static Module module(String name) {
		Module module = mock(Module.class);
		when(module.getName()).thenReturn(name);
		return module;
	}

	private static void assertHasStartHistoryWrapper(String href) {
		assertTrue(href.startsWith("javascript:SKYVE.PF.startHistory('"));
		assertTrue(href.endsWith("')"));
	}

	private static void installRequest(String uxuiName,
								UserAgentType userAgentType,
								boolean postback,
								String moduleName) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameter("m")).thenReturn(moduleName);
		RequestUxUiSelectionTestUtil.install(request,
										userAgentType,
										false,
										UxUi.newPrimeFaces(uxuiName, "ultima", "theme", "blue"));
		ExternalContext externalContext = mock(ExternalContext.class);
		when(externalContext.getRequest()).thenReturn(request);
		FacesContext context = mock(FacesContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(context.isPostback()).thenReturn(postback);
		FacesContextBridge.setCurrent(context);
	}

	@SuppressWarnings("unchecked")
	private static ThreadLocal<AbstractPersistence> threadPersistence() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			return (ThreadLocal<AbstractPersistence>) field.get(null);
		}
		catch (ReflectiveOperationException e) {
			throw new IllegalStateException("Unable to access thread-local persistence", e);
		}
	}

	private static final class TestMenuView extends MenuView {
		private static final long serialVersionUID = -147418346207348247L;

		private int createCount;
		private int initialiseCount;
		private String lastBizModule;
		private String lastUxUiName;
		private MenuModel lastMenu;

		@Override
		public void initialise() {
			initialiseCount++;
		}

		@Override
		MenuModel createMenuModel(String bizModule, String uxui) {
			createCount++;
			lastBizModule = bizModule;
			lastUxUiName = uxui;
			lastMenu = mock(MenuModel.class);
			return lastMenu;
		}
	}
}
