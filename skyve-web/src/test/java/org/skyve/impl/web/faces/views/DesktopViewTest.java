package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.sail.recording.SailRecording;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.RequestUxUiSelectionTestUtil;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.util.Icons;
import org.skyve.web.UserAgentType;
import org.skyve.web.WebAction;
import org.skyve.web.WebContext;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.faces.context.PartialViewContext;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings("static-method")
class DesktopViewTest {
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	@AfterEach
	void tearDown() throws ReflectiveOperationException {
		FacesContextBridge.setCurrent(null);
		clearThreadPersistence();
	}

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
	void bannerScriptEscapesTheBannerMarkupAsAJavaScriptString() {
		String originalIdentifier = UtilImpl.ENVIRONMENT_IDENTIFIER;
		try {
			UtilImpl.ENVIRONMENT_IDENTIFIER = null;
			assertEquals("", DesktopView.buildBannerScript());

			UtilImpl.ENVIRONMENT_IDENTIFIER = "it's <UAT>";
			String script = DesktopView.buildBannerScript();

			assertTrue(script.startsWith("$('body').append('"), script);
			assertTrue(script.endsWith("');"), script);
			String body = script.substring("$('body').append('".length(), script.length() - "');".length());
			// Neither an unescaped quote nor "</" can end the string or the enclosing script element
			assertFalse(body.replace("\\'", "").contains("'"), body);
			assertFalse(body.contains("</"), body);
			// the HTML-escaped identifier, with its ampersands escaped again for the script
			assertTrue(body.contains("it\\x26#39;s \\x26lt;UAT\\x26gt;"), body);
			assertTrue(body.contains("class=\\\"skyveEnvBanner skyveTopEnvBanner\\\""), body);
			assertTrue(body.contains("skyveTopEnvBanner") && body.contains("skyveBottomEnvBanner"), body);
		}
		finally {
			UtilImpl.ENVIRONMENT_IDENTIFIER = originalIdentifier;
		}
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
	@SuppressWarnings({"unchecked", "java:S3011"}) // Exercises the private user locale lookup and clears the framework-owned thread-local
	void localeScriptUsesEnglishBundlesWhenUserHasNoLocale() throws ReflectiveOperationException {
		DesktopView view = new ResourceAwareDesktopView(null);
		User user = mock(User.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();
		Field threadLocalPersistence = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		threadLocalPersistence.setAccessible(true);
		try {
			Method createLocaleScripts = DesktopView.class.getDeclaredMethod("createLocaleScripts");
			createLocaleScripts.setAccessible(true);
			createLocaleScripts.invoke(view);
		}
		finally {
			((ThreadLocal<AbstractPersistence>) threadLocalPersistence.get(null)).remove();
		}

		assertEquals(script("isomorphic130/locales/frameworkMessages.properties") + "\n" + script("desktop/skyveMessages.js"),
						view.getLocaleScript());
	}
	
	@SuppressWarnings("null") // a null here fails the test
	@Test
	void localeScriptUsesLocalizedSkyveMessagesWhenWebappResourceExists() {
		DesktopView view = new ResourceAwareDesktopView("/desktop/skyveMessages_de.js");
		
		view.createLocaleScripts(Locale.GERMAN);
		
		String localeScript = view.getLocaleScript();
		assertTrue(localeScript.contains("isomorphic130/locales/frameworkMessages_de.properties"));
		assertTrue(localeScript.contains("desktop/skyveMessages_de.js"));
		assertFalse(localeScript.contains("skyveMessages_de.properties"));
	}
	
	@SuppressWarnings("null") // a null here fails the test
	@Test
	void localeScriptFallsBackToEnglishSkyveMessagesWhenLocalizedWebappResourceDoesNotExist() {
		DesktopView view = new ResourceAwareDesktopView(null);
		
		view.createLocaleScripts(Locale.GERMAN);
		
		String localeScript = view.getLocaleScript();
		assertTrue(localeScript.contains("isomorphic130/locales/frameworkMessages_de.properties"));
		assertTrue(localeScript.contains("desktop/skyveMessages.js"));
		assertFalse(localeScript.contains("desktop/skyveMessages_de.js"));
	}
	
	@SuppressWarnings("null") // a null here fails the test
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

	// ----- preRender, menus and data sources -----

	@Test
	void preRenderShowsTheQueryDataSourceOfAModuleList() {
		UserImpl user = mockUser();
		Module module = mock(Module.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		Customer customer = user.getCustomer();
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getMetaDataQuery("q")).thenReturn(query);
		when(query.getName()).thenReturn("q");
		DesktopView view = new PreRenderDesktopView(user);
		view.setWebActionParameter(WebAction.l);
		view.setBizModuleParameter("admin");
		view.setQueryNameParameter("q");

		FacesContext facesContext = preRender(view);

		verify(facesContext, never()).addMessage(any(), any());
		assertEquals("details.showMember(isc.ListView.contents);isc.ListView.setGridDataSource('admin_q');", view.getUiScript());
	}

	@Test
	void preRenderReportsAnErrorWhenAModuleListHasNoModule() {
		UserImpl user = mockUser();
		DesktopView view = new PreRenderDesktopView(user);
		view.setWebActionParameter(WebAction.l);
		view.setQueryNameParameter("q");

		FacesContext facesContext = preRender(view);

		// the menu and data sources are made but there is no list to show
		assertNotNull(view.getDataSourceScript());
		assertNull(view.getUiScript());
		verify(facesContext).addMessage(isNull(), any());
	}

	@Test
	void preRenderReportsAnErrorWhenAModuleListHasNoQueryName() {
		UserImpl user = mockUser();
		DesktopView view = new PreRenderDesktopView(user);
		view.setWebActionParameter(WebAction.l);
		view.setBizModuleParameter("admin");

		FacesContext facesContext = preRender(view);

		assertNotNull(view.getDataSourceScript());
		assertNull(view.getUiScript());
		verify(facesContext).addMessage(isNull(), any());
		verify(user.getCustomer(), never()).getModule("admin");
	}

	@Test
	void preRenderNamesTheModuleOfAnEditMenuItemFromAnotherModule() {
		String menuScript = editMenuScript("other");

		assertTrue(menuScript.contains("{name:'Contact',module:'other',desc:'Edit Contact'"), menuScript);
	}

	@Test
	void preRenderOmitsTheModuleOfAnEditMenuItemFromTheMenuModule() {
		String menuScript = editMenuScript("admin");

		assertTrue(menuScript.contains("{name:'Contact',desc:'Edit Contact'"), menuScript);
	}

	/**
	 * Pre-renders an edit of a module "admin" whose menu has an edit item of a document owned by the given module,
	 * returning the menu script.
	 */
	private static String editMenuScript(String owningModuleName) {
		UserImpl user = mockUser();
		Customer customer = user.getCustomer();
		Module menuModule = mockModule(customer, "admin");
		if (! "admin".equals(owningModuleName)) {
			mockModule(customer, owningModuleName);
		}
		Document document = mock(Document.class);
		when(menuModule.getDocument(customer, "Contact")).thenReturn(document);
		when(document.getName()).thenReturn("Contact");
		when(document.getOwningModuleName()).thenReturn(owningModuleName);
		when(customer.getModules()).thenReturn(List.of(menuModule));
		EditItem item = new EditItem();
		item.setName("Edit Contact");
		item.setDocumentName("Contact");
		mockMenu(user, "admin", item);

		DesktopView view = new PreRenderDesktopView(user);
		view.setWebActionParameter(WebAction.e);
		view.setBizModuleParameter("admin");
		view.setBizDocumentParameter("Contact");
		FacesContext facesContext = preRender(view);

		verify(facesContext, never()).addMessage(any(), any());
		String result = view.getMenuScript();
		assertNotNull(result);
		return result;
	}

	@Test
	void listDataSourcesThrowsWhenTheUserHasNoMenuForAModule() {
		UserImpl user = mockUser();
		when(user.getName()).thenReturn("tester");
		Customer customer = user.getCustomer();
		Module module = mockModule(customer, "admin");
		when(customer.getModules()).thenReturn(List.of(module));

		IllegalStateException e = assertInstanceOf(IllegalStateException.class, listDataSourcesFailure(customer, user));

		assertEquals("User tester has no menu for module admin", e.getMessage());
	}

	@Test
	void listDataSourcesDefinesTheDataSourceOfAQueryListItem() throws ReflectiveOperationException {
		UserImpl user = mockUser();
		Customer customer = user.getCustomer();
		Module module = mockModule(customer, "admin");
		when(customer.getModules()).thenReturn(List.of(module));
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		Document document = mock(Document.class);
		when(module.getNullSafeMetaDataQuery("q")).thenReturn(query);
		when(query.getName()).thenReturn("q");
		when(query.getDocumentName()).thenReturn("Contact");
		when(query.getLocalisedDescription()).thenReturn("Contacts");
		when(query.getDocumentModule(customer)).thenReturn(module);
		when(query.getOwningModule()).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(document.getName()).thenReturn("Contact");
		ListItem item = new ListItem();
		item.setName("Contacts");
		item.setQueryName("q");
		mockMenu(user, "admin", item);

		String script = listDataSources(customer, user);

		assertTrue(script.startsWith(",[{ID:'admin_q',modoc:'admin.Contact'"), script);
		assertTrue(script.endsWith("]);"), script);
	}

	@Test
	void listDataSourcesThrowsWhenAListItemHasNoQueryOrDocument() {
		UserImpl user = mockUser();
		Customer customer = user.getCustomer();
		Module module = mockModule(customer, "admin");
		when(customer.getModules()).thenReturn(List.of(module));
		ListItem item = new ListItem();
		item.setName("Nothing");
		mockMenu(user, "admin", item);

		MetaDataException e = assertInstanceOf(MetaDataException.class, listDataSourcesFailure(customer, user));

		assertEquals("Menu item Nothing in module admin has no query or document name", e.getMessage());
	}

	/**
	 * A user of a customer, set on a persistence bound to the thread.
	 */
	private static UserImpl mockUser() {
		UserImpl user = mock(UserImpl.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();
		return user;
	}

	private static Module mockModule(Customer customer, String name) {
		Module result = mock(Module.class);
		when(result.getName()).thenReturn(name);
		when(customer.getModule(name)).thenReturn(result);
		return result;
	}

	/**
	 * Gives the user a desktop menu of the given items for the named module.
	 */
	private static void mockMenu(UserImpl user, String moduleName, MenuItem... items) {
		Menu menu = mock(Menu.class);
		doReturn(Boolean.TRUE).when(menu).isApplicable(anyString());
		List<MenuItem> menuItems = new ArrayList<>(List.of(items));
		when(menu.getItems()).thenReturn(menuItems);
		when(user.getModuleMenu(moduleName)).thenReturn(menu);
	}

	/**
	 * Pre-renders the view in a desktop SmartClient request of a Faces context that records any action error.
	 */
	private static FacesContext preRender(DesktopView view) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		RequestUxUiSelectionTestUtil.install(request,
												UserAgentType.desktop,
												false,
												UxUi.newSmartClient(UxUi.DESKTOP_NAME, "casablanca", "saga-blue"));
		ExternalContext externalContext = mock(ExternalContext.class);
		when(externalContext.getRequest()).thenReturn(request);
		FacesContext result = mock(FacesContext.class);
		when(result.getExternalContext()).thenReturn(externalContext);
		PartialViewContext partialViewContext = mock(PartialViewContext.class);
		when(result.getPartialViewContext()).thenReturn(partialViewContext);
		when(partialViewContext.getRenderIds()).thenReturn(new ArrayList<>());
		FacesContextBridge.setCurrent(result);

		view.preRender();
		return result;
	}

	@SuppressWarnings("java:S3011") // Exercises the private data source listing without a SmartClient runtime
	private static String listDataSources(Customer customer, UserImpl user) throws ReflectiveOperationException {
		StringBuilder result = new StringBuilder();
		listDataSourcesMethod().invoke(null, customer, user, UxUi.DESKTOP_NAME, result);
		return result.toString();
	}

	@SuppressWarnings("java:S3011") // Exercises the private data source listing without a SmartClient runtime
	private static Throwable listDataSourcesFailure(Customer customer, UserImpl user) {
		InvocationTargetException e = assertThrows(InvocationTargetException.class,
													() -> listDataSourcesMethod().invoke(null, customer, user, UxUi.DESKTOP_NAME, new StringBuilder()));
		return e.getCause();
	}

	@SuppressWarnings("java:S3011") // Exercises the private data source listing without a SmartClient runtime
	private static Method listDataSourcesMethod() throws NoSuchMethodException {
		Method result = DesktopView.class.getDeclaredMethod("listDataSources", Customer.class, UserImpl.class, String.class, StringBuilder.class);
		result.setAccessible(true);
		return result;
	}

	@SuppressWarnings({"unchecked", "java:S3011"}) // Test cleanup must access the framework-owned thread-local
	private static void clearThreadPersistence() throws ReflectiveOperationException {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}

	/**
	 * The page loads the SAIL recorder only when every part of the gate passes: the configuration, the role and a
	 * recording started in the session.
	 */
	@Test
	void sailRecordingNeedsTheWholeGate() {
		boolean originalSailRecording = UtilImpl.SAIL_RECORDING;
		try {
			UtilImpl.SAIL_RECORDING = true;
			User recorder = sailUser(true);
			HttpSession session = attributeSession();
			session.setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, recorder);
			assertFalse(sailRecording(session), "not started");

			SailRecording recording = SailRecording.start(session, recorder);
			assertTrue(sailRecording(session), "started");

			UtilImpl.SAIL_RECORDING = false;
			assertFalse(sailRecording(session), "configuration off");
			UtilImpl.SAIL_RECORDING = true;

			session.setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, sailUser(false));
			assertFalse(sailRecording(session), "no SAILRecorder role");
			session.setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, recorder);
			assertTrue(sailRecording(session), "all parts again");

			// a stopped recording is tested in skyve-war's SailRecordingTest: stopping builds XML, which needs the schemas
			assertFalse(sailRecording(null), "no session");
			assertTrue(SailRecording.get(session) == recording);
		}
		finally {
			UtilImpl.SAIL_RECORDING = originalSailRecording;
		}
	}

	private static boolean sailRecording(@Nullable HttpSession session) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(session);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(externalContext.getRequest()).thenReturn(request);
		FacesContext context = mock(FacesContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		FacesContextBridge.setCurrent(context);
		return new DesktopView().isSailRecording();
	}

	private static User sailUser(boolean recorder) {
		User result = mock(User.class);
		when(result.getName()).thenReturn("admin");
		when(result.getCustomerName()).thenReturn("demo");
		when(Boolean.valueOf(result.isInRole(SailRecording.ROLE_MODULE_NAME, SailRecording.ROLE_NAME))).thenReturn(Boolean.valueOf(recorder));
		return result;
	}

	/**
	 * A session mock that keeps its attributes.
	 */
	private static HttpSession attributeSession() {
		java.util.Map<String, Object> attributes = new java.util.HashMap<>();
		HttpSession result = mock(HttpSession.class);
		org.mockito.Mockito.doAnswer(i -> attributes.put(i.getArgument(0), i.getArgument(1))).when(result).setAttribute(anyString(), any());
		when(result.getAttribute(anyString())).thenAnswer(i -> attributes.get(i.getArgument(0)));
		return result;
	}

	/**
	 * A desktop view of a session user that needs no request locale or web resources to pre-render.
	 */
	private static final class PreRenderDesktopView extends DesktopView {
		private static final long serialVersionUID = 1L;

		private final transient UserImpl sessionUser;

		private PreRenderDesktopView(UserImpl sessionUser) {
			this.sessionUser = sessionUser;
		}

		@Override
		public void initialise() {
			// the request locale and user's contact details are not needed
		}

		@Override
		public @Nullable User getUser() {
			return sessionUser;
		}

		@Override
		boolean webResourceExists(@Nonnull String resourcePath) {
			return false;
		}
	}
}
