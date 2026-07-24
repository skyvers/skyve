package router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.impl.util.UtilImpl;
import org.skyve.web.UserAgentType;
import org.skyve.web.WebContext;

/**
 * Tests for DefaultUxUiSelector.select() and emulate().
 * Tests focus on paths that don't require H2: public pages, session-based UxUi selection,
 * and user-agent type switching.
 */
@SuppressWarnings("java:S1192") // Repeated literals are deliberate route and postback fixtures.
class DefaultUxUiSelectorTest {

	private DefaultUxUiSelector selector;

	@BeforeEach
	void setUp() {
		selector = new DefaultUxUiSelector();
	}

	// ===== select() =====

	@Test
	void selectReturnsExternalWhenUserPrincipalIsNull() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getUserPrincipal()).thenReturn(null);

		UxUi result = selector.select(UserAgentType.desktop, request);

		assertEquals(UxUis.EXTERNAL, result);
		verify(request, never()).getServletPath();
		verify(request, never()).getPathInfo();
	}

	@Test
	void selectReturnsExternalWhenSessionIsNull() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getUserPrincipal()).thenReturn(mock(java.security.Principal.class));
		when(request.getSession(false)).thenReturn(null);

		UxUi result = selector.select(UserAgentType.desktop, request);

		assertEquals(UxUis.EXTERNAL, result);
	}

	@Test
	void selectReturnsExternalWhenSessionHasNoUserAttribute() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getUserPrincipal()).thenReturn(mock(java.security.Principal.class));
		when(request.getSession(false)).thenReturn(session);
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(null);
		when(session.getAttribute(org.skyve.impl.web.AbstractWebContext.UXUI_SESSION_ATTRIBUTE_NAME)).thenReturn(null);

		UxUi result = selector.select(UserAgentType.desktop, request);

		assertEquals(UxUis.EXTERNAL, result);
	}

	@Test
	void selectReturnsPHONEForPhoneUserAgent() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getUserPrincipal()).thenReturn(mock(java.security.Principal.class));
		when(request.getSession(false)).thenReturn(session);
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(null);
		when(session.getAttribute(org.skyve.impl.web.AbstractWebContext.UXUI_SESSION_ATTRIBUTE_NAME)).thenReturn(null);

		UxUi result = selector.select(UserAgentType.phone, request);

		assertEquals(UxUis.PHONE, result);
	}

	@Test
	void selectReturnsTABLETForTabletUserAgent() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getUserPrincipal()).thenReturn(mock(java.security.Principal.class));
		when(request.getSession(false)).thenReturn(session);
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(null);
		when(session.getAttribute(org.skyve.impl.web.AbstractWebContext.UXUI_SESSION_ATTRIBUTE_NAME)).thenReturn(null);

		UxUi result = selector.select(UserAgentType.tablet, request);

		assertEquals(UxUis.TABLET, result);
	}

	@Test
	void selectUsesSessionUxUiNameWhenPresent() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getUserPrincipal()).thenReturn(mock(java.security.Principal.class));
		when(request.getSession(false)).thenReturn(session);
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(null);
		when(session.getAttribute(org.skyve.impl.web.AbstractWebContext.UXUI_SESSION_ATTRIBUTE_NAME)).thenReturn("phone");

		UxUi result = selector.select(UserAgentType.desktop, request);

		assertEquals(UxUis.PHONE, result);
	}

	@Test
	void selectIgnoresUnknownSessionUxUiNameAndFallsThrough() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getUserPrincipal()).thenReturn(mock(java.security.Principal.class));
		when(request.getSession(false)).thenReturn(session);
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(null);
		when(session.getAttribute(org.skyve.impl.web.AbstractWebContext.UXUI_SESSION_ATTRIBUTE_NAME)).thenReturn("unknownUxUi");

		UxUi result = selector.select(UserAgentType.phone, request);

		assertEquals(UxUis.PHONE, result);
	}

	@Test
	@SuppressWarnings("boxing")
	void selectRoutesToSTARTUPForSecurityAdministratorWhenSetupVisibleAndNotDismissed() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		User user = mock(User.class);
		boolean previousShowSetup = UtilImpl.SHOW_SETUP;
		UtilImpl.SHOW_SETUP = true;
		try {
			when(request.getUserPrincipal()).thenReturn(mock(java.security.Principal.class));
			when(request.getSession(false)).thenReturn(session);
			when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
			doReturn(true).when(user).isInRole(modules.admin.domain.Startup.MODULE_NAME, "SecurityAdministrator");
			when(session.getAttribute(DefaultUxUiSelector.DISMISS_STARTUP)).thenReturn(null);

			UxUi result = selector.select(UserAgentType.desktop, request);

			assertEquals(UxUis.STARTUP, result);
		} finally {
			UtilImpl.SHOW_SETUP = previousShowSetup;
		}
	}

	@Test
	@SuppressWarnings("boxing")
	void selectDoesNotRouteToSTARTUPWhenDismissedInSession() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		User user = mock(User.class);
		boolean previousShowSetup = UtilImpl.SHOW_SETUP;
		UtilImpl.SHOW_SETUP = true;
		try {
			when(request.getUserPrincipal()).thenReturn(mock(java.security.Principal.class));
			when(request.getSession(false)).thenReturn(session);
			when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
			doReturn(true).when(user).isInRole(modules.admin.domain.Startup.MODULE_NAME, "SecurityAdministrator");
			when(session.getAttribute(DefaultUxUiSelector.DISMISS_STARTUP)).thenReturn(Boolean.TRUE);
			when(session.getAttribute(org.skyve.impl.web.AbstractWebContext.UXUI_SESSION_ATTRIBUTE_NAME)).thenReturn(null);

			UxUi result = selector.select(UserAgentType.desktop, request);

			assertEquals(UxUis.EXTERNAL, result);
		} finally {
			UtilImpl.SHOW_SETUP = previousShowSetup;
		}
	}

	// ===== emulate() =====

	@Test
	void emulateReturnsPHONEForPhone() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		assertEquals(UxUis.PHONE, selector.emulate(UserAgentType.phone, request));
		verify(request, never()).getServletPath();
		verify(request, never()).getPathInfo();
	}

	@Test
	void emulateReturnsTABLETForTablet() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		assertEquals(UxUis.TABLET, selector.emulate(UserAgentType.tablet, request));
	}

	@Test
	void emulateReturnsDESKTOPForDesktop() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		assertEquals(UxUis.DESKTOP, selector.emulate(UserAgentType.desktop, request));
	}

	@Test
	@SuppressWarnings("static-method")
	void desktopUsesSmartClientColourOverride() {
		assertEquals("Tahoe", UxUis.DESKTOP.getScSkin());
		assertEquals("casablanca", UxUis.DESKTOP.getPfThemeName());
		assertEquals("smartclient", UxUis.DESKTOP.getPfThemeColour());
	}

	@Test
	void emulateReturnsEXTERNALForOtherUserAgent() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		assertEquals(UxUis.EXTERNAL, selector.emulate(UserAgentType.other, request));
	}

	@Test
	void resolveReturnsEveryRegisteredProfileByIdentity() {
		assertSame(UxUis.PHONE, selector.resolve("phone"));
		assertSame(UxUis.TABLET, selector.resolve("tablet"));
		assertSame(UxUis.DESKTOP, selector.resolve("desktop"));
		assertSame(UxUis.EXTERNAL, selector.resolve("external"));
		assertSame(UxUis.STARTUP, selector.resolve("startup"));
	}

	@Test
	void resolveRejectsUnknownName() {
		assertThrows(MetaDataException.class, () -> selector.resolve("unknownUxUi"));
	}
}
