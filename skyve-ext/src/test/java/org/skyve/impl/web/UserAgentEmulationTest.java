package org.skyve.impl.web;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.cache.DefaultCaching;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.router.UxUiSelector;
import org.skyve.web.UserAgentType;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings("java:S1192")
class UserAgentEmulationTest {
	private static boolean originalForceNonPersistentCaching;

	private ProvidedRepository originalRepository;
	private UxUiSelector selector;

	@BeforeAll
	static void startCaching() {
		originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
		DefaultCaching.get().shutdown();
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
		DefaultCaching.get().startup();
	}

	@AfterAll
	static void stopCaching() {
		DefaultCaching.get().shutdown();
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = originalForceNonPersistentCaching;
	}

	@BeforeEach
	void setUpSelector() {
		originalRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Router router = mock(Router.class);
		selector = mock(UxUiSelector.class);
		when(repository.getRouter()).thenReturn(router);
		when(router.getUxuiSelector()).thenReturn(selector);
		ProvidedRepositoryFactory.set(repository);
	}

	@AfterEach
	void restoreRepository() {
		if (originalRepository == null) {
			ProvidedRepositoryFactory.clear();
		}
		else {
			ProvidedRepositoryFactory.set(originalRepository);
		}
	}

	@Test
	void validSessionValueCreatesOneEmulatedSelection() {
		MutableRequest request = request(UserAgentType.phone);
		UxUi phone = UxUi.newPrimeFaces("phone", "external", "phone-theme");
		when(selector.emulate(UserAgentType.phone, request.request)).thenReturn(phone);

		RequestUxUiSelection selection = UserAgent.getSelection(request.request);

		assertSame(selection, UserAgent.getSelection(request.request));
		assertSame(phone, selection.getUxUi());
		assertSame(UserAgentType.phone, selection.getUserAgentType());
		assertTrue(selection.isEmulated());
		assertEquals(1, request.attributes.size());
		verify(selector, times(1)).emulate(UserAgentType.phone, request.request);
		verify(request.request, times(1)).getSession(false);
		verify(selector, never()).select(any(), any());
	}

	@Test
	void detectionCreatesPartialSelectionWithoutResolvingUxUi() {
		MutableRequest request = request(UserAgentType.tablet);

		assertThat(UserAgent.detectType(request.request), is(UserAgentType.tablet));
		assertThat(UserAgent.detectType(request.request), is(UserAgentType.tablet));
		assertEquals(1, request.attributes.size());
		RequestUxUiSelection selection = (RequestUxUiSelection) request.attributes.values().iterator().next();
		assertFalse(selection.isComplete());
		verify(request.request, times(1)).getSession(false);
		verify(selector, never()).select(any(), any());
		verify(selector, never()).emulate(any(), any());
	}

	@Test
	void missingAndUnexpectedSessionValuesUseNormalSelection() {
		assertNormalSelection(null);
	}

	@Test
	@SuppressWarnings("static-method")
	void oneTimeCommandEnablesChangesAndEndsSessionPreview() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getSession(true)).thenReturn(session);
		when(request.getParameter(AbstractWebContext.EMULATED_USER_AGENT_TYPE_PARAMETER)).thenReturn("  phone  ");

		UserAgent.consumeDevicePreviewCommand(request);
		verify(session).setAttribute(AbstractWebContext.EMULATED_USER_AGENT_TYPE_SESSION_ATTRIBUTE_NAME,
				UserAgentType.phone);

		when(request.getSession(false)).thenReturn(session);
		when(request.getParameter(AbstractWebContext.EMULATED_USER_AGENT_TYPE_PARAMETER)).thenReturn("   ");
		UserAgent.consumeDevicePreviewCommand(request);
		verify(session).removeAttribute(AbstractWebContext.EMULATED_USER_AGENT_TYPE_SESSION_ATTRIBUTE_NAME);

		when(request.getParameter(AbstractWebContext.EMULATED_USER_AGENT_TYPE_PARAMETER)).thenReturn("watch");
		assertThrows(IllegalArgumentException.class, () -> UserAgent.consumeDevicePreviewCommand(request));
	}

	private void assertNormalSelection(Object sessionValue) {
		MutableRequest request = request(sessionValue);
		UxUi normal = UxUi.newPrimeFaces("normal", "external", "normal-theme");
		when(selector.select(UserAgentType.other, request.request)).thenReturn(normal);

		RequestUxUiSelection selection = UserAgent.getSelection(request.request);

		assertSame(normal, selection.getUxUi());
		assertSame(UserAgentType.other, selection.getUserAgentType());
		assertFalse(selection.isEmulated());
	}

	private static MutableRequest request(Object sessionValue) {
		Map<String, Object> attributes = new HashMap<>();
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getAttribute(any())).thenAnswer(invocation -> attributes.get(invocation.getArgument(0)));
		if (sessionValue != null) {
			HttpSession session = mock(HttpSession.class);
			when(session.getAttribute(AbstractWebContext.EMULATED_USER_AGENT_TYPE_SESSION_ATTRIBUTE_NAME))
					.thenReturn(sessionValue);
			when(request.getSession(false)).thenReturn(session);
		}
		when(request.getHeader("User-Agent")).thenReturn("simplified-selection-unknown-agent");
		org.mockito.Mockito.doAnswer(invocation -> {
			attributes.put(invocation.getArgument(0), invocation.getArgument(1));
			return null;
		}).when(request).setAttribute(any(), any());
		return new MutableRequest(request, attributes);
	}

	private record MutableRequest(HttpServletRequest request, Map<String, Object> attributes) {
		// Test fixture.
	}
}
