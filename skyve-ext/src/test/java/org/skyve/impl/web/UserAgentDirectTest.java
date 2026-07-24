package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
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
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.router.UxUiSelector;
import org.skyve.web.UserAgentType;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings("java:S1192")
class UserAgentDirectTest {
	private static boolean originalForceNonPersistentCaching;

	private ProvidedRepository originalRepository;
	private Router router;
	private UxUiSelector selector;
	private MutableRequest request;
	private UxUi fallback;
	private UxUi direct;

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
	void setUp() {
		originalRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		router = mock(Router.class);
		selector = mock(UxUiSelector.class);
		when(repository.getRouter()).thenReturn(router);
		when(router.getUxuiSelector()).thenReturn(selector);
		ProvidedRepositoryFactory.set(repository);

		request = new MutableRequest();
		when(request.request.getHeader("User-Agent")).thenReturn("direct-selection-unknown-agent");
		fallback = uxui("fallback");
		direct = uxui("direct");
		when(selector.select(any(), any())).thenReturn(fallback);
		when(selector.emulate(any(), any())).thenReturn(fallback);
		when(selector.resolve("direct")).thenReturn(direct);
	}

	@AfterEach
	void tearDown() {
		if (originalRepository == null) {
			ProvidedRepositoryFactory.clear();
		}
		else {
			ProvidedRepositoryFactory.set(originalRepository);
		}
	}

	@Test
	void emulationWinsBeforeDirectAndNormalSelection() {
		request.emulate(UserAgentType.phone);
		when(router.selectDirect(anyString(), any())).thenReturn("direct");

		RequestUxUiSelection selection = UserAgent.getSelection(request.request);

		assertTrue(selection.isEmulated());
		assertSame(UserAgentType.phone, selection.getUserAgentType());
		assertSame(fallback, selection.getUxUi());
		verify(selector).emulate(UserAgentType.phone, request.request);
		verify(router, never()).selectDirect(anyString(), any());
		verify(selector, never()).resolve(anyString());
		verify(selector, never()).select(any(), any());
	}

	@Test
	void directMatchResolvesBeforeNormalSelectorFallback() {
		when(router.selectDirect("/direct.xhtml", UserAgentType.other)).thenReturn("direct");
		request.servletPath = "/direct.xhtml";

		RequestUxUiSelection selection = UserAgent.getSelection(request.request);

		assertSame(direct, selection.getUxUi());
		verify(selector).resolve("direct");
		verify(selector, never()).select(any(), any());
		verify(selector, never()).emulate(any(), any());
	}

	@Test
	void unmatchedRequestUsesNormalSelector() {
		RequestUxUiSelection selection = UserAgent.getSelection(request.request);

		assertSame(fallback, selection.getUxUi());
		verify(selector).select(UserAgentType.other, request.request);
		verify(selector, never()).emulate(any(), any());
	}

	@Test
	void selectionIsResolvedExactlyOnceForTheRequest() {
		request.servletPath = "/source";
		RequestUxUiSelection first = UserAgent.getSelection(request.request);
		request.servletPath = "/target";
		request.emulate(UserAgentType.phone);
		when(router.selectDirect("/target", UserAgentType.other)).thenReturn("direct");

		RequestUxUiSelection second = UserAgent.getSelection(request.request);

		assertSame(first, second);
		assertSame(fallback, second.getUxUi());
		verify(selector, times(1)).select(UserAgentType.other, request.request);
		verify(selector, never()).emulate(any(), any());
		verify(selector, never()).resolve(anyString());
	}

	@Test
	void selectionReusesEarlierPhysicalDetectionState() {
		assertSame(UserAgentType.other, UserAgent.detectType(request.request));
		RequestUxUiSelection detected = (RequestUxUiSelection) request.attributes.values().iterator().next();
		assertFalse(detected.isComplete());

		RequestUxUiSelection selection = UserAgent.getSelection(request.request);

		assertSame(detected, selection);
		assertSame(UserAgentType.other, selection.getUserAgentType());
		assertSame(fallback, selection.getUxUi());
		verify(request.request, times(1)).getHeader("User-Agent");
		verify(request.request, times(1)).setAttribute(anyString(), any());
		verify(selector, times(1)).select(UserAgentType.other, request.request);
	}

	@Test
	void matchedDefaultResolveFailsWithoutFallback() {
		UxUiSelector defaultSelector = (type, currentRequest) -> fallback;
		when(router.getUxuiSelector()).thenReturn(defaultSelector);
		when(router.selectDirect(anyString(), any())).thenReturn("direct");

		assertThrows(MetaDataException.class, () -> UserAgent.getSelection(request.request));
	}

	@Test
	void unknownAndNullNamedResolutionFailWithoutFallback() {
		when(router.selectDirect(anyString(), any())).thenReturn("unknown");
		when(selector.resolve("unknown")).thenThrow(new MetaDataException("unknown"));
		assertThrows(MetaDataException.class, () -> UserAgent.getSelection(request.request));
		verify(selector, never()).select(any(), any());

		request = new MutableRequest();
		when(request.request.getHeader("User-Agent")).thenReturn("direct-selection-unknown-agent");
		when(router.selectDirect(anyString(), any())).thenReturn("nullResult");
		when(selector.resolve("nullResult")).thenReturn(null);
		assertThrows(MetaDataException.class, () -> UserAgent.getSelection(request.request));
		verify(selector, never()).emulate(any(), any());
	}

	@Test
	void requestParametersCannotNameUxUi() {
		request.parameters.put("uxui", new String[] {"direct"});
		request.parameters.put("_uxui", new String[] {"direct"});

		RequestUxUiSelection selection = UserAgent.getSelection(request.request);

		assertSame(fallback, selection.getUxUi());
		verify(selector, never()).resolve(anyString());
	}

	@Test
	void normalizedTargetUsesServletPathAndPathInfoAndIgnoresQueryFragmentAndForwardSource() {
		request.servletPath = "/folder";
		request.pathInfo = "/view.xhtml?uxui=bad#fragment";
		request.attributes.put("jakarta.servlet.forward.servlet_path", "/ignored");

		UserAgent.getSelection(request.request);

		verify(router).selectDirect("/folder/view.xhtml", UserAgentType.other);
	}

	private static UxUi uxui(String name) {
		return UxUi.newPrimeFaces(name, "external", name + "-theme");
	}

	private static final class MutableRequest {
		private final Map<String, Object> attributes = new HashMap<>();
		private final Map<String, String[]> parameters = new HashMap<>();
		private final HttpServletRequest request = mock(HttpServletRequest.class);
		private String servletPath;
		private String pathInfo;

		private MutableRequest() {
			when(request.getAttribute(anyString())).thenAnswer(invocation -> attributes.get(invocation.getArgument(0)));
			when(request.getParameterValues(anyString())).thenAnswer(invocation -> parameters.get(invocation.getArgument(0)));
			when(request.getServletPath()).thenAnswer(invocation -> servletPath);
			when(request.getPathInfo()).thenAnswer(invocation -> pathInfo);
			org.mockito.Mockito.doAnswer(invocation -> {
				attributes.put(invocation.getArgument(0), invocation.getArgument(1));
				return null;
			}).when(request).setAttribute(anyString(), any());
		}

		private void emulate(UserAgentType type) {
			HttpSession session = mock(HttpSession.class);
			when(session.getAttribute(AbstractWebContext.EMULATED_USER_AGENT_TYPE_SESSION_ATTRIBUTE_NAME))
					.thenReturn(type);
			when(request.getSession(false)).thenReturn(session);
		}
	}
}
