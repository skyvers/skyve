package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.skyve.EXT;
import org.skyve.impl.cache.DefaultCaching;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.router.UxUiSelector;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.web.UserAgentType;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings({ "static-method", "java:S1192" }) // Repeated literals are deliberate sensitive-endpoint fixtures.
class FacesThemeSensitiveEndpointPropagationTest {
	private static boolean originalForceNonPersistentCaching;
	private ProvidedRepository originalRepository;

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
		UxUiSelector selector = mock(UxUiSelector.class);
		when(repository.getRouter()).thenReturn(router);
		when(router.getUxuiSelector()).thenReturn(selector);
		when(selector.emulate(eq(UserAgentType.phone), any())).thenReturn(UxUi.newPrimeFaces("phone", "external", "phone-theme"));
		when(selector.select(any(), any())).thenReturn(UxUi.newSmartClient("desktop", "Tahoe", "desktop-theme"));
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

	@ParameterizedTest(name = "{0}")
	@MethodSource("sensitiveTargets")
	void everySensitiveTargetResolvesRequestUxUiBeforeApplicabilityCheck(String target, String sourcePath) throws Exception {
		String source = Files.readString(Path.of(sourcePath));

		assertTrue(source.contains("UserAgent.getSelection(request).getUxUi()"), target + ": " + sourcePath);
		assertTrue(source.contains("EXT.checkAccess(") ||
				source.contains("user.canAccess(") ||
				source.contains("secureResource("), target + ": " + sourcePath);
	}

	@Test
	@SuppressWarnings("boxing")
	void deniedAccessRemainsDeniedForEmulatedSelection() {
		MutableRequest request = request(new String[] {"phone"});
		String uxuiName = UserAgent.getSelection(request.request()).getUxUi().getName();
		User user = mock(User.class);
		UserAccess access = UserAccess.dynamicImage("admin", "Contact", "portrait");
		when(user.getName()).thenReturn("denied-user");
		when(user.canAccess(access, uxuiName)).thenReturn(false);

		assertEquals("phone", uxuiName);
		assertThrows(IllegalArgumentException.class, () -> EXT.checkAccess(user, access, uxuiName));
		verify(user).canAccess(access, "phone");
	}

	@Test
	void iframeUploadBizImportAndImageMarkupTargetsRetainTheirAccessChains() throws Exception {
		String contentUpload = Files.readString(Path.of("src/main/java/org/skyve/impl/web/faces/views/ContentUploadView.java"));
		String imageMarkup = Files.readString(Path.of("src/main/java/org/skyve/impl/web/faces/views/ImageMarkupView.java"));
		String actionUpload = Files.readString(Path.of("src/main/java/org/skyve/impl/web/faces/views/ContentUploadView.java"));
		String bizImport = Files.readString(Path.of("src/main/java/org/skyve/impl/web/faces/views/BizportImportView.java"));
		String uploadBase = Files.readString(Path.of("src/main/java/org/skyve/impl/web/faces/views/AbstractUploadView.java"));

		assertTrue(contentUpload.contains("UserAgent.getSelection(request).getUxUi()"), contentUpload);
		assertTrue(contentUpload.contains("EXT.checkAccess(user, UserAccess.content("), contentUpload);
		assertTrue(imageMarkup.contains("UserAgent.getSelection(request).getUxUi()"), imageMarkup);
		assertTrue(imageMarkup.contains("EXT.checkAccess(user, UserAccess.content("), imageMarkup);
		assertTrue(actionUpload.contains("user.canExecuteAction(document, actionName)"), actionUpload);
		assertTrue(bizImport.contains("user.canExecuteAction(document, action)"), bizImport);
		assertTrue(uploadBase.contains("session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)"), uploadBase);
	}

	@Test
	void simultaneousNormalAndEmulatedServiceRequestsDoNotLeakSelection() throws Exception {
		ExecutorService executor = Executors.newFixedThreadPool(2);
		try {
			Callable<String> emulated = () -> {
				MutableRequest request = request(new String[] {"phone"});
					RequestUxUiSelection selection = UserAgent.getSelection(request.request());
					String uxuiName = selection.getUxUi().getName();
					assertTrue(selection.isEmulated());
				return uxuiName;
			};
			Callable<String> normal = () -> {
				MutableRequest request = request(null);
					RequestUxUiSelection selection = UserAgent.getSelection(request.request());
					String uxuiName = selection.getUxUi().getName();
					assertFalse(selection.isEmulated());
				return uxuiName;
			};

			Future<String> emulatedResult = executor.submit(emulated);
			Future<String> normalResult = executor.submit(normal);
			assertEquals("phone", emulatedResult.get());
			assertEquals("desktop", normalResult.get());
		}
		finally {
			executor.shutdownNow();
			assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));
		}
	}

	private static Stream<Arguments> sensitiveTargets() {
		return Stream.of(
				Arguments.of("dynamic images", "src/main/java/org/skyve/impl/web/DynamicImageServlet.java"),
				Arguments.of("named/generated reports and exports", "src/main/java/org/skyve/impl/web/ReportServlet.java"),
				Arguments.of("resource/content GET, HEAD and last-modified", "src/main/java/org/skyve/impl/web/AbstractResourceServlet.java"),
				Arguments.of("metadata", "src/main/java/org/skyve/impl/web/service/MetaDataServlet.java"),
				Arguments.of("map", "src/main/java/org/skyve/impl/web/service/MapServlet.java"),
				Arguments.of("chart", "src/main/java/org/skyve/impl/web/service/ChartServlet.java"),
				Arguments.of("smartlist", "src/main/java/org/skyve/impl/web/service/smartclient/SmartClientListServlet.java"),
				Arguments.of("smartedit", "src/main/java/org/skyve/impl/web/service/smartclient/SmartClientEditServlet.java"),
				Arguments.of("smartsearch", "src/main/java/org/skyve/impl/web/service/smartclient/SmartClientTextSearchServlet.java"),
				Arguments.of("smartcomplete", "src/main/java/org/skyve/impl/web/service/smartclient/SmartClientCompleteServlet.java"),
				Arguments.of("smartgen", "src/main/java/org/skyve/impl/web/service/smartclient/SmartClientGeneratorServlet.java"),
				Arguments.of("smartsnap", "src/main/java/org/skyve/impl/web/service/smartclient/SmartClientSnapServlet.java"),
				Arguments.of("smarttag", "src/main/java/org/skyve/impl/web/service/smartclient/SmartClientTagServlet.java"));
	}

	private static MutableRequest request(String[] carriedValues) {
		Map<String, Object> attributes = new HashMap<>();
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getAttribute(any())).thenAnswer(invocation -> attributes.get(invocation.getArgument(0)));
		if ((carriedValues != null) && (carriedValues.length == 1)) {
			HttpSession session = mock(HttpSession.class);
			when(session.getAttribute(AbstractWebContext.EMULATED_USER_AGENT_TYPE_SESSION_ATTRIBUTE_NAME))
					.thenReturn(UserAgentType.valueOf(carriedValues[0]));
			when(request.getSession(false)).thenReturn(session);
		}
		when(request.getHeader("User-Agent")).thenReturn("phase-2i-service-agent");
		org.mockito.Mockito.doAnswer(invocation -> {
			attributes.put(invocation.getArgument(0), invocation.getArgument(1));
			return null;
		}).when(request).setAttribute(any(), any());
		org.mockito.Mockito.doAnswer(invocation -> {
			attributes.remove(invocation.getArgument(0));
			return null;
		}).when(request).removeAttribute(any());
		return new MutableRequest(request);
	}

	private record MutableRequest(HttpServletRequest request) {
		// Immutable test carrier.
	}
}
