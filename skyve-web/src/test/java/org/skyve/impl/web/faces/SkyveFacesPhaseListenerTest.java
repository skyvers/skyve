package org.skyve.impl.web.faces;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.cache.ConversationCacheConfig;
import org.skyve.cache.SessionCacheConfig;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.cache.DefaultCaching;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.RequestUxUiSelection;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.faces.SkyveFacesPhaseListener.FacesViewSelectionMarker;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.router.UxUi;
import org.skyve.web.UserAgentType;

import jakarta.el.ELContext;
import jakarta.el.ELResolver;
import jakarta.faces.FacesException;
import jakarta.faces.application.ViewExpiredException;
import jakarta.faces.component.UIViewRoot;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.faces.context.PartialViewContext;
import jakarta.faces.event.PhaseEvent;
import jakarta.faces.event.PhaseId;
import jakarta.servlet.ServletContext;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings({ "static-method", "boxing", "java:S1192" }) // Repeated literals document lifecycle event contracts.
class SkyveFacesPhaseListenerTest {
	private static final String SELECTION_MARKER_KEY = FacesViewSelectionMarker.class.getName();

	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	@AfterEach
	void clearPersistence() throws Exception {
		persistenceThreadLocal().remove();
		FacesContextBridge.setCurrent(null);
	}

	@Test
	void getPhaseIdReturnsAnyPhase() {
		SkyveFacesPhaseListener listener = new SkyveFacesPhaseListener();
		assertEquals(PhaseId.ANY_PHASE, listener.getPhaseId());
	}

	@Test
	void beforePhaseAcceptsNormalEvent() {
		SkyveFacesPhaseListener listener = new SkyveFacesPhaseListener();
		PhaseEvent event = mock(PhaseEvent.class);
		FacesContext facesContext = mock(FacesContext.class);

		when(event.getFacesContext()).thenReturn(facesContext);
		when(event.getPhaseId()).thenReturn(PhaseId.APPLY_REQUEST_VALUES);
		when(facesContext.getResponseComplete()).thenReturn(false);

		assertDoesNotThrow(() -> listener.beforePhase(event));
	}

	@Test
	void afterPhaseRestoreViewWrapsUnexpectedErrorsAsFacesException() {
		SkyveFacesPhaseListener listener = new SkyveFacesPhaseListener();
		PhaseEvent event = mock(PhaseEvent.class);
		FacesContext facesContext = mock(FacesContext.class);

		when(event.getPhaseId()).thenReturn(PhaseId.RESTORE_VIEW);
		when(event.getFacesContext()).thenReturn(facesContext);
		when(facesContext.getResponseComplete()).thenReturn(false);
		when(facesContext.getExternalContext()).thenReturn(null);

		assertThrows(FacesException.class, () -> listener.afterPhase(event));
	}

	@Test
	void afterPhaseUpdateModelValuesHandlesNullViewRoot() {
		SkyveFacesPhaseListener listener = new SkyveFacesPhaseListener();
		PhaseEvent event = mock(PhaseEvent.class);
		FacesContext facesContext = mock(FacesContext.class);

		when(event.getPhaseId()).thenReturn(PhaseId.UPDATE_MODEL_VALUES);
		when(event.getFacesContext()).thenReturn(facesContext);
		when(facesContext.getViewRoot()).thenReturn(null);
		when(facesContext.getResponseComplete()).thenReturn(false);

		assertDoesNotThrow(() -> listener.afterPhase(event));
	}

	@Test
	void freshGetRecordsSerializableSelectionMarkerAndMatchingNormalPostbackProceeds() throws Exception {
		Map<String, Object> viewMap = new HashMap<>();
		UxUi selected = UxUi.newPrimeFaces("tablet", "ultima", "ultima-ocean", "ocean");
		LifecycleRequest freshGet = lifecycleRequest(selected, UserAgentType.tablet, false, false, false, viewMap);
		AbstractPersistence freshPersistence = bindPersistence();

		new SkyveFacesPhaseListener().afterPhase(freshGet.event);

		Object marker = viewMap.get(SELECTION_MARKER_KEY);
		assertNotNull(marker);
		assertInstanceOf(FacesViewSelectionMarker.class, serialiseRoundTrip(marker));
		verify(freshPersistence).begin();

		clearPersistence();
		LifecycleRequest postback = lifecycleRequest(selected, UserAgentType.tablet, false, true, false, viewMap);
		AbstractPersistence postbackPersistence = bindPersistence();
		new SkyveFacesPhaseListener().afterPhase(postback.event);

		verify(postbackPersistence).begin();
		verify(postback.request).getUserPrincipal();
	}

	@Test
	void matchingPrimeFacesAjaxPostbackProceeds() throws Exception {
		Map<String, Object> viewMap = markedViewMap(
				UxUi.newPrimeFaces("tablet", "external", "casablanca", "blue"),
				UserAgentType.tablet,
				true);
		LifecycleRequest ajax = lifecycleRequest(
				UxUi.newPrimeFaces("tablet", "external", "casablanca", "blue"),
				UserAgentType.tablet,
				true,
				true,
				true,
				viewMap);
		AbstractPersistence persistence = bindPersistence();

		new SkyveFacesPhaseListener().afterPhase(ajax.event);

		verify(persistence).begin();
		verify(ajax.partialViewContext).isAjaxRequest();
	}

	@Test
	void selectionMarkerFreePreUpgradePostbackExpiresBeforeEveryRestoreSideEffect() throws Exception {
		LifecycleRequest postback = lifecycleRequest(
				UxUi.newPrimeFaces("tablet", "external", "casablanca", "blue"),
				UserAgentType.tablet,
				false,
				true,
				false,
				new HashMap<>());
		AbstractPersistence persistence = bindPersistence();
		Runnable modelMutation = mock(Runnable.class);
		Runnable accessCheck = mock(Runnable.class);
		Runnable action = mock(Runnable.class);

		FacesException exception = assertThrows(FacesException.class, () -> {
			new SkyveFacesPhaseListener().afterPhase(postback.event);
			modelMutation.run();
			accessCheck.run();
			action.run();
		});

		assertInstanceOf(ViewExpiredException.class, exception.getCause());
		verify(postback.externalContext, never()).getSessionMap();
		verify(postback.externalContext, never()).getRequestParameterMap();
		verify(postback.viewRoot, never()).getAttributes();
		verify(postback.request, never()).getUserPrincipal();
		verify(postback.request, never()).getSession(false);
		verifyNoInteractions(persistence, modelMutation, accessCheck, action);
	}

	@Test
	void divergentNormalPostbacksExpireForEverySelectionField() throws Exception {
		UxUi original = UxUi.newPrimeFaces("tablet", "external", "casablanca", "blue");
		Map<String, Object> marked = markedViewMap(original, UserAgentType.tablet, false);

		assertRejected(marked, original, UserAgentType.phone, false, false);
		assertRejected(marked, original, UserAgentType.tablet, true, false);
		assertRejected(marked,
				UxUi.newPrimeFaces("phone", "external", "casablanca", "blue"),
				UserAgentType.tablet,
				false,
				false);
	}

	@Test
	void sameNameDifferentResolvedThemeProceedsForNormalAndAjaxPostbacks() throws Exception {
		UxUi original = UxUi.newPrimeFaces("shared", "external", "casablanca", "blue");
		Map<String, Object> marked = markedViewMap(original, UserAgentType.tablet, false);
		UxUi changedTheme = UxUi.newPrimeFaces("shared", "diamond", "diamond-indigo-dark", "indigo-dark");

		assertAccepted(marked, changedTheme, UserAgentType.tablet, false, false);
		assertAccepted(marked, changedTheme, UserAgentType.tablet, false, true);
	}

	@Test
	void freshGetRecoversWithChangedValidSelectionAndWritesNewMarker() throws Exception {
		Map<String, Object> oldViewMap = markedViewMap(
				UxUi.newPrimeFaces("tablet", "external", "casablanca", "blue"),
				UserAgentType.tablet,
				false);
		Object oldMarker = oldViewMap.get(SELECTION_MARKER_KEY);
		Map<String, Object> newViewMap = new HashMap<>();
		LifecycleRequest freshGet = lifecycleRequest(
				UxUi.newPrimeFaces("phone", "diamond", "diamond-indigo-dark", "indigo-dark"),
				UserAgentType.phone,
				true,
				false,
				false,
				newViewMap);
		bindPersistence();

		assertDoesNotThrow(() -> new SkyveFacesPhaseListener().afterPhase(freshGet.event));

		assertNotNull(newViewMap.get(SELECTION_MARKER_KEY));
		org.junit.jupiter.api.Assertions.assertNotSame(oldMarker, newViewMap.get(SELECTION_MARKER_KEY));
	}

	@Test
	void restoringASessionViewWhoseCachedConversationHasNoPersistenceThrows() throws Throwable {
		withConversationCache(() -> {
			MockWebContext webContext = cachedConversationWithNoPersistence();
			FacesView view = mock(FacesView.class);
			when(view.getDehydratedWebId()).thenReturn(webContext.getWebId());
			Map<String, Object> sessionMap = new HashMap<>();
			sessionMap.put(FacesUtil.MANAGED_BEAN_NAME_KEY, view);
			PhaseEvent event = restoreViewEvent(webContext, sessionMap, Map.of());

			FacesException exception = assertThrows(FacesException.class,
														() -> new SkyveFacesPhaseListener().afterPhase(event));

			assertCachedConversationHasNoPersistence(exception, webContext.getWebId());
			verify(view).hydrate(any());
		});
	}

	@Test
	void restoringAConversationParameterWhoseCachedConversationHasNoPersistenceThrows() throws Throwable {
		withConversationCache(() -> {
			MockWebContext webContext = cachedConversationWithNoPersistence();
			PhaseEvent event = restoreViewEvent(webContext,
												new HashMap<>(),
												Map.of(AbstractWebContext.CONTEXT_NAME, webContext.getWebId()));

			FacesException exception = assertThrows(FacesException.class,
														() -> new SkyveFacesPhaseListener().afterPhase(event));

			assertCachedConversationHasNoPersistence(exception, webContext.getWebId());
		});
	}

	@Test
	void postRenderingAViewWithNoWebContextThrowsAndStillCommits() throws Exception {
		FacesView view = mock(FacesView.class);
		when(view.getPostRenderBean()).thenReturn(mock(Bean.class));
		PhaseEvent event = renderResponseEvent(view);
		AbstractPersistence persistence = bindPersistence();

		FacesException exception = assertThrows(FacesException.class,
													() -> new SkyveFacesPhaseListener().afterPhase(event));

		IllegalStateException cause = assertInstanceOf(IllegalStateException.class, exception.getCause());
		assertEquals("Cannot post render as FacesView skyve has no web context", cause.getMessage());
		verify(persistence).commit(true);
		verify(view, never()).dehydrate();
	}

	@Test
	void cachingTheConversationOfAViewWithNoWebContextThrowsAndStillCommits() throws Exception {
		FacesView view = mock(FacesView.class);
		PhaseEvent event = renderResponseEvent(view);
		AbstractPersistence persistence = bindPersistence();

		FacesException exception = assertThrows(FacesException.class,
													() -> new SkyveFacesPhaseListener().afterPhase(event));

		IllegalStateException cause = assertInstanceOf(IllegalStateException.class, exception.getCause());
		assertEquals("Cannot cache the conversation as FacesView skyve has no web context", cause.getMessage());
		verify(persistence).commit(true);
		verify(view, never()).dehydrate();
	}

	/**
	 * Caches a conversation that has a current bean but no persistence.
	 */
	private static MockWebContext cachedConversationWithNoPersistence() {
		MockWebContext result = new MockWebContext();
		HashMap<String, Object> values = new HashMap<>();
		values.put(Bean.DOCUMENT_ID, "bean-1");
		result.setCurrentBean(new DynamicBean("test", "Doc", values));
		StateUtil.commitAndCacheConversation(result);
		return result;
	}

	private static void assertCachedConversationHasNoPersistence(FacesException exception, String webId) {
		IllegalStateException cause = assertInstanceOf(IllegalStateException.class, exception.getCause());
		assertEquals("Cached conversation " + webId + " has no persistence", cause.getMessage());
	}

	/**
	 * A fresh (non-postback) RESTORE_VIEW event with no view root, from a request in the session of the given conversation.
	 */
	private static PhaseEvent restoreViewEvent(MockWebContext webContext,
												Map<String, Object> sessionMap,
												Map<String, String> requestParameters) {
		HttpSession session = mock(HttpSession.class);
		when(session.getId()).thenReturn(webContext.getSessionId());
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(session);

		ExternalContext externalContext = mock(ExternalContext.class);
		when(externalContext.getRequest()).thenReturn(request);
		when(externalContext.getSessionMap()).thenReturn(sessionMap);
		when(externalContext.getRequestParameterMap()).thenReturn(requestParameters);

		FacesContext facesContext = mock(FacesContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);

		PhaseEvent result = mock(PhaseEvent.class);
		when(result.getPhaseId()).thenReturn(PhaseId.RESTORE_VIEW);
		when(result.getFacesContext()).thenReturn(facesContext);
		return result;
	}

	/**
	 * A RENDER_RESPONSE event for a view root whose managed bean "skyve" resolves to the given view.
	 */
	private static PhaseEvent renderResponseEvent(FacesView view) {
		UIViewRoot viewRoot = mock(UIViewRoot.class);
		when(viewRoot.getAttributes()).thenReturn(Map.of(FacesUtil.MANAGED_BEAN_NAME_KEY, "skyve"));

		ELContext elContext = mock(ELContext.class);
		ELResolver resolver = mock(ELResolver.class);
		when(elContext.getELResolver()).thenReturn(resolver);
		when(resolver.getValue(elContext, null, "skyve")).thenReturn(view);

		FacesContext facesContext = mock(FacesContext.class);
		when(facesContext.getViewRoot()).thenReturn(viewRoot);
		when(facesContext.getELContext()).thenReturn(elContext);
		FacesContextBridge.setCurrent(facesContext);

		PhaseEvent result = mock(PhaseEvent.class);
		when(result.getPhaseId()).thenReturn(PhaseId.RENDER_RESPONSE);
		when(result.getFacesContext()).thenReturn(facesContext);
		return result;
	}

	/**
	 * Runs the body with a fresh, non-persistent conversation cache, restoring the cache configuration afterwards.
	 */
	private static void withConversationCache(Executable body) throws Throwable {
		boolean originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
		String originalCacheDirectory = UtilImpl.CACHE_DIRECTORY;
		SessionCacheConfig originalSessionCache = UtilImpl.SESSION_CACHE;
		ConversationCacheConfig originalConversationCache = UtilImpl.CONVERSATION_CACHE;
		Path cacheDirectory = Files.createTempDirectory("phase-listener-cache");
		try {
			UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
			UtilImpl.CACHE_DIRECTORY = cacheDirectory.toString() + File.separator;
			UtilImpl.SESSION_CACHE = new SessionCacheConfig(100, 10);
			UtilImpl.CONVERSATION_CACHE = new ConversationCacheConfig(100, 10);
			DefaultCaching.get().shutdown();
			DefaultCaching.get().startup();
			body.execute();
		}
		finally {
			DefaultCaching.get().shutdown();
			UtilImpl.FORCE_NON_PERSISTENT_CACHING = originalForceNonPersistentCaching;
			UtilImpl.CACHE_DIRECTORY = originalCacheDirectory;
			UtilImpl.SESSION_CACHE = originalSessionCache;
			UtilImpl.CONVERSATION_CACHE = originalConversationCache;
			try (Stream<Path> paths = Files.walk(cacheDirectory)) {
				paths.sorted(Comparator.reverseOrder())
						.map(Path::toFile)
						.forEach(File::delete);
			}
		}
	}

	private static Map<String, Object> markedViewMap(UxUi uxui,
													UserAgentType type,
													boolean emulated) throws Exception {
		Map<String, Object> viewMap = new HashMap<>();
		LifecycleRequest initial = lifecycleRequest(uxui, type, emulated, false, false, viewMap);
		bindPersistence();
		new SkyveFacesPhaseListener().afterPhase(initial.event);
		clearBoundPersistence();
		return viewMap;
	}

	private static void assertRejected(Map<String, Object> viewMap,
										UxUi uxui,
										UserAgentType type,
										boolean emulated,
										boolean ajax) throws Exception {
		LifecycleRequest postback = lifecycleRequest(uxui, type, emulated, true, ajax, viewMap);
		AbstractPersistence persistence = bindPersistence();

		FacesException exception = assertThrows(FacesException.class,
				() -> new SkyveFacesPhaseListener().afterPhase(postback.event));

		assertInstanceOf(ViewExpiredException.class, exception.getCause());
		verifyNoInteractions(persistence);
		verify(postback.externalContext, never()).getSessionMap();
		verify(postback.viewRoot, never()).getAttributes();
		clearBoundPersistence();
	}

	private static void assertAccepted(Map<String, Object> viewMap,
			UxUi uxui,
			UserAgentType type,
			boolean emulated,
			boolean ajax) throws Exception {
		LifecycleRequest postback = lifecycleRequest(uxui, type, emulated, true, ajax, viewMap);
		AbstractPersistence persistence = bindPersistence();

		assertDoesNotThrow(() -> new SkyveFacesPhaseListener().afterPhase(postback.event));

		verify(persistence).begin();
		verify(postback.request, never()).getServletContext();
		clearBoundPersistence();
	}

	private static LifecycleRequest lifecycleRequest(UxUi uxui,
													UserAgentType type,
													boolean emulated,
													boolean postback,
													boolean ajax,
													Map<String, Object> viewMap) {
		Map<String, Object> attributes = new HashMap<>();
		attributes.put(UserAgent.class.getName() + ".selection", new RequestUxUiSelection(type, emulated, uxui));

		HttpServletRequest request = mock(HttpServletRequest.class);
		ServletContext servletContext = mock(ServletContext.class);
		when(request.getAttribute(any())).thenAnswer(invocation -> attributes.get(invocation.getArgument(0)));
		when(request.getServletContext()).thenReturn(servletContext);
		org.mockito.Mockito.doAnswer(invocation -> {
			attributes.put(invocation.getArgument(0), invocation.getArgument(1));
			return null;
		}).when(request).setAttribute(any(), any());

		UIViewRoot viewRoot = mock(UIViewRoot.class);
		when(viewRoot.getViewMap()).thenReturn(viewMap);
		when(viewRoot.getViewMap(false)).thenReturn(viewMap);
		when(viewRoot.getViewId()).thenReturn("/test.xhtml");

		ExternalContext externalContext = mock(ExternalContext.class);
		when(externalContext.getRequest()).thenReturn(request);
		when(externalContext.getSessionMap()).thenReturn(new HashMap<>());
		when(externalContext.getRequestParameterMap()).thenReturn(Map.of());

		PartialViewContext partialViewContext = mock(PartialViewContext.class);
		when(partialViewContext.isAjaxRequest()).thenReturn(ajax);
		FacesContext facesContext = mock(FacesContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(facesContext.getViewRoot()).thenReturn(viewRoot);
		when(facesContext.isPostback()).thenReturn(postback);
		when(facesContext.getPartialViewContext()).thenReturn(partialViewContext);
		if (ajax) {
			when(facesContext.isPostback()).thenAnswer(invocation -> partialViewContext.isAjaxRequest());
		}

		PhaseEvent event = mock(PhaseEvent.class);
		when(event.getPhaseId()).thenReturn(PhaseId.RESTORE_VIEW);
		when(event.getFacesContext()).thenReturn(facesContext);
		return new LifecycleRequest(event, externalContext, request, viewRoot, partialViewContext);
	}

	private static AbstractPersistence bindPersistence() throws ReflectiveOperationException {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		persistenceThreadLocal().set(persistence);
		return persistence;
	}

	private static Object serialiseRoundTrip(Object value) throws IOException, ClassNotFoundException {
		byte[] bytes;
		try (ByteArrayOutputStream output = new ByteArrayOutputStream();
				ObjectOutputStream objects = new ObjectOutputStream(output)) {
			objects.writeObject(value);
			bytes = output.toByteArray();
		}
		try (ObjectInputStream objects = new ObjectInputStream(new ByteArrayInputStream(bytes))) {
			return objects.readObject();
		}
	}

	@SuppressWarnings({ "unchecked", "java:S3011" }) // Test-only access is required to verify thread-local lifecycle cleanup.
	private static ThreadLocal<AbstractPersistence> persistenceThreadLocal() throws ReflectiveOperationException {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		return (ThreadLocal<AbstractPersistence>) field.get(null);
	}

	private static void clearBoundPersistence() throws ReflectiveOperationException {
		persistenceThreadLocal().remove();
	}

	private record LifecycleRequest(PhaseEvent event,
									ExternalContext externalContext,
									HttpServletRequest request,
									UIViewRoot viewRoot,
									PartialViewContext partialViewContext) {
		// Immutable test fixture.
	}
}
