package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.skyve.cache.ConversationCacheConfig;
import org.skyve.cache.SessionCacheConfig;
import org.skyve.content.AttachmentContent;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.cache.DefaultCaching;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.RequestUxUiSelectionTestUtil;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.web.UserAgentType;
import org.skyve.web.WebContext;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings({ "static-method", "java:S1192", "java:S5960" }) // Repeated values and assertions are test-only.
class ImageMarkupViewTest {
	@Test
	void postConstructDeniesAccessWhenSessionIsMissing() {
		MockHttpServletRequest request = new MockHttpServletRequest();

		try (MockFacesContext ignored = MockFacesContext.get(request, new MockHttpServletResponse())) {
			ImageMarkupView view = new ImageMarkupView();
			view.postConstruct();

			assertFalse(view.isCanAccess());
		}
	}

	@Test
	void postConstructAllowsAccessWhenUserSessionAttributeIsPresent() {
		MockHttpServletRequest request = new MockHttpServletRequest();
		request.getSession().setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, new Object());

		try (MockFacesContext ignored = MockFacesContext.get(request, new MockHttpServletResponse())) {
			ImageMarkupView view = new ImageMarkupView();
			view.postConstruct();

			assertTrue(view.isCanAccess());
		}
	}

	@Test
	void parameterSettersTrimBlankValuesToNull() {
		ImageMarkupView view = new ImageMarkupView();

		view.setContextParameter(" ");
		view.setBindingParameter("");
		view.setContentBindingParameter("\t");
		view.setContentIdParameter(null);

		assertNull(view.getContextParameter());
		assertNull(view.getBindingParameter());
		assertNull(view.getContentBindingParameter());
		assertNull(view.getContentIdParameter());
	}

	@Test
	void parameterSettersExposeSanitisedTextValues() {
		ImageMarkupView view = new ImageMarkupView();

		view.setContextParameter(" ctx ");
		view.setBindingParameter("bean.child");
		view.setContentBindingParameter("attachment.contentId");
		view.setContentIdParameter("content-123");

		assertEquals("ctx", view.getContextParameter());
		assertEquals("bean.child", view.getBindingParameter());
		assertEquals("attachment.contentId", view.getContentBindingParameter());
		assertEquals("content-123", view.getContentIdParameter());
	}

	@Test
	void simpleStateAccessorsReflectDefaultsAndAssignedValues() throws Exception {
		ImageMarkupView view = new ImageMarkupView();

		assertEquals(800, view.getImageWidth());
		assertEquals(600, view.getImageHeight());
		assertNull(view.getModuleDocument());
		assertNull(view.getNewContentId());
		assertNull(view.getSvg());

		view.setNewContentId("new-content");
		view.setSvg("<svg/>");
		setField(view, "moduleDocument", "admin.Document");

		assertEquals("admin.Document", view.getModuleDocument());
		assertEquals("new-content", view.getNewContentId());
		assertEquals("<svg/>", view.getSvg());
	}

	@Test
	void backgroundUrlIncludesContentDocumentAndUnsanitisedBinding() throws Exception {
		ImageMarkupView view = new ImageMarkupView();
		view.setContentIdParameter("content-123");
		view.setContentBindingParameter("attachment.contentId");
		setField(view, "moduleDocument", "admin.Document");

		String backgroundUrl = view.getBackgroundUrl();

		assertTrue(backgroundUrl.contains("content?_nm&_n=content-123"));
		assertTrue(backgroundUrl.contains("&_doc=admin.Document"));
		assertTrue(backgroundUrl.endsWith("&_b=attachment.contentId"));
	}

	@Test
	void applyReturnsWithoutFacesContextWhenAccessDenied() {
		ImageMarkupView view = new ImageMarkupView();

		assertDoesNotThrow(view::apply);
	}

	@Test
	void applyAddsMalformedUrlMessageWhenRequiredParametersAreMissing() throws Exception {
		ImageMarkupView view = new ImageMarkupView();
		setField(view, "canAccess", Boolean.TRUE);
		view.setContextParameter("ctx");

		try (MockFacesContext context = MockFacesContext.get()) {
			assertDoesNotThrow(view::apply);
			assertTrue(context.getMessages().hasNext());
		}
	}

	@Test
	@SuppressWarnings("unchecked") // Mockito stubs the generic thread-local accessor
	void applyChecksContentAccessForTheUnsanitisedContentBinding() throws Exception {
		boolean originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
		String originalCacheDirectory = UtilImpl.CACHE_DIRECTORY;
		SessionCacheConfig originalSessionCache = UtilImpl.SESSION_CACHE;
		ConversationCacheConfig originalConversationCache = UtilImpl.CONVERSATION_CACHE;
		Path cacheDirectory = Files.createTempDirectory("image-markup-access-cache");
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		MockWebContext webContext = new MockWebContext();
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		HashMap<String, Object> values = new HashMap<>();
		values.put(Bean.DOCUMENT_ID, "bean-1");
		webContext.setCurrentBean(new DynamicBean("test", "Doc", values));
		when(request.getSession(false)).thenReturn(session);
		when(session.getId()).thenReturn(webContext.getSessionId());
		RequestUxUiSelectionTestUtil.install(request,
												UserAgentType.desktop,
												false,
												UxUi.newPrimeFaces(UxUi.DESKTOP_NAME, "editorial", "skyve", "blue"));
		when(persistence.getUser()).thenReturn(user);
		// content access is granted, but the user has no customer to check document access with
		doReturn(Boolean.TRUE).when(user).canAccess(UserAccess.content("test", "Doc", "attachment.contentId"), UxUi.DESKTOP_NAME);
		ImageMarkupView view = new ImageMarkupView();
		setField(view, "canAccess", Boolean.TRUE);
		view.setContentBindingParameter("attachment_contentId");
		Field threadLocalPersistence = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		threadLocalPersistence.setAccessible(true);

		try (MockFacesContext context = MockFacesContext.get(request, new MockHttpServletResponse())) {
			UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
			UtilImpl.CACHE_DIRECTORY = cacheDirectory.toString() + File.separator;
			UtilImpl.SESSION_CACHE = new SessionCacheConfig(100, 10);
			UtilImpl.CONVERSATION_CACHE = new ConversationCacheConfig(100, 10);
			DefaultCaching.get().shutdown();
			DefaultCaching.get().startup();
			StateUtil.commitAndCacheConversation(webContext);
			view.setContextParameter(webContext.getWebId());
			((ThreadLocal<AbstractPersistence>) threadLocalPersistence.get(null)).set(persistence);

			assertDoesNotThrow(view::apply);
			assertTrue(context.getMessages().hasNext());
		}
		finally {
			((ThreadLocal<AbstractPersistence>) threadLocalPersistence.get(null)).remove();
			DefaultCaching.get().shutdown();
			UtilImpl.FORCE_NON_PERSISTENT_CACHING = originalForceNonPersistentCaching;
			UtilImpl.CACHE_DIRECTORY = originalCacheDirectory;
			UtilImpl.SESSION_CACHE = originalSessionCache;
			UtilImpl.CONVERSATION_CACHE = originalConversationCache;
			try (Stream<Path> paths = Files.walk(cacheDirectory)) {
				paths.sorted((a, b) -> b.compareTo(a))
						.map(Path::toFile)
						.forEach(File::delete);
			}
		}

		verify(user).canAccess(UserAccess.content("test", "Doc", "attachment.contentId"), UxUi.DESKTOP_NAME);
		verify(user).getCustomer();
		verify(persistence).rollback();
	}

	@Test
	void markupSuccessScriptResolvesOwningFrameWithoutParentOrTopAssumptions() {
		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		AttachmentContent content = new AttachmentContent("demo", "admin", "Contact", null, "user", "biz", "attachment")
				.attachment("quote's.png", "image/png", new byte[] {1});

		String script = ImageMarkupView.createMarkupSuccessScript("attachment", "content-789", bean, content);

		assertTrue(script.contains("var skyveMarkupWindow=SKYVE.Util.findSkyveWindow();"), script);
		assertTrue(script.contains("if(skyveMarkupWindow){"), script);
		assertTrue(script.contains("skyveMarkupWindow.isc.BizUtil.afterMarkupApply('attachment','content-789','admin.Contact','quote\\'s.png')"), script);
		assertTrue(script.contains("skyveMarkupWindow.SKYVE.PF.afterMarkupApply('attachment','content-789','admin.Contact','quote\\'s.png')"), script);
		assertFalse(script.contains("skyveMarkupWindow=skyveMarkupWindow.parent"), script);
		assertFalse(script.contains("window.parent.isc"), script);
		assertFalse(script.contains("top.SKYVE"), script);
	}

	@SuppressWarnings({ "java:S112", "java:S3011" }) // Reflection configures private view state without a JSF container.
	private static void setField(Object target, String name, Object value) throws Exception {
		Field field = ImageMarkupView.class.getDeclaredField(name);
		field.setAccessible(true);
		field.set(target, value);
	}
}
