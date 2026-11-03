package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.Locale;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.user.User;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import jakarta.faces.context.FacesContext;

@SuppressWarnings("static-method")
class LocalisableViewTest {
	private static final class TestLocalisableView extends LocalisableView {
		private static final long serialVersionUID = 1L;

		void doInitialise() {
			initialise();
		}
	}

	@AfterEach
	void clearRequestState() throws Exception {
		FacesContext context = FacesContext.getCurrentInstance();
		if (context != null) {
			context.release();
		}
		setThreadLocalPersistence(null);
	}

	@Test
	void basicGettersExposeEncodingAndEnvironmentValues() {
		TestLocalisableView view = new TestLocalisableView();

		assertEquals("UTF-8", view.getEncoding());
		assertEquals(UtilImpl.ENVIRONMENT_IDENTIFIER, view.getEnvironmentIdentifier());
		assertEquals(WebUtil.getEnvironmentBannersHtml(), view.getEnvironmentBanners());
		assertEquals(UtilImpl.WEB_RESOURCE_FILE_VERSION, view.getWebResourceFileVersion());
		assertNotNull(view.getI18n());
	}

	@Test
	void i18nMapAdapterUnsupportedOperationsThrow() {
		LocalisableView.I18nMapAdapter map = new LocalisableView.I18nMapAdapter();

		assertThrows(UnsupportedOperationException.class, map::size);
		assertThrows(UnsupportedOperationException.class, map::isEmpty);
		assertThrows(UnsupportedOperationException.class, () -> map.containsKey("k"));
		assertThrows(UnsupportedOperationException.class, () -> map.containsValue("v"));
		assertThrows(UnsupportedOperationException.class, () -> map.put("k", "v"));
		assertThrows(UnsupportedOperationException.class, () -> map.remove("k"));
		java.util.Map<String, String> values = java.util.Map.of("k", "v");
		assertThrows(UnsupportedOperationException.class, () -> map.putAll(values));
		assertThrows(UnsupportedOperationException.class, map::clear);
		assertThrows(UnsupportedOperationException.class, map::keySet);
		assertThrows(UnsupportedOperationException.class, map::values);
		assertThrows(UnsupportedOperationException.class, map::entrySet);
	}

	@Test
	void i18nMapAdapterGetResolvesKnownBundleKey() throws Exception {
		LocalisableView.I18nMapAdapter map = new LocalisableView.I18nMapAdapter();
		AbstractPersistence persistence = mock(AbstractPersistence.class);

		map.setLocale(Locale.ENGLISH);
		setThreadLocalPersistence(persistence);

		assertEquals("A problem was encountered.", map.get("exception.generic"));
	}

	@Test
	void initialiseUsesRequestLocaleWhenNoUserIsBound() {
		TestLocalisableView view = new TestLocalisableView();

		try (MockFacesContext ignored = setFacesRequestLocale(Locale.forLanguageTag("ar"))) {
			view.doInitialise();
		}

		assertEquals("rtl", view.getDir());
		assertEquals("ar", view.getLanguageTag());
		assertNotNull(view.getI18n());
	}

	@Test
	void initialiseDefaultsToEnglishWhenRequestLocaleIsUnavailable() {
		TestLocalisableView view = new TestLocalisableView();

		try (MockFacesContext ignored = setFacesRequestLocale(null)) {
			view.doInitialise();
		}

		assertEquals("ltr", view.getDir());
		assertEquals("en", view.getLanguageTag());
	}

	@Test
	void initialiseUsesRequestLocaleWhenBoundPersistenceHasNoUser() throws Exception {
		TestLocalisableView view = new TestLocalisableView();
		AbstractPersistence persistence = mock(AbstractPersistence.class);

		when(persistence.getUser()).thenReturn(null);
		setThreadLocalPersistence(persistence);

		try (MockFacesContext ignored = setFacesRequestLocale(Locale.forLanguageTag("ar"))) {
			view.doInitialise();
		}

		assertEquals("rtl", view.getDir());
		assertEquals("ar", view.getLanguageTag());
	}

	@Test
	void initialiseUsesRequestLocaleWhenUserLocaleIsUnavailable() throws Exception {
		TestLocalisableView view = new TestLocalisableView();
		User user = mock(User.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);

		when(user.getLocale()).thenReturn(null);
		when(persistence.getUser()).thenReturn(user);
		setThreadLocalPersistence(persistence);

		try (MockFacesContext ignored = setFacesRequestLocale(Locale.forLanguageTag("ar"))) {
			view.doInitialise();
		}

		assertEquals("rtl", view.getDir());
		assertEquals("ar", view.getLanguageTag());
	}

	@Test
	void initialisePrefersUserLocaleOverRequestLocale() throws Exception {
		TestLocalisableView view = new TestLocalisableView();
		User user = mock(User.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);

		when(user.getLocale()).thenReturn(Locale.JAPAN);
		when(persistence.getUser()).thenReturn(user);
		setThreadLocalPersistence(persistence);

		try (MockFacesContext ignored = setFacesRequestLocale(Locale.forLanguageTag("ar"))) {
			view.doInitialise();
		}

		assertEquals("ltr", view.getDir());
		assertEquals("ja-JP", view.getLanguageTag());
	}

	@SuppressWarnings("resource") // Ownership of the installed context is transferred to the caller.
	private static MockFacesContext setFacesRequestLocale(Locale locale) {
		if (locale == null) {
			MockFacesContext result = MockFacesContext.get();
			result.getExternalContext().setRequestLocale(null);
			return result;
		}

		MockHttpServletRequest request = new MockHttpServletRequest();
		request.setPreferredLocales(java.util.List.of(locale));
		return MockFacesContext.get(request, new MockHttpServletResponse());
	}

	@SuppressWarnings({ "unchecked", "java:S3011" }) // Reflection isolates the private persistence thread state for the test.
	private static void setThreadLocalPersistence(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
	}
}
