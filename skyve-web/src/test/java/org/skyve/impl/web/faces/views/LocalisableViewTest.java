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
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.user.User;

import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;

@SuppressWarnings("static-method")
class LocalisableViewTest {
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	private static final class TestLocalisableView extends LocalisableView {
		private static final long serialVersionUID = 1L;

		void doInitialise() {
			initialise();
		}
	}

	@AfterEach
	void clearRequestState() throws Exception {
		FacesContextBridge.setCurrent(null);
		setThreadLocalPersistence(null);
	}

	@Test
	void basicGettersExposeEncodingAndEnvironmentValues() {
		TestLocalisableView view = new TestLocalisableView();

		assertEquals("UTF-8", view.getEncoding());
		assertEquals(UtilImpl.ENVIRONMENT_IDENTIFIER, view.getEnvironmentIdentifier());
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
		assertThrows(UnsupportedOperationException.class, () -> map.putAll(java.util.Map.of("k", "v")));
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
		setFacesRequestLocale(Locale.forLanguageTag("ar"));

		view.doInitialise();

		assertEquals("rtl", view.getDir());
		assertEquals("ar", view.getLanguageTag());
		assertNotNull(view.getI18n());
	}

	@Test
	void initialiseDefaultsToEnglishWhenRequestLocaleIsUnavailable() {
		TestLocalisableView view = new TestLocalisableView();
		setFacesRequestLocale(null);

		view.doInitialise();

		assertEquals("ltr", view.getDir());
		assertEquals("en", view.getLanguageTag());
	}

	@Test
	void initialiseUsesRequestLocaleWhenBoundPersistenceHasNoUser() throws Exception {
		TestLocalisableView view = new TestLocalisableView();
		AbstractPersistence persistence = mock(AbstractPersistence.class);

		when(persistence.getUser()).thenReturn(null);
		setFacesRequestLocale(Locale.forLanguageTag("ar"));
		setThreadLocalPersistence(persistence);

		view.doInitialise();

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
		setFacesRequestLocale(Locale.forLanguageTag("ar"));
		setThreadLocalPersistence(persistence);

		view.doInitialise();

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
		setFacesRequestLocale(Locale.forLanguageTag("ar"));
		setThreadLocalPersistence(persistence);

		view.doInitialise();

		assertEquals("ltr", view.getDir());
		assertEquals("ja-JP", view.getLanguageTag());
	}

	private static void setFacesRequestLocale(Locale locale) {
		FacesContext context = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);

		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequestLocale()).thenReturn(locale);
		FacesContextBridge.setCurrent(context);
	}

	private static void setThreadLocalPersistence(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
	}
}
