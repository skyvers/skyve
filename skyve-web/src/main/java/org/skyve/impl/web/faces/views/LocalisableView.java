package org.skyve.impl.web.faces.views;

import java.io.Serializable;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

import jakarta.faces.context.FacesContext;

/**
 * Adds EL localisation via "i18n" map property to any Faces View that extends it.
 * Note that no methods should be final in here as a bean could be injected and a proxy needs to be made.
 */
public abstract class LocalisableView implements Serializable {
	private static final long serialVersionUID = 2440700208785488690L;

    protected static final Logger LOGGER = SkyveLoggerFactory.getLogger(LocalisableView.class);
    private static final Logger FACES_LOGGER = Category.FACES.logger();

	public static final class I18nMapAdapter implements Map<String, String>, Serializable {
		private static final long serialVersionUID = 4290123391587825685L;

		private Locale locale;
		
		/**
		 * Sets the locale used to resolve i18n keys.
		 *
		 * @param locale the locale to use for key resolution
		 */
		public void setLocale(Locale locale) {
			this.locale = locale;
		}
		
		@Override
		public int size() {
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean isEmpty() {
			throw new UnsupportedOperationException();
		}

		/**
		 * Indicates whether key lookup is supported.
		 *
		 * @param key the key to test
		 * @return never returns normally
		 */
		@Override
		public boolean containsKey(Object key) {
			throw new UnsupportedOperationException();
		}

		/**
		 * Indicates whether value lookup is supported.
		 *
		 * @param value the value to test
		 * @return never returns normally
		 */
		@Override
		public boolean containsValue(Object value) {
			throw new UnsupportedOperationException();
		}

		/**
		 * Resolves the localised value for a key.
		 *
		 * @param key the i18n key
		 * @return the localised value
		 */
		@Override
		public String get(final Object key) {
			return new FacesAction<String>() {
				@Override
				public String callback() throws Exception {
					String result = Util.nullSafeI18n((String) key, locale);
					if (UtilImpl.FACES_TRACE) FACES_LOGGER.trace("I18nMapAdapter.get {} = {}", key, result);
					return result;
				}
			}.execute();
		}

		/**
		 * Indicates whether mutating put is supported.
		 *
		 * @param key the key to store
		 * @param value the value to store
		 * @return never returns normally
		 */
		@Override
		public String put(String key, String value) {
			throw new UnsupportedOperationException();
		}

		/**
		 * Indicates whether remove is supported.
		 *
		 * @param key the key to remove
		 * @return never returns normally
		 */
		@Override
		public String remove(Object key) {
			throw new UnsupportedOperationException();
		}

		/**
		 * Indicates whether bulk put is supported.
		 *
		 * @param m the entries to insert
		 */
		@Override
		public void putAll(Map<? extends String, ? extends String> m) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void clear() {
			throw new UnsupportedOperationException();
		}

		@Override
		public Set<String> keySet() {
			throw new UnsupportedOperationException();
		}

		@Override
		public Collection<String> values() {
			throw new UnsupportedOperationException();
		}

		@Override
		public Set<Map.Entry<String, String>> entrySet() {
			throw new UnsupportedOperationException();
		}
	}

	
	private String dir;
	/**
	 * Used in the faces html tag.
	 * @return	The text direction - rtl or ltr.
	 */
	public String getDir() {
		return dir;
	}

	/**
	 * Used in the faces view tag
	 * @return UTF-8
	 */
	@SuppressWarnings("static-method")
	public String getEncoding() {
		return StandardCharsets.UTF_8.name();
	}
	
	private I18nMapAdapter i18n = new I18nMapAdapter();
	public Map<String, String> getI18n() {
		return i18n;
	}
	
	protected void initialise() {
		Locale locale = FacesContext.getCurrentInstance().getExternalContext().getRequestLocale();
		User user = CORE.getUser();
		if (user != null) {
			Locale userLocale = user.getLocale();
			if (userLocale != null) {
				locale = userLocale;
			}
		}
		dir = (locale != null) ? (Util.isRTL(locale) ? "rtl" : "ltr") : "ltr";
		i18n.setLocale(locale);
	}
	
	/**
	 * Return the environment identifier string for this skyve instance.
	 * @return	The string as defined in the json configuration.
	 */
	@SuppressWarnings("static-method")
	public String getEnvironmentIdentifier() {
		return UtilImpl.ENVIRONMENT_IDENTIFIER;
	}
	
	@SuppressWarnings("static-method")
	public String getWebResourceFileVersion() {
		return UtilImpl.WEB_RESOURCE_FILE_VERSION;
	}
}
