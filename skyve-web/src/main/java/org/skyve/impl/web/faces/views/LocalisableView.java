package org.skyve.impl.web.faces.views;

import java.io.Serializable;
import java.util.Collection;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;

import jakarta.faces.context.FacesContext;

public abstract class LocalisableView implements Serializable {
	private static final long serialVersionUID = 2440700208785488690L;

	public static final class I18nMapAdapter implements Map<String, String>, Serializable {
		private static final long serialVersionUID = 4290123391587825685L;

		private Locale locale;
		
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

		@Override
		public boolean containsKey(Object key) {
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean containsValue(Object value) {
			throw new UnsupportedOperationException();
		}

		@Override
		public String get(final Object key) {
			return new FacesAction<String>() {
				@Override
				public String callback() throws Exception {
					String result = Util.nullSafeI18n((String) key, locale);
					if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.finest("I18nMapAdapter.get " + key + " = " + result);
					return result;
				}
			}.execute();
		}

		@Override
		public String put(String key, String value) {
			throw new UnsupportedOperationException();
		}

		@Override
		public String remove(Object key) {
			throw new UnsupportedOperationException();
		}

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
	public final String getDir() {
		return dir;
	}

	/**
	 * Used in the faces view tag
	 * @return UTF-8
	 */
	@SuppressWarnings("static-method")
	public String getEncoding() {
		return Util.UTF8;
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
	public final String getEnvironmentIdentifier() {
		return UtilImpl.ENVIRONMENT_IDENTIFIER;
	}
	
	@SuppressWarnings("static-method")
	public final String getWebResourceFileVersion() {
		return UtilImpl.WEB_RESOURCE_FILE_VERSION;
	}
}
