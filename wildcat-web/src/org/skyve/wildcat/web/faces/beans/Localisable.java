package org.skyve.wildcat.web.faces.beans;

import java.io.Serializable;
import java.util.Collection;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.skyve.util.Util;
import org.skyve.wildcat.metadata.user.UserImpl;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.ServletConstants;
import org.skyve.wildcat.web.faces.FacesAction;

public abstract class Localisable implements Serializable {
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
				@SuppressWarnings("synthetic-access")
				public String callback() throws Exception {
					String result = Util.i18n((String) key, locale);
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

	private Locale locale;
	public Locale getLocale() {
		return locale;
	}

	/**
	 * Used in the faces view tag
	 * @return UTF-8
	 */
	@SuppressWarnings("static-method")
	public String getEncoding() {
		return ServletConstants.UTF8;
	}
	
	private I18nMapAdapter i18n = new I18nMapAdapter();
	public Map<String, String> getI18n() {
		return i18n;
	}
	
	protected final void initialise(UserImpl user, Locale requestLocale) {
		locale = requestLocale;
		if (user != null) {
			Locale userLocale = user.getLocale();
			if (userLocale != null) {
				locale = userLocale;
			}
		}
		dir = (locale != null) ? (Util.isRTL(locale) ? "rtl" : "ltr") : "ltr";
		i18n.setLocale(locale);
	}
}
