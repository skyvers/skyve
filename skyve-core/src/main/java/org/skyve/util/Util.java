package org.skyve.util;

import java.awt.ComponentOrientation;
import java.io.Serializable;
import java.text.MessageFormat;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.logging.Logger;

import org.skyve.domain.Bean;
import org.skyve.impl.util.TestUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

/**
 *
 */
public class Util {
	/**
	 * 
	 */
	public static final Logger LOGGER = UtilImpl.LOGGER;

	public static final String UTF8 = "UTF-8";

	/**
	 * Disallow instantiation
	 */
	private Util() {
		// nothing to see here
	}

	/**
	 * 
	 * @param object
	 * @return
	 */
	public static final <T extends Serializable> T cloneBySerialization(T object) {
		return UtilImpl.cloneBySerialization(object, true);
	}

	/**
	 * 
	 * @param object
	 * @return
	 * @throws Exception
	 */
	public static final <T extends Serializable> T cloneToTransientBySerialization(T object)
	throws Exception {
		return UtilImpl.cloneToTransientBySerialization(object, true);
	}

	/**
	 * Recurse the bean ensuring that everything is touched and loaded from the database.
	 * 
	 * @param bean The bean to load.
	 */
	public static void populateFully(Bean bean) {
		UtilImpl.populateFully(bean);
	}

	/**
	 * Recurse the bean to determine if anything has changed.
	 * 
	 * @param bean The bean to test.
	 * @return if the bean, its collections or its aggregated beans have mutated or not
	 */
	public static boolean hasChanged(Bean bean) {
		return UtilImpl.hasChanged(bean);
	}

	/**
	 * Utility method that tries to properly initialise the persistence layer proxies used by lazy loading.
	 * 
	 * @param <T>
	 * @param possibleProxy The possible proxy
	 * @return the resolved proxy or possibleProxy
	 */
	public static <T> T deproxy(T possibleProxy) throws ClassCastException {
		return UtilImpl.deproxy(possibleProxy);
	}

	/**
	 * Trims and sets "" to null.
	 * 
	 * @param value
	 * @return
	 */
	public static String processStringValue(String value) {
		return UtilImpl.processStringValue(value);
	}

	/**
	 * Internationalises a string and performs message formatting on tokens like {0}, {1} etc.
	 */
	public static String i18n(String key, Locale locale, String... values) {
		String result = key;

		if ((key != null) && (locale != null)) {
			ResourceBundle bundle = ResourceBundle.getBundle("resources.i18n", locale);
			if (bundle.containsKey(key)) {
				result = bundle.getString(key);
			}

			if ((values != null) && (values.length > 0)) {
				result = MessageFormat.format(result, (Object[]) values);
			}
		}

		return result;
	}

	public static boolean isRTL(Locale locale) {
		return (!ComponentOrientation.getOrientation(locale).isLeftToRight());
	}

	public static int UTF8Length(CharSequence sequence) {
		int count = 0;
		for (int i = 0, len = sequence.length(); i < len; i++) {
			char ch = sequence.charAt(i);
			if (ch <= 0x7F) {
				count++;
			} else if (ch <= 0x7FF) {
				count += 2;
			} else if (Character.isHighSurrogate(ch)) {
				count += 4;
				++i;
			} else {
				count += 3;
			}
		}

		return count;
	}

	/**
	 * 
	 * @param object
	 * @throws Exception
	 */
	public static void setTransient(Object object) throws Exception {
		UtilImpl.setTransient(object);
	}

	/**
	 * 
	 * @param object
	 * @param bizDataGroupId
	 * @throws Exception
	 */
	// set the data group of a bean and all its children
	public static void setDataGroup(Object object, String bizDataGroupId) throws Exception {
		UtilImpl.setDataGroup(object, bizDataGroupId);
	}

	/**
	 * Make an instance of a document bean with random values for its properties.
	 * 
	 * @param <T> The type of Document bean to produce.
	 * @param user
	 * @param module
	 * @param document The document (corresponds to type T)
	 * @param depth How far to traverse the object graph - through associations and collections.
	 *        There are relationships that are never ending - ie Contact has Interactions which has User which has Contact
	 * @return The randomly constructed bean.
	 * @throws Exception
	 */
	public static <T extends Bean> T constructRandomInstance(User user, Module module, Document document, int depth)
			throws Exception {
		return TestUtil.constructRandomInstance(user, module, document, depth);
	}

	public static String getContentDirectory() {
		return UtilImpl.CONTENT_DIRECTORY;
	}

	public static String getPasswordHashingAlgorithm() {
		return UtilImpl.PASSWORD_HASHING_ALGORITHM;
	}

	private static Boolean secureUrl = null;
	
	public static boolean isSecureUrl() {
		if (secureUrl == null) {
			secureUrl = Boolean.valueOf((UtilImpl.SERVER_URL == null) ? false : UtilImpl.SERVER_URL.startsWith("https://"));
		}
		return secureUrl.booleanValue();
	}
	
	public static String getServerUrl() {
		return UtilImpl.SERVER_URL;
	}

	public static String getSkyveContext() {
		return UtilImpl.SKYVE_CONTEXT;
	}

	public static String getSkyveContextRealPath() {
		return UtilImpl.SKYVE_CONTEXT_REAL_PATH;
	}

	public static String getHomeUri() {
		return UtilImpl.HOME_URI;
	}

	public static String getSkyveContextUrl() {
		return UtilImpl.SERVER_URL + UtilImpl.SKYVE_CONTEXT;
	}

	public static String getHomeUrl() {
		StringBuilder result = new StringBuilder(128);
		result.append(UtilImpl.SERVER_URL).append(UtilImpl.SKYVE_CONTEXT).append(UtilImpl.HOME_URI);
		return result.toString();
	}

	public static String getDocumentUrl(String bizModule, String bizDocument) {
		return getDocumentUrl(bizModule, bizDocument, null);
	}

	public static String getDocumentUrl(String bizModule, String bizDocument, String bizId) {
		StringBuilder result = new StringBuilder(128);

		result.append(UtilImpl.SERVER_URL).append(UtilImpl.SKYVE_CONTEXT).append(UtilImpl.HOME_URI);
		result.append("?a=e&m=").append(bizModule).append("&d=").append(bizDocument);
		if (bizId != null) {
			result.append("&i=").append(bizId);
		}

		return result.toString();
	}

	public static String getDocumentUrl(Bean bean) {
		return getDocumentUrl(bean.getBizModule(), bean.getBizDocument(), bean.getBizId());
	}

	public static String getGridUrl(String bizModule, String queryName) {
		StringBuilder result = new StringBuilder(128);

		result.append(UtilImpl.SERVER_URL).append(UtilImpl.SKYVE_CONTEXT).append(UtilImpl.HOME_URI);
		result.append("?a=g&m=").append(bizModule).append("&q=").append(queryName);

		return result.toString();
	}
}
