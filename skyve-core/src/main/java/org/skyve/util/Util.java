package org.skyve.util;

import java.awt.ComponentOrientation;
import java.io.Serializable;
import java.text.MessageFormat;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.TreeMap;
import java.util.logging.Logger;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.test.TestUtil;

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
		return UtilImpl.cloneBySerialization(object);
	}

	/**
	 * 
	 * @param object
	 * @return
	 * @throws Exception
	 */
	public static final <T extends Serializable> T cloneToTransientBySerialization(T object)
	throws Exception {
		return UtilImpl.cloneToTransientBySerialization(object);
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
	 * This is deprecated and has been moved to AbstractBean with the "changed" bean property.
	 * This enables the method's result to be cached in Bean proxies.
	 * 
	 * @param bean The bean to test.
	 * @return if the bean, its collections or its aggregated beans have mutated or not
	 */
	@Deprecated
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
	 * Internationalises a string for the user's locale and performs message formatting on tokens like {0}, {1} etc.
	 */
	public static String i18n(String key, String... values) {
		if (key == null) {
			return null;
		}
		
		// NB Don't attempt to get a user unless persistence has been initialised
		User u = (AbstractPersistence.IMPLEMENTATION_CLASS == null) ? null : CORE.getUser();
		return i18n(key, (u == null) ? null : u.getLocale(), values);
	}
	
	// language code -> (key -> string)
	// Use of this map is WAY faster than using ResourceBundle which sux arse.
	// This map is synchronized on during the put but reads are left free.
	// The usage of this map is almost always read.
	// The map is keyed on language code because there are less language codes than locales.
	// NB Make the map volatile to ensure it is readable by multiple threads.
	private static volatile Map<String, Map<String, String>> I18N_PROPERTIES = new TreeMap<>();
	
	/**
	 * Internationalises a string for a particular locale and performs message formatting on tokens like {0}, {1} etc.
	 */
	public static String i18n(String key, Locale locale, String... values) {
		String result = key;

		if (key != null) {
			try {
				Locale l = (locale == null) ? Locale.ENGLISH : locale;
				String lang = l.getLanguage();
				Map<String, String> properties = I18N_PROPERTIES.get(lang);
				if (properties == null) {
					synchronized (I18N_PROPERTIES) {
						properties = I18N_PROPERTIES.get(lang);
						if (properties == null) {
							ResourceBundle bundle = ResourceBundle.getBundle("resources.i18n", l, Thread.currentThread().getContextClassLoader());
							properties = new TreeMap<>();
							for (String bundleKey : bundle.keySet()) {
								properties.put(bundleKey, bundle.getString(bundleKey));
							}
							ResourceBundle.clearCache(Thread.currentThread().getContextClassLoader());
							I18N_PROPERTIES.put(lang, properties);
						}
					}
				}
				result = properties.get(key);
				if (result == null) {
					if ((lang != null) && (! lang.equals(Locale.ENGLISH.getLanguage()))) {
						result = i18n(key, Locale.ENGLISH, values);
					}
					if (result == null) {
						result = key;
					}
				}
	
				if ((values != null) && (values.length > 0)) {
					result = MessageFormat.format(result, (Object[]) values);
				}
			}
			catch (@SuppressWarnings("unused") MissingResourceException e) {
				LOGGER.warning("Could not find bundle \"resources.i18n\"");
			}
		}

		return result;
	}

	public static boolean isRTL() {
		// NB Don't attempt to get a user unless persistence has been initialised
		User u = (AbstractPersistence.IMPLEMENTATION_CLASS == null) ? null : CORE.getUser();
		return isRTL((u == null) ? null : u.getLocale());
	}

	public static boolean isRTL(Locale locale) {
		return (locale != null) && (! ComponentOrientation.getOrientation(locale).isLeftToRight());
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

	public static String getModuleDirectory() {
		return UtilImpl.MODULE_DIRECTORY;
	}

	public static String getPasswordHashingAlgorithm() {
		return UtilImpl.PASSWORD_HASHING_ALGORITHM;
	}
	
	public static String getSupportEmailAddress() {
		return ((UtilImpl.SUPPORT_EMAIL_ADDRESS == null) ? "" : UtilImpl.SUPPORT_EMAIL_ADDRESS);
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

	public static String getLoginUrl() {
		StringBuilder result = new StringBuilder(128);
		result.append(UtilImpl.SERVER_URL).append(UtilImpl.SKYVE_CONTEXT).append(UtilImpl.AUTHENTICATION_LOGIN_URI);
		return result.toString();
	}

	public static String getLoggedOutUrl() {
		StringBuilder result = new StringBuilder(128);
		result.append(UtilImpl.SERVER_URL).append(UtilImpl.SKYVE_CONTEXT).append(UtilImpl.AUTHENTICATION_LOGGED_OUT_URI);
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
	
	/**
	 * Constructs a HTML anchor for the document instance
	 *  
	 * @param bizModule - the module of the document instance
	 * @param bizDocument - the document
	 * @param bizId - the bizId of the instance
	 * @param targetNewWindow - whether the target value for the anchor is a new window
	 * @param anchorMarkup - the html markup value (usually text) of the anchor 
	 * @return - the constructed URL as a String
	 */
	public static String getDocumentAnchorUrl(String bizModule, String bizDocument, String bizId, boolean targetNewWindow, String anchorMarkup) {
		StringBuilder result = new StringBuilder(128);

		result.append("<a href=\"").append(getDocumentUrl(bizModule, bizDocument, bizId));
		if (targetNewWindow) {
			result.append("\" target=\"_blank\">");
		}
		else {
			result.append("\">");
		}
		result.append(anchorMarkup).append("</a>");

		return result.toString();
	}

	public static String getGridUrl(String bizModule, String queryName) {
		StringBuilder result = new StringBuilder(128);

		result.append(UtilImpl.SERVER_URL).append(UtilImpl.SKYVE_CONTEXT).append(UtilImpl.HOME_URI);
		result.append("?a=g&m=").append(bizModule).append("&q=").append(queryName);

		return result.toString();
	}
	
	public static String getContentUrl(String bizModule, String bizDocument, String binding, String contentId) {
		StringBuilder result = new StringBuilder(128);

		result.append(UtilImpl.SERVER_URL).append(UtilImpl.SKYVE_CONTEXT).append(UtilImpl.HOME_URI);
		result.append("content?_n=").append(contentId);
		result.append("&_doc=").append(bizModule).append('.').append(bizDocument);
		result.append("&_b=").append(binding);

		return result.toString();
	}
	
	public static String getResourceUrl(String bizModule, String bizDocument, String resourceFileName) {
		StringBuilder result = new StringBuilder(128);

		result.append(UtilImpl.SERVER_URL).append(UtilImpl.SKYVE_CONTEXT).append(UtilImpl.HOME_URI);
		result.append("resources?_n=").append(resourceFileName);
		if ((bizModule != null) && (bizDocument != null)) {
			result.append("&_doc=").append(bizModule).append('.').append(bizDocument);
		}

		return result.toString();
	}

	public static String getResourceUrl(String resourceFileName) {
		return getResourceUrl(null, null, resourceFileName);
	}
	
    /**
     * Constructs a HTML image URL for the given content item
     * (If the content item is not an image type, the content servlet returns a file type thumbnail)
     * 
	 * @param bizModule - the module of the document containing the content
	 * @param bizDocument - the document containing the content
	 * @param binding - the binding name of the content attribute
	 * @param contentId - the value of the content attribute (a String id of the item in the content store) 
	 * @param width - the width of the image in pixels
	 * @param height - the height of the image in pixels
	 * @return - the constructed URL as a String
     */
    public static String getContentImageUrl(String bizModule, String bizDocument, String binding, String contentId, int width, int height) {
    	StringBuilder result = new StringBuilder(128);
    	
    	result.append("<img src=\"");
    	result.append(getContentUrl(bizModule, bizDocument, binding, contentId));
    	result.append("&_w=").append(width);
    	result.append("&_h=").append(height);
    	result.append("\"/>");
        
    	return result.toString();
    }

	/**
	 * Constructs an HTML anchor URL for the given content item
	 *  
	 * @param bizModule - the module of the document containing the content
	 * @param bizDocument - the document containing the content
	 * @param binding - the binding name of the content attribute
	 * @param contentId - the value of the content attribute (a String id of the item in the content store) 
	 * @param targetNewWindow - whether the target value for the anchor is a new window
	 * @param anchorMarkup - the html markup value (usually text) of the anchor 
	 * @return - the constructed URL as a String
	 */	
    public static String getContentAnchorUrl(String bizModule, String bizDocument, String binding, String contentId, boolean targetNewWindow, String anchorMarkup) {
        String contentUrl = getContentUrl(bizModule, bizDocument, binding, contentId);
        StringBuilder result = new StringBuilder(128);
        
        result.append("<a href=\"").append(contentUrl);
        if (targetNewWindow) {
        	result.append("\" target=\"_blank\">");
        }
        else {
            result.append("\">");
        }
        result.append(anchorMarkup).append("</a>");
        
        return result.toString();
    }
	
	/**
	 * Constructs an HTML anchor with image URL for the given content item
	 *  
	 * @param bizModule - the module of the document containing the content
	 * @param bizDocument - the document containing the content
	 * @param binding - the binding name of the content attribute
	 * @param contentId - the value of the content attribute (a String id of the item in the content store) 
	 * @param targetNewWindow - whether the target value for the anchor is a new window
	 * @param width - the width of the image in pixels
	 * @param height - the height of the image in pixels
	 * @return - the constructed URL as a String
	 */
    public static String getContentAnchorWithImageUrl(String bizModule, String bizDocument, String binding, String contentId, boolean targetNewWindow, int width, int height) {
    	return getContentAnchorUrl(bizModule, bizDocument, binding, contentId, targetNewWindow, getContentImageUrl(bizModule, bizDocument, binding, contentId, width, height));
    }

}
