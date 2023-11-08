package org.skyve.impl.util;

import java.io.InputStream;
import java.io.Serializable;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;
import java.util.logging.Logger;

import org.hibernate.internal.util.SerializationHelper;
import org.hibernate.proxy.HibernateProxy;
import org.skyve.CORE;
import org.skyve.cache.CSRFTokenCacheConfig;
import org.skyve.cache.CacheConfig;
import org.skyve.cache.ConversationCacheConfig;
import org.skyve.cache.HibernateCacheConfig;
import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.json.Minifier;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Inverse;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DataStore;
import org.skyve.util.BeanVisitor;
import org.skyve.util.JSON;

import net.gcardone.junidecode.Junidecode;

public class UtilImpl {
	/**
	 * Disallow instantiation
	 */
	private UtilImpl() {
		// no-op
	}

	/**
	 * The raw configuration data from reading the JSON.
	 */
	public static Map<String, Object> CONFIGURATION = null;

	/**
	 * The raw configuration data from reading the override JSON.
	 * This will be an empty map if there are no overrides defined.
	 */
	public static Map<String, Object> OVERRIDE_CONFIGURATION;

	// For versioning javascript/css etc for web site
	public static final String WEB_RESOURCE_FILE_VERSION = "52";
	public static final String SKYVE_VERSION = "8.3.1-SNAPSHOT";
	public static final String SMART_CLIENT_DIR = "isomorphic120";

	public static boolean XML_TRACE = false;
	public static boolean HTTP_TRACE = false;
	public static boolean QUERY_TRACE = false;
	public static boolean COMMAND_TRACE = false;
	public static boolean FACES_TRACE = false;
	public static boolean SQL_TRACE = false;
	public static boolean CONTENT_TRACE = false;
	public static boolean SECURITY_TRACE = false;
	public static boolean BIZLET_TRACE = false;
	public static boolean DIRTY_TRACE = false;
	public static boolean PRETTY_SQL_OUTPUT = false;
	public static final Logger LOGGER = Logger.getLogger("SKYVE");

	// the name of the application archive, e.g. typically projectName.war or projectName.ear
	public static String ARCHIVE_NAME;

	// This is set in the web.xml but defaults to windows
	// as a dev environment for design time and generation gear
	public static String CONTENT_DIRECTORY = "/_/Apps/content/";

	// The cron expression to use to fire off the content garbage collection
	// Defaults to run at 7 past the hour every hour.
	public static String CONTENT_GC_CRON = "0 7 0/1 1/1 * ? *";

	// The age in minutes content must be to be eligible for garbage collection
	public static int CONTENT_GC_ELIGIBLE_AGE_MINUTES = 720;

	// The cron expression to use to fire off the evict expired state job
	// Defaults to run at 37 past midnight every day.
	public static String STATE_EVICT_CRON = "0 37 0 1/1 * ? *";

	// Should the attachments be stored on the file system or inline.
	public static boolean CONTENT_FILE_STORAGE = true;

	// The arguments to send to the JDBC TCP server when running the content management in server mode.
	public static String CONTENT_JDBC_SERVER_ARGS = null;

	// The URL to connect to the REST server when running the content management in server mode.
	public static String CONTENT_REST_SERVER_URL = null;

	// Backup folder - defaults to <content.directory>
	// If defined it must exist at startup.
	public static String BACKUP_DIRECTORY = null;
	
	// A class to load to provide external cloud backups.
	public static String BACKUP_EXTERNAL_BACKUP_CLASS = null;
	
	// Properties required to connect to a cloud provider for backup storage
	public static Map<String, Object> BACKUP_PROPERTIES = null;
	
	
	// Allowed file upload file names - default is a blacklist of harmful "executable" files
	public static String UPLOADS_FILE_WHITELIST_REGEX = "^.+\\.(?!(ADE|ADP|APP|ASA|ASP|BAS|BAT|CAB|CER|CHM|CMD|COM|CPL|CRT|CSH|DLL|DOCM|DOTM|EXE|FXP|HLP|HTA|HTR|INF|INS|ISP|ITS|JS|JSE|KSH|LNK|MAD|MAF|MAG|MAM|MAQ|MAR|MAS|MAT|MAU|MAV|MAW|MDA|MDB|MDE|MDT|MDW|MDZ|MSC|MSI|MSP|MST|OCX|OPS|PCD|PIF|POTM|PPAM|PPSM|PPTM|PRF|PRG|REG|SCF|SO|SCR|SCT|SHB|SHS|TMP|URL|VB|VBE|VBS|VBX|VSMACROS|VSS|VST|VSW|WS|WSC|WSF|WSH|XLAM|XLSB|XLSM|XSTM|XSL)$)([^.]+$)";
	
	// Max file upload size - default is 10MB the same as wildfly default
	public static int UPLOADS_FILE_MAXIMUM_SIZE_IN_MB = 10;
	
	// Allowed content upload file names - default is a blacklist of harmful "executable" files
	public static String UPLOADS_CONTENT_WHITELIST_REGEX = UPLOADS_FILE_WHITELIST_REGEX;
	
	// Max content upload size - default is 10MB the same as wildfly default
	public static int UPLOADS_CONTENT_MAXIMUM_SIZE_IN_MB = UPLOADS_FILE_MAXIMUM_SIZE_IN_MB;

	// Allowed image upload file names - default is a blacklist of harmful "executable" files
	public static String UPLOADS_IMAGE_WHITELIST_REGEX = UPLOADS_FILE_WHITELIST_REGEX;
	
	// Max image upload size - default is 10MB the same as wildfly default
	public static int UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB = UPLOADS_FILE_MAXIMUM_SIZE_IN_MB;

	// Allowed bizport upload file names - default is a XLS and XLSX files
	public static String UPLOADS_BIZPORT_WHITELIST_REGEX = "^.+\\.(XLS|XLSX)$";
	
	// Max bizport upload size - default is 10MB the same as wildfly default
	public static int UPLOADS_BIZPORT_MAXIMUM_SIZE_IN_MB = UPLOADS_FILE_MAXIMUM_SIZE_IN_MB;

	// Where to look for add-ins - defaults to <content.directory>/addins/
	public static String ADDINS_DIRECTORY = null;

	// The number of threads that are allowed to serve thumb nails at once.
	// Too many threads can cause out of memory errors.
	// You can calculate this as concurrentThreads * memory usage determined by targetSize below
	// For the default of 10 concurrentThreads at 4MB the approximately max memory usage is 40MB.
	public static int THUMBNAIL_CONCURRENT_THREADS = 10;

	// The sub-sampling doesn't kick in until the image's largest dimension is at least double the target size
	// Then it sub-samples pixels by 2, 3 etc.
	// You can calculate the approximate max memory used per image with
	// targetSize * 2 (double width) * targetSize * 2 (double height) * 4 (ARGB bytes per pixel) / 1024 (KB) / 1024 (MB)
	// assuming the images are relatively square.
	// target of 256 = max 1MB; target of 512 = max 4MB, target of 1024 = max 16MB per image.
	public static int THUMBNAIL_SUBSAMPLING_MINIMUM_TARGET_SIZE = 512;

	// Thumbnails can be stored on the file system or generated on the fly each time
	public static boolean THUMBNAIL_FILE_STORAGE = true;

	// Where to put thumbnails if fileStorage is true - defaults to <content.directory>/SKYVE_THUMBNAILS/
	// Skyve will recreate this folder if it is deleted whilst running but if defined it must exist at startup.
	public static String THUMBNAIL_DIRECTORY = null;
	
	// This is set in web.xml and should only be used when the APP server in use
	// doesn't allow us to get the absolute path of a resource - jboss 4.0.5.GA, WebLogic or any zipped deployment
	public static String APPS_JAR_DIRECTORY;

	public static boolean DEV_MODE = false;

	// If true, determine everything accessible to a user based on the skyve metadata and block access to anything else
	public static boolean ACCESS_CONTROL = true;
	
	// If it is null, then the login infrastructure will prompt for the customer name.
	// If it is set, the customer will be set to that value always.
	// This property is also used for single sign on purposes.
	public static String CUSTOMER = null;

	// eg https://www.bizhub.com.au
	public static String SERVER_URL = null;
	// eg /bizhub/web
	public static String SKYVE_CONTEXT = null;
	// eg /init.biz
	public static String HOME_URI = null;

	// This is the path on the server file system of the web context root
	public static String SKYVE_CONTEXT_REAL_PATH = null;

	// This is the path on the server file system of the properties file
	public static String PROPERTIES_FILE_PATH = null;
	
	// Implementations of Key SKYVE classes
	public static String SKYVE_REPOSITORY_CLASS = null;
	public static String SKYVE_PERSISTENCE_CLASS = null;
	public static String SKYVE_DYNAMIC_PERSISTENCE_CLASS = null;
	public static String SKYVE_CONTENT_MANAGER_CLASS = null;
	public static String SKYVE_NUMBER_GENERATOR_CLASS = null;
	public static String SKYVE_CUSTOMISATIONS_CLASS = null;

	// The directory used for temp files for file uploads etc
	public static final String TEMP_DIRECTORY = System.getProperty("java.io.tmpdir");

	public static boolean USING_JPA = false;

	// For caches
	// Cache folder - defaults to <content.directory>/SKYVE_CACHE/
	// Skyve will create this folder at startup but if defined it must exist at startup.
	public static String CACHE_DIRECTORY = null;
	public static ConversationCacheConfig CONVERSATION_CACHE = null;
	public static CSRFTokenCacheConfig CSRF_TOKEN_CACHE = null;
	public static List<HibernateCacheConfig> HIBERNATE_CACHES = new ArrayList<>();
	public static boolean HIBERNATE_FAIL_ON_MISSING_CACHE = false;
	public static List<CacheConfig<? extends Serializable, ? extends Serializable>> APP_CACHES = new ArrayList<>();

	// For database
	public static Map<String, DataStore> DATA_STORES = new TreeMap<>();
	public static DataStore DATA_STORE = null;
	public static boolean DDL_SYNC = true;
	public static String CATALOG = null;
	public static String SCHEMA = null;
	
	// For E-Mail
	public static String SMTP = null;
	public static int SMTP_PORT = 0;
	public static String SMTP_UID = null;
	public static String SMTP_PWD = null;
	// Extra java mail properties
	public static Map<String, String> SMTP_PROPERTIES = null;
	// Extra java mail headers
	public static Map<String, String> SMTP_HEADERS = null;
	public static String SMTP_SENDER = null;
	// used to intercept all email and send to this test email account
	public static String SMTP_TEST_RECIPIENT = null;
	// used to switch whether to send an email or not - false to actually send the email
	public static boolean SMTP_TEST_BOGUS_SEND = false;

	// Map Keys
	public static enum MapType {
		gmap, leaflet;
	}
	public static MapType MAP_TYPE = MapType.leaflet;
	// the layers to display on the map backdrop
	public static String MAP_LAYERS = null;
	// opening a new empty map will centre here
	public static String MAP_CENTRE = null;
	// opening a new empty map will apply this zoom level
	public static int MAP_ZOOM = 1;

	// API Keys etc
	public static String GOOGLE_MAPS_V3_API_KEY = null;
	public static String GOOGLE_RECAPTCHA_SITE_KEY = null;
	public static String CKEDITOR_CONFIG_FILE_URL = "";

	// null = prod, could be dev, test, uat or another arbitrary environment
	public static String ENVIRONMENT_IDENTIFIER = null;
	
	// email address for error.jsp
	public static String SUPPORT_EMAIL_ADDRESS = null;

	// Should scheduled jobs be manipulated by the database.
	public static boolean JOB_SCHEDULER = true;

	// Password hashing algorithm - usually bcrypt, pbkdf2, scrypt. MD5 and SHA1 are unsalted and obsolete.
	public static String PASSWORD_HASHING_ALGORITHM = "bcrypt";
	// Number of days until a password change is required - Use null to indicate no password aging
	public static int PASSWORD_EXPIRY_IN_DAYS = 0;
	// Number of previous passwords to check for duplicates - Use null to indicate no password history
	public static int PASSWORD_HISTORY_RETENTION = 0;
	// Number of sign in attempts until the user account is locked - Use null to indicate no account lockout
	public static int ACCOUNT_LOCKOUT_THRESHOLD = 3;
	// Number of seconds per failed sign in attempt to lock the account for - Only relevant if account lockout is in use.
	public static int ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS = 10;
	// How long to remember a login on a device (if checked)
	public static int REMEMBER_ME_TOKEN_TIMEOUT_HOURS = 336; // 336hrs = 14 days
	// Enables new users to register for an account when true, requires email
	public static boolean ACCOUNT_ALLOW_SELF_REGISTRATION = false;
	// google auth client id
	public static String AUTHENTICATION_GOOGLE_CLIENT_ID = null;
	// google auth secret
	public static String AUTHENTICATION_GOOGLE_SECRET = null;
	// facebook auth client id
	public static String AUTHENTICATION_FACEBOOK_CLIENT_ID = null;
	// facebook auth secret
	public static String AUTHENTICATION_FACEBOOK_SECRET = null;
	// github auth client id
	public static String AUTHENTICATION_GITHUB_CLIENT_ID = null;
	// github auth secret
	public static String AUTHENTICATION_GITHUB_SECRET = null;

	// azure AD auth client id
	public static String AUTHENTICATION_AZUREAD_CLIENT_ID = null;
	// azure AD auth tenant id
	public static String AUTHENTICATION_AZUREAD_TENANT_ID = null;
	// azure AD auth secret
	public static String AUTHENTICATION_AZUREAD_SECRET = null;

	// The Login URI to forward to
	public static String AUTHENTICATION_LOGIN_URI = "/login";
	// The Logged Out URI to forward to
	public static String AUTHENTICATION_LOGGED_OUT_URI = "/loggedOut";

	// Show setup screen on sign-in for DevOps users
	public static boolean SHOW_SETUP = false;
	
	// Enable/Disable "/health" endpoint to show health status JSON
	public static boolean HEALTH_CHECK = true;
	// Number of seconds to cache the health results to alleviate denial of service
	public static int HEALTH_CACHE_TIME_IN_SECONDS = 60; // 1 min
	
	// These 3 are used to create a user with all roles for the customer assigned, if the user does not already exist
	public static String BOOTSTRAP_CUSTOMER = null;
	public static String BOOTSTRAP_USER = null;
	public static String BOOTSTRAP_EMAIL = null;
	public static String BOOTSTRAP_PASSWORD = null;
	
	public static boolean PRIMEFLEX = false;
	
	public static Set<String> TWO_FACTOR_AUTH_CUSTOMERS = null;
	
	// for skyve script
	/**
	 * Absolute path on the filesystem to the source directory where modules live.
	 * E.g. c:/workspace/project/src/main/java
	 */
	public static String MODULE_DIRECTORY = null;

	private static String absoluteBasePath;

	public static String getAbsoluteBasePath() {
		if (absoluteBasePath == null) {
			if (APPS_JAR_DIRECTORY != null) {
				absoluteBasePath = APPS_JAR_DIRECTORY;
			} else {
				URL url = Thread.currentThread().getContextClassLoader().getResource("schemas/common.xsd");
				if (url == null) {
					UtilImpl.LOGGER.severe("Cannot determine absolute base path. Where is schemas/common.xsd?");
					ClassLoader cl = Thread.currentThread().getContextClassLoader();
					if (cl instanceof URLClassLoader) {
						UtilImpl.LOGGER.severe("The context classloader paths are:-");
						for (URL entry : ((URLClassLoader) cl).getURLs()) {
							UtilImpl.LOGGER.severe(entry.getFile());
						}
					} else {
						UtilImpl.LOGGER.severe("Cannot determine the context classloader paths...");
					}
				} else {
					absoluteBasePath = url.getPath();
					absoluteBasePath = absoluteBasePath.substring(0, absoluteBasePath.length() - 18); // remove schemas/common.xsd
					absoluteBasePath = absoluteBasePath.replace('\\', '/');
				}
			}
		}

		return absoluteBasePath;
	}

	@SuppressWarnings("unchecked")
	public static Map<String, Object> readJSONConfig(InputStream inputStream) throws Exception {
		String json = null;
		try (Scanner scanner = new Scanner(inputStream)) {
			json = scanner.useDelimiter("\\Z").next();
		}
		
		// minify the file to remove any comments
		json = Minifier.minify(json);

		return (Map<String, Object>) JSON.unmarshall(null, json);
	}

	@SuppressWarnings("unchecked")
	public static final <T extends Serializable> T cloneBySerialization(T object) {
		return (T) SerializationHelper.clone(object);
		// try {
		// ByteArrayOutputStream baos = new ByteArrayOutputStream();
		// new ObjectOutputStream(baos).writeObject(object);
		// ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray()));
		// return (T) ois.readObject();
		// }
		// catch (Exception e) {
		// throw new IllegalArgumentException(e);
		// }		
	}

	public static final <T extends Serializable> T cloneToTransientBySerialization(T object)
	throws Exception {
		if (object instanceof List<?>) {
			for (Object element : (List<?>) object) {
				if (element instanceof AbstractPersistentBean) {
					populateFully((AbstractPersistentBean) object);
				}
			}
		} else if (object instanceof AbstractPersistentBean) {
			populateFully((AbstractPersistentBean) object);
		}

		T result = cloneBySerialization(object);
		setTransient(result);

		return result;
	}

	/**
	 * Recurse the bean ensuring that everything is touched and loaded from the database.
	 * 
	 * @param bean The bean to load.
	 */
	public static void populateFully(final Bean bean) {
		User user = CORE.getUser();
		Customer customer = user.getCustomer();

		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());

		// Ensure that everything is loaded
		new BeanVisitor(false, true, false) {
			@Override
			protected boolean accept(String binding,
					Document documentAccepted,
					Document owningDocument,
					Relation owningRelation,
					Bean beanAccepted) {
				// do nothing - just visiting loads the instance from the database
				return true;
			}
		}.visit(document, bean, customer);
	}

	/**
	 * Recurse a bean to determine if anything has changed
	 */
	private static class ChangedBeanVisitor extends BeanVisitor {
		private boolean changed = false;

		private ChangedBeanVisitor() {
			// Check inverses for the cascade attribute
			super(false, true, false);
		}

		@Override
		protected boolean accept(String binding,
									Document documentAccepted,
									Document owningDocument,
									Relation owningRelation,
									Bean beanAccepted) {
			// Process an inverse if the inverse is specified as cascading.
			if ((owningRelation instanceof Inverse) && 
					(! Boolean.TRUE.equals(((Inverse) owningRelation).getCascade()))) {
				return false;
			}

			if (beanAccepted.isChanged()) {
				changed = true;
				if (UtilImpl.DIRTY_TRACE)
					UtilImpl.LOGGER.info(
							"UtilImpl.hasChanged(): Bean " + beanAccepted.toString() + " with binding " + binding + " is DIRTY");
				return false;
			}
			return true;
		}

		boolean isChanged() {
			return changed;
		}
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
		User user = CORE.getUser();
		Customer customer = user.getCustomer();

		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());

		ChangedBeanVisitor cbv = new ChangedBeanVisitor();
		cbv.visit(document, bean, customer);
		return cbv.isChanged();
	}

	/**
	 * Utility method that tries to properly initialise the persistence layer proxies used by lazy loading.
	 * 
	 * @param <T>
	 * @param possibleProxy The possible proxy
	 * @return the resolved proxy or possibleProxy
	 */
	@SuppressWarnings("unchecked")
	public static <T> T deproxy(T possibleProxy) throws ClassCastException {
		if (possibleProxy instanceof HibernateProxy) {
			return (T) ((HibernateProxy) possibleProxy).getHibernateLazyInitializer().getImplementation();
		}

		return possibleProxy;
	}

	public static void setTransient(Object object) throws Exception {
		if (object instanceof List<?>) {
			List<?> list = (List<?>) object;
			for (Object element : list) {
				setTransient(element);
			}
		} else if (object instanceof AbstractPersistentBean) {
			AbstractPersistentBean bean = (AbstractPersistentBean) object;
			bean.setBizId(UUID.randomUUID().toString());
			bean.setBizLock(null);
			bean.setBizVersion(null);

			// set references transient if applicable
			Customer customer = AbstractPersistence.get().getUser().getCustomer();
			Module module = customer.getModule(bean.getBizModule());
			Document document = module.getDocument(customer, bean.getBizDocument());

			for (String referenceName : document.getReferenceNames()) {
				Reference reference = document.getReferenceByName(referenceName);
				if (reference.isPersistent()) {
					if (reference instanceof AssociationImpl) {
						AssociationImpl association = (AssociationImpl) reference;
						if (association.getType() != AssociationType.aggregation) {
							setTransient(BindUtil.get(bean, referenceName));
						}
					} else if (reference instanceof Collection) {
						Collection collection = (Collection) reference;
						if (collection.getType() != CollectionType.aggregation) {
							// set each element of the collection transient
							setTransient(BindUtil.get(bean, referenceName));
						}
					}
				}
			}
		}
	}

	// set the data group of a bean and all its children
	public static void setDataGroup(Object object, String bizDataGroupId) throws Exception {
		if (object instanceof List<?>) {
			List<?> list = (List<?>) object;
			for (Object element : list) {
				setDataGroup(element, bizDataGroupId);
			}
		} else if (object instanceof AbstractPersistentBean) {
			AbstractPersistentBean bean = (AbstractPersistentBean) object;
			bean.setBizDataGroupId(bizDataGroupId);

			// set the bizDatagroup of references if applicable
			Customer customer = AbstractPersistence.get().getUser().getCustomer();
			Module module = customer.getModule(bean.getBizModule());
			Document document = module.getDocument(customer, bean.getBizDocument());

			for (String referenceName : document.getReferenceNames()) {
				Reference reference = document.getReferenceByName(referenceName);
				if (reference.isPersistent()) {
					if (reference instanceof AssociationImpl) {
						AssociationImpl association = (AssociationImpl) reference;
						if (association.getType() != AssociationType.aggregation) {
							setDataGroup(BindUtil.get(bean, referenceName), bizDataGroupId);
						}
					} else if (reference instanceof Collection) {
						Collection collection = (Collection) reference;
						if (collection.getType() != CollectionType.aggregation) {
							// set each element of the collection transient
							setDataGroup(BindUtil.get(bean, referenceName), bizDataGroupId);
						}
					}
				}
			}
		}
	}

	/**
	 * Process and transform empty Strings.
	 * 
	 * @param value The String.
	 * @return null, if the trimmed value is empty, otherwise value.
	 */
	public static String processStringValue(String value) {
		String result = value;

		if (result != null) {
			result = result.trim();
			if (result.isEmpty()) {
				result = null;
			}
		}

		return result;
	}
	
	/**
	 * Change unicode text to ascii.
	 */
	public static String unidecode(String value) {
		return Junidecode.unidecode(value);
	}

	/**
	 * Checks that the module directory:
	 * <ul>
	 * <li>ends with a trailing slash
	 * <li>ends with modules
	 * </ul>
	 * 
	 * @param path The supplied content path
	 * @return The updated path if any slashes or <code>/modules</code> need to be added
	 */
	public static String cleanupModuleDirectory(final String path) {
		if (path != null && path.length() > 0) {
			String updatedPath = path;

			// strip the trailing slash if any
			if (path.endsWith("/") || path.endsWith("\\")) {
				updatedPath = path.substring(0, path.length() - 1);
			}

			if (!updatedPath.endsWith("modules")) {
				updatedPath = updatedPath + "/modules/";
			}

			if (!updatedPath.endsWith("/") && !updatedPath.endsWith("\\")) {
				updatedPath = updatedPath + "/";
			}

			return updatedPath;
		}

		return path;
	}
}
