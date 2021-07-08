package org.skyve.impl.web;

import java.io.File;
import java.io.FileInputStream;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.UUID;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.faces.FacesException;
import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.SessionCookieConfig;
import javax.websocket.server.ServerContainer;
import javax.websocket.server.ServerEndpointConfig;

import org.omnifaces.cdi.push.Socket;
import org.omnifaces.cdi.push.SocketEndpoint;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.cache.CSRFTokenCacheConfig;
import org.skyve.cache.CacheExpiryPolicy;
import org.skyve.cache.Caching;
import org.skyve.cache.ConversationCacheConfig;
import org.skyve.cache.EHCacheConfig;
import org.skyve.cache.HibernateCacheConfig;
import org.skyve.cache.JCacheConfig;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.LocalSecureRepository;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.HibernateContentPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.UtilImpl.MapType;
import org.skyve.impl.util.VariableExpander;
import org.skyve.impl.web.faces.SkyveSocketEndpoint;
import org.skyve.persistence.DataStore;
import org.skyve.util.Util;

public class SkyveContextListener implements ServletContextListener {
	@Override
	public void contextInitialized(ServletContextEvent evt) {
		ServletContext ctx = evt.getServletContext();

		populateUtilImpl(ctx);

		final Caching caching = EXT.getCaching();
		caching.startup();
		try {
			EXT.getAddInManager().startup();
			
			// ensure that the schema is created before trying to init the job scheduler
			AbstractPersistence p = null;
			try {
				p = (AbstractPersistence) CORE.getPersistence(); // syncs the schema if required
				p.begin();
				// If this is not prod and we have a bootstrap stanza
				if ((UtilImpl.ENVIRONMENT_IDENTIFIER != null) && (UtilImpl.BOOTSTRAP_CUSTOMER != null)) {
					SuperUser u = new SuperUser();
					u.setCustomerName(UtilImpl.BOOTSTRAP_CUSTOMER);
					u.setContactName(UtilImpl.BOOTSTRAP_USER);
					u.setName(UtilImpl.BOOTSTRAP_USER);
					u.setPasswordHash(EXT.hashPassword(UtilImpl.BOOTSTRAP_PASSWORD));
					p.setUser(u);
	
					EXT.bootstrap(p);
				}
			}
			catch (Throwable t) {
				if (p != null) {
					p.rollback();
				}
				throw new IllegalStateException("Cannot initialise either the data schema or the bootstrap user.", t);
			}
			finally {
				if (p != null) {
					p.commit(true);
				}
			}
			
			EXT.getReporting().startup();
			
			EXT.getJobScheduler().startup();
			
			// Set up the session cookie
			SessionCookieConfig scc = ctx.getSessionCookieConfig();
			scc.setHttpOnly(true);
			scc.setSecure(Util.isSecureUrl());
						
			// Start a websocket end point
			// NB From org.omnifaces.cdi.push.Socket.registerEndpointIfNecessary() called by org.omnifaces.ApplicationListener
			try {
				ServerContainer container = (ServerContainer) ctx.getAttribute(ServerContainer.class.getName());
				ServerEndpointConfig config = ServerEndpointConfig.Builder.create(SkyveSocketEndpoint.class, SocketEndpoint.URI_TEMPLATE).build();
				container.addEndpoint(config);
				// to stop the <o:socket/> from moaning that the endpoint is not configured
				ctx.setAttribute(Socket.class.getName(), Boolean.TRUE);
			}
			catch (Exception e) {
				throw new FacesException(e);
			}

		}
		// in case of error, close the caches to relinquish resources and file locks
		catch (Throwable t) {
			caching.shutdown();
			throw t;
		}
	}
	
	@SuppressWarnings("unchecked")
	private static void populateUtilImpl(ServletContext ctx) {
		UtilImpl.SKYVE_CONTEXT_REAL_PATH = ctx.getRealPath("/");
		
		// This can be set in web.xml or as a command line -D parameter, but if not set, 
		// it defaults to <app-name>.properties where <app-name>
		// is derived from the ear file - ie <app-name>.ear -> <app-name>.properties in the same directory.
		// Some app server's dont like a properties file in their deployment directories or some people 
		// wish to deploy a zipped archive.
		UtilImpl.PROPERTIES_FILE_PATH = System.getProperty("PROPERTIES_FILE_PATH");
		if (UtilImpl.PROPERTIES_FILE_PATH == null) {
			UtilImpl.PROPERTIES_FILE_PATH = ctx.getInitParameter("PROPERTIES_FILE_PATH");
		}
		String archiveName = null;
		if (UtilImpl.PROPERTIES_FILE_PATH == null) {
			UtilImpl.LOGGER.info("SKYVE CONTEXT REAL PATH = " + UtilImpl.SKYVE_CONTEXT_REAL_PATH);
			File archive = new File(UtilImpl.SKYVE_CONTEXT_REAL_PATH);
			if (archive.getParentFile().getName().endsWith("ear")) {
				archive = archive.getParentFile();
				archiveName = archive.getName();
			} 
			else {
				archiveName = archive.getName();
			}
			archiveName = archiveName.substring(0, archiveName.length() - 4);
			UtilImpl.PROPERTIES_FILE_PATH = archive.getParent() + '/' + archiveName + ".json";
		}
		UtilImpl.ARCHIVE_NAME = archiveName;

		Map<String, Object> properties = null;
		try (FileInputStream fis = new FileInputStream(UtilImpl.PROPERTIES_FILE_PATH)) {
			final VariableExpander variableExpander = new VariableExpander();
			properties = variableExpander.expand(UtilImpl.readJSONConfig(fis), System.getenv());
		}
		catch (Exception e) {
			throw new IllegalStateException("Cannot open or read " + UtilImpl.PROPERTIES_FILE_PATH, e);
		}
		UtilImpl.CONFIGURATION = properties;
		
		// Content directory
		Map<String, Object> content = getObject(null, "content", properties, true);
		UtilImpl.CONTENT_DIRECTORY = getString("content", "directory", content, true);
		// clean up the content directory path
		UtilImpl.CONTENT_DIRECTORY = cleanupDirectory(UtilImpl.CONTENT_DIRECTORY);
		testWritableDirectory("content.directory", UtilImpl.CONTENT_DIRECTORY);

		// Find any overrides
		Map<String, Object> overrides = null;
		File overridesFile = new File(UtilImpl.CONTENT_DIRECTORY, archiveName + ".json");
		if (overridesFile.exists()) {
			try (FileInputStream fis = new FileInputStream(overridesFile)) {
				final VariableExpander variableExpander = new VariableExpander();
				overrides = variableExpander.expand(UtilImpl.readJSONConfig(fis), System.getenv());
			}
			catch (Exception e) {
				throw new IllegalStateException("Cannot open or read " + overridesFile.getAbsolutePath(), e);
			}
		}
		else {
			overrides = new HashMap<>();
		}
		UtilImpl.OVERRIDE_CONFIGURATION = overrides;

		// Apply overrides to configuration
		merge(overrides, properties);
		
		// Trace settings
		Map<String, Object> trace = getObject(null, "trace", properties, true);
		UtilImpl.XML_TRACE = getBoolean("trace", "xml", trace);
		UtilImpl.HTTP_TRACE = getBoolean("trace", "http", trace);
		UtilImpl.COMMAND_TRACE = getBoolean("trace", "command", trace);
		UtilImpl.FACES_TRACE = getBoolean("trace", "faces", trace);
		UtilImpl.QUERY_TRACE = getBoolean("trace", "query", trace);
		UtilImpl.SQL_TRACE = getBoolean("trace", "sql", trace);
		UtilImpl.CONTENT_TRACE = getBoolean("trace", "content", trace);
		UtilImpl.SECURITY_TRACE = getBoolean("trace", "security", trace);
		UtilImpl.BIZLET_TRACE = getBoolean("trace", "bizlet", trace);
		UtilImpl.DIRTY_TRACE = getBoolean("trace", "dirty", trace);

		// Content settings
		UtilImpl.CONTENT_GC_CRON = getString("content", "gcCron", content, true);
		UtilImpl.CONTENT_SERVER_ARGS = getString("content", "serverArgs", content, false);
		UtilImpl.CONTENT_FILE_STORAGE = getBoolean("content", "fileStorage", content);

		// Uploads settings
		Map<String, Object> uploads = getObject(null, "uploads", properties, false);
		if (uploads != null) {
			// File
			Map<String, Object> values = getObject("uploads", "file", uploads, false);
			if (values != null) {
				String whitelistRegex = Util.processStringValue(getString("uploads.file", "whitelistRegex", values, false));
				if (whitelistRegex != null) {
					try {
						Pattern.compile(whitelistRegex);
					}
					catch (PatternSyntaxException e) {
						throw new IllegalStateException("uploads.file.whitelistRegex is not a valid regex pattern", e);
					}
					UtilImpl.UPLOADS_FILE_WHITELIST_REGEX = whitelistRegex;
				}
				Number maximumSizeMB = getNumber("uploads.file", "maximumSizeMB", values, false);
				if (maximumSizeMB != null) {
					UtilImpl.UPLOADS_FILE_MAXIMUM_SIZE_IN_MB = maximumSizeMB.intValue();
				}
			}

			// Content
			values = getObject("uploads", "content", uploads, false);
			if (values != null) {
				String whitelistRegex = Util.processStringValue(getString("uploads.content", "whitelistRegex", values, false));
				if (whitelistRegex != null) {
					try {
						Pattern.compile(whitelistRegex);
					}
					catch (PatternSyntaxException e) {
						throw new IllegalStateException("uploads.content.whitelistRegex is not a valid regex pattern", e);
					}
					UtilImpl.UPLOADS_CONTENT_WHITELIST_REGEX = whitelistRegex;
				}
				Number maximumSizeMB = getNumber("uploads.content", "maximumSizeMB", values, false);
				if (maximumSizeMB != null) {
					UtilImpl.UPLOADS_CONTENT_MAXIMUM_SIZE_IN_MB = maximumSizeMB.intValue();
				}
			}

			// Image
			values = getObject("uploads", "image", uploads, false);
			if (values != null) {
				String whitelistRegex = Util.processStringValue(getString("uploads.image", "whitelistRegex", values, false));
				if (whitelistRegex != null) {
					try {
						Pattern.compile(whitelistRegex);
					}
					catch (PatternSyntaxException e) {
						throw new IllegalStateException("uploads.image.whitelistRegex is not a valid regex pattern", e);
					}
					UtilImpl.UPLOADS_IMAGE_WHITELIST_REGEX = whitelistRegex;
				}
				Number maximumSizeMB = getNumber("uploads.image", "maximumSizeMB", values, false);
				if (maximumSizeMB != null) {
					UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB = maximumSizeMB.intValue();
				}
			}

			// BizPort
			values = getObject("uploads", "bizport", uploads, false);
			if (values != null) {
				String whitelistRegex = Util.processStringValue(getString("uploads.bizport", "whitelistRegex", values, false));
				if (whitelistRegex != null) {
					try {
						Pattern.compile(whitelistRegex);
					}
					catch (PatternSyntaxException e) {
						throw new IllegalStateException("uploads.bizport.whitelistRegex is not a valid regex pattern", e);
					}
					UtilImpl.UPLOADS_BIZPORT_WHITELIST_REGEX = whitelistRegex;
				}
				Number maximumSizeMB = getNumber("uploads.bizport", "maximumSizeMB", values, false);
				if (maximumSizeMB != null) {
					UtilImpl.UPLOADS_BIZPORT_MAXIMUM_SIZE_IN_MB = maximumSizeMB.intValue();
				}
			}
		}
		
		// Add-ins settings
		Map<String, Object> addins = getObject(null, "addins", properties, false);
		if (addins != null) {
			UtilImpl.ADDINS_DIRECTORY = getString("addins", "directory", addins, false);
			if (UtilImpl.ADDINS_DIRECTORY != null) {
				// clean up the add-ins directory path
				UtilImpl.ADDINS_DIRECTORY = cleanupDirectory(UtilImpl.ADDINS_DIRECTORY);
				testWritableDirectory("addins.directory", UtilImpl.ADDINS_DIRECTORY);
			}
		}
		
		// Thumb nail settings
		Map<String, Object> thumbnail = getObject(null, "thumbnail", properties, false);
		if (thumbnail != null) {
			UtilImpl.THUMBNAIL_CONCURRENT_THREADS = getInt("thumbnail", "concurrentThreads", thumbnail);
			UtilImpl.THUMBNAIL_SUBSAMPLING_MINIMUM_TARGET_SIZE = getInt("thumbnail", "subsamplingMinimumTargetSize", thumbnail);
			UtilImpl.THUMBNAIL_FILE_STORAGE = getBoolean("thumbnail", "fileStorage", thumbnail);
			UtilImpl.THUMBNAIL_DIRECTORY = getString("thumbnail", "directory", thumbnail, false);
			if (UtilImpl.THUMBNAIL_DIRECTORY != null) {
				// clean up the thumb nail directory path
				UtilImpl.THUMBNAIL_DIRECTORY = cleanupDirectory(UtilImpl.THUMBNAIL_DIRECTORY);
				testWritableDirectory("thumbnail.directory", UtilImpl.THUMBNAIL_DIRECTORY);
			}
		}

		// The following URLs cannot be set from the web context (could be many URLs to reach the web server after all).
		// There are container specific ways but we don't want that.
		Map<String, Object> url = getObject(null, "url", properties, true);
		UtilImpl.SERVER_URL = getString("url", "server", url, true);
		UtilImpl.SKYVE_CONTEXT = getString("url", "context", url, true);
		UtilImpl.HOME_URI = getString("url", "home", url, true);
		
		Map<String, Object> state = getObject(null, "state", properties, true);
		Map<String, Object> conversations = getObject("state", "conversations", state, true);
		UtilImpl.CONVERSATION_CACHE = new ConversationCacheConfig(getInt("state.conversations", "heapSizeEntries", conversations),
																	getInt("state.conversations", "offHeapSizeMB", conversations),
																	getInt("state.conversations", "diskSizeGB", conversations) * 1024,
																	getInt("state.conversations", "expiryTimeMinutes", conversations));
		Map<String, Object> tokens = getObject("state", "csrfTokens", state, true);
		UtilImpl.CSRF_TOKEN_CACHE = new CSRFTokenCacheConfig(getInt("state.csrfTokens", "heapSizeEntries", tokens),
																	getInt("state.csrfTokens", "offHeapSizeMB", tokens),
																	getInt("state.csrfTokens", "diskSizeGB", tokens) * 1024,
																	getInt("state.csrfTokens", "expiryTimeMinutes", tokens));
		UtilImpl.STATE_EVICT_CRON = getString("state", "evictCron", state, false);

		Map<String, Object> caches = getObject(null, "caches", properties, false);
		if (caches != null) {
			// for each cache defined
			for (String cacheName : caches.keySet()) {
				Map<String, Object> cache = getObject("caches", cacheName, caches, true);
				String prefix = String.format("caches.%s", cacheName);
				String type = getString(prefix, "type", cache, true);
				
				int heapSizeEntries = getInt(prefix, "heapSizeEntries", cache);
				Number offHeapSizeInMB = getNumber(prefix, "offHeapSizeMB", cache, false);
				Number diskSizeInGB = getNumber(prefix, "diskSizeGB", cache, false);
				Boolean persistent = (Boolean) get(prefix, "persistent", cache, false);
				String expiryPolicyString = getString(prefix, "expiryPolicy", cache, false);
				Number expiryInMinutes = getNumber(prefix, "expiryTimeMinutes", cache, false);
				ClassLoader ctxClassLoader = Thread.currentThread().getContextClassLoader();
				String keyClassName = getString(prefix, "keyClass", cache, true);
				String valueClassName = getString(prefix, "valueClass", cache, true);
				Class<? extends Serializable> keyClass = null;
				Class<? extends Serializable> valueClass = null;
				try {
					keyClass = (Class<? extends Serializable>) ctxClassLoader.loadClass(keyClassName);
				}
				catch (Exception e) {
					throw new IllegalStateException("Could not load key class " + keyClassName, e);
				}
				try {
					valueClass = (Class<? extends Serializable>) ctxClassLoader.loadClass(valueClassName);
				}
				catch (Exception e) {
					throw new IllegalStateException("Could not load value class " + keyClassName, e);
				}
				
				if ("jcache".equals(type)) {
					UtilImpl.APP_CACHES.add(new JCacheConfig<>(cacheName,
																heapSizeEntries,
																(offHeapSizeInMB == null) ? 0L : offHeapSizeInMB.longValue(),
																(expiryPolicyString == null) ? CacheExpiryPolicy.eternal : CacheExpiryPolicy.valueOf(expiryPolicyString),
																(expiryInMinutes == null) ? 0L : expiryInMinutes.longValue(),
																keyClass,
																valueClass));
				}
				else if ("ehcache".equals(type)) {
					UtilImpl.APP_CACHES.add(new EHCacheConfig<>(cacheName,
																	heapSizeEntries,
																	(offHeapSizeInMB == null) ? 0L : offHeapSizeInMB.longValue(),
																	(expiryPolicyString == null) ? CacheExpiryPolicy.eternal : CacheExpiryPolicy.valueOf(expiryPolicyString),
																	(expiryInMinutes == null) ? 0L : expiryInMinutes.longValue(),
																	keyClass,
																	valueClass,
																	(diskSizeInGB == null) ? 0L : diskSizeInGB.longValue() * 1024L,
																	Boolean.TRUE.equals(persistent)));
				}
				else {
					throw new IllegalStateException("Cache type " + type + " is not a known type");
				}
			}
		}
		
		Map<String, Object> dataStores = getObject(null, "dataStores", properties, true);
		// for each datastore defined
		for (String dataStoreName : dataStores.keySet()) {
			Map<String, Object> dataStore = getObject("dataStores", dataStoreName, dataStores, true);
			String prefix = String.format("dataStores.%s", dataStoreName);
			String dialect = getString(prefix, "dialect", dataStore, true);
			
			String jndi = getString(prefix, "jndi", dataStore, false);
			if (jndi == null) {
				UtilImpl.DATA_STORES.put(dataStoreName, 
											new DataStore(getString(prefix, "driver", dataStore, true), 
															getString(prefix, "url", dataStore, true), 
															getString(prefix, "user", dataStore, false),
															getString(prefix, "password", dataStore, false), 
															dialect,
															getInt(prefix, "oltpConnectionTimeoutInSeconds", dataStore),
															getInt(prefix, "asyncConnectionTimeoutInSeconds", dataStore)));
			}
			else {
				UtilImpl.DATA_STORES.put(dataStoreName, new DataStore(jndi,
																		dialect,
																		getInt(prefix, "oltpConnectionTimeoutInSeconds", dataStore),
																		getInt(prefix, "asyncConnectionTimeoutInSeconds", dataStore)));
			}
		}
		
		Map<String, Object> hibernate = getObject(null, "hibernate", properties, true);
		UtilImpl.DATA_STORE = UtilImpl.DATA_STORES.get(getString("hibernate", "dataStore", hibernate, true));
		if (UtilImpl.DATA_STORE == null) {
			throw new IllegalStateException("hibernate.dataStore " + UtilImpl.DATA_STORE + " is not defined in dataStores");
		}
		UtilImpl.DDL_SYNC = getBoolean("hibernate", "ddlSync", hibernate);
		UtilImpl.CATALOG = getString("hibernate", "catalog", hibernate, false);
		UtilImpl.SCHEMA = getString("hibernate", "schema", hibernate, false);
		UtilImpl.PRETTY_SQL_OUTPUT = getBoolean("hibernate", "prettySql", hibernate);

		Map<String, Object> hibernateCaches = getObject("hibernate", "caches", hibernate, false);
		if (hibernateCaches != null) {
			// for each cache defined
			for (String cacheName : hibernateCaches.keySet()) {
				Map<String, Object> cache = getObject("hibernate.caches", cacheName, hibernateCaches, true);
				String prefix = String.format("hibernate.caches.%s", cacheName);
				int heapSizeEntries = getInt(prefix, "heapSizeEntries", cache);
				Number offHeapSizeInMB = getNumber(prefix, "offHeapSizeMB", cache, false);
				String expiryPolicyString = getString(prefix, "expiryPolicy", cache, false);
				Number expiryInMinutes = getNumber(prefix, "expiryTimeMinutes", cache, false);
				UtilImpl.HIBERNATE_CACHES.add(new HibernateCacheConfig(cacheName,
																		heapSizeEntries,
																		(offHeapSizeInMB == null) ? 0L : offHeapSizeInMB.longValue(),
																		(expiryPolicyString == null) ? CacheExpiryPolicy.eternal : CacheExpiryPolicy.valueOf(expiryPolicyString),
																		(expiryInMinutes == null) ? 0L : expiryInMinutes.longValue()));
			}
		}
		// Ensure that deployment fails if a cache is missing 
		// (but for testing and other stand alone applications, it'll create them on the fly)
		UtilImpl.HIBERNATE_FAIL_ON_MISSING_CACHE = true;
		
		Map<String, Object> factories = getObject(null, "factories", properties, true);

		// NB Need the repository set before setting persistence
		UtilImpl.SKYVE_REPOSITORY_CLASS = getString("factories", "repositoryClass", factories, false);
		if (AbstractRepository.get() == null) {
			if (UtilImpl.SKYVE_REPOSITORY_CLASS == null) {
				UtilImpl.LOGGER.info("SET SKYVE REPOSITORY CLASS TO DEFAULT");
				AbstractRepository.set(new LocalSecureRepository());
			}
			else {
				UtilImpl.LOGGER.info("SET SKYVE REPOSITORY CLASS TO " + UtilImpl.SKYVE_REPOSITORY_CLASS);
				try {
					AbstractRepository.set((AbstractRepository) Thread.currentThread().getContextClassLoader().loadClass(UtilImpl.SKYVE_REPOSITORY_CLASS).getDeclaredConstructor().newInstance());
				}
				catch (Exception e) {
					throw new IllegalStateException("Could not create factories.repositoryClass " + UtilImpl.SKYVE_REPOSITORY_CLASS, e);
				}
			}
		}

		UtilImpl.SKYVE_PERSISTENCE_CLASS = getString("factories", "persistenceClass", factories, false);
		if (AbstractPersistence.IMPLEMENTATION_CLASS == null) {
			if (UtilImpl.SKYVE_PERSISTENCE_CLASS == null) {
				UtilImpl.LOGGER.info("SET SKYVE PERSISTENCE CLASS TO DEFAULT");
				AbstractPersistence.IMPLEMENTATION_CLASS = HibernateContentPersistence.class;
			}
			else {
				UtilImpl.LOGGER.info("SET SKYVE PERSISTENCE CLASS TO " + UtilImpl.SKYVE_PERSISTENCE_CLASS);
				try {
					AbstractPersistence.IMPLEMENTATION_CLASS = (Class<? extends AbstractPersistence>) Class.forName(UtilImpl.SKYVE_PERSISTENCE_CLASS);
				}
				catch (ClassNotFoundException e) {
					throw new IllegalStateException("Could not find factories.persistenceClass " + UtilImpl.SKYVE_PERSISTENCE_CLASS, e);
				}
			}
		}

		// Can't load the class here as it may not be available to this class loader - it could be in the add-in.
		// So we load it in the startup method.		
		UtilImpl.SKYVE_CONTENT_MANAGER_CLASS = getString("factories", "contentManagerClass", factories, false);

		Map<String, Object> smtp = getObject(null, "smtp", properties, true);
		UtilImpl.SMTP = getString("smtp", "server", smtp, true);
		UtilImpl.SMTP_PORT = getInt("smtp", "port", smtp);
		UtilImpl.SMTP_UID = getString("smtp", "uid", smtp, false);
		UtilImpl.SMTP_PWD = getString("smtp", "pwd", smtp, false);
		Map<String, Object> smtpProperties = getObject("smtp", "properties", smtp, false);
		if (smtpProperties != null) {
			UtilImpl.SMTP_PROPERTIES = new TreeMap<>();
			for (Entry<String, Object> entry : smtpProperties.entrySet()) {
				String key = entry.getKey();
				Object value = entry.getValue();
				if ((key != null) && (value != null)) {
					UtilImpl.SMTP_PROPERTIES.put(key, value.toString());
				}
			}
		}
		UtilImpl.SMTP_SENDER = getString("smtp", "sender", smtp, true);
		UtilImpl.SMTP_TEST_RECIPIENT = getString("smtp", "testRecipient", smtp, false);
		UtilImpl.SMTP_TEST_BOGUS_SEND = getBoolean("smtp", "testBogusSend", smtp);

		Map<String, Object> map = getObject(null, "map", properties, true);
		String value = getString("map", "type", map, true);
		UtilImpl.MAP_TYPE = (value == null) ?  MapType.leaflet : MapType.valueOf(value);
		UtilImpl.MAP_LAYERS = getString("map", "layers", map, true);
		UtilImpl.MAP_CENTRE = getString("map", "centre", map, false);
		Number zoom = getNumber("map", "zoom", map, false);
		if (zoom != null) {
			UtilImpl.MAP_ZOOM = zoom.intValue();
		}

		Map<String, Object> account = getObject(null, "account", properties, true);
		UtilImpl.PASSWORD_HASHING_ALGORITHM = getString("account", "passwordHashingAlgorithm", account, true);
		Number number = getNumber("account", "passwordExpiryInDays", account, false);
		if (number != null) {
			UtilImpl.PASSWORD_EXPIRY_IN_DAYS = number.intValue();
		}
		number = getNumber("account", "passwordHistoryRetention", account, false);
		if (number != null) {
			UtilImpl.PASSWORD_HISTORY_RETENTION = number.intValue();
		}
		number = getNumber("account", "accountLockoutThreshold", account, false);
		if (number != null) {
			UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD = number.intValue();
		}
		number = getNumber("account", "accountLockoutDurationMultipleInSeconds", account, false);
		if (number != null) {
			UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS = number.intValue();
		}
		value = getString("account", "loginUri", account, false);
		if (value != null) {
			UtilImpl.AUTHENTICATION_LOGIN_URI = value;
		}
		UtilImpl.ACCOUNT_ALLOW_SELF_REGISTRATION = getBoolean("account", "allowUserSelfRegistration", account);
		UtilImpl.AUTHENTICATION_GOOGLE_CLIENT_ID = getString("account", "googleAuthClientId", account, false);
		UtilImpl.AUTHENTICATION_GOOGLE_SECRET = getString("account", "googleAuthSecret", account, false);
		UtilImpl.AUTHENTICATION_FACEBOOK_CLIENT_ID = getString("account", "facebookAuthClientId", account, false);
		UtilImpl.AUTHENTICATION_FACEBOOK_SECRET = getString("account", "facebookAuthSecret", account, false);
		UtilImpl.AUTHENTICATION_GITHUB_CLIENT_ID = getString("account", "githubAuthClientId", account, false);
		UtilImpl.AUTHENTICATION_GITHUB_SECRET = getString("account", "githubAuthSecret", account, false);
		
		Map<String, Object> environment = getObject(null, "environment", properties, true);
		UtilImpl.ENVIRONMENT_IDENTIFIER = getString("environment", "identifier", environment, false);
		UtilImpl.DEV_MODE = getBoolean("environment", "devMode", environment);
		UtilImpl.CUSTOMER = getString("environment", "customer", environment, false);
		UtilImpl.JOB_SCHEDULER = getBoolean("environment", "jobScheduler", environment);
		UtilImpl.APPS_JAR_DIRECTORY = getString("environment", "appsJarDirectory", environment, false);
		UtilImpl.MODULE_DIRECTORY = getString("environment", "moduleDirectory", environment, false);
		if (UtilImpl.MODULE_DIRECTORY != null) {
			// clean up the module directory path
			UtilImpl.MODULE_DIRECTORY = UtilImpl.cleanupModuleDirectory(UtilImpl.MODULE_DIRECTORY);

			File moduleDirectory = new File(UtilImpl.MODULE_DIRECTORY);
			if (! moduleDirectory.exists()) {
				throw new IllegalStateException("environment.moduleDirectory " + UtilImpl.MODULE_DIRECTORY + " does not exist.");
			}
			if (! moduleDirectory.isDirectory()) {
				throw new IllegalStateException("environment.moduleDirectory " + UtilImpl.MODULE_DIRECTORY + " is not a directory.");
			}
		}
		UtilImpl.SUPPORT_EMAIL_ADDRESS = getString("environment", "supportEmailAddress", environment, false);
		UtilImpl.SHOW_SETUP = getBoolean("environment", "showSetup", environment);

		Map<String, Object> api = getObject(null, "api", properties, true);
		UtilImpl.GOOGLE_MAPS_V3_API_KEY = getString("api", "googleMapsV3Key", api, false);
		UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY = getString("api", "googleRecaptchaSiteKey", api, false);
		UtilImpl.CKEDITOR_CONFIG_FILE_URL = getString("api", "ckEditorConfigFileUrl", api, false);
		if (UtilImpl.CKEDITOR_CONFIG_FILE_URL == null) {
			UtilImpl.CKEDITOR_CONFIG_FILE_URL = "";
		}
		
		Map<String, Object> bootstrap = getObject(null, "bootstrap", properties, false);
		if (bootstrap != null) {
			UtilImpl.BOOTSTRAP_CUSTOMER = getString("bootstrap", "customer", bootstrap, true);
			UtilImpl.BOOTSTRAP_USER = getString("bootstrap", "user", bootstrap, true);
			UtilImpl.BOOTSTRAP_EMAIL = getString("bootstrap", "email", bootstrap, false);
			if (UtilImpl.BOOTSTRAP_EMAIL == null) {
				UtilImpl.BOOTSTRAP_EMAIL = "pleaseupdate@test.com";
			}
			UtilImpl.BOOTSTRAP_PASSWORD = getString("bootstrap", "password", bootstrap, true);
		}
		
		String primeFlex = UtilImpl.processStringValue(ctx.getInitParameter("org.skyve.web.faces.PRIMEFLEX"));
		if (primeFlex != null) {
			UtilImpl.PRIMEFLEX = Boolean.parseBoolean(primeFlex);
		}
	}
	
	private static void merge(Map<String, Object> overrides, Map<String, Object> properties) {
		for (String key : overrides.keySet()) {
			Object override = overrides.get(key);
			Object original = properties.get(key);
			if ((original == null) || (override == null)) {
				properties.put(key, override);
			}
			else if ((override instanceof String) && (original instanceof String)) {
				properties.put(key, override);
			}
			else if ((override instanceof Boolean) && (original instanceof Boolean)) {
				properties.put(key, override);
			}
			else if ((override instanceof Number) && (original instanceof Number)) {
				properties.put(key, override);
			}
			else if ((override instanceof Map) && (original instanceof Map)) {
				@SuppressWarnings("unchecked")
				Map<String, Object> overrideMap = (Map<String, Object>) override;
				@SuppressWarnings("unchecked")
				Map<String, Object> originalMap = (Map<String, Object>) original;
				merge(overrideMap, originalMap);
			}
			else {
				throw new IllegalStateException("Cannot apply override " + override + " to " + key);
			}
		}
	}
	
	private static Object get(String prefix, String key, Map<String, Object> properties, boolean required) {
		Object result = properties.get(key);
		if (required && (result == null)) {
			if (prefix != null) {
				throw new IllegalStateException(String.format("Property %s.%s does not exist in the JSON configuration.", prefix, key));
			}
			throw new IllegalStateException(String.format("Property %s does not exist in the JSON configuration.", key));
		}
		return result;
	}

	private static boolean getBoolean(String prefix, String key, Map<String, Object> properties) {
		Boolean result = (Boolean) get(prefix, key, properties, true);
		return result.booleanValue();
	}
	
	private static int getInt(String prefix, String key, Map<String, Object> properties) {
		return getNumber(prefix, key, properties, true).intValue();
	}

	private static Number getNumber(String prefix, String key, Map<String, Object> properties, boolean required) {
		return (Number) get(prefix, key, properties, required);
	}

	private static String getString(String prefix, String key, Map<String, Object> properties, boolean required) {
		return (String) get(prefix, key, properties, required);
	}

	@SuppressWarnings("unchecked")
	private static Map<String, Object> getObject(String prefix, String key, Map<String, Object> properties, boolean required) {
		return (Map<String, Object>) get(prefix, key, properties, required);
	}
	
	@Override
	public void contextDestroyed(ServletContextEvent evt) {
		try {
			try {
				try {
					try {
						EXT.getJobScheduler().shutdown();
					}
					finally {
						EXT.getReporting().shutdown();
					}
				}
				finally {
					// Ensure the caches are destroyed even in the event of other failures first
					// so that resources and file locks are relinquished.
					EXT.getCaching().shutdown();
				}
			}
			finally {
				// Ensure the content manager is destroyed so that resources and files locks are relinquished
				@SuppressWarnings("resource")
				AbstractContentManager cm = (AbstractContentManager) EXT.newContentManager();
				try {
					cm.close();
					cm.shutdown();
				}
				catch (Exception e) {
					UtilImpl.LOGGER.info("Could not close or shutdown of the content manager - this is probably OK although resources may be left hanging or locked");
					e.printStackTrace();
				}
			}
		}
		finally {
			// Ensure the add-in manager is stopped
			EXT.getAddInManager().shutdown();
		}
	}

	/**
	 * Checks that the content directory:
	 * <ul>
	 * <li>ends with a trailing slash
	 * </ul>
	 * 
	 * @param path The supplied content path
	 * @return The updated path if any slashes need to be added
	 */
	static String cleanupDirectory(final String path) {
		if (path != null && path.length() > 0) {
			String updatedPath = path.replace("\\", "/");

			if (!updatedPath.endsWith("/")) {
				updatedPath = updatedPath + "/";
			}

			return updatedPath;
		}

		return path;
	}

	/**
	 * Checks a directory path property value exists, is a directory and is writable.
	 * @param propertyName	The property name to report in exceptions.
	 * @param directoryPath	The property value to test.
	 */
	private static void testWritableDirectory(String propertyName, String directoryPath) {
		File directory = new File(directoryPath);
		
		if (! directory.exists()) {
			throw new IllegalStateException(propertyName + " " + directoryPath + " does not exist.");
		}
		
		if (! directory.isDirectory()) {
			throw new IllegalStateException(propertyName + " " + directoryPath + " is not a directory.");
		}
		
		// Check the directory is writable
		File testFile = new File(directory, "SKYVE_TEST_WRITE_" + UUID.randomUUID().toString());
		try {
			testFile.createNewFile();
		}
		catch (@SuppressWarnings("unused") Exception e) {
			throw new IllegalStateException(propertyName + " " + directoryPath + " is not writable.");
		}
		finally {
			testFile.delete();
		}
	}
	
}
