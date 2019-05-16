package org.skyve.impl.web;

import static java.lang.Boolean.TRUE;

import java.io.File;
import java.io.FileInputStream;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.UUID;

import javax.faces.FacesException;
import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.websocket.server.ServerContainer;
import javax.websocket.server.ServerEndpointConfig;

import org.omnifaces.cdi.push.Socket;
import org.omnifaces.cdi.push.SocketEndpoint;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.elastic.ElasticContentManager;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.LocalSecureRepository;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.HibernateContentPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.VariableExpander;
import org.skyve.impl.web.faces.SkyveSocketEndpoint;
import org.skyve.job.JobScheduler;
import org.skyve.persistence.DataStore;

public class SkyveContextListener implements ServletContextListener {
	@Override
	@SuppressWarnings("unchecked")
	public void contextInitialized(ServletContextEvent evt) {
		ServletContext ctx = evt.getServletContext();

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
		if (UtilImpl.PROPERTIES_FILE_PATH == null) {
			UtilImpl.LOGGER.info("SKYVE CONTEXT REAL PATH = " + UtilImpl.SKYVE_CONTEXT_REAL_PATH);
			File archive = new File(UtilImpl.SKYVE_CONTEXT_REAL_PATH);
			String archiveName = null;
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

		Map<String, Object> properties = null;
		try (FileInputStream fis = new FileInputStream(UtilImpl.PROPERTIES_FILE_PATH)) {
			final VariableExpander variableExpander = new VariableExpander();
			properties = variableExpander.expand(UtilImpl.readJSONConfig(fis), System.getenv());
		}
		catch (Exception e) {
			throw new IllegalStateException("Cannot open or read " + UtilImpl.PROPERTIES_FILE_PATH, e);
		}
		UtilImpl.CONFIGURATION = properties;
		
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
		Map<String, Object> content = getObject(null, "content", properties, true);
		UtilImpl.CONTENT_DIRECTORY = getString("content", "directory", content, true);
		// clean up the content directory path
		UtilImpl.CONTENT_DIRECTORY = cleanupDirectory(UtilImpl.CONTENT_DIRECTORY);
		testWritableDirectory("content.directory", UtilImpl.CONTENT_DIRECTORY);
		UtilImpl.CONTENT_GC_CRON = getString("content", "gcCron", content, true);
		UtilImpl.CONTENT_SERVER_ARGS = getString("content", "serverArgs", content, false);
		UtilImpl.CONTENT_FILE_STORAGE = getBoolean("content", "fileStorage", content);

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
		
		Map<String, Object> conversations = getObject(null, "conversations", properties, true);
		UtilImpl.MAX_CONVERSATIONS_IN_MEMORY = getInt("conversations", "maxInMemory", conversations);
		UtilImpl.CONVERSATION_EVICTION_TIME_MINUTES = getInt("conversations", "evictionTimeMinutes", conversations);

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
															dialect));
			}
			else {
				UtilImpl.DATA_STORES.put(dataStoreName, new DataStore(jndi, dialect));
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
					AbstractRepository.set((AbstractRepository) Thread.currentThread().getContextClassLoader().loadClass(UtilImpl.SKYVE_REPOSITORY_CLASS).newInstance());
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

		UtilImpl.SKYVE_CONTENT_MANAGER_CLASS = getString("factories", "contentManagerClass", factories, false);
		if (UtilImpl.SKYVE_CONTENT_MANAGER_CLASS == null) {
			AbstractContentManager.IMPLEMENTATION_CLASS = ElasticContentManager.class;
		}
		else {
			try {
				AbstractContentManager.IMPLEMENTATION_CLASS = (Class<? extends AbstractContentManager>) Class.forName(UtilImpl.SKYVE_CONTENT_MANAGER_CLASS);
			}
			catch (ClassNotFoundException e) {
				throw new IllegalStateException("Could not find factories.contentManagerClass " + UtilImpl.SKYVE_CONTENT_MANAGER_CLASS, e);
			}
		}
		
		Map<String, Object> smtp = getObject(null, "smtp", properties, true);
		UtilImpl.SMTP = getString("smtp", "server", smtp, true);
		UtilImpl.SMTP_PORT = Integer.toString(getInt("smtp", "port", smtp));
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
		if (UtilImpl.SMTP_TEST_BOGUS_SEND && (UtilImpl.SMTP_TEST_RECIPIENT == null)) {
			throw new IllegalStateException("smtp.testBogusSend is true but no smtp.testRecipient is defined");
		}

		Map<String, Object> environment = getObject(null, "environment", properties, true);
		UtilImpl.ENVIRONMENT_IDENTIFIER = getString("environment", "identifier", environment, false);
		UtilImpl.DEV_MODE = getBoolean("environment", "devMode", environment);
		UtilImpl.CUSTOMER = getString("environment", "customer", environment, false);
		UtilImpl.JOB_SCHEDULER = getBoolean("environment", "jobScheduler", environment);
		UtilImpl.PASSWORD_HASHING_ALGORITHM = getString("environment", "passwordHashingAlgorithm", environment, true);
		UtilImpl.APPS_JAR_DIRECTORY = getString("environment", "appsJarDirectory", environment, false);
		UtilImpl.MODULE_DIRECTORY = getString("environment", "moduleDirectory", environment, false);
		if (UtilImpl.MODULE_DIRECTORY != null) {
			// clean up the module directory path
			UtilImpl.MODULE_DIRECTORY = cleanupModuleDirectory(UtilImpl.MODULE_DIRECTORY);

			File moduleDirectory = new File(UtilImpl.MODULE_DIRECTORY);
			if (!moduleDirectory.exists()) {
				throw new IllegalStateException("environment.moduleDirectory " + UtilImpl.MODULE_DIRECTORY + " does not exist.");
			}
			if (!moduleDirectory.isDirectory()) {
				throw new IllegalStateException(
						"environment.moduleDirectory " + UtilImpl.MODULE_DIRECTORY + " is not a directory.");
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
		
		UtilImpl.CONTENT_DIRECTORY = getString("content", "directory", content, true);

		// ensure that the schema is created before trying to init the job scheduler
		AbstractPersistence p = null;
		try {
			p = (AbstractPersistence) CORE.getPersistence(); // syncs the schema if required
			p.begin();
			// If this is not prod and we have a bootstrap stanza
			if ((UtilImpl.ENVIRONMENT_IDENTIFIER != null) && (bootstrap != null)) {
				SuperUser u = new SuperUser();
				u.setCustomerName(UtilImpl.BOOTSTRAP_CUSTOMER);
				u.setContactName(UtilImpl.BOOTSTRAP_USER);
				u.setName(UtilImpl.BOOTSTRAP_USER);
				u.setPasswordHash(EXT.hashPassword(UtilImpl.BOOTSTRAP_PASSWORD));
				p.setUser(u);

				EXT.bootstrap(p);
			}
		}
		catch (Exception e) {
			if (p != null) {
				p.rollback();
			}
			throw new IllegalStateException("Cannot initialise either the data schema or the bootstrap user.", e);
		}
		finally {
			if (p != null) {
				p.commit(true);
			}
		}
		
		JobScheduler.init();
		ConversationUtil.initConversationsCache();
		
		// Start a websocket end point
		// NB From org.omnifaces.cdi.push.Socket.registerEndpointIfNecessary() called by org.omnifaces.ApplicationListener
		try {
			ServerContainer container = (ServerContainer) ctx.getAttribute(ServerContainer.class.getName());
			ServerEndpointConfig config = ServerEndpointConfig.Builder.create(SkyveSocketEndpoint.class, SocketEndpoint.URI_TEMPLATE).build();
			container.addEndpoint(config);
			// to stop the <o:socket/> from moaning that the endpoint is not configured
			ctx.setAttribute(Socket.class.getName(), TRUE);
		}
		catch (Exception e) {
			throw new FacesException(e);
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
		Number result = (Number) get(prefix, key, properties, true);
		return result.intValue();
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
		JobScheduler.dispose();
		ConversationUtil.destroyConversationsCache();
		
		@SuppressWarnings("resource")
		AbstractContentManager cm = (AbstractContentManager) EXT.newContentManager();
		try {
			cm.close();
			cm.dispose();
		}
		catch (Exception e) {
			UtilImpl.LOGGER.info("Could not close or dispose of the content manager - this is probably OK although resources may be left hanging or locked");
			e.printStackTrace();
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
			throw new IllegalStateException(propertyName + " " + directoryPath + " is not writeable.");
		}
		finally {
			testFile.delete();
		}
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
	static String cleanupModuleDirectory(final String path) {
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
