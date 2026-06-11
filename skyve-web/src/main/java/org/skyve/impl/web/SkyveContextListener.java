package org.skyve.impl.web;

import java.io.File;
import java.io.FileInputStream;
import java.io.Serializable;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.function.Consumer;
import java.util.function.IntConsumer;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.cache.ArchivedDocumentCacheConfig;
import org.skyve.cache.CSRFTokenCacheConfig;
import org.skyve.cache.CacheExpiryPolicy;
import org.skyve.cache.Caching;
import org.skyve.cache.ConversationCacheConfig;
import org.skyve.cache.EHCacheConfig;
import org.skyve.cache.GeoIPCacheConfig;
import org.skyve.cache.HibernateCacheConfig;
import org.skyve.cache.JCacheConfig;
import org.skyve.cache.SessionCacheConfig;
import org.skyve.domain.number.NumberGenerator;
import org.skyve.impl.archive.support.ArchiveLuceneIndexerSingleton;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.domain.number.NumberGeneratorStaticSingleton;
import org.skyve.impl.geoip.GeoIPServiceStaticSingleton;
import org.skyve.impl.job.JobSchedulerStaticSingleton;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.impl.mail.SMTPMailService;
import org.skyve.impl.metadata.controller.CustomisationsStaticSingleton;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.RDBMSDynamicPersistence;
import org.skyve.impl.persistence.hibernate.HibernateContentPersistence;
import org.skyve.impl.sms.SMSServiceStaticSingleton;
import org.skyve.impl.util.TwoFactorAuthConfigurationSingleton;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.UtilImpl.ArchiveConfig;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveSchedule;
import org.skyve.impl.util.UtilImpl.MapType;
import org.skyve.impl.util.VariableExpander;
import org.skyve.impl.web.filter.DevLoginFilter;
import org.skyve.impl.web.filter.ResponseHeaderFilter;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.controller.Customisations;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.persistence.DataStore;
import org.skyve.persistence.DynamicPersistence;
import org.skyve.util.GeoIPService;
import org.skyve.util.MailService;
import org.skyve.util.PushMessage;
import org.skyve.util.SMSService;
import org.skyve.util.Util;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;

import jakarta.servlet.FilterRegistration;
import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletContextEvent;
import jakarta.servlet.ServletContextListener;
import jakarta.servlet.SessionCookieConfig;

/**
 * Servlet context listener that initializes and tears down Skyve runtime services
 * and reads application configuration from JSON.
 */
public class SkyveContextListener implements ServletContextListener {
	private static record AppCacheSettings(String cacheName,
									 String type,
									 int heapSizeEntries,
									 long offHeapSize,
									 CacheExpiryPolicy expiryPolicy,
									 long expiry,
									 Class<? extends Serializable> keyClass,
									 Class<? extends Serializable> valueClass,
									 long diskSize,
									 boolean persistent) {
	}

	private static final String DEV_LOGIN_FILTER_CLASS_NAME = DevLoginFilter.class.getName();
	private static final String RESPONSE_HEADER_FILTER_CLASS_NAME = ResponseHeaderFilter.class.getName();
	private static final String CONTENT_KEY = "content";
	private static final String DIRECTORY_KEY = "directory";
	private static final String TRACE_KEY = "trace";
	private static final String SECURITY_KEY = "security";
	private static final String BACKUP_KEY = "backup";
	private static final String UPLOADS_KEY = "uploads";
	private static final String WHITELIST_REGEX_KEY = "whitelistRegex";
	private static final String MAXIMUM_SIZE_MB_KEY = "maximumSizeMB";
	private static final String THUMBNAIL_KEY = "thumbnail";
	private static final String STATE_KEY = "state";
	private static final String HEAP_SIZE_ENTRIES_KEY = "heapSizeEntries";
	private static final String OFF_HEAP_SIZE_MB_KEY = "offHeapSizeMB";
	private static final String DISK_SIZE_GB_KEY = "diskSizeGB";
	private static final String EXPIRY_TIME_MINUTES_KEY = "expiryTimeMinutes";
	private static final String STATE_CONVERSATIONS_KEY = "state.conversations";
	private static final String STATE_CSRF_TOKENS_KEY = "state.csrfTokens";
	private static final String STATE_SESSIONS_KEY = "state.sessions";
	private static final String STATE_GEO_IPS_KEY = "state.geoIPs";
	private static final String CACHES_KEY = "caches";
	private static final String HIBERNATE_KEY = "hibernate";
	private static final String FACTORIES_KEY = "factories";
	private static final String ACCOUNT_KEY = "account";
	private static final String ENVIRONMENT_KEY = "environment";
	private static final String HEALTH_KEY = "health";
	private static final String BOOTSTRAP_KEY = "bootstrap";
	private static final String API_KEY = "api";
	private static final String DOES_NOT_EXIST = " does not exist.";
	private static final String MAP_KEY = "map";
	private static final String PUSH_KEY = "push";
	private static final String SMTP_KEY = "smtp";
	private static final String URL_KEY = "url";
	private static final String WARNING_BANNER = "*******************************************************************************************************";

    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(SkyveContextListener.class);

	/**
	 * Initialize Skyve services, caches, and per-customer startup hooks when the
	 * web application context is created.
	 *
	 * @param evt The servlet context event.
	 */
	@Override
	public void contextInitialized(ServletContextEvent evt) {
		ServletContext ctx = evt.getServletContext();
		populateUtilImpl(ctx);

		final Caching caching = EXT.getCaching();
		caching.startup();
		try {
			EXT.getAddInManager().startup();
			
			// ensure that the schema is created before trying to init the job scheduler
			initialiseSchemaAndBootstrap();
			
			JobSchedulerStaticSingleton.setDefault();
			
			EXT.getReporting().startup();
			
			ArchiveLuceneIndexerSingleton.getInstance().startup();

			JobScheduler jobScheduler = EXT.getJobScheduler();
			jobScheduler.startup();

			TwoFactorAuthConfigurationSingleton.getInstance().startup();

			// Set up the session cookie
			SessionCookieConfig scc = ctx.getSessionCookieConfig();
			scc.setHttpOnly(true);
			scc.setSecure(Util.isSecureUrl());

			// Notify any observers of the startup.
			ProvidedRepository repository = ProvidedRepositoryFactory.get();
			if (UtilImpl.CUSTOMER != null) {
				// if a default customer is specified, only notify that one
				CustomerImpl internalCustomer = (CustomerImpl) repository.getCustomer(UtilImpl.CUSTOMER);
				if (internalCustomer == null) {
					throw new IllegalStateException("UtilImpl.CUSTOMER " + UtilImpl.CUSTOMER + DOES_NOT_EXIST);
				}
				internalCustomer.notifyStartup();
			}
			else {
				// notify all customers
				for (String customerName : repository.getAllCustomerNames()) {
					CustomerImpl internalCustomer = (CustomerImpl) repository.getCustomer(customerName);
					if (internalCustomer == null) {
						throw new IllegalStateException("Customer " + customerName + DOES_NOT_EXIST);
					}
					internalCustomer.notifyStartup();
				}
			}
			
			// Validate Skyve meta-data
			jobScheduler.validateMetaData();
			
			// Start the stale-receiver reaper if configured
			if (UtilImpl.PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS > 0) {
				PushMessage.startReaper(UtilImpl.PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS);
			}
		}
		// in case of error, close the caches to relinquish resources and file locks
		catch (Exception t) {
			caching.shutdown();
			throw t;
		}
	}

	/**
	 * Populate {@link UtilImpl} static configuration from the servlet context and
	 * JSON configuration files.
	 *
	 * @param ctx The servlet context used to resolve init params and real paths.
	 */
	public static void populateUtilImpl(ServletContext ctx) {
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
			LOGGER.info("SKYVE CONTEXT REAL PATH = {}", UtilImpl.SKYVE_CONTEXT_REAL_PATH);
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

		validateRequiredFilters(ctx);

		Map<String, Object> properties = readExpandedConfiguration(new File(UtilImpl.PROPERTIES_FILE_PATH));
		UtilImpl.CONFIGURATION = properties;

		Map<String, Object> content = getObject(null, CONTENT_KEY, properties, true);
		UtilImpl.CONTENT_DIRECTORY = getString(CONTENT_KEY, DIRECTORY_KEY, content, true);
		UtilImpl.CONTENT_DIRECTORY = cleanupDirectory(UtilImpl.CONTENT_DIRECTORY);
		testWritableDirectory(CONTENT_KEY + '.' + DIRECTORY_KEY, UtilImpl.CONTENT_DIRECTORY);

		Map<String, Object> overrides;
		File overridesFile = new File(UtilImpl.CONTENT_DIRECTORY, archiveName + ".json");
		if (overridesFile.exists()) {
			overrides = readExpandedConfiguration(overridesFile);
		}
		else {
			overrides = new HashMap<>();
		}
		UtilImpl.OVERRIDE_CONFIGURATION = overrides;

		merge(overrides, properties);

		configureTraceSettings(properties);
		configureContentSettings(content);
		configureBackupSettings(properties);
		configureUploadsSettings(properties);
		configurePushSettings(properties);
		configureAddinsSettings(properties);
		configureThumbnailSettings(properties);
		configureUrlSettings(properties);
		configureStateSettings(properties);
		configureApplicationCaches(properties);
		configureDataStores(properties);
		configureHibernateSettings(properties);
		configureFactorySettings(properties);
		configureMapSettings(properties);
		configureAccountSettings(properties);
		configureEnvironmentSettings(properties);
		configureHealthSettings(properties);
		configureApiSettings(properties);
		configureBootstrapSettings(properties);
		configurePrimeFlex(ctx);
		configureSecuritySettings(properties);
		configureArchiveProperties(properties);
	}

	private static void initialiseSchemaAndBootstrap() {
		AbstractPersistence persistence = null;
		try {
			persistence = (AbstractPersistence) CORE.getPersistence();
			persistence.begin();
			if ((UtilImpl.ENVIRONMENT_IDENTIFIER != null) && (UtilImpl.BOOTSTRAP_CUSTOMER != null)) {
				SuperUser user = new SuperUser();
				user.setCustomerName(UtilImpl.BOOTSTRAP_CUSTOMER);
				user.setContactName(UtilImpl.BOOTSTRAP_USER);
				user.setName(UtilImpl.BOOTSTRAP_USER);
				user.setPasswordHash(EXT.hashPassword(UtilImpl.BOOTSTRAP_PASSWORD));
				persistence.setUser(user);

				EXT.bootstrap(persistence);
			}
		}
		catch (Exception e) {
			if (persistence != null) {
				persistence.rollback();
			}
			throw new IllegalStateException("Cannot initialise either the data schema or the bootstrap user.", e);
		}
		finally {
			if (persistence != null) {
				persistence.commit(true);
			}
		}
	}

	private static void validateRequiredFilters(ServletContext ctx) {
		boolean securityHeadersFilterExists = false;
		for (Entry<String, ? extends FilterRegistration> entry : ctx.getFilterRegistrations().entrySet()) {
			FilterRegistration reg = entry.getValue();
			String className = reg.getClassName();
			if (DEV_LOGIN_FILTER_CLASS_NAME.equals(className)) {
				UtilImpl.DEV_LOGIN_FILTER_USED = true;
				LOGGER.warn(WARNING_BANNER);
				LOGGER.warn("DevLoginFilter is in use - Skyve will open services that should not be open in a legit deployment");
				LOGGER.warn(WARNING_BANNER);
			}
			else if (RESPONSE_HEADER_FILTER_CLASS_NAME.equals(className)
					&& ResponseHeaderFilter.SECURITY_HEADERS_FILTER_NAME.equals(reg.getName())) {
				securityHeadersFilterExists = true;
			}
		}
		if (! securityHeadersFilterExists) {
			throw new IllegalStateException("A Filter <filter-name>SecurityHeadersFilter</filter-name> of <filter-class>org.skyve.impl.web.filter.ResponseHeaderFilter</filter-class> is required in web.xml.");
		}
	}

	private static Map<String, Object> readExpandedConfiguration(File file) {
		try (FileInputStream fis = new FileInputStream(file)) {
			VariableExpander variableExpander = new VariableExpander();
			return variableExpander.expand(UtilImpl.readJSONConfig(fis), System.getenv());
		}
		catch (Exception e) {
			throw new IllegalStateException("Cannot open or read " + file.getAbsolutePath(), e);
		}
	}

	/**
	 * Read and validate archive configuration settings from the JSON configuration
	 * map and apply them to {@link UtilImpl}.
	 *
	 * @param properties The root configuration map.
	 */
	private static void configureArchiveProperties(Map<String, Object> properties) {
        String archKey = "archive";

        Map<String, Object> archiveProps = getObject(null, archKey, properties, false);
        if (archiveProps == null) {
            LOGGER.info("Archiving is not configured");
            return;
        }

        if (isMultiTenant()) {
            LOGGER.warn("Archiving is configured, but this appears to be a multi-tenancy instance");
            throw new IllegalArgumentException("Archiving is not supported on multi-tenancy instances");
        }

		int runtime = getNumber(archKey, "exportRuntimeSec", archiveProps, true).intValue();
		int batchSize = getNumber(archKey, "exportBatchSize", archiveProps, true).intValue();

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> docProps = (List<Map<String, Object>>) get(archKey, "documents", archiveProps, true);

        List<ArchiveConfig.ArchiveDocConfig> docConfigs = new ArrayList<>();
        for (Map<String, Object> docProp : docProps) {

            String module = getString(null, "module", docProp, true);
            String document = getString(null, "document", docProp, true);
			String directory = getString(null, DIRECTORY_KEY, docProp, true);
            int retainDeletedDocumentsDays = getInt(null, "retainDeletedDocumentsDays", docProp);

            docConfigs.add(new ArchiveDocConfig(module, document, directory, retainDeletedDocumentsDays));
        }

        // Setup the archive doc cache, with some defaults
        ArchivedDocumentCacheConfig cacheConfig = ArchivedDocumentCacheConfig.DEFAULT;

        Map<String, Object> cacheProps = getObject(archKey, "cache", archiveProps, false);
        if (cacheProps != null) {

            final String cacheKey = archKey + ".cache";

			Number heapSizeEntriesNumber = getNumber(cacheKey, HEAP_SIZE_ENTRIES_KEY, cacheProps, false);
			long heapSizeEntries = (heapSizeEntriesNumber == null) ? 100L : heapSizeEntriesNumber.longValue();
			Number expiryInMinutesNumber = getNumber(cacheKey, EXPIRY_TIME_MINUTES_KEY, cacheProps, false);
			long expiryInMinutes = (expiryInMinutesNumber == null) ? 10L : expiryInMinutesNumber.longValue();

            cacheConfig = new ArchivedDocumentCacheConfig(heapSizeEntries, expiryInMinutes);
        }

        ArchiveSchedule schedule = ArchiveSchedule.DEFUALT;
        Map<String, Object> scheduleSettings = getObject(archKey, "schedule", archiveProps, false);
        if (scheduleSettings != null) {
            final String key = archKey + ".schedule";

            String cron = getString(key, "cron", scheduleSettings, true);
            String customer = UtilImpl.CUSTOMER;
            schedule = new ArchiveSchedule(cron, customer, "archive_user");
        }

        UtilImpl.ARCHIVE_CONFIG = new ArchiveConfig(runtime, batchSize,
                Collections.unmodifiableList(docConfigs), cacheConfig, schedule);

        LOGGER.debug("Using archive config: {}", UtilImpl.ARCHIVE_CONFIG);
    }

    /**
     * Is this app configured for multiple tenants?
	 *
	 * @return {@code true} when no default customer is configured
     */
    private static boolean isMultiTenant() {
        return UtilImpl.CUSTOMER == null;
    }

	/**
	 * Configures the mail service implementation and related SMTP settings.
	 *
	 * @param properties the root configuration properties
	 * @param factories the configured factory class names
	 */
	static void configureMailServiceAndSmtp(Map<String, Object> properties, Map<String, Object> factories) {
		boolean smtpRequired = configureMailService(factories);

		Map<String, Object> smtp = getObject(null, SMTP_KEY, properties, smtpRequired);
		if (smtp == null) {
			return;
		}

		UtilImpl.SMTP = getString(SMTP_KEY, "server", smtp, smtpRequired);
		Number smtpPort = getNumber(SMTP_KEY, "port", smtp, smtpRequired);
		UtilImpl.SMTP_PORT = (smtpPort == null) ? 0 : smtpPort.intValue();
		UtilImpl.SMTP_UID = getString(SMTP_KEY, "uid", smtp, false);
		UtilImpl.SMTP_PWD = getString(SMTP_KEY, "pwd", smtp, false);

		UtilImpl.SMTP_PROPERTIES = toStringMap(getObject(SMTP_KEY, "properties", smtp, false));

		UtilImpl.SMTP_HEADERS = toStringMap(getObject(SMTP_KEY, "headers", smtp, false));

		UtilImpl.SMTP_SENDER = getString(SMTP_KEY, "sender", smtp, smtpRequired);
		UtilImpl.SMTP_TEST_RECIPIENT = getString(SMTP_KEY, "testRecipient", smtp, false);
		Boolean smtpTestBogusSend = (Boolean) get(SMTP_KEY, "testBogusSend", smtp, smtpRequired);
		UtilImpl.SMTP_TEST_BOGUS_SEND = Boolean.TRUE.equals(smtpTestBogusSend);
	}

	private static boolean configureMailService(Map<String, Object> factories) {
		UtilImpl.SKYVE_MAIL_SERVICE_CLASS = getString(FACTORIES_KEY, "mailServiceClass", factories, false);
		if (UtilImpl.SKYVE_MAIL_SERVICE_CLASS == null) {
			MailServiceStaticSingleton.setDefault();
			return true;
		}

		try {
			Class<?> loadedClass = Thread.currentThread().getContextClassLoader().loadClass(UtilImpl.SKYVE_MAIL_SERVICE_CLASS);
			MailService mailService = (MailService) loadedClass.getDeclaredConstructor().newInstance();
			MailServiceStaticSingleton.set(mailService);
			return (mailService instanceof SMTPMailService);
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not create factories.mailServiceClass " + UtilImpl.SKYVE_MAIL_SERVICE_CLASS, e);
		}
	}

	private static TreeMap<String, String> toStringMap(Map<String, Object> values) {
		TreeMap<String, String> result = new TreeMap<>();
		if (values == null) {
			return result;
		}

		for (Entry<String, Object> entry : values.entrySet()) {
			String key = entry.getKey();
			Object value = entry.getValue();
			if ((key != null) && (value != null)) {
				result.put(key, value.toString());
			}
		}
		return result;
	}

	private static void configureTraceSettings(Map<String, Object> properties) {
		Map<String, Object> trace = getObject(null, TRACE_KEY, properties, true);
		UtilImpl.XML_TRACE = getBoolean(TRACE_KEY, "xml", trace);
		UtilImpl.HTTP_TRACE = getBoolean(TRACE_KEY, "http", trace);
		UtilImpl.COMMAND_TRACE = getBoolean(TRACE_KEY, "command", trace);
		UtilImpl.FACES_TRACE = getBoolean(TRACE_KEY, "faces", trace);
		UtilImpl.QUERY_TRACE = getBoolean(TRACE_KEY, "query", trace);
		UtilImpl.SQL_TRACE = getBoolean(TRACE_KEY, "sql", trace);
		UtilImpl.CONTENT_TRACE = getBoolean(TRACE_KEY, CONTENT_KEY, trace);
		UtilImpl.SECURITY_TRACE = getBoolean(TRACE_KEY, SECURITY_KEY, trace);
		UtilImpl.BIZLET_TRACE = getBoolean(TRACE_KEY, "bizlet", trace);
		UtilImpl.DIRTY_TRACE = getBoolean(TRACE_KEY, "dirty", trace);
	}

	private static void configureContentSettings(Map<String, Object> content) {
		UtilImpl.CONTENT_GC_CRON = getString(CONTENT_KEY, "gcCron", content, true);
		Number gcEligibleAgeMinutes = getNumber(CONTENT_KEY, "gcEligibleAgeMinutes", content, false);
		if (gcEligibleAgeMinutes != null) {
			UtilImpl.CONTENT_GC_ELIGIBLE_AGE_MINUTES = gcEligibleAgeMinutes.intValue();
		}
		UtilImpl.CONTENT_JDBC_SERVER_ARGS = getString(CONTENT_KEY, "serverArgs", content, false);
		UtilImpl.CONTENT_REST_SERVER_URL = getString(CONTENT_KEY, "serverUrl", content, false);
		UtilImpl.CONTENT_FILE_STORAGE = getBoolean(CONTENT_KEY, "fileStorage", content);
		UtilImpl.CONTENT_FILE_SUFFIXES = getBoolean(CONTENT_KEY, "fileSuffixes", content);
	}

	private static void configureBackupSettings(Map<String, Object> properties) {
		Map<String, Object> backup = getObject(null, BACKUP_KEY, properties, false);
		if (backup == null) {
			return;
		}

		UtilImpl.BACKUP_DIRECTORY = getString(BACKUP_KEY, DIRECTORY_KEY, backup, false);
		if (UtilImpl.BACKUP_DIRECTORY != null) {
			UtilImpl.BACKUP_DIRECTORY = cleanupDirectory(UtilImpl.BACKUP_DIRECTORY);
			testWritableDirectory(BACKUP_KEY + '.' + DIRECTORY_KEY, UtilImpl.BACKUP_DIRECTORY);
		}
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = getString(BACKUP_KEY, "externalBackupClass", backup, false);
		UtilImpl.BACKUP_PROPERTIES = getObject(BACKUP_KEY, "properties", backup, false);
		Number restoreMaxExtractEntries = getNumber(BACKUP_KEY, "restoreMaxExtractEntries", backup, false);
		if (restoreMaxExtractEntries != null) {
			UtilImpl.BACKUP_RESTORE_MAX_EXTRACT_ENTRIES = restoreMaxExtractEntries.intValue();
		}
		Number restoreMaxExtractSizeMB = getNumber(BACKUP_KEY, "restoreMaxExtractSizeMB", backup, false);
		if (restoreMaxExtractSizeMB != null) {
			UtilImpl.BACKUP_RESTORE_MAX_EXTRACT_SIZE_MB = restoreMaxExtractSizeMB.intValue();
		}
	}

	private static void configureUploadsSettings(Map<String, Object> properties) {
		Map<String, Object> uploads = getObject(null, UPLOADS_KEY, properties, false);
		if (uploads == null) {
			return;
		}

		configureUploadCategory(uploads,
				"file",
				value -> UtilImpl.UPLOADS_FILE_WHITELIST_REGEX = value,
				value -> UtilImpl.UPLOADS_FILE_MAXIMUM_SIZE_IN_MB = value);
		configureUploadCategory(uploads,
				CONTENT_KEY,
				value -> UtilImpl.UPLOADS_CONTENT_WHITELIST_REGEX = value,
				value -> UtilImpl.UPLOADS_CONTENT_MAXIMUM_SIZE_IN_MB = value);
		configureUploadCategory(uploads,
				"image",
				value -> UtilImpl.UPLOADS_IMAGE_WHITELIST_REGEX = value,
				value -> UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB = value);
		configureUploadCategory(uploads,
				"bizport",
				value -> UtilImpl.UPLOADS_BIZPORT_WHITELIST_REGEX = value,
				value -> UtilImpl.UPLOADS_BIZPORT_MAXIMUM_SIZE_IN_MB = value);
		// facesServlet.setMultipartConfig() doesn't exist unless registering the servlet yourself.
		// Multipart upload limits are therefore applied through container configuration instead.
	}

	private static void configurePushSettings(Map<String, Object> properties) {
		Map<String, Object> push = getObject(null, PUSH_KEY, properties, false);
		if (push == null) {
			return;
		}

		UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS = getInt(PUSH_KEY, "keepAliveTimeInSeconds", push);
		if (UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS < 1) {
			throw new IllegalStateException("push.keepAliveTimeInSeconds must be greater than 0");
		}

		applyOptionalInt(push, PUSH_KEY, "queueSize", value -> {
			UtilImpl.PUSH_MESSAGE_QUEUE_SIZE = value;
			if (UtilImpl.PUSH_MESSAGE_QUEUE_SIZE < 1) {
				throw new IllegalStateException("push.queueSize must be greater than 0");
			}
		});
		applyOptionalInt(push, PUSH_KEY, "maxReceiversPerUser", value -> {
			UtilImpl.PUSH_MAX_RECEIVERS_PER_USER = value;
			if (UtilImpl.PUSH_MAX_RECEIVERS_PER_USER < 0) {
				throw new IllegalStateException("push.maxReceiversPerUser must be greater than or equal to 0");
			}
		});
		applyOptionalInt(push, PUSH_KEY, "maxReceiversTotal", value -> {
			UtilImpl.PUSH_MAX_RECEIVERS_TOTAL = value;
			if (UtilImpl.PUSH_MAX_RECEIVERS_TOTAL < 0) {
				throw new IllegalStateException("push.maxReceiversTotal must be greater than or equal to 0");
			}
		});
		applyOptionalInt(push, PUSH_KEY, "staleReceiverTimeoutInSeconds", value -> {
			UtilImpl.PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS = value;
			if (UtilImpl.PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS < 0) {
				throw new IllegalStateException("push.staleReceiverTimeoutInSeconds must be greater than or equal to 0");
			}
		});
	}

	private static void configureAddinsSettings(Map<String, Object> properties) {
		Map<String, Object> addins = getObject(null, "addins", properties, false);
		if (addins == null) {
			return;
		}

		UtilImpl.ADDINS_DIRECTORY = getString("addins", DIRECTORY_KEY, addins, false);
		if (UtilImpl.ADDINS_DIRECTORY != null) {
			UtilImpl.ADDINS_DIRECTORY = cleanupDirectory(UtilImpl.ADDINS_DIRECTORY);
			testWritableDirectory("addins.directory", UtilImpl.ADDINS_DIRECTORY);
		}
	}

	private static void configureThumbnailSettings(Map<String, Object> properties) {
		Map<String, Object> thumbnail = getObject(null, THUMBNAIL_KEY, properties, false);
		if (thumbnail == null) {
			return;
		}

		UtilImpl.THUMBNAIL_CONCURRENT_THREADS = getInt(THUMBNAIL_KEY, "concurrentThreads", thumbnail);
		UtilImpl.THUMBNAIL_SUBSAMPLING_MINIMUM_TARGET_SIZE = getInt(THUMBNAIL_KEY, "subsamplingMinimumTargetSize", thumbnail);
		UtilImpl.THUMBNAIL_FILE_STORAGE = getBoolean(THUMBNAIL_KEY, "fileStorage", thumbnail);
		UtilImpl.THUMBNAIL_DIRECTORY = getString(THUMBNAIL_KEY, DIRECTORY_KEY, thumbnail, false);
		if (UtilImpl.THUMBNAIL_DIRECTORY != null) {
			UtilImpl.THUMBNAIL_DIRECTORY = cleanupDirectory(UtilImpl.THUMBNAIL_DIRECTORY);
			testWritableDirectory(THUMBNAIL_KEY + '.' + DIRECTORY_KEY, UtilImpl.THUMBNAIL_DIRECTORY);
		}
	}

	private static void configureUrlSettings(Map<String, Object> properties) {
		Map<String, Object> url = getObject(null, URL_KEY, properties, true);
		UtilImpl.SERVER_URL = getString(URL_KEY, "server", url, true);
		UtilImpl.SKYVE_CONTEXT = getString(URL_KEY, "context", url, true);
		UtilImpl.HOME_URI = getString(URL_KEY, "home", url, true);
	}

	private static void configureStateSettings(Map<String, Object> properties) {
		Map<String, Object> state = getObject(null, STATE_KEY, properties, true);
		UtilImpl.CACHE_DIRECTORY = getString(STATE_KEY, DIRECTORY_KEY, state, false);
		if (UtilImpl.CACHE_DIRECTORY != null) {
			UtilImpl.CACHE_DIRECTORY = cleanupDirectory(UtilImpl.CACHE_DIRECTORY);
			testWritableDirectory(STATE_KEY + '.' + DIRECTORY_KEY, UtilImpl.CACHE_DIRECTORY);
		}

		UtilImpl.CACHE_MULTIPLE = Boolean.TRUE.equals(get(STATE_KEY, "multiple", state, false));
		Map<String, Object> conversations = getObject(STATE_KEY, "conversations", state, true);
		UtilImpl.CONVERSATION_CACHE = new ConversationCacheConfig(getInt(STATE_CONVERSATIONS_KEY, HEAP_SIZE_ENTRIES_KEY, conversations),
													getInt(STATE_CONVERSATIONS_KEY, OFF_HEAP_SIZE_MB_KEY, conversations),
													toDiskSizeMb(STATE_CONVERSATIONS_KEY, conversations),
													getInt(STATE_CONVERSATIONS_KEY, EXPIRY_TIME_MINUTES_KEY, conversations));
		Map<String, Object> tokens = getObject(STATE_KEY, "csrfTokens", state, true);
		UtilImpl.CSRF_TOKEN_CACHE = new CSRFTokenCacheConfig(getInt(STATE_CSRF_TOKENS_KEY, HEAP_SIZE_ENTRIES_KEY, tokens),
											getInt(STATE_CSRF_TOKENS_KEY, OFF_HEAP_SIZE_MB_KEY, tokens),
											toDiskSizeMb(STATE_CSRF_TOKENS_KEY, tokens),
											getInt(STATE_CSRF_TOKENS_KEY, EXPIRY_TIME_MINUTES_KEY, tokens));
		Map<String, Object> sessions = getObject(STATE_KEY, "sessions", state, true);
		UtilImpl.SESSION_CACHE = new SessionCacheConfig(getInt(STATE_SESSIONS_KEY, HEAP_SIZE_ENTRIES_KEY, sessions),
										getInt(STATE_SESSIONS_KEY, OFF_HEAP_SIZE_MB_KEY, sessions),
										toDiskSizeMb(STATE_SESSIONS_KEY, sessions),
										getInt(STATE_SESSIONS_KEY, EXPIRY_TIME_MINUTES_KEY, sessions));
		Map<String, Object> geoIps = getObject(STATE_KEY, "geoIPs", state, true);
		UtilImpl.GEO_IP_CACHE = new GeoIPCacheConfig(getInt(STATE_GEO_IPS_KEY, HEAP_SIZE_ENTRIES_KEY, geoIps),
										getInt(STATE_GEO_IPS_KEY, OFF_HEAP_SIZE_MB_KEY, geoIps),
										toDiskSizeMb(STATE_GEO_IPS_KEY, geoIps),
										getInt(STATE_GEO_IPS_KEY, EXPIRY_TIME_MINUTES_KEY, geoIps));
		UtilImpl.STATE_EVICT_CRON = getString(STATE_KEY, "evictCron", state, false);
	}

	private static void configureUploadCategory(Map<String, Object> uploads,
									 String category,
									 Consumer<String> regexSetter,
									 IntConsumer maximumSizeSetter) {
		Map<String, Object> values = getObject(UPLOADS_KEY, category, uploads, false);
		if (values == null) {
			return;
		}

		String uploadKey = UPLOADS_KEY + '.' + category;
		String whitelistRegex = Util.processStringValue(getString(uploadKey, WHITELIST_REGEX_KEY, values, false));
		if (whitelistRegex != null) {
			validateRegex(uploadKey + '.' + WHITELIST_REGEX_KEY, whitelistRegex);
			regexSetter.accept(whitelistRegex);
		}

		Number maximumSizeMB = getNumber(uploadKey, MAXIMUM_SIZE_MB_KEY, values, false);
		if (maximumSizeMB != null) {
			maximumSizeSetter.accept(maximumSizeMB.intValue());
		}
	}

	private static long toDiskSizeMb(String key, Map<String, Object> values) {
		return getInt(key, DISK_SIZE_GB_KEY, values) * 1024L;
	}

	private static void validateRegex(String key, String regex) {
		try {
			Pattern.compile(regex);
		}
		catch (PatternSyntaxException e) {
			throw new IllegalStateException(key + " is not a valid regex pattern", e);
		}
	}

	private static void configureMapSettings(Map<String, Object> properties) {
		Map<String, Object> map = getObject(null, MAP_KEY, properties, true);
		String value = getString(MAP_KEY, "type", map, true);
		UtilImpl.MAP_TYPE = MapType.valueOf(value);
		UtilImpl.MAP_LAYERS = getString(MAP_KEY, "layers", map, true);
		UtilImpl.MAP_CENTRE = getString(MAP_KEY, "centre", map, false);
		Number zoom = getNumber(MAP_KEY, "zoom", map, false);
		if (zoom != null) {
			UtilImpl.MAP_ZOOM = zoom.intValue();
		}
	}

	private static void configureAccountSettings(Map<String, Object> properties) {
		Map<String, Object> account = getObject(null, ACCOUNT_KEY, properties, true);
		UtilImpl.PASSWORD_HASHING_ALGORITHM = getString(ACCOUNT_KEY, "passwordHashingAlgorithm", account, true);
		applyOptionalInt(account, ACCOUNT_KEY, "passwordExpiryInDays", value -> UtilImpl.PASSWORD_EXPIRY_IN_DAYS = value);
		applyOptionalInt(account, ACCOUNT_KEY, "passwordHistoryRetention", value -> UtilImpl.PASSWORD_HISTORY_RETENTION = value);
		applyOptionalInt(account, ACCOUNT_KEY, "accountLockoutThreshold", value -> UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD = value);
		applyOptionalInt(account,
				ACCOUNT_KEY,
				"accountLockoutDurationMultipleInSeconds",
				value -> UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS = value);

		String loginUri = getString(ACCOUNT_KEY, "loginUri", account, false);
		if (loginUri != null) {
			UtilImpl.AUTHENTICATION_LOGIN_URI = loginUri;
		}

		UtilImpl.ACCOUNT_ALLOW_SELF_REGISTRATION = getBoolean(ACCOUNT_KEY, "allowUserSelfRegistration", account);
		UtilImpl.AUTHENTICATION_GOOGLE_CLIENT_ID = getString(ACCOUNT_KEY, "googleAuthClientId", account, false);
		UtilImpl.AUTHENTICATION_GOOGLE_SECRET = getString(ACCOUNT_KEY, "googleAuthSecret", account, false);
		UtilImpl.AUTHENTICATION_FACEBOOK_CLIENT_ID = getString(ACCOUNT_KEY, "facebookAuthClientId", account, false);
		UtilImpl.AUTHENTICATION_FACEBOOK_SECRET = getString(ACCOUNT_KEY, "facebookAuthSecret", account, false);
		UtilImpl.AUTHENTICATION_GITHUB_CLIENT_ID = getString(ACCOUNT_KEY, "githubAuthClientId", account, false);
		UtilImpl.AUTHENTICATION_GITHUB_SECRET = getString(ACCOUNT_KEY, "githubAuthSecret", account, false);
		UtilImpl.AUTHENTICATION_AZUREAD_CLIENT_ID = getString(ACCOUNT_KEY, "azureAdAuthClientId", account, false);
		UtilImpl.AUTHENTICATION_AZUREAD_TENANT_ID = getString(ACCOUNT_KEY, "azureAdAuthTenantId", account, false);
		UtilImpl.AUTHENTICATION_AZUREAD_SECRET = getString(ACCOUNT_KEY, "azureAdAuthSecret", account, false);
		applyOptionalInt(account, ACCOUNT_KEY, "rememberMeTokenTimeoutHours", value -> UtilImpl.REMEMBER_ME_TOKEN_TIMEOUT_HOURS = value);

		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = new HashSet<>();
		List<String> tfaCustomers = getList(ACCOUNT_KEY, "tfaCustomers", account, false);
		if (tfaCustomers != null) {
			for (String customerName : tfaCustomers) {
				String name = UtilImpl.processStringValue(customerName);
				if (name != null) {
					UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS.add(name);
				}
			}
		}

		applyOptionalInt(account, ACCOUNT_KEY, "tfaResendCooldownSeconds", value -> {
			UtilImpl.TWO_FACTOR_AUTH_RESEND_COOLDOWN_SECONDS = value;
			if (UtilImpl.TWO_FACTOR_AUTH_RESEND_COOLDOWN_SECONDS < 1) {
				throw new IllegalStateException("account.tfaResendCooldownSeconds must be greater than 0");
			}
		});
	}

	private static void configureEnvironmentSettings(Map<String, Object> properties) {
		Map<String, Object> environment = getObject(null, ENVIRONMENT_KEY, properties, true);
		UtilImpl.ENVIRONMENT_IDENTIFIER = getString(ENVIRONMENT_KEY, "identifier", environment, false);

		if ((UtilImpl.ENVIRONMENT_IDENTIFIER == null) && UtilImpl.DEV_LOGIN_FILTER_USED) {
			LOGGER.error(WARNING_BANNER);
			LOGGER.error("DevLoginFilter is in use in prod!! - stopping deployment...");
			LOGGER.error("The DevLoginFilter ({}) should not be used in prod - see web.xml", DEV_LOGIN_FILTER_CLASS_NAME);
			LOGGER.warn(WARNING_BANNER);
			throw new IllegalStateException("The DevLoginFilter (" + DEV_LOGIN_FILTER_CLASS_NAME + ") should not be used in prod - see web.xml");
		}

		UtilImpl.DEV_MODE = getBoolean(ENVIRONMENT_KEY, "devMode", environment);
		Boolean accessControl = (Boolean) get(ENVIRONMENT_KEY, "accessControl", environment, false);
		if (accessControl != null) {
			UtilImpl.ACCESS_CONTROL = accessControl.booleanValue();
		}

		UtilImpl.CUSTOMER = getString(ENVIRONMENT_KEY, "customer", environment, false);
		UtilImpl.JOB_SCHEDULER = getBoolean(ENVIRONMENT_KEY, "jobScheduler", environment);
		UtilImpl.APPS_JAR_DIRECTORY = getString(ENVIRONMENT_KEY, "appsJarDirectory", environment, false);
		UtilImpl.MODULE_DIRECTORY = getString(ENVIRONMENT_KEY, "moduleDirectory", environment, false);
		if (UtilImpl.MODULE_DIRECTORY != null) {
			UtilImpl.MODULE_DIRECTORY = UtilImpl.cleanupModuleDirectory(UtilImpl.MODULE_DIRECTORY);

			File moduleDirectory = new File(UtilImpl.MODULE_DIRECTORY);
			if (! moduleDirectory.exists()) {
				throw new IllegalStateException(ENVIRONMENT_KEY + ".moduleDirectory " + UtilImpl.MODULE_DIRECTORY + DOES_NOT_EXIST);
			}
			if (! moduleDirectory.isDirectory()) {
				throw new IllegalStateException(ENVIRONMENT_KEY + ".moduleDirectory " + UtilImpl.MODULE_DIRECTORY + " is not a directory.");
			}
		}

		UtilImpl.SUPPORT_EMAIL_ADDRESS = getString(ENVIRONMENT_KEY, "supportEmailAddress", environment, false);
		UtilImpl.SHOW_SETUP = getBoolean(ENVIRONMENT_KEY, "showSetup", environment);
	}

	private static void configureHealthSettings(Map<String, Object> properties) {
		Map<String, Object> health = getObject(null, HEALTH_KEY, properties, false);
		if (health == null) {
			return;
		}

		UtilImpl.HEALTH_CHECK = getBoolean(HEALTH_KEY, "check", health);
		UtilImpl.HEALTH_CACHE_TIME_IN_SECONDS = getInt(HEALTH_KEY, "cacheTimeInSeconds", health);
	}

	private static void configureApiSettings(Map<String, Object> properties) {
		Map<String, Object> api = getObject(null, API_KEY, properties, true);
		UtilImpl.CHECK_FOR_BREACHED_PASSWORD = Boolean.TRUE.equals(get(API_KEY, "checkForBreachedPassword", api, false));
		UtilImpl.GOOGLE_MAPS_V3_API_KEY = getString(API_KEY, "googleMapsV3Key", api, false);
		UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY = getString(API_KEY, "googleRecaptchaSiteKey", api, false);
		UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY = getString(API_KEY, "googleRecaptchaSecretKey", api, false);
		UtilImpl.CLOUDFLARE_TURNSTILE_SITE_KEY = getString(API_KEY, "cloudflareTurnstileSiteKey", api, false);
		UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY = getString(API_KEY, "cloudflareTurnstileSecretKey", api, false);
		UtilImpl.GEO_IP_KEY = getString(API_KEY, "geoIPKey", api, false);

		String geoIpCountryCodes = getString(API_KEY, "geoIPCountryCodes", api, false);
		if (geoIpCountryCodes != null) {
			String[] codes = geoIpCountryCodes.split("\\|");
			UtilImpl.GEO_IP_COUNTRY_CODES = new CopyOnWriteArraySet<>(Arrays.asList(codes));
		}

		Boolean geoIpWhitelist = (Boolean) get(API_KEY, "geoIPWhitelist", api, false);
		if (geoIpWhitelist != null) {
			UtilImpl.GEO_IP_WHITELIST = geoIpWhitelist.booleanValue();
		}

		UtilImpl.CKEDITOR_CONFIG_FILE_URL = getString(API_KEY, "ckEditorConfigFileUrl", api, false);
		if (UtilImpl.CKEDITOR_CONFIG_FILE_URL == null) {
			UtilImpl.CKEDITOR_CONFIG_FILE_URL = "";
		}
	}

	private static void configureApplicationCaches(Map<String, Object> properties) {
		Map<String, Object> caches = getObject(null, CACHES_KEY, properties, false);
		if (caches != null) {
			for (String cacheName : caches.keySet()) {
				Map<String, Object> cache = getObject(CACHES_KEY, cacheName, caches, true);
				String prefix = String.format("caches.%s", cacheName);
				String type = getString(CACHES_KEY + '.' + cacheName, "type", cache, true);
				int heapSizeEntries = getInt(prefix, HEAP_SIZE_ENTRIES_KEY, cache);
				Number offHeapSizeInMb = getNumber(prefix, OFF_HEAP_SIZE_MB_KEY, cache, false);
				Number expiryInMinutes = getNumber(prefix, EXPIRY_TIME_MINUTES_KEY, cache, false);
				Number diskSizeInGb = getNumber(prefix, DISK_SIZE_GB_KEY, cache, false);
				String keyClassName = getString(prefix, "keyClass", cache, true);
				String valueClassName = getString(prefix, "valueClass", cache, true);
				String expiryPolicyString = getString(prefix, "expiryPolicy", cache, false);
				Boolean persistent = (Boolean) get(prefix, "persistent", cache, false);
				Class<? extends Serializable> keyClass = loadSerializableClass(keyClassName, prefix + ".keyClass");
				Class<? extends Serializable> valueClass = loadSerializableClass(valueClassName, prefix + ".valueClass");
				CacheExpiryPolicy expiryPolicy = (expiryPolicyString == null) ? CacheExpiryPolicy.eternal : CacheExpiryPolicy.valueOf(expiryPolicyString);
				long offHeapSize = (offHeapSizeInMb == null) ? 0L : offHeapSizeInMb.longValue();
				long expiry = (expiryInMinutes == null) ? 0L : expiryInMinutes.longValue();
				long diskSize = (diskSizeInGb == null) ? 0L : diskSizeInGb.longValue() * 1024L;

				addApplicationCache(new AppCacheSettings(cacheName,
						type,
						heapSizeEntries,
						offHeapSize,
						expiryPolicy,
						expiry,
						keyClass,
						valueClass,
						diskSize,
						Boolean.TRUE.equals(persistent)));
			}
		}
	}

	private static void addApplicationCache(AppCacheSettings settings) {
		if ("jcache".equals(settings.type())) {
			UtilImpl.APP_CACHES.add(new JCacheConfig<>(settings.cacheName(),
						settings.heapSizeEntries(),
						settings.offHeapSize(),
						settings.expiryPolicy(),
						settings.expiry(),
						settings.keyClass(),
						settings.valueClass()));
			return;
		}
		if ("ehcache".equals(settings.type())) {
			UtilImpl.APP_CACHES.add(new EHCacheConfig<>(settings.cacheName(),
						settings.heapSizeEntries(),
						settings.offHeapSize(),
						settings.expiryPolicy(),
						settings.expiry(),
						settings.keyClass(),
						settings.valueClass(),
						settings.diskSize(),
						settings.persistent()));
			return;
		}
		throw new IllegalStateException("Cache type " + settings.type() + " is not a known type");
	}

	private static void configureDataStores(Map<String, Object> properties) {
		Map<String, Object> dataStores = getObject(null, "dataStores", properties, true);
		for (String dataStoreName : dataStores.keySet()) {
			Map<String, Object> dataStore = getObject("dataStores", dataStoreName, dataStores, true);
			String prefix = String.format("dataStores.%s", dataStoreName);
			String dialect = getString(prefix, "dialect", dataStore, true);
			String jndi = getString(prefix, "jndi", dataStore, false);
			if (jndi == null) {
				UtilImpl.DATA_STORES.put(dataStoreName,
						new DataStore(getString(prefix, "driver", dataStore, true),
								getString(prefix, URL_KEY, dataStore, true),
								getString(prefix, "user", dataStore, false),
								getString(prefix, "password", dataStore, false),
								dialect,
								getInt(prefix, "oltpConnectionTimeoutInSeconds", dataStore),
								getInt(prefix, "asyncConnectionTimeoutInSeconds", dataStore)));
			}
			else {
				UtilImpl.DATA_STORES.put(dataStoreName,
						new DataStore(jndi,
								dialect,
								getInt(prefix, "oltpConnectionTimeoutInSeconds", dataStore),
								getInt(prefix, "asyncConnectionTimeoutInSeconds", dataStore)));
			}
		}
	}

	private static void configureHibernateSettings(Map<String, Object> properties) {
		Map<String, Object> hibernate = getObject(null, HIBERNATE_KEY, properties, true);
		String dataStoreName = getString(HIBERNATE_KEY, "dataStore", hibernate, true);
		UtilImpl.DATA_STORE = UtilImpl.DATA_STORES.get(dataStoreName);
		if (UtilImpl.DATA_STORE == null) {
			throw new IllegalStateException("hibernate.dataStore " + dataStoreName + " is not defined in dataStores");
		}
		UtilImpl.DDL_SYNC = getBoolean(HIBERNATE_KEY, "ddlSync", hibernate);
		UtilImpl.CATALOG = getString(HIBERNATE_KEY, "catalog", hibernate, false);
		UtilImpl.SCHEMA = getString(HIBERNATE_KEY, "schema", hibernate, false);
		UtilImpl.PRETTY_SQL_OUTPUT = getBoolean(HIBERNATE_KEY, "prettySql", hibernate);

		Map<String, Object> hibernateCaches = getObject(HIBERNATE_KEY, CACHES_KEY, hibernate, false);
		if (hibernateCaches != null) {
			for (String cacheName : hibernateCaches.keySet()) {
				Map<String, Object> cache = getObject("hibernate.caches", cacheName, hibernateCaches, true);
				String prefix = String.format("hibernate.caches.%s", cacheName);
				int heapSizeEntries = getInt(prefix, HEAP_SIZE_ENTRIES_KEY, cache);
				Number offHeapSizeInMb = getNumber(prefix, OFF_HEAP_SIZE_MB_KEY, cache, false);
				String expiryPolicyString = getString(prefix, "expiryPolicy", cache, false);
				Number expiryInMinutes = getNumber(prefix, EXPIRY_TIME_MINUTES_KEY, cache, false);
				UtilImpl.HIBERNATE_CACHES.add(new HibernateCacheConfig(cacheName,
														heapSizeEntries,
														(offHeapSizeInMb == null) ? 0L : offHeapSizeInMb.longValue(),
														(expiryPolicyString == null) ? CacheExpiryPolicy.eternal : CacheExpiryPolicy.valueOf(expiryPolicyString),
														(expiryInMinutes == null) ? 0L : expiryInMinutes.longValue()));
			}
		}

		UtilImpl.HIBERNATE_FAIL_ON_MISSING_CACHE = true;
	}

	private static void configureFactorySettings(Map<String, Object> properties) {
		Map<String, Object> factories = getObject(null, FACTORIES_KEY, properties, true);
		configureRepositoryFactory(factories);
		configurePersistenceFactories(factories);
		configureSingletonFactories(factories);

		Customisations customisations = CustomisationsStaticSingleton.get();
		customisations.registerCustomExpressions();
		customisations.registerCustomFormatters();

		configureMailServiceAndSmtp(properties, factories);
		configureServiceFactories(factories);
	}

	private static void configureRepositoryFactory(Map<String, Object> factories) {
		UtilImpl.SKYVE_REPOSITORY_CLASS = getString(FACTORIES_KEY, "repositoryClass", factories, false);
		if (! ProvidedRepositoryFactory.isConfigured()) {
			if (UtilImpl.SKYVE_REPOSITORY_CLASS == null) {
				LOGGER.info("SET SKYVE REPOSITORY CLASS TO DEFAULT");
				ProvidedRepositoryFactory.set(new DefaultRepository());
			}
			else {
				LOGGER.info("SET SKYVE REPOSITORY CLASS TO {}", UtilImpl.SKYVE_REPOSITORY_CLASS);
				ProvidedRepositoryFactory.set(instantiateFactory(ProvidedRepository.class,
						UtilImpl.SKYVE_REPOSITORY_CLASS,
						"factories.repositoryClass"));
			}
		}
	}

	@SuppressWarnings("unchecked")
	private static void configurePersistenceFactories(Map<String, Object> factories) {
		UtilImpl.SKYVE_PERSISTENCE_CLASS = getString(FACTORIES_KEY, "persistenceClass", factories, false);
		if (AbstractPersistence.IMPLEMENTATION_CLASS == null) {
			if (UtilImpl.SKYVE_PERSISTENCE_CLASS == null) {
				LOGGER.info("SET SKYVE PERSISTENCE CLASS TO DEFAULT (HibernateContentPersistence)");
				AbstractPersistence.IMPLEMENTATION_CLASS = HibernateContentPersistence.class;
			}
			else {
				LOGGER.info("SET SKYVE PERSISTENCE CLASS TO {}", UtilImpl.SKYVE_PERSISTENCE_CLASS);
				AbstractPersistence.IMPLEMENTATION_CLASS =
						(Class<? extends AbstractPersistence>) loadFactoryClass(UtilImpl.SKYVE_PERSISTENCE_CLASS, "factories.persistenceClass");
			}
		}

		UtilImpl.SKYVE_DYNAMIC_PERSISTENCE_CLASS = getString(FACTORIES_KEY, "dynamicPersistenceClass", factories, false);
		if (AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS == null) {
			if (UtilImpl.SKYVE_DYNAMIC_PERSISTENCE_CLASS == null) {
				LOGGER.info("SET SKYVE DYNAMIC PERSISTENCE CLASS TO DEFAULT (RDBMSDynamicPersistence)");
				AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = RDBMSDynamicPersistence.class;
			}
			else {
				LOGGER.info("SET SKYVE DYNAMIC PERSISTENCE CLASS TO {}", UtilImpl.SKYVE_DYNAMIC_PERSISTENCE_CLASS);
				AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS =
						(Class<? extends DynamicPersistence>) loadFactoryClass(UtilImpl.SKYVE_DYNAMIC_PERSISTENCE_CLASS,
								"factories.dynamicPersistenceClass");
			}
		}
	}

	private static void configureSingletonFactories(Map<String, Object> factories) {
		UtilImpl.SKYVE_CONTENT_MANAGER_CLASS = getString(FACTORIES_KEY, "contentManagerClass", factories, false);

		UtilImpl.SKYVE_NUMBER_GENERATOR_CLASS = getString(FACTORIES_KEY, "numberGeneratorClass", factories, false);
		if (UtilImpl.SKYVE_NUMBER_GENERATOR_CLASS == null) {
			NumberGeneratorStaticSingleton.setDefault();
		}
		else {
			NumberGenerator numberGenerator = instantiateFactory(NumberGenerator.class,
					UtilImpl.SKYVE_NUMBER_GENERATOR_CLASS,
					"factories.numberGeneratorClass");
			NumberGeneratorStaticSingleton.set(numberGenerator);
		}

		UtilImpl.SKYVE_CUSTOMISATIONS_CLASS = getString(FACTORIES_KEY, "customisationsClass", factories, false);
		if (UtilImpl.SKYVE_CUSTOMISATIONS_CLASS == null) {
			CustomisationsStaticSingleton.setDefault();
		}
		else {
			Customisations customisations = instantiateFactory(Customisations.class,
					UtilImpl.SKYVE_CUSTOMISATIONS_CLASS,
					"factories.customisationsClass");
			CustomisationsStaticSingleton.set(customisations);
		}
	}

	private static void configureServiceFactories(Map<String, Object> factories) {
		UtilImpl.SKYVE_GEOIP_SERVICE_CLASS = getString(FACTORIES_KEY, "geoIPServiceClass", factories, false);
		if (UtilImpl.SKYVE_GEOIP_SERVICE_CLASS == null) {
			GeoIPServiceStaticSingleton.setDefault();
		}
		else {
			GeoIPService geoIpService = instantiateFactory(GeoIPService.class,
					UtilImpl.SKYVE_GEOIP_SERVICE_CLASS,
					"factories.geoIPServiceClass");
			GeoIPServiceStaticSingleton.set(geoIpService);
		}

		UtilImpl.SKYVE_SMS_SERVICE_CLASS = getString(FACTORIES_KEY, "smsServiceClass", factories, false);
		if (UtilImpl.SKYVE_SMS_SERVICE_CLASS == null) {
			SMSServiceStaticSingleton.setDefault();
		}
		else {
			SMSService smsService = instantiateFactory(SMSService.class,
					UtilImpl.SKYVE_SMS_SERVICE_CLASS,
					"factories.smsServiceClass");
			SMSServiceStaticSingleton.set(smsService);
		}
	}

	private static void configureBootstrapSettings(Map<String, Object> properties) {
		Map<String, Object> bootstrap = getObject(null, BOOTSTRAP_KEY, properties, false);
		if (bootstrap == null) {
			return;
		}

		UtilImpl.BOOTSTRAP_CUSTOMER = getString(BOOTSTRAP_KEY, "customer", bootstrap, true);
		UtilImpl.BOOTSTRAP_USER = getString(BOOTSTRAP_KEY, "user", bootstrap, true);
		UtilImpl.BOOTSTRAP_EMAIL = getString(BOOTSTRAP_KEY, "email", bootstrap, false);
		if (UtilImpl.BOOTSTRAP_EMAIL == null) {
			UtilImpl.BOOTSTRAP_EMAIL = "pleaseupdate@test.com";
		}
		UtilImpl.BOOTSTRAP_PASSWORD = getString(BOOTSTRAP_KEY, "password", bootstrap, true);
	}

	private static void configurePrimeFlex(ServletContext ctx) {
		String primeFlex = UtilImpl.processStringValue(ctx.getInitParameter("org.skyve.web.faces.PRIMEFLEX"));
		if (primeFlex != null) {
			UtilImpl.PRIMEFLEX = Boolean.parseBoolean(primeFlex);
		}
	}

	private static void configureSecuritySettings(Map<String, Object> properties) {
		Map<String, Object> security = getObject(null, SECURITY_KEY, properties, false);
		if (security == null) {
			return;
		}

		if (security.containsKey("ipAddressChecks")) {
			UtilImpl.IP_ADDRESS_CHECKS = getBoolean(SECURITY_KEY, "ipAddressChecks", security);
		}
		if (security.containsKey("ipAddressHistoryCheckCount")) {
			UtilImpl.IP_ADDRESS_HISTORY_CHECK_COUNT = getInt(SECURITY_KEY, "ipAddressHistoryCheckCount", security);
		}

		UtilImpl.SECURITY_NOTIFICATIONS_EMAIL_ADDRESS = getString(SECURITY_KEY, "securityNotificationsEmail", security, false);
		UtilImpl.GEO_IP_BLOCK_NOTIFICATIONS = getBoolean(SECURITY_KEY, "geoIPBlockNotifications", security);
		UtilImpl.PASSWORD_CHANGE_NOTIFICATIONS = getBoolean(SECURITY_KEY, "passwordChangeNotifications", security);
		UtilImpl.DIFFERENT_COUNTRY_LOGIN_NOTIFICATIONS = getBoolean(SECURITY_KEY, "differentCountryLoginNotifications", security);
		UtilImpl.IP_ADDRESS_CHANGE_NOTIFICATIONS = getBoolean(SECURITY_KEY, "ipAddressChangeNotifications", security);
		UtilImpl.ACCESS_EXCEPTION_NOTIFICATIONS = getBoolean(SECURITY_KEY, "accessExceptionNotifications", security);
		UtilImpl.SECURITY_EXCEPTION_NOTIFICATIONS = getBoolean(SECURITY_KEY, "securityExceptionNotifications", security);
		if (security.containsKey("concurrentSessionWarnings")) {
			UtilImpl.CONCURRENT_SESSION_WARNINGS = getBoolean(SECURITY_KEY, "concurrentSessionWarnings", security);
		}
		if (security.containsKey("concurrentSessionNotifications")) {
			UtilImpl.CONCURRENT_SESSION_NOTIFICATIONS = getBoolean(SECURITY_KEY, "concurrentSessionNotifications", security);
		}
	}

	private static Class<?> loadFactoryClass(String className, String propertyKey) {
		try {
			return Thread.currentThread().getContextClassLoader().loadClass(className);
		}
		catch (ClassNotFoundException e) {
			throw new IllegalStateException("Could not find " + propertyKey + ' ' + className, e);
		}
	}

	@SuppressWarnings("unchecked")
	private static Class<? extends Serializable> loadSerializableClass(String className, String propertyKey) {
		return (Class<? extends Serializable>) loadFactoryClass(className, propertyKey);
	}

	private static <T> T instantiateFactory(Class<T> expectedType, String className, String propertyKey) {
		try {
			Class<?> loadedClass = loadFactoryClass(className, propertyKey);
			Object instance = loadedClass.getDeclaredConstructor().newInstance();
			return expectedType.cast(instance);
		}
		catch (IllegalStateException e) {
			throw e;
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not create " + propertyKey + ' ' + className, e);
		}
	}

	private static void applyOptionalInt(Map<String, Object> properties, String key, String name, IntConsumer setter) {
		Number number = getNumber(key, name, properties, false);
		if (number != null) {
			setter.accept(number.intValue());
		}
	}

	/**
	 * Recursively merges override values into a base configuration map.
	 *
	 * @param overrides the overriding values
	 * @param properties the destination properties to update
	 */
	private static void merge(Map<String, Object> overrides, Map<String, Object> properties) {
		for (Entry<String, Object> entry : overrides.entrySet()) {
			String key = entry.getKey();
			Object override = entry.getValue();
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
			else if ((override instanceof Map<?, ?>) && (original instanceof Map<?, ?>)) {
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
	
	/**
	 * Look up a configuration value and optionally enforce existence.
	 *
	 * @param prefix Optional configuration prefix for error messages.
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @param required Whether the key must exist.
	 * @return The raw configuration value (may be null when not required).
	 */
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

	/**
	 * Retrieve a required boolean configuration value.
	 *
	 * @param prefix Optional configuration prefix for error messages.
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @return The boolean value.
	 */
	private static boolean getBoolean(String prefix, String key, Map<String, Object> properties) {
		Boolean result = (Boolean) get(prefix, key, properties, true);
		return result.booleanValue();
	}
	
	/**
	 * Retrieve a required integer configuration value.
	 *
	 * @param prefix Optional configuration prefix for error messages.
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @return The integer value.
	 */
	private static int getInt(String prefix, String key, Map<String, Object> properties) {
		return getNumber(prefix, key, properties, true).intValue();
	}

	/**
	 * Retrieve a numeric configuration value.
	 *
	 * @param prefix Optional configuration prefix for error messages.
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @param required Whether the key must exist.
	 * @return The numeric value (may be null when not required).
	 */
	private static Number getNumber(String prefix, String key, Map<String, Object> properties, boolean required) {
		return (Number) get(prefix, key, properties, required);
	}

	/**
	 * Retrieve a string configuration value.
	 *
	 * @param prefix Optional configuration prefix for error messages.
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @param required Whether the key must exist.
	 * @return The string value (may be null when not required).
	 */
	private static String getString(String prefix, String key, Map<String, Object> properties, boolean required) {
		return (String) get(prefix, key, properties, required);
	}

	/**
	 * Retrieve a nested object configuration map.
	 *
	 * @param prefix Optional configuration prefix for error messages.
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @param required Whether the key must exist.
	 * @return The nested map (may be null when not required).
	 */
	@SuppressWarnings("unchecked")
	private static Map<String, Object> getObject(String prefix, String key, Map<String, Object> properties, boolean required) {
		return (Map<String, Object>) get(prefix, key, properties, required);
	}
	
	/**
	 * Retrieve a list configuration value.
	 *
	 * @param prefix Optional configuration prefix for error messages.
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @param required Whether the key must exist.
	 * @return The list value (may be null when not required).
	 */
	@SuppressWarnings("unchecked")
	private static List<String> getList(String prefix, String key, Map<String, Object> properties, boolean required) {
		return (List<String>) get(prefix, key, properties, required);
	}
	
	/**
	 * Shutdown Skyve services and notify per-customer shutdown hooks when the
	 * web application context is destroyed.
	 *
	 * @param evt The servlet context event.
	 */
	@Override
	public void contextDestroyed(ServletContextEvent evt) {
		try {
			try {
				try {
					try {
						try {
							try {
								try {
									try {
										try {
											// Notify any observers of the shutdown.
											ProvidedRepository repository = ProvidedRepositoryFactory.get();
											if (UtilImpl.CUSTOMER != null) {
												// if a default customer is specified, only notify that one
												CustomerImpl internalCustomer = (CustomerImpl) repository.getCustomer(UtilImpl.CUSTOMER);
												if (internalCustomer == null) {
													throw new IllegalStateException("UtilImpl.CUSTOMER " + UtilImpl.CUSTOMER + DOES_NOT_EXIST);
												}
												internalCustomer.notifyShutdown();
											} 
											else {
												// notify all customers
												for (String customerName : repository.getAllCustomerNames()) {
													CustomerImpl internalCustomer = (CustomerImpl) repository.getCustomer(customerName);
													if (internalCustomer == null) {
														throw new IllegalStateException("Customer " + customerName + DOES_NOT_EXIST);
													}
													internalCustomer.notifyShutdown();
												}
											}
										}
										finally {
											PushMessage.stopReaper();
										}
									}
									finally {
										// Ensure Two Factor Auth Configuration is finalized
										TwoFactorAuthConfigurationSingleton.getInstance().shutdown();
									}
								}
								finally {
									ArchiveLuceneIndexerSingleton.getInstance().shutdown();
								}
							}
							finally {
								EXT.getJobScheduler().shutdown();
							}
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
						LOGGER.info("Could not close or shutdown of the content manager - this is probably OK although resources may be left hanging or locked", e);
					}
				}
			}
			finally {
				// Ensure the add-in manager is stopped
				EXT.getAddInManager().shutdown();
			}
		}
		finally {
			ProvidedRepositoryFactory.clear();
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
	@SuppressWarnings("java:S1075")
	static String cleanupDirectory(final String path) {
		if ((path != null) && (! path.isEmpty())) {
			String updatedPath = path.replace("\\", "/");

			if (! updatedPath.endsWith("/")) {
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
			throw new IllegalStateException(propertyName + " " + directoryPath + DOES_NOT_EXIST);
		}
		
		if (! directory.isDirectory()) {
			throw new IllegalStateException(propertyName + " " + directoryPath + " is not a directory.");
		}
		
		// Check the directory is writable
		File testFile = new File(directory, "SKYVE_TEST_WRITE_" + UUID.randomUUID().toString());
		try {
			if (! testFile.createNewFile()) {
				throw new IllegalStateException(propertyName + " " + directoryPath + " is not writable.");
			}
		}
		catch (@SuppressWarnings("unused") Exception e) {
			throw new IllegalStateException(propertyName + " " + directoryPath + " is not writable.");
		}
		finally {
			try {
				Files.deleteIfExists(testFile.toPath());
			}
			catch (Exception e) {
				LOGGER.warn("Could not remove temporary writability probe {}", testFile, e);
			}
		}
	}
}
