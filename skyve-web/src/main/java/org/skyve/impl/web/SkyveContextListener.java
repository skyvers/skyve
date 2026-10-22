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
import org.skyve.dialog.Dialogs;
import org.skyve.domain.number.NumberGenerator;
import org.skyve.impl.archive.support.ArchiveLuceneIndexerSingleton;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.dialog.DialogsStaticSingleton;
import org.skyve.impl.domain.number.NumberGeneratorStaticSingleton;
import org.skyve.impl.geoip.GeoIPServiceStaticSingleton;
import org.skyve.impl.job.JobSchedulerStaticSingleton;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.impl.mail.SMTPMailService;
import org.skyve.impl.metadata.controller.CustomisationsStaticSingleton;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
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
	static final String SHUTTING_DOWN_ATTRIBUTE_NAME = SkyveContextListener.class.getName() + ".shuttingDown";

	private static record AppCacheSettings(@Nonnull String cacheName,
									 @Nonnull String type,
									 int heapSizeEntries,
									 long offHeapSize,
									 @Nonnull CacheExpiryPolicy expiryPolicy,
									 long expiry,
									 @Nonnull Class<? extends Serializable> keyClass,
									 @Nonnull Class<? extends Serializable> valueClass,
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
	public void contextInitialized(@SuppressWarnings("null") @Nonnull ServletContextEvent evt) {
		ServletContext ctx = evt.getServletContext();
		populateUtilImpl(ctx);

		final Caching caching = EXT.getCaching();
		caching.startup();
		try {
			EXT.getAddInManager().startup();
			
			// ensure that the schema is created before trying to init the job scheduler
			initialiseSchema();
			
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

			// Notify observers before metadata validation.
			ProvidedRepository.notifyAllCustomersObservers(c -> ((CustomerImpl) c).notifyStartup());
			
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
	public static void populateUtilImpl(@Nonnull ServletContext ctx) {
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

		Map<String, Object> content = getRequiredObject(null, CONTENT_KEY, properties);
		UtilImpl.CONTENT_DIRECTORY = getRequiredString(CONTENT_KEY, DIRECTORY_KEY, content);
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

	private static void initialiseSchema() {
		AbstractPersistence persistence = null;
		try {
			persistence = (AbstractPersistence) CORE.getPersistence();
			persistence.begin();
		}
		catch (Exception e) {
			if (persistence != null) {
				persistence.rollback();
			}
			throw new IllegalStateException("Cannot initialise the data schema.", e);
		}
		finally {
			if (persistence != null) {
				persistence.commit(true);
			}
		}
	}

	private static void validateRequiredFilters(@Nonnull ServletContext ctx) {
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
				// NB it would b good to test that the filter has REQUEST and ERROR dispatcher types, but the Servlet API doesn't provide a way to get that information from the FilterRegistration object.
			}
		}
		if (! securityHeadersFilterExists) {
			throw new IllegalStateException("A Filter <filter-name>SecurityHeadersFilter</filter-name> of <filter-class>org.skyve.impl.web.filter.ResponseHeaderFilter</filter-class> is required in web.xml.");
		}
	}

	private static @Nonnull Map<String, Object> readExpandedConfiguration(@Nonnull File file) {
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
	private static void configureArchiveProperties(@Nonnull Map<String, Object> properties) {
        String archKey = "archive";

        Map<String, Object> archiveProps = getOptionalObject(archKey, properties);
        if (archiveProps == null) {
            LOGGER.info("Archiving is not configured");
            return;
        }

        if (isMultiTenant()) {
            LOGGER.warn("Archiving is configured, but this appears to be a multi-tenancy instance");
            throw new IllegalArgumentException("Archiving is not supported on multi-tenancy instances");
        }

		int runtime = getRequiredNumber(archKey, "exportRuntimeSec", archiveProps).intValue();
		int batchSize = getRequiredNumber(archKey, "exportBatchSize", archiveProps).intValue();

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> docProps = (List<Map<String, Object>>) getRequired(archKey, "documents", archiveProps);

        List<ArchiveConfig.ArchiveDocConfig> docConfigs = new ArrayList<>();
        for (Map<String, Object> docProp : docProps) {

            String module = getRequiredString(null, "module", docProp);
            String document = getRequiredString(null, "document", docProp);
			String directory = getRequiredString(null, DIRECTORY_KEY, docProp);
            int retainDeletedDocumentsDays = getInt(null, "retainDeletedDocumentsDays", docProp);

            docConfigs.add(new ArchiveDocConfig(module, document, directory, retainDeletedDocumentsDays));
        }

        // Setup the archive doc cache, with some defaults
        ArchivedDocumentCacheConfig cacheConfig = ArchivedDocumentCacheConfig.DEFAULT;

        Map<String, Object> cacheProps = getOptionalObject("cache", archiveProps);
        if (cacheProps != null) {

            final String cacheKey = archKey + ".cache";

			Number heapSizeEntriesNumber = getOptionalNumber(HEAP_SIZE_ENTRIES_KEY, cacheProps);
			long heapSizeEntries = (heapSizeEntriesNumber == null) ? 100L : heapSizeEntriesNumber.longValue();
			Number expiryInMinutesNumber = getOptionalNumber(EXPIRY_TIME_MINUTES_KEY, cacheProps);
			long expiryInMinutes = (expiryInMinutesNumber == null) ? 10L : expiryInMinutesNumber.longValue();

            cacheConfig = new ArchivedDocumentCacheConfig(heapSizeEntries, expiryInMinutes);
        }

        ArchiveSchedule schedule = ArchiveSchedule.DEFUALT;
        Map<String, Object> scheduleSettings = getOptionalObject("schedule", archiveProps);
        if (scheduleSettings != null) {
            final String key = archKey + ".schedule";

            String cron = getRequiredString(key, "cron", scheduleSettings);
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
	static void configureMailServiceAndSmtp(@Nonnull Map<String, Object> properties, @Nonnull Map<String, Object> factories) {
		boolean smtpRequired = configureMailService(factories);

		Map<String, Object> smtp = (smtpRequired ? getRequiredObject(null, SMTP_KEY, properties) : getOptionalObject(SMTP_KEY, properties));
		if (smtp == null) {
			return;
		}

		UtilImpl.SMTP = (smtpRequired ? getRequiredString(SMTP_KEY, "server", smtp) : getOptionalString("server", smtp));
		Number smtpPort = (smtpRequired ? getRequiredNumber(SMTP_KEY, "port", smtp) : getOptionalNumber("port", smtp));
		UtilImpl.SMTP_PORT = (smtpPort == null) ? 0 : smtpPort.intValue();
		UtilImpl.SMTP_UID = getOptionalString("uid", smtp);
		UtilImpl.SMTP_PWD = getOptionalString("pwd", smtp);

		UtilImpl.SMTP_PROPERTIES = toStringMap(getOptionalObject("properties", smtp));

		UtilImpl.SMTP_HEADERS = toStringMap(getOptionalObject("headers", smtp));

		UtilImpl.SMTP_SENDER = (smtpRequired ? getRequiredString(SMTP_KEY, "sender", smtp) : getOptionalString("sender", smtp));
		UtilImpl.SMTP_TEST_RECIPIENT = getOptionalString("testRecipient", smtp);
		Boolean smtpTestBogusSend = (Boolean) (smtpRequired ? getRequired(SMTP_KEY, "testBogusSend", smtp) : getOptional("testBogusSend", smtp));
		UtilImpl.SMTP_TEST_BOGUS_SEND = Boolean.TRUE.equals(smtpTestBogusSend);
	}

	private static boolean configureMailService(@Nonnull Map<String, Object> factories) {
		UtilImpl.SKYVE_MAIL_SERVICE_CLASS = getOptionalString("mailServiceClass", factories);
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

	private static @Nonnull TreeMap<String, String> toStringMap(@Nullable Map<String, Object> values) {
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

	private static void configureTraceSettings(@Nonnull Map<String, Object> properties) {
		Map<String, Object> trace = getRequiredObject(null, TRACE_KEY, properties);
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

	private static void configureContentSettings(@Nonnull Map<String, Object> content) {
		UtilImpl.CONTENT_GC_CRON = getRequiredString(CONTENT_KEY, "gcCron", content);
		Number gcEligibleAgeMinutes = getOptionalNumber("gcEligibleAgeMinutes", content);
		if (gcEligibleAgeMinutes != null) {
			UtilImpl.CONTENT_GC_ELIGIBLE_AGE_MINUTES = gcEligibleAgeMinutes.intValue();
		}
		UtilImpl.CONTENT_JDBC_SERVER_ARGS = getOptionalString("serverArgs", content);
		UtilImpl.CONTENT_REST_SERVER_URL = getOptionalString("serverUrl", content);
		UtilImpl.CONTENT_FILE_STORAGE = getBoolean(CONTENT_KEY, "fileStorage", content);
		UtilImpl.CONTENT_FILE_SUFFIXES = getBoolean(CONTENT_KEY, "fileSuffixes", content);
	}

	private static void configureBackupSettings(@Nonnull Map<String, Object> properties) {
		Map<String, Object> backup = getOptionalObject(BACKUP_KEY, properties);
		if (backup == null) {
			return;
		}

		UtilImpl.BACKUP_DIRECTORY = getOptionalString(DIRECTORY_KEY, backup);
		if (UtilImpl.BACKUP_DIRECTORY != null) {
			UtilImpl.BACKUP_DIRECTORY = cleanupDirectory(UtilImpl.BACKUP_DIRECTORY);
			testWritableDirectory(BACKUP_KEY + '.' + DIRECTORY_KEY, UtilImpl.BACKUP_DIRECTORY);
		}
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = getOptionalString("externalBackupClass", backup);
		UtilImpl.BACKUP_PROPERTIES = getOptionalObject("properties", backup);
		Number restoreMaxExtractEntries = getOptionalNumber("restoreMaxExtractEntries", backup);
		if (restoreMaxExtractEntries != null) {
			UtilImpl.BACKUP_RESTORE_MAX_EXTRACT_ENTRIES = restoreMaxExtractEntries.intValue();
		}
		Number restoreMaxExtractSizeMB = getOptionalNumber("restoreMaxExtractSizeMB", backup);
		if (restoreMaxExtractSizeMB != null) {
			UtilImpl.BACKUP_RESTORE_MAX_EXTRACT_SIZE_MB = restoreMaxExtractSizeMB.intValue();
		}
	}

	private static void configureUploadsSettings(@Nonnull Map<String, Object> properties) {
		Map<String, Object> uploads = getOptionalObject(UPLOADS_KEY, properties);
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
				"video",
				value -> UtilImpl.UPLOADS_VIDEO_WHITELIST_REGEX = value,
				value -> UtilImpl.UPLOADS_VIDEO_MAXIMUM_SIZE_IN_MB = value);
		configureUploadCategory(uploads,
				"bizport",
				value -> UtilImpl.UPLOADS_BIZPORT_WHITELIST_REGEX = value,
				value -> UtilImpl.UPLOADS_BIZPORT_MAXIMUM_SIZE_IN_MB = value);
		// facesServlet.setMultipartConfig() doesn't exist unless registering the servlet yourself.
		// Multipart upload limits are therefore applied through container configuration instead.
	}

	private static void configurePushSettings(@Nonnull Map<String, Object> properties) {
		Map<String, Object> push = getOptionalObject(PUSH_KEY, properties);
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

	private static void configureAddinsSettings(@Nonnull Map<String, Object> properties) {
		Map<String, Object> addins = getOptionalObject("addins", properties);
		if (addins == null) {
			return;
		}

		UtilImpl.ADDINS_DIRECTORY = getOptionalString(DIRECTORY_KEY, addins);
		if (UtilImpl.ADDINS_DIRECTORY != null) {
			UtilImpl.ADDINS_DIRECTORY = cleanupDirectory(UtilImpl.ADDINS_DIRECTORY);
			testWritableDirectory("addins.directory", UtilImpl.ADDINS_DIRECTORY);
		}
	}

	private static void configureThumbnailSettings(@Nonnull Map<String, Object> properties) {
		Map<String, Object> thumbnail = getOptionalObject(THUMBNAIL_KEY, properties);
		if (thumbnail == null) {
			return;
		}

		UtilImpl.THUMBNAIL_CONCURRENT_THREADS = getInt(THUMBNAIL_KEY, "concurrentThreads", thumbnail);
		UtilImpl.THUMBNAIL_SUBSAMPLING_MINIMUM_TARGET_SIZE = getInt(THUMBNAIL_KEY, "subsamplingMinimumTargetSize", thumbnail);
		UtilImpl.THUMBNAIL_FILE_STORAGE = getBoolean(THUMBNAIL_KEY, "fileStorage", thumbnail);
		UtilImpl.THUMBNAIL_DIRECTORY = getOptionalString(DIRECTORY_KEY, thumbnail);
		if (UtilImpl.THUMBNAIL_DIRECTORY != null) {
			UtilImpl.THUMBNAIL_DIRECTORY = cleanupDirectory(UtilImpl.THUMBNAIL_DIRECTORY);
			testWritableDirectory(THUMBNAIL_KEY + '.' + DIRECTORY_KEY, UtilImpl.THUMBNAIL_DIRECTORY);
		}
	}

	private static void configureUrlSettings(@Nonnull Map<String, Object> properties) {
		Map<String, Object> url = getRequiredObject(null, URL_KEY, properties);
		UtilImpl.SERVER_URL = getRequiredString(URL_KEY, "server", url);
		UtilImpl.SKYVE_CONTEXT = getRequiredString(URL_KEY, "context", url);
		UtilImpl.HOME_URI = getRequiredString(URL_KEY, "home", url);
	}

	private static void configureStateSettings(@Nonnull Map<String, Object> properties) {
		Map<String, Object> state = getRequiredObject(null, STATE_KEY, properties);
		UtilImpl.CACHE_DIRECTORY = getOptionalString(DIRECTORY_KEY, state);
		if (UtilImpl.CACHE_DIRECTORY != null) {
			UtilImpl.CACHE_DIRECTORY = cleanupDirectory(UtilImpl.CACHE_DIRECTORY);
			testWritableDirectory(STATE_KEY + '.' + DIRECTORY_KEY, UtilImpl.CACHE_DIRECTORY);
		}

		UtilImpl.CACHE_MULTIPLE = Boolean.TRUE.equals(getOptional("multiple", state));
		Map<String, Object> conversations = getRequiredObject(STATE_KEY, "conversations", state);
		UtilImpl.CONVERSATION_CACHE = new ConversationCacheConfig(getInt(STATE_CONVERSATIONS_KEY, HEAP_SIZE_ENTRIES_KEY, conversations),
													getInt(STATE_CONVERSATIONS_KEY, OFF_HEAP_SIZE_MB_KEY, conversations),
													toDiskSizeMb(STATE_CONVERSATIONS_KEY, conversations),
													getInt(STATE_CONVERSATIONS_KEY, EXPIRY_TIME_MINUTES_KEY, conversations));
		Map<String, Object> tokens = getRequiredObject(STATE_KEY, "csrfTokens", state);
		UtilImpl.CSRF_TOKEN_CACHE = new CSRFTokenCacheConfig(getInt(STATE_CSRF_TOKENS_KEY, HEAP_SIZE_ENTRIES_KEY, tokens),
											getInt(STATE_CSRF_TOKENS_KEY, OFF_HEAP_SIZE_MB_KEY, tokens),
											toDiskSizeMb(STATE_CSRF_TOKENS_KEY, tokens),
											getInt(STATE_CSRF_TOKENS_KEY, EXPIRY_TIME_MINUTES_KEY, tokens));
		Map<String, Object> sessions = getRequiredObject(STATE_KEY, "sessions", state);
		UtilImpl.SESSION_CACHE = new SessionCacheConfig(getInt(STATE_SESSIONS_KEY, HEAP_SIZE_ENTRIES_KEY, sessions),
										getInt(STATE_SESSIONS_KEY, OFF_HEAP_SIZE_MB_KEY, sessions),
										toDiskSizeMb(STATE_SESSIONS_KEY, sessions),
										getInt(STATE_SESSIONS_KEY, EXPIRY_TIME_MINUTES_KEY, sessions));
		Map<String, Object> geoIps = getRequiredObject(STATE_KEY, "geoIPs", state);
		UtilImpl.GEO_IP_CACHE = new GeoIPCacheConfig(getInt(STATE_GEO_IPS_KEY, HEAP_SIZE_ENTRIES_KEY, geoIps),
										getInt(STATE_GEO_IPS_KEY, OFF_HEAP_SIZE_MB_KEY, geoIps),
										toDiskSizeMb(STATE_GEO_IPS_KEY, geoIps),
										getInt(STATE_GEO_IPS_KEY, EXPIRY_TIME_MINUTES_KEY, geoIps));
		UtilImpl.STATE_EVICT_CRON = getOptionalString("evictCron", state);
	}

	private static void configureUploadCategory(@Nonnull Map<String, Object> uploads,
									 @Nonnull String category,
									 @Nonnull Consumer<String> regexSetter,
									 @Nonnull IntConsumer maximumSizeSetter) {
		Map<String, Object> values = getOptionalObject(category, uploads);
		if (values == null) {
			return;
		}

		String uploadKey = UPLOADS_KEY + '.' + category;
		String whitelistRegex = Util.processStringValue(getOptionalString(WHITELIST_REGEX_KEY, values));
		if (whitelistRegex != null) {
			validateRegex(uploadKey + '.' + WHITELIST_REGEX_KEY, whitelistRegex);
			regexSetter.accept(whitelistRegex);
		}

		Number maximumSizeMB = getOptionalNumber(MAXIMUM_SIZE_MB_KEY, values);
		if (maximumSizeMB != null) {
			maximumSizeSetter.accept(maximumSizeMB.intValue());
		}
	}

	private static long toDiskSizeMb(@Nonnull String key, @Nonnull Map<String, Object> values) {
		return getInt(key, DISK_SIZE_GB_KEY, values) * 1024L;
	}

	private static void validateRegex(@Nonnull String key, @Nonnull String regex) {
		try {
			Pattern.compile(regex);
		}
		catch (PatternSyntaxException e) {
			throw new IllegalStateException(key + " is not a valid regex pattern", e);
		}
	}

	private static void configureMapSettings(@Nonnull Map<String, Object> properties) {
		Map<String, Object> map = getRequiredObject(null, MAP_KEY, properties);
		String value = getRequiredString(MAP_KEY, "type", map);
		UtilImpl.MAP_TYPE = MapType.valueOf(value);
		UtilImpl.MAP_LAYERS = getRequiredString(MAP_KEY, "layers", map);
		UtilImpl.MAP_CENTRE = getOptionalString("centre", map);
		Number zoom = getOptionalNumber("zoom", map);
		if (zoom != null) {
			UtilImpl.MAP_ZOOM = zoom.intValue();
		}
	}

	private static void configureAccountSettings(@Nonnull Map<String, Object> properties) {
		Map<String, Object> account = getRequiredObject(null, ACCOUNT_KEY, properties);
		UtilImpl.PASSWORD_HASHING_ALGORITHM = getRequiredString(ACCOUNT_KEY, "passwordHashingAlgorithm", account);
		applyOptionalInt(account, ACCOUNT_KEY, "passwordExpiryInDays", value -> UtilImpl.PASSWORD_EXPIRY_IN_DAYS = value);
		applyOptionalInt(account, ACCOUNT_KEY, "passwordHistoryRetention", value -> UtilImpl.PASSWORD_HISTORY_RETENTION = value);
		applyOptionalInt(account, ACCOUNT_KEY, "accountLockoutThreshold", value -> UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD = value);
		applyOptionalInt(account,
				ACCOUNT_KEY,
				"accountLockoutDurationMultipleInSeconds",
				value -> UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS = value);

		String loginUri = getOptionalString("loginUri", account);
		if (loginUri != null) {
			UtilImpl.AUTHENTICATION_LOGIN_URI = loginUri;
		}

		UtilImpl.ACCOUNT_ALLOW_SELF_REGISTRATION = getBoolean(ACCOUNT_KEY, "allowUserSelfRegistration", account);
		UtilImpl.AUTHENTICATION_GOOGLE_CLIENT_ID = getOptionalString("googleAuthClientId", account);
		UtilImpl.AUTHENTICATION_GOOGLE_SECRET = getOptionalString("googleAuthSecret", account);
		UtilImpl.AUTHENTICATION_FACEBOOK_CLIENT_ID = getOptionalString("facebookAuthClientId", account);
		UtilImpl.AUTHENTICATION_FACEBOOK_SECRET = getOptionalString("facebookAuthSecret", account);
		UtilImpl.AUTHENTICATION_GITHUB_CLIENT_ID = getOptionalString("githubAuthClientId", account);
		UtilImpl.AUTHENTICATION_GITHUB_SECRET = getOptionalString("githubAuthSecret", account);
		UtilImpl.AUTHENTICATION_AZUREAD_CLIENT_ID = getOptionalString("azureAdAuthClientId", account);
		UtilImpl.AUTHENTICATION_AZUREAD_TENANT_ID = getOptionalString("azureAdAuthTenantId", account);
		UtilImpl.AUTHENTICATION_AZUREAD_SECRET = getOptionalString("azureAdAuthSecret", account);
		applyOptionalInt(account, ACCOUNT_KEY, "rememberMeTokenTimeoutHours", value -> UtilImpl.REMEMBER_ME_TOKEN_TIMEOUT_HOURS = value);

		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = new HashSet<>();
		List<String> tfaCustomers = getOptionalList("tfaCustomers", account);
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

	private static void configureEnvironmentSettings(@Nonnull Map<String, Object> properties) {
		Map<String, Object> environment = getRequiredObject(null, ENVIRONMENT_KEY, properties);
		UtilImpl.ENVIRONMENT_IDENTIFIER = getOptionalString("identifier", environment);

		if ((UtilImpl.ENVIRONMENT_IDENTIFIER == null) && UtilImpl.DEV_LOGIN_FILTER_USED) {
			LOGGER.error(WARNING_BANNER);
			LOGGER.error("DevLoginFilter is in use in prod!! - stopping deployment...");
			LOGGER.error("The DevLoginFilter ({}) should not be used in prod - see web.xml", DEV_LOGIN_FILTER_CLASS_NAME);
			LOGGER.warn(WARNING_BANNER);
			throw new IllegalStateException("The DevLoginFilter (" + DEV_LOGIN_FILTER_CLASS_NAME + ") should not be used in prod - see web.xml");
		}

		UtilImpl.DEV_MODE = getBoolean(ENVIRONMENT_KEY, "devMode", environment);
		Boolean accessControl = (Boolean) getOptional("accessControl", environment);
		if (accessControl != null) {
			UtilImpl.ACCESS_CONTROL = accessControl.booleanValue();
		}

		UtilImpl.CUSTOMER = getOptionalString("customer", environment);
		UtilImpl.JOB_SCHEDULER = getBoolean(ENVIRONMENT_KEY, "jobScheduler", environment);
		UtilImpl.APPS_JAR_DIRECTORY = getOptionalString("appsJarDirectory", environment);
		UtilImpl.MODULE_DIRECTORY = getOptionalString("moduleDirectory", environment);
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

		UtilImpl.SUPPORT_EMAIL_ADDRESS = getOptionalString("supportEmailAddress", environment);
		UtilImpl.SHOW_SETUP = getBoolean(ENVIRONMENT_KEY, "showSetup", environment);
	}

	private static void configureHealthSettings(@Nonnull Map<String, Object> properties) {
		Map<String, Object> health = getOptionalObject(HEALTH_KEY, properties);
		if (health == null) {
			return;
		}

		UtilImpl.HEALTH_CHECK = getBoolean(HEALTH_KEY, "check", health);
		UtilImpl.HEALTH_CACHE_TIME_IN_SECONDS = getInt(HEALTH_KEY, "cacheTimeInSeconds", health);
	}

	private static void configureApiSettings(@Nonnull Map<String, Object> properties) {
		Map<String, Object> api = getRequiredObject(null, API_KEY, properties);
		UtilImpl.CHECK_FOR_BREACHED_PASSWORD = Boolean.TRUE.equals(getOptional("checkForBreachedPassword", api));
		UtilImpl.GOOGLE_MAPS_V3_API_KEY = getOptionalString("googleMapsV3Key", api);
		UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY = getOptionalString("googleRecaptchaSiteKey", api);
		UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY = getOptionalString("googleRecaptchaSecretKey", api);
		UtilImpl.CLOUDFLARE_TURNSTILE_SITE_KEY = getOptionalString("cloudflareTurnstileSiteKey", api);
		UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY = getOptionalString("cloudflareTurnstileSecretKey", api);
		UtilImpl.GEO_IP_KEY = getOptionalString("geoIPKey", api);

		String geoIpCountryCodes = getOptionalString("geoIPCountryCodes", api);
		if (geoIpCountryCodes != null) {
			String[] codes = geoIpCountryCodes.split("\\|");
			UtilImpl.GEO_IP_COUNTRY_CODES = new CopyOnWriteArraySet<>(Arrays.asList(codes));
		}

		Boolean geoIpWhitelist = (Boolean) getOptional("geoIPWhitelist", api);
		if (geoIpWhitelist != null) {
			UtilImpl.GEO_IP_WHITELIST = geoIpWhitelist.booleanValue();
		}

		UtilImpl.CKEDITOR_CONFIG_FILE_URL = getOptionalString("ckEditorConfigFileUrl", api);
		if (UtilImpl.CKEDITOR_CONFIG_FILE_URL == null) {
			UtilImpl.CKEDITOR_CONFIG_FILE_URL = "";
		}
	}

	private static void configureApplicationCaches(@Nonnull Map<String, Object> properties) {
		Map<String, Object> caches = getOptionalObject(CACHES_KEY, properties);
		if (caches != null) {
			for (String cacheName : caches.keySet()) {
				Map<String, Object> cache = getRequiredObject(CACHES_KEY, cacheName, caches);
				String prefix = String.format("caches.%s", cacheName);
				String type = getRequiredString(CACHES_KEY + '.' + cacheName, "type", cache);
				int heapSizeEntries = getInt(prefix, HEAP_SIZE_ENTRIES_KEY, cache);
				Number offHeapSizeInMb = getOptionalNumber(OFF_HEAP_SIZE_MB_KEY, cache);
				Number expiryInMinutes = getOptionalNumber(EXPIRY_TIME_MINUTES_KEY, cache);
				Number diskSizeInGb = getOptionalNumber(DISK_SIZE_GB_KEY, cache);
				String keyClassName = getRequiredString(prefix, "keyClass", cache);
				String valueClassName = getRequiredString(prefix, "valueClass", cache);
				String expiryPolicyString = getOptionalString("expiryPolicy", cache);
				Boolean persistent = (Boolean) getOptional("persistent", cache);
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

	private static void addApplicationCache(@Nonnull AppCacheSettings settings) {
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

	private static void configureDataStores(@Nonnull Map<String, Object> properties) {
		Map<String, Object> dataStores = getRequiredObject(null, "dataStores", properties);
		for (String dataStoreName : dataStores.keySet()) {
			Map<String, Object> dataStore = getRequiredObject("dataStores", dataStoreName, dataStores);
			String prefix = String.format("dataStores.%s", dataStoreName);
			String dialect = getRequiredString(prefix, "dialect", dataStore);
			String jndi = getOptionalString("jndi", dataStore);
			if (jndi == null) {
				UtilImpl.DATA_STORES.put(dataStoreName,
						new DataStore(getRequiredString(prefix, "driver", dataStore),
								getRequiredString(prefix, URL_KEY, dataStore),
								getOptionalString("user", dataStore),
								getOptionalString("password", dataStore),
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

	private static void configureHibernateSettings(@Nonnull Map<String, Object> properties) {
		Map<String, Object> hibernate = getRequiredObject(null, HIBERNATE_KEY, properties);
		String dataStoreName = getRequiredString(HIBERNATE_KEY, "dataStore", hibernate);
		UtilImpl.DATA_STORE = UtilImpl.DATA_STORES.get(dataStoreName);
		if (UtilImpl.DATA_STORE == null) {
			throw new IllegalStateException("hibernate.dataStore " + dataStoreName + " is not defined in dataStores");
		}
		UtilImpl.DDL_SYNC = getBoolean(HIBERNATE_KEY, "ddlSync", hibernate);
		UtilImpl.CATALOG = getOptionalString("catalog", hibernate);
		UtilImpl.SCHEMA = getOptionalString("schema", hibernate);
		UtilImpl.PRETTY_SQL_OUTPUT = getBoolean(HIBERNATE_KEY, "prettySql", hibernate);

		Map<String, Object> hibernateCaches = getOptionalObject(CACHES_KEY, hibernate);
		if (hibernateCaches != null) {
			for (String cacheName : hibernateCaches.keySet()) {
				Map<String, Object> cache = getRequiredObject("hibernate.caches", cacheName, hibernateCaches);
				String prefix = String.format("hibernate.caches.%s", cacheName);
				int heapSizeEntries = getInt(prefix, HEAP_SIZE_ENTRIES_KEY, cache);
				Number offHeapSizeInMb = getOptionalNumber(OFF_HEAP_SIZE_MB_KEY, cache);
				String expiryPolicyString = getOptionalString("expiryPolicy", cache);
				Number expiryInMinutes = getOptionalNumber(EXPIRY_TIME_MINUTES_KEY, cache);
				UtilImpl.HIBERNATE_CACHES.add(new HibernateCacheConfig(cacheName,
														heapSizeEntries,
														(offHeapSizeInMb == null) ? 0L : offHeapSizeInMb.longValue(),
														(expiryPolicyString == null) ? CacheExpiryPolicy.eternal : CacheExpiryPolicy.valueOf(expiryPolicyString),
														(expiryInMinutes == null) ? 0L : expiryInMinutes.longValue()));
			}
		}

		UtilImpl.HIBERNATE_FAIL_ON_MISSING_CACHE = true;
	}

	private static void configureFactorySettings(@Nonnull Map<String, Object> properties) {
		Map<String, Object> factories = getRequiredObject(null, FACTORIES_KEY, properties);
		configureRepositoryFactory(factories);
		configurePersistenceFactories(factories);
		configureSingletonFactories(factories);

		Customisations customisations = CustomisationsStaticSingleton.get();
		customisations.registerCustomExpressions();
		customisations.registerCustomFormatters();

		configureMailServiceAndSmtp(properties, factories);
		configureServiceFactories(factories);
	}

	private static void configureRepositoryFactory(@Nonnull Map<String, Object> factories) {
		UtilImpl.SKYVE_REPOSITORY_CLASS = getOptionalString("repositoryClass", factories);
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
	private static void configurePersistenceFactories(@Nonnull Map<String, Object> factories) {
		UtilImpl.SKYVE_PERSISTENCE_CLASS = getOptionalString("persistenceClass", factories);
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

		UtilImpl.SKYVE_DYNAMIC_PERSISTENCE_CLASS = getOptionalString("dynamicPersistenceClass", factories);
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

	private static void configureSingletonFactories(@Nonnull Map<String, Object> factories) {
		UtilImpl.SKYVE_CONTENT_MANAGER_CLASS = getOptionalString("contentManagerClass", factories);

		UtilImpl.SKYVE_NUMBER_GENERATOR_CLASS = getOptionalString("numberGeneratorClass", factories);
		if (UtilImpl.SKYVE_NUMBER_GENERATOR_CLASS == null) {
			NumberGeneratorStaticSingleton.setDefault();
		}
		else {
			NumberGenerator numberGenerator = instantiateFactory(NumberGenerator.class,
					UtilImpl.SKYVE_NUMBER_GENERATOR_CLASS,
					"factories.numberGeneratorClass");
			NumberGeneratorStaticSingleton.set(numberGenerator);
		}

		UtilImpl.SKYVE_CUSTOMISATIONS_CLASS = getOptionalString("customisationsClass", factories);
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

	/**
	 * Installs the configured or default dialogs, GeoIP and SMS services.
	 *
	 * @param factories	The {@code factories} configuration; never {@code null}.
	 */
	private static void configureServiceFactories(@Nonnull Map<String, Object> factories) {
		UtilImpl.SKYVE_DIALOGS_CLASS = getOptionalString("dialogsClass", factories);
		if (UtilImpl.SKYVE_DIALOGS_CLASS == null) {
			DialogsStaticSingleton.setDefault();
		}
		else {
			Dialogs dialogs = instantiateFactory(Dialogs.class,
													UtilImpl.SKYVE_DIALOGS_CLASS,
													"factories.dialogsClass");
			DialogsStaticSingleton.set(dialogs);
		}

		UtilImpl.SKYVE_GEOIP_SERVICE_CLASS = getOptionalString("geoIPServiceClass", factories);
		if (UtilImpl.SKYVE_GEOIP_SERVICE_CLASS == null) {
			GeoIPServiceStaticSingleton.setDefault();
		}
		else {
			GeoIPService geoIpService = instantiateFactory(GeoIPService.class,
															UtilImpl.SKYVE_GEOIP_SERVICE_CLASS,
															"factories.geoIPServiceClass");
			GeoIPServiceStaticSingleton.set(geoIpService);
		}

		UtilImpl.SKYVE_SMS_SERVICE_CLASS = getOptionalString("smsServiceClass", factories);
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

	private static void configureBootstrapSettings(@Nonnull Map<String, Object> properties) {
		Map<String, Object> bootstrap = getOptionalObject(BOOTSTRAP_KEY, properties);
		if (bootstrap == null) {
			return;
		}

		UtilImpl.BOOTSTRAP_CUSTOMER = getRequiredString(BOOTSTRAP_KEY, "customer", bootstrap);
		UtilImpl.BOOTSTRAP_USER = getRequiredString(BOOTSTRAP_KEY, "user", bootstrap);
		UtilImpl.BOOTSTRAP_EMAIL = getOptionalString("email", bootstrap);
		if (UtilImpl.BOOTSTRAP_EMAIL == null) {
			UtilImpl.BOOTSTRAP_EMAIL = "pleaseupdate@test.com";
		}
		UtilImpl.BOOTSTRAP_PASSWORD = getRequiredString(BOOTSTRAP_KEY, "password", bootstrap);
	}

	private static void configurePrimeFlex(@Nonnull ServletContext ctx) {
		String primeFlex = UtilImpl.processStringValue(ctx.getInitParameter("org.skyve.web.faces.PRIMEFLEX"));
		if (primeFlex != null) {
			UtilImpl.PRIMEFLEX = Boolean.parseBoolean(primeFlex);
		}
	}

	private static void configureSecuritySettings(@Nonnull Map<String, Object> properties) {
		Map<String, Object> security = getOptionalObject(SECURITY_KEY, properties);
		if (security == null) {
			return;
		}

		if (security.containsKey("ipAddressChecks")) {
			UtilImpl.IP_ADDRESS_CHECKS = getBoolean(SECURITY_KEY, "ipAddressChecks", security);
		}
		if (security.containsKey("ipAddressHistoryCheckCount")) {
			UtilImpl.IP_ADDRESS_HISTORY_CHECK_COUNT = getInt(SECURITY_KEY, "ipAddressHistoryCheckCount", security);
		}

		UtilImpl.SECURITY_NOTIFICATIONS_EMAIL_ADDRESS = getOptionalString("securityNotificationsEmail", security);
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

	private static @Nonnull Class<?> loadFactoryClass(@Nonnull String className, @Nonnull String propertyKey) {
		try {
			return Thread.currentThread().getContextClassLoader().loadClass(className);
		}
		catch (ClassNotFoundException e) {
			throw new IllegalStateException("Could not find " + propertyKey + ' ' + className, e);
		}
	}

	@SuppressWarnings("unchecked")
	private static @Nonnull Class<? extends Serializable> loadSerializableClass(@Nonnull String className, @Nonnull String propertyKey) {
		return (Class<? extends Serializable>) loadFactoryClass(className, propertyKey);
	}

	private static @Nonnull <T> T instantiateFactory(@Nonnull Class<T> expectedType, @Nonnull String className, @Nonnull String propertyKey) {
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

	private static void applyOptionalInt(@Nonnull Map<String, Object> properties, @Nonnull String key, @Nonnull String name, @Nonnull IntConsumer setter) {
		Number number = getOptionalNumber(name, properties);
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
	private static void merge(@Nonnull Map<String, Object> overrides, @Nonnull Map<String, Object> properties) {
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
	 * Look up a configuration value that must exist.
	 *
	 * @param prefix Optional configuration prefix for error messages.
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @return The raw configuration value.
	 * @throws IllegalStateException if the key does not exist.
	 */
	private static @Nonnull Object getRequired(@Nullable String prefix, @Nonnull String key, @Nonnull Map<String, Object> properties) {
		Object result = properties.get(key);
		if (result == null) {
			if (prefix != null) {
				throw new IllegalStateException(String.format("Property %s.%s does not exist in the JSON configuration.", prefix, key));
			}
			throw new IllegalStateException(String.format("Property %s does not exist in the JSON configuration.", key));
		}
		return result;
	}

	/**
	 * Look up an optional configuration value.
	 *
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @return The raw configuration value, or null if the key does not exist.
	 */
	private static @Nullable Object getOptional(@Nonnull String key, @Nonnull Map<String, Object> properties) {
		return properties.get(key);
	}

	/**
	 * Retrieve a required boolean configuration value.
	 *
	 * @param prefix Optional configuration prefix for error messages.
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @return The boolean value.
	 */
	private static boolean getBoolean(@Nullable String prefix, @Nonnull String key, @Nonnull Map<String, Object> properties) {
		return ((Boolean) getRequired(prefix, key, properties)).booleanValue();
	}
	
	/**
	 * Retrieve a required integer configuration value.
	 *
	 * @param prefix Optional configuration prefix for error messages.
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @return The integer value.
	 */
	private static int getInt(@Nullable String prefix, @Nonnull String key, @Nonnull Map<String, Object> properties) {
		return getRequiredNumber(prefix, key, properties).intValue();
	}

	/**
	 * Retrieve a required numeric configuration value.
	 *
	 * @param prefix Optional configuration prefix for error messages.
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @return The numeric value.
	 * @throws IllegalStateException if the key does not exist.
	 */
	private static @Nonnull Number getRequiredNumber(@Nullable String prefix, @Nonnull String key, @Nonnull Map<String, Object> properties) {
		return (Number) getRequired(prefix, key, properties);
	}

	/**
	 * Retrieve an optional numeric configuration value.
	 *
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @return The numeric value, or null if the key does not exist.
	 */
	private static @Nullable Number getOptionalNumber(@Nonnull String key, @Nonnull Map<String, Object> properties) {
		return (Number) getOptional(key, properties);
	}

	/**
	 * Retrieve a required string configuration value.
	 *
	 * @param prefix Optional configuration prefix for error messages.
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @return The string value.
	 * @throws IllegalStateException if the key does not exist.
	 */
	private static @Nonnull String getRequiredString(@Nullable String prefix, @Nonnull String key, @Nonnull Map<String, Object> properties) {
		return (String) getRequired(prefix, key, properties);
	}

	/**
	 * Retrieve an optional string configuration value.
	 *
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @return The string value, or null if the key does not exist.
	 */
	private static @Nullable String getOptionalString(@Nonnull String key, @Nonnull Map<String, Object> properties) {
		return (String) getOptional(key, properties);
	}

	/**
	 * Retrieve a required nested object configuration value.
	 *
	 * @param prefix Optional configuration prefix for error messages.
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @return The nested object value.
	 * @throws IllegalStateException if the key does not exist.
	 */
	@SuppressWarnings("unchecked")
	private static @Nonnull Map<String, Object> getRequiredObject(@Nullable String prefix, @Nonnull String key, @Nonnull Map<String, Object> properties) {
		return (Map<String, Object>) getRequired(prefix, key, properties);
	}

	/**
	 * Retrieve an optional nested object configuration value.
	 *
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @return The nested object value, or null if the key does not exist.
	 */
	@SuppressWarnings("unchecked")
	private static @Nullable Map<String, Object> getOptionalObject(@Nonnull String key, @Nonnull Map<String, Object> properties) {
		return (Map<String, Object>) getOptional(key, properties);
	}

	/**
	 * Retrieve an optional list configuration value.
	 *
	 * @param key The key to look up.
	 * @param properties The configuration map.
	 * @return The list value, or null if the key does not exist.
	 */
	@SuppressWarnings("unchecked")
	private static @Nullable List<String> getOptionalList(@Nonnull String key, @Nonnull Map<String, Object> properties) {
		return (List<String>) getOptional(key, properties);
	}

	/**
	 * Shutdown Skyve services and notify per-customer shutdown hooks when the
	 * web application context is destroyed.
	 *
	 * @param evt The servlet context event.
	 */
	@Override
	public void contextDestroyed(@SuppressWarnings("null") @Nonnull ServletContextEvent evt) {
		evt.getServletContext().setAttribute(SHUTTING_DOWN_ATTRIBUTE_NAME, Boolean.TRUE);

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
											ProvidedRepository.notifyAllCustomersObservers(c -> ((CustomerImpl) c).notifyShutdown());
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
	static @Nullable String cleanupDirectory(final @Nullable String path) {
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
	private static void testWritableDirectory(@Nonnull String propertyName, @Nonnull String directoryPath) {
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
