package org.skyve.impl.web;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Properties;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.elasticsearch.ESClient;
import org.skyve.impl.job.JobScheduler;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.LocalSecureRepository;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.HibernateElasticSearchPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.Persistence;

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
		String propertiesFilePath = System.getProperty("PROPERTIES_FILE_PATH");
		if (propertiesFilePath == null) {
			propertiesFilePath = ctx.getInitParameter("PROPERTIES_FILE_PATH");
		}
		if (propertiesFilePath == null) {
			UtilImpl.LOGGER.info("SKYVE CONTEXT REAL PATH = " + UtilImpl.SKYVE_CONTEXT_REAL_PATH);
			File ear = new File(UtilImpl.SKYVE_CONTEXT_REAL_PATH).getParentFile();
			String earName = ear.getName();
			earName = earName.substring(0, earName.length() - 4);
			propertiesFilePath = ear.getParent() + '/' + earName + ".properties";
		}

		Properties properties = new Properties();
		try (InputStream is = new FileInputStream(propertiesFilePath)) {
			properties.load(is);
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not find app properties file " + propertiesFilePath, e);
		}

		String value = UtilImpl.processStringValue(properties.getProperty("XML_TRACE"));
		UtilImpl.XML_TRACE = (value != null) && Boolean.parseBoolean(value);
		value = UtilImpl.processStringValue(properties.getProperty("HTTP_TRACE"));
		UtilImpl.HTTP_TRACE = (value != null) && Boolean.parseBoolean(value);
		value = UtilImpl.processStringValue(properties.getProperty("COMMAND_TRACE"));
		UtilImpl.COMMAND_TRACE = (value != null) && Boolean.parseBoolean(value);
		value = UtilImpl.processStringValue(properties.getProperty("FACES_TRACE"));
		UtilImpl.FACES_TRACE = (value != null) && Boolean.parseBoolean(value);
		value = UtilImpl.processStringValue(properties.getProperty("QUERY_TRACE"));
		UtilImpl.QUERY_TRACE = (value != null) && Boolean.parseBoolean(value);
		value = UtilImpl.processStringValue(properties.getProperty("SQL_TRACE"));
		UtilImpl.SQL_TRACE = (value != null) && Boolean.parseBoolean(value);
		value = UtilImpl.processStringValue(properties.getProperty("CONTENT_TRACE"));
		UtilImpl.CONTENT_TRACE = (value != null) && Boolean.parseBoolean(value);
		value = UtilImpl.processStringValue(properties.getProperty("SECURITY_TRACE"));
		UtilImpl.SECURITY_TRACE = (value != null) && Boolean.parseBoolean(value);
		value = UtilImpl.processStringValue(properties.getProperty("BIZLET_TRACE"));
		UtilImpl.BIZLET_TRACE = (value != null) && Boolean.parseBoolean(value);
		value = UtilImpl.processStringValue(properties.getProperty("DIRTY_TRACE"));
		UtilImpl.DIRTY_TRACE = (value != null) && Boolean.parseBoolean(value);
		value = UtilImpl.processStringValue(properties.getProperty("PRETTY_SQL_OUTPUT"));
		UtilImpl.PRETTY_SQL_OUTPUT = (value != null) && Boolean.parseBoolean(value);

		UtilImpl.APPS_JAR_DIRECTORY = UtilImpl.processStringValue(properties.getProperty("APPS_JAR_DIRECTORY"));
		UtilImpl.CONTENT_DIRECTORY = UtilImpl.processStringValue(properties.getProperty("CONTENT_DIRECTORY"));
		UtilImpl.CONTENT_GC_CRON = UtilImpl.processStringValue(properties.getProperty("CONTENT_GC_CRON"));
		value = UtilImpl.processStringValue(properties.getProperty("CONTENT_FILE_STORAGE"));
		UtilImpl.CONTENT_FILE_STORAGE = (value != null) && Boolean.parseBoolean(value);
		
		value = UtilImpl.processStringValue(properties.getProperty("DEV_MODE"));
		UtilImpl.DEV_MODE = (value != null) && Boolean.parseBoolean(value);

		// The following URLs cannot be set from the web context (could be many URLs to reach the web server after all).
		// There are container specific ways but we don't want that.
		UtilImpl.SERVER_URL = UtilImpl.processStringValue(properties.getProperty("SERVER_URL"));
		UtilImpl.SKYVE_CONTEXT = UtilImpl.processStringValue(properties.getProperty("SKYVE_CONTEXT"));
		UtilImpl.HOME_URI = UtilImpl.processStringValue(properties.getProperty("HOME_URI"));
		
		value = UtilImpl.processStringValue(properties.getProperty("MAX_CONVERSATIONS_IN_MEMORY"));
		if (value != null) {
			UtilImpl.MAX_CONVERSATIONS_IN_MEMORY = Integer.parseInt(value);
		}
		value = UtilImpl.processStringValue(properties.getProperty("CONVERSATION_EVICTION_TIME_MINUTES"));
		if (value != null) {
			UtilImpl.CONVERSATION_EVICTION_TIME_MINUTES = Integer.parseInt(value);
		}
		UtilImpl.DATASOURCE = UtilImpl.processStringValue(properties.getProperty("DATASOURCE"));
		UtilImpl.DIALECT = UtilImpl.processStringValue(properties.getProperty("DIALECT"));
		value = UtilImpl.processStringValue(properties.getProperty("DDL_SYNC"));
		if (value != null) {
			UtilImpl.DDL_SYNC = Boolean.parseBoolean(value);
		}
		UtilImpl.SMTP = UtilImpl.processStringValue(properties.getProperty("SMTP"));
		UtilImpl.SMTP_PORT = UtilImpl.processStringValue(properties.getProperty("SMTP_PORT"));
		UtilImpl.SMTP_UID = UtilImpl.processStringValue(properties.getProperty("SMTP_UID"));
		UtilImpl.SMTP_PWD = UtilImpl.processStringValue(properties.getProperty("SMTP_PWD"));
		UtilImpl.SMTP_SENDER = UtilImpl.processStringValue(properties.getProperty("SMTP_SENDER"));
		UtilImpl.SMTP_TEST_RECIPIENT = UtilImpl.processStringValue(properties.getProperty("SMTP_TEST_RECIPIENT"));
		value = UtilImpl.processStringValue(properties.getProperty("SMTP_TEST_BOGUS_SEND"));
		if (value != null) {
			UtilImpl.SMTP_TEST_BOGUS_SEND = Boolean.parseBoolean(value);
		}
		UtilImpl.CUSTOMER = UtilImpl.processStringValue(properties.getProperty("CUSTOMER"));
		value = UtilImpl.processStringValue(properties.getProperty("PASSWORD_HASHING_ALGORITHM"));
		if (value != null) {
			UtilImpl.PASSWORD_HASHING_ALGORITHM = value;
		}
		
		// NB Need the repository set before setting persistence
		UtilImpl.SKYVE_REPOSITORY_CLASS = UtilImpl.processStringValue(properties.getProperty("SKYVE_REPOSITORY_CLASS"));
		if (AbstractRepository.get() == null) {
			if (UtilImpl.SKYVE_REPOSITORY_CLASS == null) {
				UtilImpl.LOGGER.info("SET SKYVE REPOSITORY CLASS TO DEFAULT");
				AbstractRepository.set(new LocalSecureRepository());
			}
			else {
				UtilImpl.LOGGER.info("SET TO " + UtilImpl.SKYVE_REPOSITORY_CLASS);
				try {
					AbstractRepository.set((AbstractRepository) Thread.currentThread().getContextClassLoader().loadClass(UtilImpl.SKYVE_REPOSITORY_CLASS).newInstance());
				}
				catch (Exception e) {
					throw new IllegalStateException("Could not create SKYVE_REPOSITORY_CLASS " + UtilImpl.SKYVE_REPOSITORY_CLASS, e);
				}
			}
		}

		UtilImpl.SKYVE_PERSISTENCE_CLASS = UtilImpl.processStringValue(properties.getProperty("SKYVE_PERSISTENCE_CLASS"));
		if (AbstractPersistence.IMPLEMENTATION_CLASS == null) {
			if (UtilImpl.SKYVE_PERSISTENCE_CLASS == null) {
				AbstractPersistence.IMPLEMENTATION_CLASS = HibernateElasticSearchPersistence.class;
			}
			else {
				try {
					AbstractPersistence.IMPLEMENTATION_CLASS = (Class<? extends AbstractPersistence>) Class.forName(UtilImpl.SKYVE_PERSISTENCE_CLASS);
				}
				catch (ClassNotFoundException e) {
					throw new IllegalStateException("Could not find SKYVE_PERSISTENCE_CLASS " + UtilImpl.SKYVE_PERSISTENCE_CLASS, e);
				}
			}
		}

		UtilImpl.SKYVE_CONTENT_MANAGER_CLASS = UtilImpl.processStringValue(properties.getProperty("SKYVE_CONTENT_MANAGER_CLASS"));
		if (UtilImpl.SKYVE_CONTENT_MANAGER_CLASS == null) {
			AbstractContentManager.IMPLEMENTATION_CLASS = ESClient.class;
		}
		else {
			try {
				AbstractContentManager.IMPLEMENTATION_CLASS = (Class<? extends AbstractContentManager>) Class.forName(UtilImpl.SKYVE_CONTENT_MANAGER_CLASS);
			}
			catch (ClassNotFoundException e) {
				throw new IllegalStateException("Could not find SKYVE_CONTENT_MANAGER_CLASS " + UtilImpl.SKYVE_CONTENT_MANAGER_CLASS, e);
			}
		}

		// ensure that the schema is created before trying to init the job scheduler
		Persistence p = null;
		try {
			p = CORE.getPersistence();
		}
		finally {
			if (p != null) {
				p.commit(true);
			}
		}
		
		JobScheduler.init();
		WebUtil.initConversationsCache();
	}

	@Override
	public void contextDestroyed(ServletContextEvent evt) {
		JobScheduler.dispose();
		WebUtil.destroyConversationsCache();
		
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
}
