package org.skyve.wildcat.web;

import java.io.File;
import java.io.FileInputStream;
import java.util.Properties;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

import org.skyve.CORE;
import org.skyve.persistence.Persistence;
import org.skyve.wildcat.job.JobScheduler;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.metadata.repository.LocalSecureRepository;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.persistence.hibernate.HibernateJackrabbitPersistence;
import org.skyve.wildcat.util.UtilImpl;

public class WildcatContextListener implements ServletContextListener {

	@Override
	@SuppressWarnings("unchecked")
	public void contextInitialized(ServletContextEvent evt) {
		ServletContext ctx = evt.getServletContext();

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
			String realPath = ctx.getRealPath("/");
			UtilImpl.LOGGER.info("REAL PATH = " + realPath);
			File ear = new File(realPath).getParentFile();
			String earName = ear.getName();
			earName = earName.substring(0, earName.length() - 4);
			propertiesFilePath = ear.getParent() + '/' + earName + ".properties";
		}

		Properties properties = new Properties();
		try {
			properties.load(new FileInputStream(propertiesFilePath));
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not find app properties file " + propertiesFilePath, e);
		}

		UtilImpl.XML_TRACE = Boolean.parseBoolean(properties.getProperty("XML_TRACE"));
		UtilImpl.HTTP_TRACE = Boolean.parseBoolean(properties.getProperty("HTTP_TRACE"));
		UtilImpl.COMMAND_TRACE = Boolean.parseBoolean(properties.getProperty("COMMAND_TRACE"));
		UtilImpl.COMMAND_TRACE = Boolean.parseBoolean(properties.getProperty("FACES_TRACE"));
		UtilImpl.QUERY_TRACE = Boolean.parseBoolean(properties.getProperty("QUERY_TRACE"));
		UtilImpl.FACES_TRACE = Boolean.parseBoolean(properties.getProperty("FACES_TRACE"));
		UtilImpl.SQL_TRACE = Boolean.parseBoolean(properties.getProperty("SQL_TRACE"));
		UtilImpl.CONTENT_TRACE = Boolean.parseBoolean(properties.getProperty("CONTENT_TRACE"));
		UtilImpl.SECURITY_TRACE = Boolean.parseBoolean(properties.getProperty("SECURITY_TRACE"));
		UtilImpl.BIZLET_TRACE = Boolean.parseBoolean(properties.getProperty("BIZLET_TRACE"));
		UtilImpl.DIRTY_TRACE = Boolean.parseBoolean(properties.getProperty("DIRTY_TRACE"));
		UtilImpl.PRETTY_SQL_OUTPUT = Boolean.parseBoolean(properties.getProperty("PRETTY_SQL_OUTPUT"));
		UtilImpl.APPS_JAR_DIRECTORY = properties.getProperty("APPS_JAR_DIRECTORY");
		UtilImpl.CONTENT_DIRECTORY = properties.getProperty("CONTENT_DIRECTORY");

		String devMode = properties.getProperty("DEV_MODE");
		if (devMode != null) {
			UtilImpl.DEV_MODE = Boolean.parseBoolean(devMode);
		}

		// The following URLs cannot be set from the web context (could be many URLs to reach the web server after all).
		// There are container specific ways but we don't want that.
		UtilImpl.SERVER_URL = properties.getProperty("SERVER_URL");
		UtilImpl.WILDCAT_CONTEXT = properties.getProperty("WILDCAT_CONTEXT");
		UtilImpl.HOME_URI = properties.getProperty("HOME_URI");
		
		String maxConversations = properties.getProperty("MAX_CONVERSATIONS_IN_MEMORY");
		if (maxConversations != null) {
			UtilImpl.MAX_CONVERSATIONS_IN_MEMORY = Integer.parseInt(maxConversations);
		}
		String conversationEvictionTime = properties.getProperty("CONVERSATION_EVICTION_TIME_MINUTES");
		if (conversationEvictionTime != null) {
			UtilImpl.CONVERSATION_EVICTION_TIME_MINUTES = Integer.parseInt(conversationEvictionTime);
		}
		UtilImpl.DATASOURCE = properties.getProperty("DATASOURCE");
		UtilImpl.DIALECT = properties.getProperty("DIALECT");
		String ddlSync = properties.getProperty("DDL_SYNC");
		if (ddlSync != null) {
			UtilImpl.DDL_SYNC = Boolean.parseBoolean(ddlSync);
		}
		UtilImpl.SMTP = properties.getProperty("SMTP");
		UtilImpl.SMTP_PORT = properties.getProperty("SMTP_PORT");
		UtilImpl.SMTP_UID = properties.getProperty("SMTP_UID");
		UtilImpl.SMTP_PWD = properties.getProperty("SMTP_PWD");
		UtilImpl.SMTP_SENDER = properties.getProperty("SMTP_SENDER");
		UtilImpl.SMTP_TEST_RECIPIENT = properties.getProperty("SMTP_TEST_RECIPIENT");
		String smtpTestBogusSend = properties.getProperty("SMTP_TEST_BOGUS_SEND");
		if (smtpTestBogusSend != null) {
			UtilImpl.SMTP_TEST_BOGUS_SEND = Boolean.parseBoolean(smtpTestBogusSend);
		}
		UtilImpl.CUSTOMER = properties.getProperty("CUSTOMER");
		String passwordHashingAlgorithm = properties.getProperty("PASSWORD_HASHING_ALGORITHM");
		if (passwordHashingAlgorithm != null) {
			UtilImpl.PASSWORD_HASHING_ALGORITHM = passwordHashingAlgorithm;
		}
		
		// NB Need the repository set before setting persistence
		UtilImpl.WILDCAT_REPOSITORY_CLASS = properties.getProperty("WILDCAT_REPOSITORY_CLASS");
		if (AbstractRepository.get() == null) {
			if (UtilImpl.WILDCAT_REPOSITORY_CLASS == null) {
				UtilImpl.LOGGER.info("SET WILDCAT REPOSITORY CLASS TO DEFAULT");
				AbstractRepository.set(new LocalSecureRepository());
			}
			else {
				UtilImpl.LOGGER.info("SET TO " + UtilImpl.WILDCAT_REPOSITORY_CLASS);
				try {
					AbstractRepository.set((AbstractRepository) Thread.currentThread().getContextClassLoader().loadClass(UtilImpl.WILDCAT_REPOSITORY_CLASS).newInstance());
				}
				catch (Exception e) {
					throw new IllegalStateException("Could not create WILDCAT_REPOSITORY_CLASS " + UtilImpl.WILDCAT_REPOSITORY_CLASS, e);
				}
			}
		}

		UtilImpl.WILDCAT_PERSISTENCE_CLASS = properties.getProperty("WILDCAT_PERSISTENCE_CLASS");
		if (AbstractPersistence.IMPLEMENTATION_CLASS == null) {
			if (UtilImpl.WILDCAT_PERSISTENCE_CLASS == null) {
				AbstractPersistence.IMPLEMENTATION_CLASS = HibernateJackrabbitPersistence.class;
			}
			else {
				try {
					AbstractPersistence.IMPLEMENTATION_CLASS = (Class<? extends AbstractPersistence>) Class.forName(UtilImpl.WILDCAT_PERSISTENCE_CLASS);
				}
				catch (ClassNotFoundException e) {
					throw new IllegalStateException("Could not find WILDCAT_PERSISTENCE_CLASS " + UtilImpl.WILDCAT_PERSISTENCE_CLASS, e);
				}
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
	}
}
