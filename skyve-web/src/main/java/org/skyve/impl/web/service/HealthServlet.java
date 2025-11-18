package org.skyve.impl.web.service;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.Connection;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.ContentManager;
import org.skyve.content.MimeType;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.persistence.DataStore;
import org.skyve.util.Util;
import org.skyve.util.monitoring.Monitoring;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Returns a JSON response regarding the health of the relevant Skyve system components, or a 404 if turned off in the JSON.
 * The response is cached for the JSON configured number of seconds to alleviate denial of service attacks.
 * <br/>
 * The way to test using this service is...
 * <ol>
 * <li>Can I hit it and get a response.</li>
 * <li>Either parse the JSON response to evaluate or string search for "error".</li>
 * </ol>
 * 
 * @author mike
 */
public class HealthServlet extends HttpServlet {
	private static final long serialVersionUID = -509208309881530817L;

    private static final Logger LOGGER = LoggerFactory.getLogger(HealthServlet.class);

	// The thread-safe cached response
	private static AtomicReference<StringBuilder> cachedResponse = new AtomicReference<>();
	// The thread-safe millis at caching instant - used to determine whether to use the cached response or not
	private static AtomicLong responseInstant = new AtomicLong(Long.MIN_VALUE);
	
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		// If health check is off, send the 404 page
		if (! UtilImpl.HEALTH_CHECK) {
			response.sendError(HttpServletResponse.SC_NOT_FOUND);
			return;
		}
		
		response.setContentType(MimeType.json.toString());
        response.setCharacterEncoding(Util.UTF8);
        response.addHeader("Cache-control", "private,no-cache,no-store"); // never
		response.addDateHeader("Expires", 0); // never

		// If cached response is too old, perform the health check again
		if (responseInstant.get() < System.currentTimeMillis() - (UtilImpl.HEALTH_CACHE_TIME_IN_SECONDS * 1000)) {
			responseInstant.set(System.currentTimeMillis());
			StringBuilder payload = determineResponse();
			cachedResponse.set(payload);
			try (PrintWriter pw = response.getWriter()) {
				Util.chunkCharsToWriter(payload, pw);
			}
			LOGGER.info("Health Check Response = " + payload);
		}
		else { // cached response can be used
			StringBuilder payload = cachedResponse.get();
			try (PrintWriter pw = response.getWriter()) {
				Util.chunkCharsToWriter(payload, pw);
			}
			LOGGER.info("Cached Response = " + payload);
		}
		response.setStatus(HttpServletResponse.SC_OK);
	}
	
	/**
	 * Create a JSON response representing the status of the system components.
	 * @return	The JSON.
	 */
	private static StringBuilder determineResponse() {
		StringBuilder result = new StringBuilder(128);
		
		// Persistence
		result.append("{\"persistence\":\"");
		try {
			AbstractHibernatePersistence p = (AbstractHibernatePersistence) AbstractPersistence.get();
			try {
				result.append("ok");
				
				// Primary Data Store
				result.append("\",\"database\":\"");
				p.begin();
				try {
					ProvidedRepository r = ProvidedRepositoryFactory.get();
					Module m = r.getModule(null, AppConstants.ADMIN_MODULE_NAME);
					Document d = m.getDocument(null, AppConstants.CONFIGURATION_DOCUMENT_NAME);
					Persistent persistent = d.getPersistent();
					if (persistent == null) {
						throw new DomainException("admin.Configuration not persistent");
					}
					StringBuilder sql = new StringBuilder(64);
					sql.append("select 1 from ").append(persistent.getPersistentIdentifier()).append(" where 1 = 0");
					p.newSQL(sql.toString()).scalarResults(Number.class);
					result.append("ok");
				}
				finally {
					p.rollback();
				}
			}
			finally {
				p.commit(true);
			}
		}
		catch (Throwable t) {
			t.printStackTrace();
			result.append("error");
		}
		
		// Data Stores
		result.append("\",\"datastores\":\"");
		try {
			for (DataStore ds : UtilImpl.DATA_STORES.values()) {
				if (ds != UtilImpl.DATA_STORE) {
					try (Connection c = EXT.getDataStoreConnection(ds)) {
						// Nothing to do with the connection once obtained but close it
					}
				}
			}
			result.append("ok");
		}
		catch (Throwable t) {
			t.printStackTrace();
			result.append("error");
		}
		
		// Repository
		result.append("\",\"repository\":\"");
		try {
			// Obtain the repository
			if (CORE.getRepository() == null) {
				result.append("error");
			}
			else {
				result.append("ok");
			}
		}
		catch (Throwable t) {
			t.printStackTrace();
			result.append("error");
		}

		// Add-ins
		result.append("\",\"addins\":\"");
		try {
			// Obtain the addin manager
			EXT.getAddInManager();
			result.append("ok");
		}
		catch (Throwable t) {
			t.printStackTrace();
			result.append("error");
		}

		// Content
		result.append("\",\"content\":\"");
		try {
			// Open and close a new content manager
			try (ContentManager cm = EXT.newContentManager()) {
				result.append("ok");
			}
		}
		catch (Throwable t) {
			t.printStackTrace();
			result.append("error");
		}

		// Job Scheduler
		result.append("\",\"jobs\":\"");
		if (UtilImpl.JOB_SCHEDULER) {
			try {
				// Obtain the job scheduler manager
				EXT.getJobScheduler();
				result.append("ok");
			}
			catch (Throwable t) {
				t.printStackTrace();
				result.append("error");
			}
		}
		else {
			result.append("off");
		}

		// Caching
		result.append("\",\"caching\":\"");
		try {
			// Check for a bogus token that will get the CSRF token cache
			StateUtil.checkToken("", null);
			result.append("ok");
		}
		catch (Throwable t) {
			t.printStackTrace();
			result.append("error");
		}

		// Resources
		result.append("\",\"percentageSystemLoad\":").append(Monitoring.percentageSystemLoad());
		result.append(",\"percentageUsedMemory\":").append(Monitoring.percentageUsedMemory());
		result.append(",\"totalMemoryInMiB\":").append(Monitoring.totalMemoryInMiB());
		result.append(",\"freeMemoryInMiB\":").append(Monitoring.freeMemoryInMiB());
		result.append(",\"maxMemoryInMiB\":").append(Monitoring.maxMemoryInMiB());

		result.append('}');
		
		return result;
	}
}
