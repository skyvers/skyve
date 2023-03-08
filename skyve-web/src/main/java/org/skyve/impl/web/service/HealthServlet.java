package org.skyve.impl.web.service;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.Connection;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.ContentManager;
import org.skyve.content.MimeType;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.DataStore;
import org.skyve.util.Util;

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

	// The thread-safe cached response
	private static AtomicReference<String> cachedResponse = new AtomicReference<>();
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
			String payload = determineResponse();
			cachedResponse.set(payload);
			try (PrintWriter pw = response.getWriter()) {
				pw.append(payload);
			}
			Util.LOGGER.info("Health Check Response = " + payload);
		}
		else { // cached response can be used
			String payload = cachedResponse.get();
			try (PrintWriter pw = response.getWriter()) {
				pw.append(payload);
			}
			Util.LOGGER.info("Cached Response = " + payload);
		}
		response.setStatus(HttpServletResponse.SC_OK);
	}
	
	/**
	 * Create a JSON response representing the status of the system components.
	 * @return	The JSON.
	 */
	private static String determineResponse() {
		StringBuilder result = new StringBuilder(128);
		
		// Persistence
		result.append("{\"persistence\":\"");
		try {
			AbstractHibernatePersistence p = (AbstractHibernatePersistence) AbstractPersistence.get();
			try {
				result.append("ok");
				
				// Primary Data Store
				result.append("\",\"database\":\"");
				try {
					p.begin();
					p.newSQL("select 1 from ADM_Configuration where 1 = 0").scalarResults(Number.class);
					p.rollback();
					result.append("ok");
				}
				catch (@SuppressWarnings("unused") Throwable t) {
					result.append("error");
				}
			}
			finally {
				p.commit(true);
			}
		}
		catch (@SuppressWarnings("unused") Throwable t) {
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
		catch (@SuppressWarnings("unused") Throwable t) {
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
		catch (@SuppressWarnings("unused") Throwable t) {
			result.append("error");
		}

		// Add-ins
		result.append("\",\"addins\":\"");
		try {
			// Obtain the addin manager
			EXT.getAddInManager();
			result.append("ok");
		}
		catch (@SuppressWarnings("unused") Throwable t) {
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
		catch (@SuppressWarnings("unused") Throwable t) {
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
			catch (@SuppressWarnings("unused") Throwable t) {
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
		catch (@SuppressWarnings("unused") Throwable t) {
			result.append("error");
		}

		result.append("\"}");
		
		return result.toString();
	}
}
