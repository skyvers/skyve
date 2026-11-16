package org.skyve.impl.web.service;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Method;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.user.User;

import util.AbstractH2Test;

/**
 * Checks the health of the primary data store against the H2 database.
 */
class HealthServletH2Test extends AbstractH2Test {
	private User user;

	@AfterEach
	void restorePersistenceUser() {
		// The health check commits and closes the thread's persistence, so give the base class one to clean up with
		if (user != null) {
			AbstractPersistence.get().setUser(user);
		}
	}

	@Test
	void determineResponseReportsTheDatabaseOkWhenTheConfigurationTableCanBeQueried() throws Exception {
		user = CORE.getPersistence().getUser();
		Method determineResponse = HealthServlet.class.getDeclaredMethod("determineResponse");
		determineResponse.setAccessible(true);

		String json = determineResponse.invoke(null).toString();

		assertTrue(json.startsWith("{\"persistence\":\"ok\",\"database\":\"ok\""), json);
	}
}
