package org.skyve.impl.content.jdbc;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.DriverManager;
import java.util.Comparator;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.DataStore;

@SuppressWarnings("static-method")
class JDBCRemoteServerWrappersTest {
	private final String originalServerArgs = UtilImpl.CONTENT_JDBC_SERVER_ARGS;
	private final Map<String, DataStore> originalDataStores = UtilImpl.DATA_STORES;
	private final String originalContentDirectory = UtilImpl.CONTENT_DIRECTORY;
	private Path tempContentDirectory;

	@AfterEach
	void restoreServerArgs() throws Exception {
		UtilImpl.CONTENT_JDBC_SERVER_ARGS = originalServerArgs;
		UtilImpl.DATA_STORES = originalDataStores;
		UtilImpl.CONTENT_DIRECTORY = originalContentDirectory;
		JDBCRemoteContentManagerServer.shutdown();
		if (tempContentDirectory != null) {
			try (var stream = Files.walk(tempContentDirectory)) {
				stream.sorted(Comparator.reverseOrder()).forEach(path -> {
					try {
						Files.deleteIfExists(path);
					}
					catch (Exception e) {
						throw new IllegalStateException(e);
					}
				});
			}
		}
	}

	@Test
	void testLuceneWrapperStartupPropagatesServerGuardFailure() throws Exception {
		UtilImpl.CONTENT_JDBC_SERVER_ARGS = null;

		try (JDBCRemoteLuceneContentManagerServer wrapper = new JDBCRemoteLuceneContentManagerServer()) {
			IllegalStateException ex = assertThrows(IllegalStateException.class, wrapper::startup);
			assertTrue(ex.getMessage().contains("there are no server arguments defined"));
		}
	}

	@Test
	void testElasticWrapperStartupPropagatesServerGuardFailure() {
		UtilImpl.CONTENT_JDBC_SERVER_ARGS = null;

		try (JDBCRemoteElasticContentManagerServer wrapper = new JDBCRemoteElasticContentManagerServer()) {
			IllegalStateException ex = assertThrows(IllegalStateException.class, wrapper::startup);
			assertTrue(ex.getMessage().contains("there are no server arguments defined"));
		}
	}

	@Test
	void testLuceneWrapperStartupAndShutdownWithConfiguredServer() throws Exception {
		configureContentServer();
		tempContentDirectory = Files.createTempDirectory("skyve-jdbc-lucene-wrapper-");
		UtilImpl.CONTENT_DIRECTORY = tempContentDirectory.toString();

		try (JDBCRemoteLuceneContentManagerServer wrapper = new JDBCRemoteLuceneContentManagerServer()) {
			wrapper.startup();
			assertNotNull(wrapper.all());
			wrapper.shutdown();
		}
	}

	@Test
	void testElasticWrapperStartupAndShutdownWithConfiguredServer() throws Exception {
		configureContentServer();

		try (JDBCRemoteElasticContentManagerServer wrapper = new JDBCRemoteElasticContentManagerServer()) {
			wrapper.startup();
			assertNotNull(wrapper.all());
			wrapper.shutdown();
		}
	}

	@Test
	void testLuceneWrapperShutdownWithoutStartup() throws Exception {
		try (JDBCRemoteLuceneContentManagerServer wrapper = new JDBCRemoteLuceneContentManagerServer()) {
			assertDoesNotThrow(wrapper::shutdown);
		}
	}

	@Test
	void testElasticWrapperShutdownWithoutStartup() {
		try (JDBCRemoteElasticContentManagerServer wrapper = new JDBCRemoteElasticContentManagerServer()) {
			assertDoesNotThrow(wrapper::shutdown);
		}
	}

	private static void configureContentServer() throws Exception {
		String dbUrl = "jdbc:h2:mem:content_wrapper_test;DB_CLOSE_DELAY=-1";
		Map<String, DataStore> dataStores = new TreeMap<>();
		dataStores.put("CONTENT", new DataStore("org.h2.Driver", dbUrl, "org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect"));
		UtilImpl.DATA_STORES = dataStores;
		int port = 20000 + (int) (System.nanoTime() % 1000L);
		UtilImpl.CONTENT_JDBC_SERVER_ARGS = "-tcpPort " + port + " -tcpDaemon";
		try (Connection ignored = DriverManager.getConnection(dbUrl)) {
			// force database initialization before alias registration
		}
	}
}
