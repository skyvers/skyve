package org.skyve.impl.tools.jasperreports;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.util.Comparator;
import java.util.stream.Stream;

import org.hibernate.SessionFactory;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect;
import org.skyve.impl.tools.TestPaths;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.persistence.DataStore;

abstract class JasperQueryBootstrapSupport {
	private static final String JDBC_URL = "jdbc:h2:mem:skyve_tools_jasper;DB_CLOSE_DELAY=-1";

	private static ProvidedRepository originalRepository;
	private static boolean bootstrapComplete;
	private static Path bootstrapBasePath;

	@BeforeAll
	static void setupJasperQueryBootstrap() throws Exception {
		originalRepository = ProvidedRepositoryFactory.get();
		UtilImpl.clear();

		URL ormResource = Thread.currentThread().getContextClassLoader().getResource("modules/admin/domain/admin_orm.hbm.xml");
		assertNotNull(ormResource, "Expected modules/admin/domain/admin_orm.hbm.xml on the test classpath");
		bootstrapBasePath = Files.createTempDirectory("skyve-tools-jasper-");
		String repositoryPath = TestPaths.skyveWarMainJava().toString().replace('\\', '/') + '/';
		Path schemaSource = Paths.get(repositoryPath, "schemas");
		copyDirectory(schemaSource, bootstrapBasePath.resolve("schemas"));
		copyDirectory(schemaSource, bootstrapBasePath.resolve("modules/schemas"));
		copyDirectory(schemaSource, bootstrapBasePath.resolve("customers/schemas"));
		Path ormTarget = bootstrapBasePath.resolve("modules/admin/domain/admin_orm.hbm.xml");
		Files.createDirectories(ormTarget.getParent());
		Files.copy(Paths.get(ormResource.toURI()), ormTarget);

		UtilImpl.APPS_JAR_DIRECTORY = bootstrapBasePath.toString().replace('\\', '/') + '/';
		UtilImpl.DATA_STORE = new DataStore("org.h2.Driver", JDBC_URL, "sa", "", H2SpatialDialect.class.getName());
		UtilImpl.DATA_STORES.put("jasper", UtilImpl.DATA_STORE);
		UtilImpl.DDL_SYNC = false;
		UtilImpl.SQL_TRACE = false;
		UtilImpl.QUERY_TRACE = false;
		UtilImpl.HIBERNATE_FAIL_ON_MISSING_CACHE = false;

		ProvidedRepositoryFactory.set(new LocalDesignRepository(repositoryPath));
		createTables();
		bootstrapComplete = true;
	}

	@AfterAll
	static void tearDownJasperQueryBootstrap() throws Exception {
		try {
			if (bootstrapComplete) {
				SessionFactory sessionFactory = getSessionFactory();
				if ((sessionFactory != null) && (! sessionFactory.isClosed())) {
					sessionFactory.close();
				}
			}
		}
		catch (@SuppressWarnings("unused") NoClassDefFoundError | ExceptionInInitializerError e) {
			// Nothing to close if Hibernate bootstrap never completed.
		}
		finally {
			UtilImpl.clear();
			ProvidedRepositoryFactory.set(originalRepository);
			deleteDirectory(bootstrapBasePath);
		}
	}

	private static void copyDirectory(Path source, Path target) throws Exception {
		try (Stream<Path> paths = Files.walk(source)) {
			for (Path path : (Iterable<Path>) paths::iterator) {
				Path destination = target.resolve(source.relativize(path).toString());
				if (Files.isDirectory(path)) {
					Files.createDirectories(destination);
				}
				else {
					Files.createDirectories(destination.getParent());
					Files.copy(path, destination);
				}
			}
		}
	}

	private static void deleteDirectory(Path path) throws Exception {
		if (path == null || ! Files.exists(path)) {
			return;
		}

		try (Stream<Path> paths = Files.walk(path)) {
			for (Path current : (Iterable<Path>) paths.sorted(Comparator.reverseOrder())::iterator) {
				Files.deleteIfExists(current);
			}
		}
	}

	private static void createTables() throws Exception {
		try (Connection ddlConnection = DriverManager.getConnection(JDBC_URL, "sa", "");
				Statement statement = ddlConnection.createStatement()) {
			statement.execute("CREATE TABLE IF NOT EXISTS ADM_Contact (" +
					"bizId VARCHAR(36) PRIMARY KEY, " +
					"bizVersion INTEGER, " +
					"bizKey VARCHAR(128) NOT NULL)");
		}
	}

	private static SessionFactory getSessionFactory() throws Exception {
		var sessionFactoryField = AbstractHibernatePersistence.class.getDeclaredField("sf");
		sessionFactoryField.setAccessible(true);
		return (SessionFactory) sessionFactoryField.get(null);
	}
}
