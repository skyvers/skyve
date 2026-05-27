package modules.test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.hibernate.boot.Metadata;
import org.hibernate.boot.spi.MetadataImplementor;
import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.impl.backup.DDL;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.persistence.hibernate.dialect.DDLDelegate;

@SuppressWarnings("static-method")
class DDLH2Test extends AbstractSkyveTestDispose {
	private static final String TABLE_NAME = "TEST_ALLATTRIBUTESPERSISTENT";
	private static final String COLUMN_NAME = "TEXT";

	@Test
	void testGenerateDDLUpdateScriptAppendsDDLDelegateStatements() throws Exception {
		alterTextColumnLength(100);
		try {
			List<String> delegateUpdates = getDelegateUpdates();
			assertTrue(hasTextColumnUpdate(delegateUpdates), "DDLDelegate should report the forced text column change");

			List<String> updateDDL = new ArrayList<>();
			((AbstractHibernatePersistence) p).generateDDL(null, null, updateDDL);

			assertFalse(updateDDL.isEmpty(), "Update DDL should contain statements after a schema mismatch");
			assertTrue(hasTextColumnUpdate(updateDDL), "Generated update DDL should include the forced text column change");
			assertTrue(updateDDL.size() >= delegateUpdates.size(), "Update DDL should include the delegate statements at the end");
			assertEquals(delegateUpdates,
						updateDDL.subList(updateDDL.size() - delegateUpdates.size(), updateDDL.size()),
						"DDLDelegate output should be appended to the generated update DDL");
		}
		finally {
			DDL.sync(true);
		}
	}

	@Test
	void testDDLSyncExecutesInMemoryStatements() throws Exception {
		alterTextColumnLength(100);
		try {
			List<String> updates = DDL.sync(false);
			assertTrue(hasTextColumnUpdate(updates), "DDL.sync(false) should surface the forced text column update");

			List<String> executed = DDL.sync(true);
			assertTrue(hasTextColumnUpdate(executed), "DDL.sync(true) should execute the in-memory update statements");

			List<String> afterExecution = DDL.sync(false);
			assertFalse(hasTextColumnUpdate(afterExecution), "DDL.sync(true) should restore the forced text column mismatch");
		}
		finally {
			DDL.sync(true);
		}
	}

	@Test
	void testDDLCreateAndDropGenerateInMemoryStatements() throws Exception {
		assertFalse(DDL.create(null, false).isEmpty(), "DDL.create() should generate create statements in memory");
		assertFalse(DDL.drop(null, false).isEmpty(), "DDL.drop() should generate drop statements in memory");
	}

	private static void alterTextColumnLength(int length) throws Exception {
		try (Connection connection = EXT.getDataStoreConnection(); Statement statement = connection.createStatement()) {
			connection.setAutoCommit(true);
			statement.execute("alter table TEST_AllAttributesPersistent alter column text varchar(" + length + ")");
		}
	}

	@SuppressWarnings("resource")
	private static List<String> getDelegateUpdates() throws Exception {
		Metadata metadata = getMetadata();
		MetadataImplementor metadataImplementor = (MetadataImplementor) metadata;
		org.hibernate.service.ServiceRegistry serviceRegistry = metadataImplementor.getMetadataBuildingOptions().getServiceRegistry();
		return DDLDelegate.migrate(serviceRegistry, metadata, AbstractHibernatePersistence.getDialect(), false);
	}

	private static Metadata getMetadata() throws Exception {
		Field metadataField = AbstractHibernatePersistence.class.getDeclaredField("metadata");
		metadataField.setAccessible(true);
		return (Metadata) metadataField.get(null);
	}

	private static boolean hasTextColumnUpdate(List<String> updates) {
		for (String update : updates) {
			String uppercase = update.toUpperCase(Locale.ROOT);
			if (uppercase.contains(TABLE_NAME) && uppercase.contains(COLUMN_NAME)) {
				return true;
			}
		}
		return false;
	}
}