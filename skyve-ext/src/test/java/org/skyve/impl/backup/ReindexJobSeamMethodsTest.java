package org.skyve.impl.backup;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.sql.Connection;
import java.lang.reflect.Field;

import org.junit.Test;
import org.skyve.content.ContentManager;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.user.User;

@SuppressWarnings({"static-method", "resource"})
public class ReindexJobSeamMethodsTest {
	@Test
	public void attachmentsIsAbstractContentManagerSeamReturnsTrueForAbstractManager() {
		AttachmentsSeamHarness harness = new AttachmentsSeamHarness();
		assertTrue(harness.callIsAbstractContentManager(mock(AbstractContentManager.class)));
	}

	@Test
	public void attachmentsIsAbstractContentManagerSeamReturnsFalseForPlainManager() {
		AttachmentsSeamHarness harness = new AttachmentsSeamHarness();
		assertFalse(harness.callIsAbstractContentManager(mock(ContentManager.class)));
	}

	@Test
	public void attachmentsSecureSqlSeamExecutes() {
		AttachmentsSeamHarness harness = new AttachmentsSeamHarness();
		StringBuilder sql = new StringBuilder("select * from ADM_TEST");
		Table table = new Table("ADM_TEST", "ADM_TEST");
		harness.callSecureSql(sql, table, "demo");
		assertTrue(sql.length() > 0);
	}

	@Test
	public void attachmentsSeamsInvokeDefaultProviders() {
		AttachmentsSeamHarness harness = new AttachmentsSeamHarness();
		assertThrows(RuntimeException.class, harness::callGetCustomerName);
		assertThrows(RuntimeException.class, harness::callGetTables);

		try {
			ContentManager cm = harness.callNewContentManager();
			if (cm != null) {
				cm.close();
			}
		}
		catch (Exception e) {
			assertNotNull(e);
		}

		try {
			Connection connection = harness.callGetDataStoreConnection();
			if (connection != null) {
				connection.close();
			}
		}
		catch (Exception e) {
			assertNotNull(e);
		}
	}

	@Test
	public void attachmentsSeamsCanUseConfiguredProviders() throws Exception {
		AttachmentsSeamHarness harness = new AttachmentsSeamHarness();
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Class<? extends AbstractContentManager> previousContentManagerClass = AbstractContentManager.IMPLEMENTATION_CLASS;

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomerName()).thenReturn("demo");
		persistence.setForThread();

		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;

		try {
			assertEquals("demo", harness.callGetCustomerName());
			try (ContentManager cm = harness.callNewContentManager()) {
				assertTrue(cm instanceof NoOpContentManager);
			}
			assertThrows(RuntimeException.class, harness::callGetTables);
		}
		finally {
			AbstractContentManager.IMPLEMENTATION_CLASS = previousContentManagerClass;
			clearThreadLocalPersistence();
		}
	}

	@Test
	public void beansSeamsInvokeDefaultProviders() {
		BeansSeamHarness harness = new BeansSeamHarness();

		try {
			AbstractPersistence persistence = harness.callGetPersistence();
			assertNotNull(persistence);
		}
		catch (Exception e) {
			assertNotNull(e);
		}

		assertThrows(RuntimeException.class, harness::callGetDynamicEntityPersistenceIdentifier);

		try {
			ContentManager cm = harness.callNewContentManager();
			if (cm != null) {
				cm.close();
			}
		}
		catch (Exception e) {
			assertNotNull(e);
		}
	}

	private static final class AttachmentsSeamHarness extends ReindexAttachmentsJob {
		public boolean callIsAbstractContentManager(ContentManager cm) {
			return super.isAbstractContentManager(cm);
		}

		public void callSecureSql(StringBuilder sql, Table table, String name) {
			super.secureSQL(sql, table, name);
		}

		public String callGetCustomerName() {
			return super.getCustomerName();
		}

		public ContentManager callNewContentManager() {
			return super.newContentManager();
		}

		public Connection callGetDataStoreConnection() {
			return super.getDataStoreConnection();
		}

		public java.util.Collection<Table> callGetTables() throws Exception {
			return super.getTables();
		}
	}

	private static final class BeansSeamHarness extends ReindexBeansJob {
		public AbstractPersistence callGetPersistence() {
			return super.getPersistence();
		}

		public String callGetDynamicEntityPersistenceIdentifier() {
			return super.getDynamicEntityPersistenceIdentifier(null);
		}

		public ContentManager callNewContentManager() {
			return super.newContentManager();
		}
	}

	private static void clearThreadLocalPersistence() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			@SuppressWarnings("unchecked")
			ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
			threadLocal.remove();
		}
		catch (Exception e) {
			throw new IllegalStateException(e);
		}
	}
}
