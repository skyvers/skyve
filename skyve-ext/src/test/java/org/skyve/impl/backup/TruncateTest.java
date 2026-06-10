package org.skyve.impl.backup;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.junit.After;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;
import org.skyve.domain.Bean;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.SQL;

@SuppressWarnings("static-method")
public class TruncateTest {
	@After
	public void teardown() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	public void constructorCreatesUtilityInstance() {
		assertThat(new Truncate(), is(org.hamcrest.CoreMatchers.notNullValue()));
	}

	@Test
	public void truncateDatabaseUnlinksAndDeletesTablesInConstraintSafeOrder() throws Exception {
		AbstractPersistence persistence = bindMockPersistence();
		SQL sql = fluentSQL();
		when(persistence.newSQL(anyString())).thenReturn(sql);
		boolean originalCommandTrace = UtilImpl.COMMAND_TRACE;
		Collection<Table> tables = new ArrayList<>();
		Table customerTable = new Table("CustomerDoc", "ADM_CUSTOMER_DOC");
		customerTable.fields.put(Bean.DOCUMENT_ID, Table.TEXT);
		customerTable.fields.put("parent_id", Table.ASSOCIATION);
		customerTable.fields.put("owner_id", Table.ASSOCIATION);
		customerTable.fields.put(Bean.CUSTOMER_NAME, Table.TEXT);
		tables.add(customerTable);
		Table customerTableWithoutReferences = new Table("CustomerNoRefs", "ADM_CUSTOMER_NO_REFS");
		customerTableWithoutReferences.fields.put(Bean.CUSTOMER_NAME, Table.TEXT);
		tables.add(customerTableWithoutReferences);
		JoinTable joinTable = new JoinTable("CustomerDoc_items", "ADM_CUSTOMER_DOC_items", "CustomerDoc", "ADM_CUSTOMER_DOC", true);
		tables.add(joinTable);
		Table extensionTable = new Table("ExtensionDoc", "ADM_EXTENSION_DOC");
		extensionTable.fields.put(Bean.DOCUMENT_ID, Table.TEXT);
		extensionTable.fields.put("base_id", Table.ASSOCIATION);
		tables.add(extensionTable);

		try {
			UtilImpl.COMMAND_TRACE = true;
			invokeTruncate(tables, "demo", true, false);
		}
		finally {
			UtilImpl.COMMAND_TRACE = originalCommandTrace;
		}

		ArgumentCaptor<String> sqlCaptor = ArgumentCaptor.forClass(String.class);
		verify(persistence, times(6)).newSQL(sqlCaptor.capture());
		assertThat(sqlCaptor.getAllValues(), is(List.of(
				"update ADM_CUSTOMER_DOC set parent_id = null,owner_id = null where bizCustomer = 'demo'",
				"update ADM_EXTENSION_DOC set base_id = null",
				"delete from ADM_CUSTOMER_DOC_items where owner_id in (select bizId from ADM_CUSTOMER_DOC where bizCustomer = 'demo')",
				"delete from ADM_EXTENSION_DOC",
				"delete from ADM_CUSTOMER_DOC where bizCustomer = 'demo'",
				"delete from ADM_CUSTOMER_NO_REFS where bizCustomer = 'demo'")));
		verify(sql, times(6)).noTimeout();
		verify(sql, times(6)).execute();
		verify(persistence, times(6)).commit(false);
		verify(persistence, times(6)).begin();

		InOrder order = inOrder(persistence);
		for (String expectedSql : sqlCaptor.getAllValues()) {
			order.verify(persistence).newSQL(expectedSql);
			order.verify(persistence).commit(false);
			order.verify(persistence).begin();
		}
	}

	@Test
	public void truncateDoesNothingWhenDatabaseAndContentAreFalse() throws Exception {
		AbstractPersistence persistence = bindMockPersistence();
		Collection<Table> tables = List.of(new Table("CustomerDoc", "ADM_CUSTOMER_DOC"));

		invokeTruncate(tables, "demo", false, false);

		verify(persistence, times(0)).newSQL(anyString());
		verify(persistence, times(0)).commit(false);
		verify(persistence, times(0)).begin();
	}

	@Test
	public void truncateContentOnlyUsesConfiguredContentManager() throws Exception {
		Class<? extends AbstractContentManager> originalContentManagerClass = AbstractContentManager.IMPLEMENTATION_CLASS;
		try {
			AbstractContentManager.IMPLEMENTATION_CLASS = RecordingContentManager.class;
			RecordingContentManager.truncatedCustomerName = null;

			invokeTruncate(List.of(), "demo", false, true);

			assertEquals("demo", RecordingContentManager.truncatedCustomerName);
		}
		finally {
			AbstractContentManager.IMPLEMENTATION_CLASS = originalContentManagerClass;
		}
	}

	private static void invokeTruncate(Collection<Table> tables, String customerName, boolean database, boolean content) throws Exception {
		Method method = Truncate.class.getDeclaredMethod("truncate", Collection.class, String.class, boolean.class, boolean.class);
		method.setAccessible(true);
		method.invoke(null, tables, customerName, Boolean.valueOf(database), Boolean.valueOf(content));
	}

	private static SQL fluentSQL() {
		SQL sql = mock(SQL.class);
		when(sql.noTimeout()).thenReturn(sql);
		return sql;
	}

	private static AbstractPersistence bindMockPersistence() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
		return persistence;
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}

	public static class RecordingContentManager extends NoOpContentManager {
		private static String truncatedCustomerName;

		@Override
		public void truncateIndexing(String customerName) {
			assertNull(truncatedCustomerName);
			truncatedCustomerName = customerName;
		}
	}
}
