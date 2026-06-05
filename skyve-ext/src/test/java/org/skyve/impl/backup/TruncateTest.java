package org.skyve.impl.backup;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
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
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.persistence.SQL;

@SuppressWarnings("static-method")
public class TruncateTest {
	@After
	public void teardown() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	public void truncateDatabaseUnlinksAndDeletesTablesInConstraintSafeOrder() throws Exception {
		AbstractPersistence persistence = bindMockPersistence();
		SQL sql = fluentSQL();
		when(persistence.newSQL(anyString())).thenReturn(sql);
		Collection<Table> tables = new ArrayList<>();
		Table customerTable = new Table("CustomerDoc", "ADM_CUSTOMER_DOC");
		customerTable.fields.put(Bean.DOCUMENT_ID, Table.TEXT);
		customerTable.fields.put("parent_id", Table.ASSOCIATION);
		customerTable.fields.put("owner_id", Table.ASSOCIATION);
		customerTable.fields.put(Bean.CUSTOMER_NAME, Table.TEXT);
		tables.add(customerTable);
		JoinTable joinTable = new JoinTable("CustomerDoc_items", "ADM_CUSTOMER_DOC_items", "CustomerDoc", "ADM_CUSTOMER_DOC", true);
		tables.add(joinTable);
		Table extensionTable = new Table("ExtensionDoc", "ADM_EXTENSION_DOC");
		extensionTable.fields.put(Bean.DOCUMENT_ID, Table.TEXT);
		extensionTable.fields.put("base_id", Table.ASSOCIATION);
		tables.add(extensionTable);

		invokeTruncate(tables, "demo", true, false);

		ArgumentCaptor<String> sqlCaptor = ArgumentCaptor.forClass(String.class);
		verify(persistence, times(5)).newSQL(sqlCaptor.capture());
		assertThat(sqlCaptor.getAllValues(), is(List.of(
				"update ADM_CUSTOMER_DOC set parent_id = null,owner_id = null where bizCustomer = 'demo'",
				"update ADM_EXTENSION_DOC set base_id = null",
				"delete from ADM_CUSTOMER_DOC_items where owner_id in (select bizId from ADM_CUSTOMER_DOC where bizCustomer = 'demo')",
				"delete from ADM_EXTENSION_DOC",
				"delete from ADM_CUSTOMER_DOC where bizCustomer = 'demo'")));
		verify(sql, times(5)).noTimeout();
		verify(sql, times(5)).execute();
		verify(persistence, times(5)).commit(false);
		verify(persistence, times(5)).begin();

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
}
