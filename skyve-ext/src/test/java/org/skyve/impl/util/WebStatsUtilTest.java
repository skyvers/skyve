package org.skyve.impl.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.nullable;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.skyve.domain.Bean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.SQL;
import org.skyve.web.UserAgentType;

@SuppressWarnings("static-method")
class WebStatsUtilTest {
	private final String originalCustomer = UtilImpl.CUSTOMER;

	@AfterEach
	void tearDown() throws Exception {
		UtilImpl.CUSTOMER = originalCustomer;
		unbindPersistenceFromThread();
	}

	@Test
	void recordHitInsertsWhenNoMonthlyRowExistsAndTruncatesUserAgent() throws Exception {
		UtilImpl.CUSTOMER = null;
		AbstractPersistence persistence = bindPersistence();
		SQL select = sqlReturning(List.of());
		SQL insert = sqlReturning(List.of());
		when(persistence.newSQL(anyString())).thenReturn(select, insert);
		User user = user();
		String longHeader = "x".repeat(450);

		WebStatsUtil.recordHit(user, longHeader, UserAgentType.phone);

		ArgumentCaptor<String> sql = ArgumentCaptor.forClass(String.class);
		verify(persistence, times(2)).newSQL(sql.capture());
		assertTrue(sql.getAllValues().get(0).contains("and bizCustomer = :customer"));
		assertTrue(sql.getAllValues().get(1).startsWith("insert into ADM_UserMonthlyHits"));
		verify(select).putParameter("device", "P", false);
		verify(select).putParameter("userAgentHeader", longHeader.substring(0, 400), false);
		verify(insert).putParameter("device", "P", false);
		verify(insert).putParameter("userAgentHeader", longHeader.substring(0, 400), false);
		verify(insert).execute();
		verify(persistence).begin();
		verify(persistence).commit(true);
	}

	@Test
	void recordHitUpdatesExistingMonthlyRowAndHandlesNullUserAgent() throws Exception {
		UtilImpl.CUSTOMER = "singleTenant";
		AbstractPersistence persistence = bindPersistence();
		SQL select = sqlReturning(List.of("MONTHLY-1"));
		SQL update = sqlReturning(List.of());
		when(persistence.newSQL(anyString())).thenReturn(select, update);

		WebStatsUtil.recordHit(user(), null, UserAgentType.tablet);

		ArgumentCaptor<String> sql = ArgumentCaptor.forClass(String.class);
		verify(persistence, times(2)).newSQL(sql.capture());
		assertTrue(sql.getAllValues().get(0).contains("and userAgentHeader is null"));
		assertTrue(! sql.getAllValues().get(0).contains("and bizCustomer = :customer"));
		assertTrue(sql.getAllValues().get(1).startsWith("update ADM_UserMonthlyHits"));
		verify(select).putParameter("device", "T", false);
		verify(update).putParameter(Bean.DOCUMENT_ID, "MONTHLY-1", false);
		verify(update).execute();
		verify(persistence).commit(true);
	}

	@Test
	void recordHitRollsBackAndStillCommitsWhenSelectFails() throws Exception {
		AbstractPersistence persistence = bindPersistence();
		SQL select = sqlReturning(List.of());
		when(select.scalarResults(String.class)).thenThrow(new IllegalStateException("boom"));
		when(persistence.newSQL(anyString())).thenReturn(select);
		User currentUser = user();

		assertThrows(IllegalStateException.class, () -> WebStatsUtil.recordHit(currentUser, "agent", UserAgentType.other));

		verify(select).putParameter("device", "O", false);
		verify(persistence).rollback();
		verify(persistence).commit(true);
	}

	@Test
	void recordLoginCreatesLoginRecordForUser() throws Exception {
		AbstractPersistence persistence = bindPersistence();
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document loginRecordDocument = mock(Document.class);
		TestLoginRecord loginRecord = loginRecordBean();
		when(user.getName()).thenReturn("mike");
		when(user.getCustomer()).thenReturn(customer);
		when(persistence.getUser()).thenReturn(user);
		when(customer.getModule(AppConstants.ADMIN_MODULE_NAME)).thenReturn(module);
		when(module.getDocument(customer, AppConstants.USER_LOGIN_RECORD_DOCUMENT_NAME)).thenReturn(loginRecordDocument);
		when(loginRecordDocument.newInstance(user)).thenReturn(loginRecord);

		WebStatsUtil.recordLogin(user, "203.0.113.42");

		assertEquals("mike", loginRecord.getDynamic(AppConstants.USER_NAME_ATTRIBUTE_NAME));
		assertTrue(loginRecord.getDynamic(AppConstants.LOGIN_DATE_TIME_ATTRIBUTE_NAME) instanceof DateTime);
		assertEquals(Boolean.FALSE, loginRecord.getDynamic(AppConstants.FAILED_ATTRIBUTE_NAME));
		assertEquals("203.0.113.42", loginRecord.getDynamic(AppConstants.IP_ADDRESS_ATTRIBUTE_NAME));
		verify(persistence).setUser(user);
		verify(persistence).save(loginRecordDocument, loginRecord);
	}

	private static AbstractPersistence bindPersistence() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);
		return persistence;
	}

	private static User user() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Persistent persistent = mock(Persistent.class);
		when(user.getName()).thenReturn("mike");
		when(user.getId()).thenReturn("USER-1");
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getName()).thenReturn("bizhub");
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "UserMonthlyHits")).thenReturn(document);
		when(document.getPersistent()).thenReturn(persistent);
		when(persistent.getPersistentIdentifier()).thenReturn("ADM_UserMonthlyHits");
		return user;
	}

	private static SQL sqlReturning(List<String> scalarResults) {
		SQL sql = mock(SQL.class);
		when(sql.putParameter(anyString(), any(Integer.class))).thenReturn(sql);
		when(sql.putParameter(anyString(), nullable(String.class), eq(false))).thenReturn(sql);
		when(sql.scalarResults(String.class)).thenReturn(scalarResults);
		return sql;
	}

	private static TestLoginRecord loginRecordBean() {
		TestLoginRecord bean = new TestLoginRecord();
		HashMap<String, Object> properties = new HashMap<>();
		properties.put(AppConstants.USER_NAME_ATTRIBUTE_NAME, null);
		properties.put(AppConstants.LOGIN_DATE_TIME_ATTRIBUTE_NAME, null);
		properties.put(AppConstants.FAILED_ATTRIBUTE_NAME, null);
		properties.put(AppConstants.IP_ADDRESS_ATTRIBUTE_NAME, null);
		bean.putAllDynamic(properties);
		return bean;
	}

	private static class TestLoginRecord extends AbstractPersistentBean {
		private static final long serialVersionUID = 1L;

		@Override
		public String getBizModule() {
			return AppConstants.ADMIN_MODULE_NAME;
		}

		@Override
		public String getBizDocument() {
			return AppConstants.USER_LOGIN_RECORD_DOCUMENT_NAME;
		}

		@Override
		public String getBizKey() {
			return "login";
		}
	}

	@SuppressWarnings("unchecked")
	private static void bindPersistenceToThread(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).set(persistence);
	}

	@SuppressWarnings("unchecked")
	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}
}
