package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

@SuppressWarnings("static-method")
class BeanForReportMessageTest {
	@AfterEach
	void clearThreadLocalPersistence() throws Exception {
		getThreadLocalPersistence().remove();
	}

	@Test
	void getMessageWithLiteralStringReturnsUnchanged() {
		assertThat(new BeanForReport(), notNullValue());

		// Binder.formatMessage with no binding placeholders returns the message unchanged.
		// We use a null bean because there are no placeholders to resolve.
		String message = "Hello, world!";
		String result = BeanForReport.getMessage(null, message);
		assertThat(result, notNullValue());
		assertThat(result, is(message));
	}

	@Test
	void getMessageWithNullMessageThrowsNPE() {
		// BindUtil.formatMessage(null, bean) does not guard against a null message string
		org.junit.jupiter.api.Assertions.assertThrows(
				NullPointerException.class,
				() -> BeanForReport.getMessage(null, null));
	}

	@Test
	@SuppressWarnings("boxing")
	void delegatesPersistenceBackedOperations() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		Bean bean = mock(Bean.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("mod")).thenReturn(module);
		when(module.getDocument(customer, "doc")).thenReturn(document);
		doReturn(bean).when(persistence).retrieve(document, "123");
		persistence.setUser(user);
		when(bean.evaluateCondition("editable")).thenReturn(true);

		ThreadLocal<AbstractPersistence> threadLocal = getThreadLocalPersistence();
		AbstractPersistence original = threadLocal.get();
		try {
			threadLocal.set(persistence);
			assertSame(bean, BeanForReport.getBean("mod", "doc", "123"));
			assertSame(user, BeanForReport.getUser());
			assertThat(BeanForReport.getMessage("mod", "doc", "123", "Hello"), is("Hello"));
			assertTrue(BeanForReport.evaluateCondition("mod", "doc", "123", "editable"));
			assertFalse(BeanForReport.evaluateCondition("mod", "doc", "123", "missing"));
		}
		finally {
			if (original == null) {
				threadLocal.remove();
			}
			else {
				threadLocal.set(original);
			}
		}
	}

	@SuppressWarnings("unchecked")
	private static ThreadLocal<AbstractPersistence> getThreadLocalPersistence() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		return (ThreadLocal<AbstractPersistence>) field.get(null);
	}
}
