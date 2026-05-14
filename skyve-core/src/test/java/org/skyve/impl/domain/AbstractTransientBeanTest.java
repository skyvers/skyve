package org.skyve.impl.domain;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class AbstractTransientBeanTest {

	/** Concrete subclass for testing. */
	private static class ConcreteTransientBean extends AbstractTransientBean {
		private static final long serialVersionUID = 1L;

		@Override
		public String getBizModule() {
			return "test";
		}

		@Override
		public String getBizDocument() {
			return "TestDoc";
		}

		@Override
		public String getBizKey() {
			return getBizId();
		}
	}

	@Test
	void bizIdIsAutoPopulatedOnConstruction() {
		ConcreteTransientBean bean = new ConcreteTransientBean();
		assertNotNull(bean.getBizId());
		assertFalse(bean.getBizId().isEmpty());
	}

	@Test
	void setBizIdUpdatesId() {
		ConcreteTransientBean bean = new ConcreteTransientBean();
		bean.setBizId("custom-id");
		assertTrue("custom-id".equals(bean.getBizId()));
	}

	@Test
	void bizCustomerIsNullByDefault() {
		ConcreteTransientBean bean = new ConcreteTransientBean();
		assertNull(bean.getBizCustomer());
	}

	@Test
	void setBizCustomerUpdatesCustomer() {
		ConcreteTransientBean bean = new ConcreteTransientBean();
		bean.setBizCustomer("acme");
		assertTrue("acme".equals(bean.getBizCustomer()));
	}

	@Test
	void bizDataGroupIdIsNullByDefault() {
		ConcreteTransientBean bean = new ConcreteTransientBean();
		assertNull(bean.getBizDataGroupId());
	}

	@Test
	void setBizDataGroupIdUpdatesDataGroupId() {
		ConcreteTransientBean bean = new ConcreteTransientBean();
		bean.setBizDataGroupId("group1");
		assertTrue("group1".equals(bean.getBizDataGroupId()));
	}

	@Test
	void bizUserIdIsNullByDefault() {
		ConcreteTransientBean bean = new ConcreteTransientBean();
		assertNull(bean.getBizUserId());
	}

	@Test
	void setBizUserIdUpdatesBizUserId() {
		ConcreteTransientBean bean = new ConcreteTransientBean();
		bean.setBizUserId("user1");
		assertTrue("user1".equals(bean.getBizUserId()));
	}

	@Test
	void isPersistedReturnsFalse() {
		ConcreteTransientBean bean = new ConcreteTransientBean();
		assertFalse(bean.isPersisted());
	}

	@Test
	void isNotPersistedReturnsTrue() {
		ConcreteTransientBean bean = new ConcreteTransientBean();
		assertTrue(bean.isNotPersisted());
	}
}
