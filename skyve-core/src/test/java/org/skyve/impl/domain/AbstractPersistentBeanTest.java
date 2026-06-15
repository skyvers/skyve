package org.skyve.impl.domain;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.OptimisticLock;

@SuppressWarnings("java:S8692") // system clock OK
class AbstractPersistentBeanTest {

	/** Minimal concrete subclass for testing */
	private static class TestPersistentBean extends AbstractPersistentBean {
		private static final long serialVersionUID = 1L;

		@Override
		public String getBizKey() {
			return "testKey";
		}

		@Override
		public String getBizModule() {
			return "testModule";
		}

		@Override
		public String getBizDocument() {
			return "TestBean";
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultBizIdIsNotNull() {
		TestPersistentBean bean = new TestPersistentBean();
		assertNotNull(bean.getBizId());
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizIdRoundtrip() {
		TestPersistentBean bean = new TestPersistentBean();
		bean.setBizId("custom-id");
		assertThat(bean.getBizId(), is("custom-id"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizVersionRoundtrip() {
		TestPersistentBean bean = new TestPersistentBean();
		assertNull(bean.getBizVersion());
		bean.setBizVersion(Integer.valueOf(3));
		assertThat(bean.getBizVersion(), is(Integer.valueOf(3)));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizLockRoundtrip() {
		TestPersistentBean bean = new TestPersistentBean();
		assertNull(bean.getBizLock());
		OptimisticLock lock = new OptimisticLock("admin", new java.util.Date());
		bean.setBizLock(lock);
		assertThat(bean.getBizLock(), is(lock));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizCustomerRoundtrip() {
		TestPersistentBean bean = new TestPersistentBean();
		assertNull(bean.getBizCustomer());
		bean.setBizCustomer("skyve");
		assertThat(bean.getBizCustomer(), is("skyve"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizTaggedRoundtrip() {
		TestPersistentBean bean = new TestPersistentBean();
		assertNull(bean.getBizTagged());
		bean.setBizTagged(Boolean.TRUE);
		assertThat(bean.getBizTagged(), is(Boolean.TRUE));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizFlagCommentRoundtrip() {
		TestPersistentBean bean = new TestPersistentBean();
		assertNull(bean.getBizFlagComment());
		bean.setBizFlagComment("a comment");
		assertThat(bean.getBizFlagComment(), is("a comment"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizDataGroupIdRoundtrip() {
		TestPersistentBean bean = new TestPersistentBean();
		assertNull(bean.getBizDataGroupId());
		bean.setBizDataGroupId("dg1");
		assertThat(bean.getBizDataGroupId(), is("dg1"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizUserIdRoundtrip() {
		TestPersistentBean bean = new TestPersistentBean();
		assertNull(bean.getBizUserId());
		bean.setBizUserId("user1");
		assertThat(bean.getBizUserId(), is("user1"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getBizKeyBizIdStringContainsBothParts() {
		TestPersistentBean bean = new TestPersistentBean();
		bean.setBizKey("myBizKey");
		bean.setBizId("myBizId");
		String result = bean.getBizKeyBizIdString();
		assertTrue(result.contains("myBizKey"), "result should contain bizKey: " + result);
		assertTrue(result.contains("myBizId"), "result should contain bizId: " + result);
	}
}
