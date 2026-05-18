package org.skyve.domain;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.OptimisticLock;

class DynamicPersistentBeanTest {

	private static Map<String, Object> mapWithPersistentKeys() {
		Map<String, Object> m = new HashMap<>();
		m.put(PersistentBean.VERSION_NAME, null);
		m.put(PersistentBean.LOCK_NAME, null);
		m.put(PersistentBean.FLAG_COMMENT_NAME, null);
		m.put(PersistentBean.TAGGED_NAME, null);
		m.put(Bean.BIZ_KEY, null);
		return m;
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorSetsModuleAndDocument() {
		DynamicPersistentBean bean = new DynamicPersistentBean("mod", "Doc", mapWithPersistentKeys());
		assertThat(bean.getBizModule(), is("mod"));
		assertThat(bean.getBizDocument(), is("Doc"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getBizVersionNullByDefault() {
		DynamicPersistentBean bean = new DynamicPersistentBean("mod", "Doc", mapWithPersistentKeys());
		assertNull(bean.getBizVersion());
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizVersionRoundtrip() {
		DynamicPersistentBean bean = new DynamicPersistentBean("mod", "Doc", mapWithPersistentKeys());
		bean.setBizVersion(Integer.valueOf(5));
		assertThat(bean.getBizVersion(), is(Integer.valueOf(5)));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizLockRoundtrip() {
		DynamicPersistentBean bean = new DynamicPersistentBean("mod", "Doc", mapWithPersistentKeys());
		assertNull(bean.getBizLock());
		OptimisticLock lock = new OptimisticLock("admin", new java.util.Date());
		bean.setBizLock(lock);
		assertThat(bean.getBizLock(), is(lock));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizKeyRoundtrip() {
		DynamicPersistentBean bean = new DynamicPersistentBean("mod", "Doc", mapWithPersistentKeys());
		assertNull(bean.getBizKey());
		bean.setBizKey("myKey");
		assertThat(bean.getBizKey(), is("myKey"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizFlagCommentRoundtrip() {
		DynamicPersistentBean bean = new DynamicPersistentBean("mod", "Doc", mapWithPersistentKeys());
		assertNull(bean.getBizFlagComment());
		bean.setBizFlagComment("flagged");
		assertThat(bean.getBizFlagComment(), is("flagged"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizTaggedRoundtrip() {
		DynamicPersistentBean bean = new DynamicPersistentBean("mod", "Doc", mapWithPersistentKeys());
		assertNull(bean.getBizTagged());
		bean.setBizTagged(Boolean.TRUE);
		assertThat(bean.getBizTagged(), is(Boolean.TRUE));
	}
}
