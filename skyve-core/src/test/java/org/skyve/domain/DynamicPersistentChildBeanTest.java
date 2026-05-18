package org.skyve.domain;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

class DynamicPersistentChildBeanTest {

	private static Map<String, Object> mapWithAllKeys() {
		Map<String, Object> m = new HashMap<>();
		m.put(PersistentBean.VERSION_NAME, null);
		m.put(PersistentBean.LOCK_NAME, null);
		m.put(PersistentBean.FLAG_COMMENT_NAME, null);
		m.put(PersistentBean.TAGGED_NAME, null);
		m.put(Bean.BIZ_KEY, null);
		m.put(ChildBean.PARENT_NAME, null);
		m.put(Bean.ORDINAL_NAME, null);
		return m;
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorSetsModuleAndDocument() {
		DynamicPersistentChildBean bean = new DynamicPersistentChildBean("mod", "Line", mapWithAllKeys());
		assertThat(bean.getBizModule(), is("mod"));
		assertThat(bean.getBizDocument(), is("Line"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getParentNullByDefault() {
		DynamicPersistentChildBean bean = new DynamicPersistentChildBean("mod", "Line", mapWithAllKeys());
		assertNull(bean.getParent());
	}

	@Test
	@SuppressWarnings("static-method")
	void setParentRoundtrip() {
		DynamicPersistentChildBean bean = new DynamicPersistentChildBean("mod", "Line", mapWithAllKeys());
		DynamicBean parent = new DynamicBean("mod", "Header", new HashMap<>());
		bean.setParent(parent);
		assertSame(parent, bean.getParent());
	}

	@Test
	@SuppressWarnings("static-method")
	void getBizOrdinalNullByDefault() {
		DynamicPersistentChildBean bean = new DynamicPersistentChildBean("mod", "Line", mapWithAllKeys());
		assertNull(bean.getBizOrdinal());
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizOrdinalRoundtrip() {
		DynamicPersistentChildBean bean = new DynamicPersistentChildBean("mod", "Line", mapWithAllKeys());
		bean.setBizOrdinal(Integer.valueOf(2));
		assertThat(bean.getBizOrdinal(), is(Integer.valueOf(2)));
	}
}
