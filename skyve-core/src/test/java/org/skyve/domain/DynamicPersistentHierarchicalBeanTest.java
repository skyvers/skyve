package org.skyve.domain;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

public class DynamicPersistentHierarchicalBeanTest {

	private static Map<String, Object> mapWithAllKeys() {
		Map<String, Object> m = new HashMap<>();
		m.put(PersistentBean.VERSION_NAME, null);
		m.put(PersistentBean.LOCK_NAME, null);
		m.put(PersistentBean.FLAG_COMMENT_NAME, null);
		m.put(PersistentBean.TAGGED_NAME, null);
		m.put(Bean.BIZ_KEY, null);
		m.put(HierarchicalBean.PARENT_ID, null);
		return m;
	}

	@Test
	@SuppressWarnings("static-method")
	public void constructorSetsModuleAndDocument() {
		DynamicPersistentHierarchicalBean bean = new DynamicPersistentHierarchicalBean("mod", "Doc", mapWithAllKeys());
		assertThat(bean.getBizModule(), is("mod"));
		assertThat(bean.getBizDocument(), is("Doc"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getBizParentIdNullByDefault() {
		DynamicPersistentHierarchicalBean bean = new DynamicPersistentHierarchicalBean("mod", "Doc", mapWithAllKeys());
		assertNull(bean.getBizParentId());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setBizParentIdRoundtrip() {
		DynamicPersistentHierarchicalBean bean = new DynamicPersistentHierarchicalBean("mod", "Doc", mapWithAllKeys());
		bean.setBizParentId("parent-id");
		assertThat(bean.getBizParentId(), is("parent-id"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setBizVersionRoundtrip() {
		DynamicPersistentHierarchicalBean bean = new DynamicPersistentHierarchicalBean("mod", "Doc", mapWithAllKeys());
		bean.setBizVersion(Integer.valueOf(2));
		assertThat(bean.getBizVersion(), is(Integer.valueOf(2)));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getParentReturnsNullWhenNoParentId() {
		DynamicPersistentHierarchicalBean bean = new DynamicPersistentHierarchicalBean("mod", "Doc", mapWithAllKeys());
		// No bizParentId set → getParent() short-circuits and returns null without CORE
		assertNull(bean.getParent());
	}
}
