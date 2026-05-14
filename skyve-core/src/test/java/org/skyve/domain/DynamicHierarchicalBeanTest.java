package org.skyve.domain;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

public class DynamicHierarchicalBeanTest {

	private static Map<String, Object> mapWithParentKey() {
		Map<String, Object> m = new HashMap<>();
		m.put(HierarchicalBean.PARENT_ID, null);
		return m;
	}

	@Test
	@SuppressWarnings("static-method")
	public void constructorSetsModuleAndDocument() {
		DynamicHierarchicalBean bean = new DynamicHierarchicalBean("mod", "Doc", mapWithParentKey());
		assertThat(bean.getBizModule(), is("mod"));
		assertThat(bean.getBizDocument(), is("Doc"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getBizParentIdNullByDefault() {
		DynamicHierarchicalBean bean = new DynamicHierarchicalBean("mod", "Doc", mapWithParentKey());
		assertNull(bean.getBizParentId());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setBizParentIdRoundtrip() {
		DynamicHierarchicalBean bean = new DynamicHierarchicalBean("mod", "Doc", mapWithParentKey());
		bean.setBizParentId("parent-id");
		assertThat(bean.getBizParentId(), is("parent-id"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getParentReturnsNullWhenNoParentId() {
		DynamicHierarchicalBean bean = new DynamicHierarchicalBean("mod", "Doc", mapWithParentKey());
		// getBizParentId() is null so getParent() should return null without calling CORE
		assertNull(bean.getParent());
	}
}
