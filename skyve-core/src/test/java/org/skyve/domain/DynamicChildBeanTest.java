package org.skyve.domain;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

class DynamicChildBeanTest {

	private static Map<String, Object> mapWithChildKeys() {
		Map<String, Object> m = new HashMap<>();
		m.put(ChildBean.PARENT_NAME, null);
		m.put(Bean.ORDINAL_NAME, null);
		return m;
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorSetsModuleAndDocument() {
		DynamicChildBean bean = new DynamicChildBean("mod", "Line", mapWithChildKeys());
		assertThat(bean.getBizModule(), is("mod"));
		assertThat(bean.getBizDocument(), is("Line"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getParentNullByDefault() {
		DynamicChildBean bean = new DynamicChildBean("mod", "Line", mapWithChildKeys());
		assertNull(bean.getParent());
	}

	@Test
	@SuppressWarnings("static-method")
	void setParentRoundtrip() {
		DynamicChildBean bean = new DynamicChildBean("mod", "Line", mapWithChildKeys());
		DynamicBean parent = new DynamicBean("mod", "Header", new HashMap<>());
		bean.setParent(parent);
		assertSame(parent, bean.getParent());
	}

	@Test
	@SuppressWarnings("static-method")
	void getBizOrdinalNullByDefault() {
		DynamicChildBean bean = new DynamicChildBean("mod", "Line", mapWithChildKeys());
		assertNull(bean.getBizOrdinal());
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizOrdinalRoundtrip() {
		DynamicChildBean bean = new DynamicChildBean("mod", "Line", mapWithChildKeys());
		bean.setBizOrdinal(Integer.valueOf(3));
		assertThat(bean.getBizOrdinal(), is(Integer.valueOf(3)));
	}
}
