package org.skyve.content;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.Map;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.domain.PersistentBean;

@SuppressWarnings("static-method")
class BeanContentTest {

	private static PersistentBean mockBean(String customer, String module, String document,
			String dataGroupId, String userId, String bizId) {
		PersistentBean bean = Mockito.mock(PersistentBean.class);
		Mockito.when(bean.getBizCustomer()).thenReturn(customer);
		Mockito.when(bean.getBizModule()).thenReturn(module);
		Mockito.when(bean.getBizDocument()).thenReturn(document);
		Mockito.when(bean.getBizDataGroupId()).thenReturn(dataGroupId);
		Mockito.when(bean.getBizUserId()).thenReturn(userId);
		Mockito.when(bean.getBizId()).thenReturn(bizId);
		return bean;
	}

	@Test
	void testConstructorStoresFieldsFromBean() {
		PersistentBean bean = mockBean("demo", "admin", "User", "dg1", "user1", "biz1");

		BeanContent content = new BeanContent(bean);

		assertEquals("demo", content.getBizCustomer());
		assertEquals("admin", content.getBizModule());
		assertEquals("User", content.getBizDocument());
		assertEquals("dg1", content.getBizDataGroupId());
		assertEquals("user1", content.getBizUserId());
		assertEquals("biz1", content.getBizId());
	}

	@Test
	void testConstructorWithNullDataGroupId() {
		PersistentBean bean = mockBean("demo", "admin", "User", null, "user1", "biz1");

		BeanContent content = new BeanContent(bean);

		assertNotNull(content);
		assertEquals("biz1", content.getBizId());
	}

	@Test
	void testGetPropertiesReturnsEmptyMapByDefault() {
		PersistentBean bean = mockBean("demo", "admin", "User", null, "user1", "biz1");

		BeanContent content = new BeanContent(bean);
		Map<String, String> properties = content.getProperties();

		assertNotNull(properties);
		assertEquals(0, properties.size());
	}

	@Test
	void testGetPropertiesMapIsMutable() {
		PersistentBean bean = mockBean("demo", "admin", "User", null, "user1", "biz1");

		BeanContent content = new BeanContent(bean);
		content.getProperties().put("key", "value");

		assertEquals("value", content.getProperties().get("key"));
	}
}
