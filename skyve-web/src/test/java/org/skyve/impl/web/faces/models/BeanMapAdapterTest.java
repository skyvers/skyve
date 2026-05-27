package org.skyve.impl.web.faces.models;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;

@SuppressWarnings("static-method")
class BeanMapAdapterTest {

	@Test
	void mapOperationsUseDelegateState() {
		Bean bean = mock(Bean.class);
		BeanMapAdapter adapter = new BeanMapAdapter(bean, null);

		assertTrue(adapter.isEmpty());
		adapter.putAll(Map.of("a", "alpha", "b", "beta"));
		assertEquals(2, adapter.size());
		assertTrue(adapter.containsKey("a"));
		assertTrue(adapter.containsValue("beta"));
		assertEquals("alpha", adapter.remove("a"));
		assertFalse(adapter.containsKey("a"));
		adapter.clear();
		assertTrue(adapter.isEmpty());
	}

	@Test
	void setBeanResetsDelegateCache() {
		Bean bean1 = mock(Bean.class);
		Bean bean2 = mock(Bean.class);
		BeanMapAdapter adapter = new BeanMapAdapter(bean1, null);
		adapter.putAll(Map.of("k", "v"));

		adapter.setBean(bean2);

		assertEquals(bean2, adapter.getBean());
		assertTrue(adapter.isEmpty());
	}

	@Test
	void equalsAndHashCodeDelegateToUnderlyingBean() {
		Bean bean1 = mock(Bean.class);
		Bean bean2 = mock(Bean.class);

		BeanMapAdapter adapter1 = new BeanMapAdapter(bean1, null);
		BeanMapAdapter adapter2 = new BeanMapAdapter(bean2, null);

		assertEquals(bean1.hashCode(), adapter1.hashCode());
		assertEquals(adapter1, adapter1);
		assertNotEquals(adapter1, adapter2);
	}
}