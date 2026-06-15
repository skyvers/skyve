package org.skyve.impl.bind;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.beans.PropertyDescriptor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class DeproxyingPropertyUtilsBeanTest {

	/** Simple POJO used as a test fixture. */
	public static class SampleBean {
		private String name;
		private List<String> items = new ArrayList<>();
		private Map<String, Object> attributes = new HashMap<>();

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}

		public List<String> getItems() {
			return items;
		}

		public void setItems(List<String> items) {
			this.items = items;
		}

		public Map<String, Object> getAttributes() {
			return attributes;
		}

		public void setAttributes(Map<String, Object> attributes) {
			this.attributes = attributes;
		}
	}

	@Test
	void getSimplePropertyReturnsValue() throws Exception {
		DeproxyingPropertyUtilsBean bean = new DeproxyingPropertyUtilsBean();
		SampleBean sample = new SampleBean();
		sample.setName("hello");
		Object result = bean.getSimpleProperty(sample, "name");
		assertEquals("hello", result);
	}

	@Test
	void getSimplePropertyReturnsNullWhenNotSet() throws Exception {
		DeproxyingPropertyUtilsBean bean = new DeproxyingPropertyUtilsBean();
		SampleBean sample = new SampleBean();
		Object result = bean.getSimpleProperty(sample, "name");
		assertNull(result);
	}

	@Test
	void setSimplePropertyUpdatesValue() throws Exception {
		DeproxyingPropertyUtilsBean bean = new DeproxyingPropertyUtilsBean();
		SampleBean sample = new SampleBean();
		bean.setSimpleProperty(sample, "name", "world");
		assertEquals("world", sample.getName());
	}

	@Test
	void setSimplePropertyWithNullValue() throws Exception {
		DeproxyingPropertyUtilsBean bean = new DeproxyingPropertyUtilsBean();
		SampleBean sample = new SampleBean();
		sample.setName("existing");
		bean.setSimpleProperty(sample, "name", null);
		assertNull(sample.getName());
	}

	@Test
	void getPropertyTypeReturnsCorrectClass() throws Exception {
		DeproxyingPropertyUtilsBean bean = new DeproxyingPropertyUtilsBean();
		SampleBean sample = new SampleBean();
		Class<?> type = bean.getPropertyType(sample, "name");
		assertEquals(String.class, type);
	}

	@Test
	void getPropertyDescriptorReturnsNonNull() throws Exception {
		DeproxyingPropertyUtilsBean bean = new DeproxyingPropertyUtilsBean();
		SampleBean sample = new SampleBean();
		PropertyDescriptor descriptor = bean.getPropertyDescriptor(sample, "name");
		assertNotNull(descriptor);
		assertEquals("name", descriptor.getName());
	}

	@Test
	void getIndexedPropertyReturnsElement() throws Exception {
		DeproxyingPropertyUtilsBean bean = new DeproxyingPropertyUtilsBean();
		SampleBean sample = new SampleBean();
		sample.getItems().add("first");
		sample.getItems().add("second");
		Object result = bean.getIndexedProperty(sample, "items", 0);
		assertEquals("first", result);
	}

	@Test
	void setIndexedPropertyUpdatesElement() throws Exception {
		DeproxyingPropertyUtilsBean bean = new DeproxyingPropertyUtilsBean();
		SampleBean sample = new SampleBean();
		sample.getItems().add("old");
		bean.setIndexedProperty(sample, "items", 0, "new");
		assertEquals("new", sample.getItems().get(0));
	}

	@Test
	void getMappedPropertyReturnsValue() throws Exception {
		DeproxyingPropertyUtilsBean bean = new DeproxyingPropertyUtilsBean();
		SampleBean sample = new SampleBean();
		sample.getAttributes().put("colour", "red");
		Object result = bean.getMappedProperty(sample, "attributes", "colour");
		assertEquals("red", result);
	}

	@Test
	void setMappedPropertyUpdatesValue() throws Exception {
		DeproxyingPropertyUtilsBean bean = new DeproxyingPropertyUtilsBean();
		SampleBean sample = new SampleBean();
		bean.setMappedProperty(sample, "attributes", "colour", "blue");
		assertEquals("blue", sample.getAttributes().get("colour"));
	}
}
