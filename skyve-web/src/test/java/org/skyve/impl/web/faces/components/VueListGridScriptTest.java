package org.skyve.impl.web.faces.components;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Method;
import java.util.Map;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class VueListGridScriptTest {
	@Test
	void stateConstructorStoresAttributesAndToStringIncludesConfiguredState() {
		VueListGridScript script = new VueListGridScript("container",
				"admin",
				"Invoice",
				"sales",
				"Order",
				"qOrders",
				"modelOrders",
				"ctx-1",
				true,
				false,
				true,
				false,
				true,
				"onSelected");

		Map<String, Object> attributes = script.getAttributes();
		assertEquals("container", attributes.get("containerId"));
		assertEquals("admin", attributes.get("owningModuleName"));
		assertEquals("Invoice", attributes.get("owningDocumentName"));
		assertEquals("sales", attributes.get("drivingModuleName"));
		assertEquals("Order", attributes.get("drivingDocumentName"));
		assertEquals("qOrders", attributes.get("queryName"));
		assertEquals("modelOrders", attributes.get("modelName"));
		assertEquals("ctx-1", attributes.get("contextId"));
		assertEquals(Boolean.TRUE, attributes.get("showAdd"));
		assertEquals(Boolean.FALSE, attributes.get("showZoom"));
		assertEquals(Boolean.TRUE, attributes.get("showFilter"));
		assertEquals(Boolean.FALSE, attributes.get("showSummary"));
		assertEquals(Boolean.TRUE, attributes.get("showSnap"));
		assertEquals("onSelected", attributes.get("selectedRemoteCommand"));

		String asString = script.toString();
		assertTrue(asString.contains("owningModuleName=admin"));
		assertTrue(asString.contains("owningDocumentName=Invoice"));
		assertTrue(asString.contains("queryName=qOrders"));
		assertTrue(asString.contains("modelName=modelOrders"));
		assertTrue(asString.contains("contextId=ctx-1"));
		assertTrue(asString.contains("containerId=container"));
	}

	@Test
	void stateConstructorOmitsOptionalNullAttributes() {
		VueListGridScript script = new VueListGridScript("container",
				"admin",
				"Invoice",
				"sales",
				"Order",
				null,
				null,
				null,
				false,
				false,
				false,
				false,
				false,
				null);

		Map<String, Object> attributes = script.getAttributes();
		assertFalse(attributes.containsKey("queryName"));
		assertFalse(attributes.containsKey("modelName"));
		assertFalse(attributes.containsKey("contextId"));
		assertFalse(attributes.containsKey("selectedRemoteCommand"));
	}

	@Test
	void grabAttributesRestoresTransientStateFromComponentAttributes() throws Exception {
		VueListGridScript script = new VueListGridScript();
		Map<String, Object> attributes = script.getAttributes();
		attributes.put("containerId", "container-a");
		attributes.put("owningModuleName", "admin");
		attributes.put("owningDocumentName", "Invoice");
		attributes.put("drivingModuleName", "sales");
		attributes.put("drivingDocumentName", "Order");
		attributes.put("queryName", "invoiceQuery");
		attributes.put("modelName", "invoiceModel");
		attributes.put("contextId", "ctx-a");
		attributes.put("showAdd", Boolean.TRUE);
		attributes.put("showZoom", Boolean.TRUE);
		attributes.put("showFilter", Boolean.FALSE);
		attributes.put("showSummary", Boolean.TRUE);
		attributes.put("showSnap", Boolean.FALSE);
		attributes.put("selectedRemoteCommand", "onSelection");

		Method grabAttributes = VueListGridScript.class.getDeclaredMethod("grabAttributes", Map.class);
		grabAttributes.setAccessible(true);
		grabAttributes.invoke(script, attributes);

		String asString = script.toString();
		assertTrue(asString.contains("owningModuleName=admin"));
		assertTrue(asString.contains("owningDocumentName=Invoice"));
		assertTrue(asString.contains("queryName=invoiceQuery"));
		assertTrue(asString.contains("modelName=invoiceModel"));
		assertTrue(asString.contains("contextId=ctx-a"));
		assertTrue(asString.contains("containerId=container-a"));
	}

	@Test
	void defaultConstructorCreatesUsableComponentState() {
		VueListGridScript script = new VueListGridScript();

		assertNotNull(script.getAttributes());
		assertNotNull(script.toString());
	}
}