package org.skyve.impl.web.faces.components;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.metadata.view.widget.FilterParameterImpl;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.metadata.FilterOperator;

import jakarta.el.ELContext;
import jakarta.el.ValueExpression;
import jakarta.faces.context.FacesContext;

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
		assertFalse(attributes.containsKey("stickyHeaderAnchorSelector"));
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
		assertFalse(attributes.containsKey("stickyHeaderAnchorSelector"));
	}

	@Test
	void stickyHeaderAnchorSelectorConstructorStoresAttributesAndToStringRemainsUsable() {
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
				null,
				".layout-topbar,#header");

		Map<String, Object> attributes = script.getAttributes();
		assertEquals(".layout-topbar,#header", attributes.get("stickyHeaderAnchorSelector"));
		assertNotNull(script.toString());
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
		attributes.put("stickyHeaderAnchorSelector", ".layout-topbar,#header");
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

	@Test
	void setManagedBeanNameStoresAttributeWithoutRecursing() {
		VueListGridScript script = new VueListGridScript();

		// "managedBeanName" as an attribute key would make Faces call setManagedBeanName() again
		script.setManagedBeanName("skyve");
		assertEquals("skyve", script.getAttributes().get("managedBean"));

		script.setManagedBeanName(null);
		assertFalse(script.getAttributes().containsKey("managedBean"));
	}

	@Test
	@SuppressWarnings("unchecked")
	void setParametersStoresFilterParametersAndParametersLikeSmartClient() {
		FilterParameterImpl filterParameter = new FilterParameterImpl();
		filterParameter.setFilterBinding("baseOffice.name");
		filterParameter.setOperator(FilterOperator.greater);
		filterParameter.setValueBinding("bizId");
		ParameterImpl parameter = new ParameterImpl();
		parameter.setName("bizId");
		parameter.setValue("fixed");

		VueListGridScript script = new VueListGridScript();
		script.setParameters(List.of(filterParameter), List.of(parameter));

		List<Map<String, String>> params = (List<Map<String, String>>) script.getAttributes().get("params");
		assertEquals(2, params.size());
		Map<String, String> filter = params.get(0);
		assertEquals("baseOffice_name", filter.get("name"));
		assertEquals("greaterThan", filter.get("operator"));
		assertEquals("bizId", filter.get("valueBinding"));
		assertFalse(filter.containsKey("value"));
		assertEquals("baseOffice.name", filter.get("newName"));
		Map<String, String> param = params.get(1);
		assertEquals(":bizId", param.get("name"));
		assertEquals("equals", param.get("operator"));
		assertEquals("fixed", param.get("value"));
		assertFalse(param.containsKey("valueBinding"));
		assertEquals("bizId", param.get("newName"));
	}

	@Test
	void setParametersWithNothingDefinedRemovesTheAttribute() {
		VueListGridScript script = new VueListGridScript();
		ParameterImpl parameter = new ParameterImpl();
		parameter.setName("bizId");
		script.setParameters(null, List.of(parameter));
		assertTrue(script.getAttributes().containsKey("params"));

		script.setParameters(List.of(), null);

		assertFalse(script.getAttributes().containsKey("params"));
	}

	@Test
	void resolveParamValueUsesLiteralValueWhenThereIsNoBinding() {
		assertEquals("fixed", VueListGridScript.resolveParamValue(param(null, "fixed"), null));
		assertNull(VueListGridScript.resolveParamValue(param(null, null), null));
	}

	@Test
	void resolveParamValueIsNullForABindingWithoutABean() {
		assertNull(VueListGridScript.resolveParamValue(param("name", null), null));
	}

	@Test
	void resolveParamValueSerialisesBoundValues() {
		Enumeration status = mock(Enumeration.class);
		when(status.toCode()).thenReturn("inOffice");
		Map<String, Object> officeValues = new HashMap<>();
		officeValues.put(Bean.DOCUMENT_ID, "office-1");
		Map<String, Object> values = new HashMap<>();
		values.put("office", new DynamicBean("whosin", "Office", officeValues));
		values.put("status", status);
		values.put("count", Integer.valueOf(3));
		values.put("missing", null);
		DynamicBean bean = new DynamicBean("whosin", "Staff", values);

		assertEquals("office-1", VueListGridScript.resolveParamValue(param("office", null), bean));
		assertEquals("inOffice", VueListGridScript.resolveParamValue(param("status", null), bean));
		assertEquals("3", VueListGridScript.resolveParamValue(param("count", null), bean));
		assertNull(VueListGridScript.resolveParamValue(param("missing", null), bean));
	}

	private static Map<String, String> param(String valueBinding, String value) {
		Map<String, String> result = new TreeMap<>();
		result.put("name", "p");
		result.put("operator", "equals");
		if (valueBinding != null) {
			result.put("valueBinding", valueBinding);
		}
		if (value != null) {
			result.put("value", value);
		}
		return result;
	}

	@Test
	void setUxUiNameAndShowContextMenuStoreAttributes() {
		VueListGridScript script = new VueListGridScript();

		script.setUxUiName("external");
		script.setShowContextMenu(false);
		assertEquals("external", script.getAttributes().get("uxui"));
		assertEquals(Boolean.FALSE, script.getAttributes().get("contextMenu"));

		script.setUxUiName(null);
		script.setShowContextMenu(true);
		assertFalse(script.getAttributes().containsKey("uxui"));
		assertEquals(Boolean.TRUE, script.getAttributes().get("contextMenu"));
	}

	@Test
	void evaluateConditionIsTrueOnlyForATrueValueExpression() throws Exception {
		Method evaluateCondition = VueListGridScript.class.getDeclaredMethod("evaluateCondition", FacesContext.class, String.class);
		evaluateCondition.setAccessible(true);
		VueListGridScript script = new VueListGridScript();
		FacesContext context = mock(FacesContext.class);
		ELContext elContext = mock(ELContext.class);
		when(context.getELContext()).thenReturn(elContext);

		// no value expression
		assertEquals(Boolean.FALSE, evaluateCondition.invoke(script, context, VueListGridScript.DISABLE_ADD_VALUE_EXPRESSION));

		ValueExpression trueExpression = mock(ValueExpression.class);
		when(trueExpression.getValue(elContext)).thenReturn(Boolean.TRUE);
		script.setValueExpression(VueListGridScript.DISABLE_ADD_VALUE_EXPRESSION, trueExpression);
		ValueExpression falseExpression = mock(ValueExpression.class);
		when(falseExpression.getValue(elContext)).thenReturn(Boolean.FALSE);
		script.setValueExpression(VueListGridScript.DISABLE_ZOOM_VALUE_EXPRESSION, falseExpression);

		assertEquals(Boolean.TRUE, evaluateCondition.invoke(script, context, VueListGridScript.DISABLE_ADD_VALUE_EXPRESSION));
		assertEquals(Boolean.FALSE, evaluateCondition.invoke(script, context, VueListGridScript.DISABLE_ZOOM_VALUE_EXPRESSION));
		// no faces context
		assertEquals(Boolean.FALSE, evaluateCondition.invoke(script, null, VueListGridScript.DISABLE_ADD_VALUE_EXPRESSION));
	}

	@Test
	void evaluateConditionFallsBackToALiteralAttributeValue() throws Exception {
		Method evaluateCondition = VueListGridScript.class.getDeclaredMethod("evaluateCondition", FacesContext.class, String.class);
		evaluateCondition.setAccessible(true);
		VueListGridScript script = new VueListGridScript();
		script.getAttributes().put(VueListGridScript.DISABLE_ADD_VALUE_EXPRESSION, Boolean.TRUE);
		script.getAttributes().put(VueListGridScript.DISABLE_ZOOM_VALUE_EXPRESSION, "false");

		assertEquals(Boolean.TRUE, evaluateCondition.invoke(script, null, VueListGridScript.DISABLE_ADD_VALUE_EXPRESSION));
		assertEquals(Boolean.FALSE, evaluateCondition.invoke(script, null, VueListGridScript.DISABLE_ZOOM_VALUE_EXPRESSION));
	}
}
