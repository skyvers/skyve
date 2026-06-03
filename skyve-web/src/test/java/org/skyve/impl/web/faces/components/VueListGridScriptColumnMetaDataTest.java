package org.skyve.impl.web.faces.components;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.skyve.impl.web.service.smartclient.SmartClientQueryColumnDefinition;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.util.Binder.TargetMetaData;

@SuppressWarnings({"static-method", "boxing"})
class VueListGridScriptColumnMetaDataTest {
	@Test
	void toMapUsesImplicitTypeConversionWhenAttributeMissing() throws Exception {
		MetaDataQueryColumn mdColumn = mock(MetaDataQueryColumn.class);
		when(mdColumn.isHidden()).thenReturn(true);

		SmartClientQueryColumnDefinition scColumn = mock(SmartClientQueryColumnDefinition.class);
		when(scColumn.getName()).thenReturn("amount");
		when(scColumn.getTitle()).thenReturn("Amount");
		when(scColumn.isCanFilter()).thenReturn(false);
		when(scColumn.getValueMap()).thenReturn(null);

		TargetMetaData tmd = new TargetMetaData(mock(Document.class), null, Integer.class);
		Map<String, Object> map = invokeToMap(mdColumn, scColumn, tmd, mock(Customer.class));

		assertEquals("amount", map.get("field"));
		assertEquals("Amount", map.get("header"));
		assertEquals("numeric", map.get("type"));
		assertEquals(Boolean.FALSE, map.get("sortable"));
		assertEquals(Boolean.FALSE, map.get("filterable"));
		assertEquals(Boolean.TRUE, map.get("hidden"));
		assertFalse(map.containsKey("enumValues"));
	}

	@Test
	void toMapUsesAttributeFlatteningSortabilityAndEnumValues() throws Exception {
		MetaDataQueryProjectedColumn mdColumn = mock(MetaDataQueryProjectedColumn.class);
		when(mdColumn.isHidden()).thenReturn(false);
		when(mdColumn.isSortable()).thenReturn(true);

		SmartClientQueryColumnDefinition scColumn = mock(SmartClientQueryColumnDefinition.class);
		when(scColumn.getName()).thenReturn("status");
		when(scColumn.getTitle()).thenReturn("Status");
		when(scColumn.isCanFilter()).thenReturn(true);
		Map<String, String> values = new TreeMap<>();
		values.put("A", "Active");
		values.put("I", "Inactive");
		when(scColumn.getValueMap()).thenReturn(values);

		Attribute attribute = mock(Attribute.class);
		when(attribute.getAttributeType()).thenReturn(AttributeType.text);
		when(attribute.getDomainType()).thenReturn(DomainType.constant);
		TargetMetaData tmd = new TargetMetaData(mock(Document.class), attribute, String.class);

		Map<String, Object> map = invokeToMap(mdColumn, scColumn, tmd, mock(Customer.class));

		assertEquals("status", map.get("field"));
		assertEquals("Status", map.get("header"));
		assertEquals("enum", map.get("type"));
		assertEquals(Boolean.TRUE, map.get("sortable"));
		assertEquals(Boolean.TRUE, map.get("filterable"));
		assertEquals(Boolean.FALSE, map.get("hidden"));

		@SuppressWarnings("unchecked")
		var enumValues = (java.util.List<Map<String, String>>) map.get("enumValues");
		assertEquals(2, enumValues.size());
		assertEquals("A", enumValues.get(0).get("value"));
		assertEquals("Active", enumValues.get(0).get("label"));
		assertEquals("I", enumValues.get(1).get("value"));
		assertEquals("Inactive", enumValues.get(1).get("label"));
	}

	@SuppressWarnings("unchecked")
	private static Map<String, Object> invokeToMap(MetaDataQueryColumn mdColumn,
			SmartClientQueryColumnDefinition scColumn,
			TargetMetaData targetMetaData,
			Customer customer) throws Exception {
		Class<?> clazz = Class.forName("org.skyve.impl.web.faces.components.VueListGridScript$ColumnMetaData");
		Constructor<?> constructor = clazz.getDeclaredConstructor(MetaDataQueryColumn.class,
				SmartClientQueryColumnDefinition.class,
				TargetMetaData.class,
				Customer.class);
		constructor.setAccessible(true);
		Object columnMetaData = constructor.newInstance(mdColumn, scColumn, targetMetaData, customer);

		Method toMap = clazz.getDeclaredMethod("toMap");
		toMap.setAccessible(true);
		return (Map<String, Object>) toMap.invoke(columnMetaData);
	}
}
