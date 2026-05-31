package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.LinkedHashMap;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.HorizontalAlignment;

import sun.misc.Unsafe;

class SmartClientFieldAndDataGridDefinitionUnsafeTest {
	@Test
	void fieldToJavascriptIncludesLookupMaskValidationHelpAndTextAlign() throws Exception {
		SmartClientFieldDefinition def = allocate(SmartClientFieldDefinition.class);
		def.name = "customer";
		def.title = "Customer";
		def.type = "text";
		def.editorType = "comboBox";
		def.length = Integer.valueOf(42);
		def.valueMap = new LinkedHashMap<>();
		def.valueMap.put("A", "Active");
		def.required = true;
		def.requiredMessage = "Need customer";
		def.mask = "###-###";
		def.textBoxStyle = "inputField";
		def.validation = "{type:'regexp'}";
		def.align = HorizontalAlignment.right;
		def.setHelpText("Use\nhelp");

		SmartClientLookupDefinition lookup = mock(SmartClientLookupDefinition.class);
		when(lookup.getOptionDataSource()).thenReturn("mod_query_doc_rel");
		when(lookup.getDisplayField()).thenReturn("bizKey");
		when(lookup.getPickListFields()).thenReturn(List.of("code", "label"));
		when(lookup.getFilterFields()).thenReturn(List.of("label"));
		def.lookup = lookup;

		String js = def.toJavascript();
		assertTrue(js.contains("name:'customer'"));
		assertTrue(js.contains("editorType:'comboBox'"));
		assertTrue(js.contains("length:42"));
		assertTrue(js.contains("valueMap:{"));
		assertTrue(js.contains("bizRequired:true,requiredMessage:'Need customer'"));
		assertTrue(js.contains("mask:'###-###',maskSaveLiterals:true"));
		assertTrue(js.contains("textBoxStyle:'inputField '"));
		assertTrue(js.contains("validators:[{type:'regexp'}]"));
		assertTrue(js.contains("textAlign:'right'"));
		assertTrue(js.contains("icons:[{src:'icons/help.png'"));
		assertTrue(js.contains("optionDataSource:'mod_query_doc_rel'"));
		assertTrue(js.contains("pickListFields:[{name:'code'},{name:'label'}]"));
		assertTrue(js.contains("filterFields:['label']"));
	}

	@Test
	void fieldToJavascriptAddsAllowEmptyForSelectAndSkipsCheckboxTextAlign() throws Exception {
		SmartClientFieldDefinition def = allocate(SmartClientFieldDefinition.class);
		def.name = "status";
		def.title = "Status";
		def.type = "select";
		def.required = false;
		def.align = HorizontalAlignment.left;

		String js = def.toJavascript();
		assertTrue(js.contains("allowEmptyValue:true"));

		def.type = "checkbox";
		String checkboxJs = def.toJavascript();
		assertFalse(checkboxJs.contains("textAlign:"));
	}

	@Test
	void dataGridToJavascriptIncludesCoreFlagsAndRangeFields() throws Exception {
		SmartClientDataGridFieldDefinition def = allocate(SmartClientDataGridFieldDefinition.class);
		def.name = "amount";
		def.title = "Amount";
		def.type = "integer";
		def.defaultValueJavascriptExpression = "10";
		def.editorType = "spinner";
		def.required = true;
		def.requiredMessage = "Need amount";
		def.valueMap = new LinkedHashMap<>();
		def.valueMap.put("1", "One");
		def.align = HorizontalAlignment.centre;
		def.length = Integer.valueOf(8);
		def.setEditable(false);
		def.pixelWidth = Integer.valueOf(120);
		def.escape = true;

		String js = def.toJavascript();
		assertTrue(js.contains("name:'amount'"));
		assertTrue(js.contains("defaultValue:10"));
		assertTrue(js.contains("editorType:'spinner'"));
		assertTrue(js.contains("bizRequired:true,requiredMessage:'Need amount'"));
		assertTrue(js.contains("valueMap:{"));
		assertTrue(js.contains("align:'center'"));
		assertTrue(js.contains("length:8"));
		assertTrue(js.contains("canEdit:false"));
		assertTrue(js.contains("width:120"));
		assertTrue(js.contains("escapeHTML:true"));
	}

	@Test
	void dataGridToJavascriptIncludesLookupEditorPropertiesWhenNotBindingToGrid() throws Exception {
		SmartClientDataGridFieldDefinition def = allocate(SmartClientDataGridFieldDefinition.class);
		def.name = "customerId";
		def.title = "Customer";
		def.type = "text";
		def.required = false;

		SmartClientLookupDefinition lookup = mock(SmartClientLookupDefinition.class);
		when(lookup.isBindingToDataGrid()).thenReturn(false);
		when(lookup.getOptionDataSource()).thenReturn("mod_query_doc_rel");
		when(lookup.getDisplayField()).thenReturn("bizKey");
		when(lookup.getPickListFields()).thenReturn(List.of("code", "label"));
		when(lookup.getFilterFields()).thenReturn(List.of("label"));
		def.lookup = lookup;

		String js = def.toJavascript();
		assertTrue(js.contains("editorProperties:{optionDataSource:'mod_query_doc_rel'"));
		assertTrue(js.contains("allowEmptyValue:true"));
		assertTrue(js.contains("var v=(r?r.customerId:null);"));
		assertTrue(js.contains("vm[v]=r.customerId_bizKey;return v;"));
		assertTrue(js.contains("sortByDisplayField:true,valueField:'customerId',displayField:'customerId_bizKey'"));
		assertTrue(js.contains("pickListFields:[{name:'code'},{name:'label'}]"));
		assertTrue(js.contains("filterFields:['label']"));
	}

	@Test
	void dataGridToJavascriptIncludesLookupEditorPropertiesWhenBindingToGrid() throws Exception {
		SmartClientDataGridFieldDefinition def = allocate(SmartClientDataGridFieldDefinition.class);
		def.name = "customer";
		def.title = "Customer";
		def.type = "text";
		def.required = true;

		SmartClientLookupDefinition lookup = mock(SmartClientLookupDefinition.class);
		when(lookup.isBindingToDataGrid()).thenReturn(true);
		when(lookup.getOptionDataSource()).thenReturn("mod_query_doc_rel");
		when(lookup.getDisplayField()).thenReturn("bizKey");
		when(lookup.getPickListFields()).thenReturn(List.of());
		when(lookup.getFilterFields()).thenReturn(List.of());
		def.lookup = lookup;

		String js = def.toJavascript();
		assertTrue(js.contains("var v=(r?r.bizId:null);"));
		assertTrue(js.contains("vm[v]=r.bizKey;return v;"));
		assertTrue(js.contains("sortByDisplayField:true,valueField:'bizId',displayField:'bizKey'"));
		assertTrue(js.contains("pickListFields:[]"));
		assertFalse(js.contains("allowEmptyValue:true"));
		assertFalse(js.contains("filterFields:["));
	}

	@Test
	void dataGridToJavascriptIncludesGeometryImageAndLinkFormatters() throws Exception {
		SmartClientDataGridFieldDefinition def = allocate(SmartClientDataGridFieldDefinition.class);
		def.name = "location";
		def.title = "Location";
		def.type = "geometry";

		String geometryJs = def.toJavascript();
		assertTrue(geometryJs.contains("formatCellValue:function(v){return isc.GeometryItem.format(v)}"));

		def.type = "image";
		def.pixelWidth = Integer.valueOf(32);
		String imageJs = def.toJavascript();
		assertTrue(imageJs.contains("formatCellValue:function(v,rec,row,col){if(v){var u='content?_n='+v+'"));
		assertTrue(imageJs.contains("&_w=32&_h=32"));

		def.type = "link";
		String linkJs = def.toJavascript();
		assertTrue(linkJs.contains("formatCellValue:function(v,rec,row,col){return (v ? '<a href=\"content?_n='+v+"));
	}

	private static <T> T allocate(Class<T> type) throws Exception {
		Field f = Unsafe.class.getDeclaredField("theUnsafe");
		f.setAccessible(true);
		Unsafe unsafe = (Unsafe) f.get(null);
		return type.cast(unsafe.allocateInstance(type));
	}
}
