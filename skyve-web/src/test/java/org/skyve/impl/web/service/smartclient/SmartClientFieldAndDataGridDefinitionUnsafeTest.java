package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.controller.CustomisationsStaticSingleton;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Memo;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.metadata.controller.Customisations;

@SuppressWarnings({"static-method", "boxing"})
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
	void fieldToJavascriptEscapesTitleRequiredMessageAndHelpByDefault() throws Exception {
		SmartClientFieldDefinition def = unsafeFieldDefinition();

		String js = def.toJavascript();

		assertTrue(js.contains("title:'" + SmartClientViewRenderer.escapeSmartClientText(unsafeText(), true) + " *'"), js);
		assertTrue(js.contains("requiredMessage:'" + SmartClientViewRenderer.escapeSmartClientText(unsafeText(), true)), js);
		assertTrue(js.contains("prompt:'" + SmartClientViewRenderer.escapeSmartClientText(unsafeText(), true) + "'"), js);
	}

	@Test
	void fieldToJavascriptEscapesTitleRequiredMessageAndHelpWhenExplicitlyTrue() throws Exception {
		SmartClientFieldDefinition def = unsafeFieldDefinition();
		def.setEscapeTitle(Boolean.TRUE);
		def.setEscapeRequiredMessage(Boolean.TRUE);
		def.setEscapeHelp(Boolean.TRUE);

		String js = def.toJavascript();

		assertTrue(js.contains("title:'" + SmartClientViewRenderer.escapeSmartClientText(unsafeText(), true) + " *'"), js);
		assertTrue(js.contains("requiredMessage:'" + SmartClientViewRenderer.escapeSmartClientText(unsafeText(), true) + "'"), js);
		assertTrue(js.contains("prompt:'" + SmartClientViewRenderer.escapeSmartClientText(unsafeText(), true) + "'"), js);
	}

	@Test
	void fieldToJavascriptLeavesTrustedTitleRequiredMessageAndHelpRawWhenFalse() throws Exception {
		SmartClientFieldDefinition def = unsafeFieldDefinition();
		def.setEscapeTitle(Boolean.FALSE);
		def.setEscapeRequiredMessage(Boolean.FALSE);
		def.setEscapeHelp(Boolean.FALSE);

		String js = def.toJavascript();

		assertTrue(js.contains("title:'" + SmartClientViewRenderer.escapeSmartClientText(unsafeText(), false) + " *'"), js);
		assertTrue(js.contains("requiredMessage:'" + SmartClientViewRenderer.escapeSmartClientText(unsafeText(), false) + "'"), js);
		assertTrue(js.contains("prompt:'" + SmartClientViewRenderer.escapeSmartClientText(unsafeText(), false) + "'"), js);
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
	void dataGridToJavascriptEscapesStringDefaultValueAsJavascriptLiteral() {
		DocumentImpl document = new DocumentImpl();
		document.setName("Fixture");
		document.setOwningModuleName("test");
		Memo memo = new Memo();
		memo.setName("memo");
		memo.setDisplayName("Memo");
		memo.setDefaultValue("Test {el:newGeometry('POINT(0 0)')}");
		document.putAttribute(memo);
		TextField widget = new TextField();
		widget.setBinding("memo");
		Customisations customisations = mock(Customisations.class);
		when(customisations.determineDefaultColumnTextAlignment("desktop", memo.getAttributeType()))
				.thenReturn(HorizontalAlignment.left);
		Customisations previousCustomisations = CustomisationsStaticSingleton.get();
		CustomisationsStaticSingleton.set(customisations);

		try {
			SmartClientDataGridFieldDefinition def = new SmartClientDataGridFieldDefinition(null,
					new CustomerImpl(),
					null,
					document,
					widget,
					null,
					false,
					false,
					false,
					"desktop");

			String js = def.toJavascript();

			assertTrue(js.contains("defaultValue:'Test {el:newGeometry(\\'POINT(0 0)\\')}'"), js);
			assertFalse(js.contains("defaultValue:'Test {el:newGeometry('POINT(0 0)')}'"), js);
		}
		finally {
			CustomisationsStaticSingleton.set(previousCustomisations);
		}
	}

	@Test
	void dataGridToJavascriptEscapesTitleByDefault() throws Exception {
		SmartClientDataGridFieldDefinition def = unsafeDataGridDefinition();

		String js = def.toJavascript();

		assertTrue(js.contains("title:'" + SmartClientViewRenderer.escapeSmartClientText(unsafeText(), true) + "'"), js);
		assertTrue(js.contains("requiredMessage:'" + SmartClientViewRenderer.escapeSmartClientText(unsafeText(), true)), js);
	}

	@Test
	void dataGridToJavascriptEscapesTitleWhenExplicitlyTrue() throws Exception {
		SmartClientDataGridFieldDefinition def = unsafeDataGridDefinition();
		def.setEscapeTitle(Boolean.TRUE);

		String js = def.toJavascript();

		assertTrue(js.contains("title:'" + SmartClientViewRenderer.escapeSmartClientText(unsafeText(), true) + "'"), js);
	}

	@Test
	void dataGridToJavascriptLeavesTrustedTitleRawWhenFalse() throws Exception {
		SmartClientDataGridFieldDefinition def = unsafeDataGridDefinition();
		def.setEscapeTitle(Boolean.FALSE);

		String js = def.toJavascript();

		assertTrue(js.contains("title:'" + SmartClientViewRenderer.escapeSmartClientText(unsafeText(), false) + "'"), js);
		assertTrue(js.contains("requiredMessage:'" + SmartClientViewRenderer.escapeSmartClientText(unsafeText(), true)), js);
	}

	@Test
	void dataGridEditableGetterReflectsSetterAndAllowsEditingInJavascript() throws Exception {
		SmartClientDataGridFieldDefinition def = allocate(SmartClientDataGridFieldDefinition.class);
		def.name = "description";
		def.title = "Description";
		def.type = "text";

		def.setEditable(true);

		assertTrue(def.getEditable());
		assertFalse(def.toJavascript().contains("canEdit:false"));
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
		Class<?> unsafeType = Class.forName("sun.misc.Unsafe");
		Field f = unsafeType.getDeclaredField("theUnsafe");
		f.setAccessible(true);
		Object unsafe = f.get(null);
		Method allocateInstance = unsafeType.getMethod("allocateInstance", Class.class);
		return type.cast(allocateInstance.invoke(unsafe, type));
	}

	private static SmartClientFieldDefinition unsafeFieldDefinition() throws Exception {
		SmartClientFieldDefinition def = allocate(SmartClientFieldDefinition.class);
		def.name = "unsafe";
		def.title = unsafeText();
			def.type = "text";
			def.required = true;
			def.requiredMessage = unsafeText();
			def.setEscapeTitle(true);
			def.setEscapeRequiredMessage(true);
			def.setEscapeHelp(true);
			def.setHelpText(unsafeText());
			return def;
		}

	private static SmartClientDataGridFieldDefinition unsafeDataGridDefinition() throws Exception {
		SmartClientDataGridFieldDefinition def = allocate(SmartClientDataGridFieldDefinition.class);
		def.name = "unsafe";
			def.title = unsafeText();
			def.type = "text";
			def.required = true;
			def.setEscapeTitle(true);
			return def;
		}

	private static String unsafeText() {
		return "<img src=x onerror=alert(1)> & \"quoted\" 'single'";
	}
}
