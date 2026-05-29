package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.LinkedHashMap;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.converters.Format.TextCase;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;

@SuppressWarnings("static-method")
class SmartClientAttributeDefinitionTest {

	@Test
	void constructorWithNullBindingUsesProvidedName() {
		TestDefinition definition = new TestDefinition("fieldName");

		assertEquals("fieldName", definition.getName());
		assertEquals("fieldName", definition.getTitle());
		assertEquals("text", definition.getType());
	}

	@Test
	void valueMapAsStringReturnsPlaceholderWhenEmpty() {
		TestDefinition definition = new TestDefinition("fieldName");
		definition.valueMap = new LinkedHashMap<>();

		assertEquals("[' ']", definition.valueMapLiteral());
	}

	@Test
	void valueMapAsStringBuildsJavascriptObjectWhenPresent() {
		TestDefinition definition = new TestDefinition("fieldName");
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		map.put("A", "Alpha");
		map.put("B", "Beta");
		definition.valueMap = map;

		assertEquals("{'A':'Alpha', 'B':'Beta'}", definition.valueMapLiteral());
	}

	@Test
	void appendEditorPropertiesAllowsEmptyForTriStateCheckbox() {
		TestDefinition definition = new TestDefinition("fieldName");
		definition.triStateCheckBox = true;

		StringBuilder js = new StringBuilder();
		definition.appendEditorProperties(js, false, null, null);

		assertTrue(js.toString().contains("allowEmptyValue:true"));
	}

	@Test
	void appendEditorPropertiesForGeometryAddsFormatter() {
		TestDefinition definition = new TestDefinition("fieldName");
		definition.type = "geometry";

		StringBuilder js = new StringBuilder();
		definition.appendEditorProperties(js, false, null, null);

		assertTrue(js.toString().contains("isc.GeometryItem.format"));
	}

	@Test
	void appendEditorPropertiesForImageBuildsThumbnailFormatter() {
		TestDefinition definition = new TestDefinition("imageField");
		definition.type = "image";
		definition.pixelWidth = Integer.valueOf(72);

		StringBuilder js = new StringBuilder();
		definition.appendEditorProperties(js, false, Integer.valueOf(48), "images/placeholder.png");

		String result = js.toString();
		assertTrue(result.contains("formatCellValue:function"));
		assertTrue(result.contains("_w=72&_h=48"));
		assertTrue(result.contains("images/placeholder.png"));
	}

	@Test
	void appendEditorPropertiesForLinkBuildsContentAnchor() {
		TestDefinition definition = new TestDefinition("contentBinding");
		definition.type = "link";

		StringBuilder js = new StringBuilder();
		definition.appendEditorProperties(js, false, null, null);

		assertTrue(js.toString().contains("target=\"_blank\""));
		assertTrue(js.toString().contains("content?_n='+v"));
	}

	@Test
	void appendEditorPropertiesAddsMaskValidationAndDisplayField() {
		TestDefinition definition = new TestDefinition("code");
		definition.setMask("AA-##");
		definition.setTextBoxStyle("textItem customStyle");
		definition.validation = "{type:'regexp'}";
		definition.hasDisplayField = true;

		StringBuilder js = new StringBuilder();
		definition.appendEditorProperties(js, false, null, null);

		String result = js.toString();
		assertTrue(result.contains("mask:'AA-##'"));
		assertTrue(result.contains("textBoxStyle:'textItem customStyle '"));
		assertTrue(result.contains("validators:[{type:'regexp'}]"));
		assertTrue(result.contains("displayField:'_display_code'"));
	}

	@Test
	@SuppressWarnings("boxing")
	void appendEditorPropertiesUsesLookupConfigurationWhenPresent() {
		TestDefinition definition = new TestDefinition("relation");
		SmartClientLookupDefinition lookup = mock(SmartClientLookupDefinition.class);
		doReturn(false).when(lookup).isBindingToDataGrid();
		when(lookup.getOptionDataSource()).thenReturn("admin_user_lookup");
		when(lookup.getDisplayField()).thenReturn("bizKey");
		when(lookup.getPickListFields()).thenReturn(List.of("bizKey", "status"));
		when(lookup.getFilterFields()).thenReturn(List.of("bizKey"));
		definition.lookup = lookup;

		StringBuilder js = new StringBuilder();
		definition.appendEditorProperties(js, false, null, null);

		String result = js.toString();
		assertTrue(result.contains("optionDataSource:'admin_user_lookup'"));
		assertTrue(result.contains("pickListFields:[{name:'bizKey'},{name:'status'}]"));
		assertTrue(result.contains("filterFields:['bizKey']"));
		assertTrue(result.contains("valueField:'relation'"));
		assertTrue(result.contains("displayField:'relation_bizKey'"));
	}

	@Test
	void setRequiredMessageControlsEnumAllowEmptyBehavior() {
		TestDefinition definition = new TestDefinition("choice");
		definition.type = "enum";

		definition.setRequiredMessage(null);
		StringBuilder optionalJs = new StringBuilder();
		definition.appendEditorProperties(optionalJs, false, null, null);
		assertTrue(optionalJs.toString().contains("allowEmptyValue:true"));

		definition.setRequiredMessage("Required");
		StringBuilder requiredJs = new StringBuilder();
		definition.appendEditorProperties(requiredJs, false, null, null);
		assertFalse(requiredJs.toString().contains("allowEmptyValue:true"));
	}

	@Test
	void setMaskAndStyleAppliesUpperCasePrefixWhenMaskPresent() {
		Text text = new Text();
		TextFormat format = new TextFormat();
		format.setMask("AA-##");
		format.setCase(TextCase.upper);
		text.setFormat(format);

		TestDefinition definition = new TestDefinition("code");
		definition.setMaskAndStyle(text);

		assertEquals(">AA-##", definition.mask);
	}

	@Test
	void setMaskAndStyleSetsTextBoxStyleWhenCaseHasNoMask() {
		Text text = new Text();
		TextFormat format = new TextFormat();
		format.setCase(TextCase.lower);
		text.setFormat(format);

		TestDefinition definition = new TestDefinition("code");
		definition.setMaskAndStyle(text);

		assertEquals("textItem bizhubTextLower", definition.textBoxStyle);
	}

	private static final class TestDefinition extends SmartClientAttributeDefinition {
		private TestDefinition(String name) {
			super(null, null, null, null, null, name, false, false, false, null);
			this.valueMap = new LinkedHashMap<>();
		}

		private String valueMapLiteral() {
			return getValueMapAsString();
		}
	}
}