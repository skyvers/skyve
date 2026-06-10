package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.controller.CustomisationsStaticSingleton;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator;
import org.skyve.domain.types.converters.Format.TextCase;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.controller.Customisations;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;

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
	void constructorWithConstantDomainBindingUsesCustomisationsAndConstantValueMap() throws Exception {
		User user = mock(User.class);
		CustomerImpl customer = org.mockito.Mockito.spy(new CustomerImpl());
		Module module = mock(Module.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		DocumentImpl document = new DocumentImpl();
		document.setName("Contact");
		document.setOwningModuleName("sales");

		Text status = new Text();
		status.setName("status");
		status.setDisplayName("Status");
		status.setLength(20);
		status.setDomainType(DomainType.constant);
		document.putAttribute(status);

		when(repository.getModule(customer, "sales")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		doReturn(List.of(
				new org.skyve.metadata.model.document.Bizlet.DomainValue("A", "Alpha"),
				new org.skyve.metadata.model.document.Bizlet.DomainValue("B", "Beta")))
				.when(customer).getConstantDomainValues(null, "sales", "Contact", status);

		Customisations customisations = mock(Customisations.class);
		when(customisations.determineDefaultColumnTextAlignment("desktop", org.skyve.metadata.model.Attribute.AttributeType.text))
				.thenReturn(HorizontalAlignment.left);
		when(customisations.determineDefaultColumnWidth("desktop", org.skyve.metadata.model.Attribute.AttributeType.text))
				.thenReturn(Integer.valueOf(88));
		Customisations previousCustomisations = CustomisationsStaticSingleton.get();
		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		try {
			ProvidedRepositoryFactory.set(repository);
			CustomisationsStaticSingleton.set(customisations);
			TestDefinition definition = new TestDefinition(user, customer, module, document, "status", null, false, false, false, "desktop");

			assertEquals("status", definition.getName());
			assertEquals("Status", definition.getTitle());
			assertEquals("enum", definition.getType());
			assertEquals(HorizontalAlignment.left, definition.getAlign());
			assertEquals(Integer.valueOf(88), definition.getPixelWidth());
			assertFalse(definition.required);
			assertNotNull(definition.valueMap);
			assertEquals("Alpha", definition.valueMap.get("A"));
			assertEquals("Beta", definition.valueMap.get("B"));
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
			CustomisationsStaticSingleton.set(previousCustomisations);
		}
	}

	@Test
	void constructorWithTextBindingUsesValidatorMaskAndCustomisations() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		DocumentImpl document = new DocumentImpl();
		document.setName("Contact");
		document.setOwningModuleName("sales");

		Text code = new Text();
		code.setName("code");
		code.setDisplayName("Code");
		code.setLength(12);
		code.setRequired(true);
		code.setRequiredMessage("Code required");
		TextFormat format = new TextFormat();
		format.setMask("AA-##");
		format.setCase(TextCase.upper);
		code.setFormat(format);
		TextValidator validator = new TextValidator();
		validator.setRegularExpression("^[A-Z0-9-]+$");
		code.setValidator(validator);
		document.putAttribute(code);

		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);

		Customisations customisations = mock(Customisations.class);
		when(customisations.determineDefaultWidgetTextAlignment("desktop", org.skyve.metadata.model.Attribute.AttributeType.text))
				.thenReturn(HorizontalAlignment.right);
		when(customisations.determineDefaultColumnWidth("desktop", org.skyve.metadata.model.Attribute.AttributeType.text))
				.thenReturn(Integer.valueOf(120));
		Customisations previousCustomisations = CustomisationsStaticSingleton.get();
		try {
			CustomisationsStaticSingleton.set(customisations);
			TestDefinition definition = new TestDefinition(user, customer, module, document, "code", null, false, false, true, "desktop");

			assertEquals("code", definition.getName());
			assertEquals("Code", definition.getTitle());
			assertEquals("text", definition.getType());
			assertEquals(HorizontalAlignment.right, definition.getAlign());
			assertEquals(Integer.valueOf(120), definition.getPixelWidth());
			assertTrue(definition.required);
			assertEquals("Code required", definition.requiredMessage);
			assertEquals(">AA-##", definition.mask);
			assertEquals("{expression:'^[A-Z0-9-]+$',type:'regexp',errorMessage:'Code is not formatted correctly.'}", definition.validation);
			assertNull(definition.valueMap);
		}
		finally {
			CustomisationsStaticSingleton.set(previousCustomisations);
		}
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
		assertTrue(result.contains("filterEditorProperties:{optionDataSource:'admin_user_lookup'"));
	}

	@Test
	@SuppressWarnings("boxing")
	void appendEditorPropertiesLookupRequiredOmitsAllowEmptyValue() {
		TestDefinition definition = new TestDefinition("relation");
		SmartClientLookupDefinition lookup = mock(SmartClientLookupDefinition.class);
		doReturn(false).when(lookup).isBindingToDataGrid();
		when(lookup.getOptionDataSource()).thenReturn("admin_user_lookup");
		when(lookup.getDisplayField()).thenReturn("bizKey");
		when(lookup.getPickListFields()).thenReturn(List.of("bizKey"));
		when(lookup.getFilterFields()).thenReturn(List.of());
		definition.lookup = lookup;
		definition.setRequiredMessage("Required");

		StringBuilder js = new StringBuilder();
		definition.appendEditorProperties(js, false, null, null);

		String result = js.toString();
		assertFalse(result.contains("allowEmptyValue:true"));
		assertFalse(result.contains("filterFields:["));
	}

	@Test
	@SuppressWarnings("boxing")
	void appendEditorPropertiesLookupWithEmptyPickListStillBuildsEmptyList() {
		TestDefinition definition = new TestDefinition("relation");
		SmartClientLookupDefinition lookup = mock(SmartClientLookupDefinition.class);
		doReturn(false).when(lookup).isBindingToDataGrid();
		when(lookup.getOptionDataSource()).thenReturn("admin_user_lookup");
		when(lookup.getDisplayField()).thenReturn("bizKey");
		when(lookup.getPickListFields()).thenReturn(List.of());
		when(lookup.getFilterFields()).thenReturn(List.of());
		definition.lookup = lookup;

		StringBuilder js = new StringBuilder();
		definition.appendEditorProperties(js, false, null, null);

		String result = js.toString();
		assertTrue(result.contains("pickListFields:[]"));
		assertFalse(result.contains("filterFields:["));
	}

	@Test
	@SuppressWarnings("boxing")
	void appendEditorPropertiesLookupBoundToDataGridUsesBizIdFields() {
		TestDefinition definition = new TestDefinition("relation");
		SmartClientLookupDefinition lookup = mock(SmartClientLookupDefinition.class);
		doReturn(true).when(lookup).isBindingToDataGrid();
		when(lookup.getOptionDataSource()).thenReturn("admin_user_lookup");
		when(lookup.getDisplayField()).thenReturn("bizKey");
		when(lookup.getPickListFields()).thenReturn(List.of("bizKey"));
		when(lookup.getFilterFields()).thenReturn(List.of());
		definition.lookup = lookup;

		StringBuilder js = new StringBuilder();
		definition.appendEditorProperties(js, true, null, null);

		String result = js.toString();
		assertTrue(result.contains("defaultDynamicValue:'if(this.grid){var r=this.grid.getRecord(this.rowNum);var v=(r?r.bizId:null);"));
		assertTrue(result.contains("valueField:'bizId'"));
		assertTrue(result.contains("displayField:'bizKey'"));
		assertFalse(result.contains("filterEditorProperties"));
		assertFalse(result.contains("filterFields:["));
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
	void setMaskAndStyleAppliesLowerCasePrefixWhenMaskPresent() {
		Text text = new Text();
		TextFormat format = new TextFormat();
		format.setMask("AA-##");
		format.setCase(TextCase.lower);
		text.setFormat(format);

		TestDefinition definition = new TestDefinition("code");
		definition.setMaskAndStyle(text);

		assertEquals("<AA-##", definition.mask);
	}

	@Test
	void setMaskAndStyleAppliesUpperCaseStyleWhenMaskMissing() {
		Text text = new Text();
		TextFormat format = new TextFormat();
		format.setCase(TextCase.upper);
		text.setFormat(format);

		TestDefinition definition = new TestDefinition("code");
		definition.setMaskAndStyle(text);

		assertEquals("textItem bizhubTextUpper", definition.textBoxStyle);
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

	@Test
	void setMaskAndStyleEscapesSmartClientMaskTokensAndCapitalisesLetters() {
		Text text = new Text();
		TextFormat format = new TextFormat();
		format.setMask("A0?aC<>L");
		format.setCase(TextCase.capital);
		text.setFormat(format);

		TestDefinition definition = new TestDefinition("code");
		definition.setMaskAndStyle(text);

		assertTrue(definition.mask.contains("\\0"));
		assertTrue(definition.mask.contains("\\?"));
		assertTrue(definition.mask.contains("\\a"));
		assertTrue(definition.mask.contains("\\C"));
		assertTrue(definition.mask.contains("\\<"));
		assertTrue(definition.mask.contains("\\>"));
		assertTrue(definition.mask.contains(">L<"));
	}

	@Test
	void gettersAndSettersExposeConfiguredValues() {
		TestDefinition definition = new TestDefinition("fieldName");

		definition.setEditorType("ComboBoxItem");
		definition.setLength(Integer.valueOf(42));
		definition.setName("renamed");
		definition.setTitle("Renamed");
		definition.setType("integer");
		definition.setHasDisplayField(true);
		definition.setEscape(false);
		definition.setPixelWidth(Integer.valueOf(120));
		definition.setAlign(HorizontalAlignment.right);

		assertEquals("ComboBoxItem", definition.getEditorType());
		assertEquals(Integer.valueOf(42), definition.getLength());
		assertEquals("renamed", definition.getName());
		assertEquals("Renamed", definition.getTitle());
		assertEquals("integer", definition.getType());
		assertTrue(definition.isHasDisplayField());
		assertFalse(definition.isEscape());
		assertEquals(Integer.valueOf(120), definition.getPixelWidth());
		assertEquals(HorizontalAlignment.right, definition.getAlign());
		assertEquals(Map.of(), definition.getValueMap());
	}

	@Test
	void appendEditorPropertiesForImageUsesDefaultDimensionsWhenUnset() {
		TestDefinition definition = new TestDefinition("imageField");
		definition.type = "image";

		StringBuilder js = new StringBuilder();
		definition.appendEditorProperties(js, false, null, null);

		String result = js.toString();
		assertTrue(result.contains("_w=64&_h=64"));
		assertTrue(result.contains("if(rec && rec.bizId){return ''}"));
	}

	private static final class TestDefinition extends SmartClientAttributeDefinition {
		private TestDefinition(String name) {
			super(null, null, null, null, null, name, false, false, false, null);
			this.valueMap = new LinkedHashMap<>();
		}

		private TestDefinition(User user,
							Customer customer,
							Module module,
							org.skyve.metadata.model.document.Document document,
							String binding,
							String name,
							boolean runtime,
							boolean isQueryColumn,
							boolean isField,
							String uxui) {
			super(user, customer, module, document, binding, name, runtime, isQueryColumn, isField, uxui);
		}

		private String valueMapLiteral() {
			return getValueMapAsString();
		}
	}
}