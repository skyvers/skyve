package org.skyve.impl.sail.execution.sc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.sc.Locator.InputType;
import org.skyve.impl.sail.execution.sc.Locator.Kind;
import org.skyve.metadata.sail.language.step.context.PushListContext;

@SuppressWarnings("static-method")
class SmartClientAutomationContextTest {
	private static final String LIST_NEW = "sail:{\"kind\":\"list\",\"part\":\"new\"}";
	private static final String LIST_ZOOM = "sail:{\"kind\":\"list\",\"part\":\"zoom\"}";
	private static final String LIST_ROWS = "sail:{\"kind\":\"list\",\"part\":\"rows\"}/body/row[%d]/col[%d]";

	@Test
	void locatorStoresSelectorAndOptionalInputType() {
		Locator plain = new Locator("//plain");
		Locator typed = new Locator("//typed", InputType.CHECKBOX);

		assertEquals("//plain", plain.getLocator());
		assertNull(plain.getInputType());
		assertFalse(plain.isSail());
		assertEquals("//typed", typed.getLocator());
		assertEquals(InputType.CHECKBOX, typed.getInputType());
	}

	@Test
	void sailLocatorIsJsonSpecFollowedByAutoTestSuffix() {
		Locator item = Locator.item("admin.Contact", "name", "/element", InputType.TEXT);

		assertEquals("sail:{\"view\":\"admin.Contact\",\"kind\":\"item\",\"name\":\"name\"}/element", item.getLocator());
		assertEquals(InputType.TEXT, item.getInputType());
		assertTrue(item.isSail());
	}

	@Test
	void sailLocatorEscapesJsonAndOmitsNulls() {
		assertEquals("sail:{\"view\":\"m.D\",\"kind\":\"tab\",\"name\":\"Say \\\"hi\\\" \\\\ Backup/Restore\"}",
						Locator.sail("m.D", Kind.tab, "Say \"hi\" \\ Backup/Restore", null, null, null, null));
		assertEquals("sail:{\"kind\":\"listGrid\",\"ordinal\":2,\"part\":\"rows\"}",
						Locator.sail(null, Kind.listGrid, null, null, Integer.valueOf(2), "rows", null));
		assertEquals("sail:{\"view\":\"m.D\",\"kind\":\"cell\",\"name\":\"grid\",\"field\":\"flag\"}",
						Locator.cell("m.D", "grid", "flag", null, InputType.CHECKBOX).getLocator());
	}

	@Test
	void putAccumulatesLocatorsByIdentifierWithoutDuplicates() {
		SmartClientAutomationContext context = new SmartClientAutomationContext();
		Locator first = new Locator("//first");
		Locator second = new Locator("//second", InputType.TEXT);

		context.put("customer.name", first);
		context.put("customer.name", second);
		context.put("customer.name", new Locator("//first"));

		assertEquals(List.of(first, second), context.getLocators("customer.name"));
		assertNull(context.getLocators("missing"));
	}

	@Test
	void inlineGridBindingIsTracked() {
		SmartClientAutomationContext context = new SmartClientAutomationContext();
		assertNull(context.getInlineGridBinding());
		context.setInlineGridBinding("inlineGrid");
		assertEquals("inlineGrid", context.getInlineGridBinding());
		context.setInlineGridBinding(null);
		assertNull(context.getInlineGridBinding());
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void generateListContextRegistersToolbarAndSelectionLocators() {
		SmartClientAutomationContext context = new SmartClientAutomationContext();
		PushListContext push = new PushListContext();
		push.setModuleName("sales");
		push.setDocumentName("Customer");
		push.setQueryName("activeCustomers");

		context.generate(new SmartClientGenerateListContext(push));

		assertEquals(LIST_NEW, context.getLocators("sales.activeCustomers.new").get(0).getLocator());
		assertEquals(LIST_ZOOM, context.getLocators("sales.activeCustomers.zoom").get(0).getLocator());
		assertEquals(LIST_ROWS, context.getLocators("sales.activeCustomers.select").get(0).getLocator());
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void generateListContextUsesCurrentContextWhenPushOmitsModuleAndDocument() {
		SmartClientAutomationContext context = new SmartClientAutomationContext();
		context.setModuleName("admin");
		context.setDocumentName("Contact");
		PushListContext push = new PushListContext();
		push.setModelName("recent");

		context.generate(new SmartClientGenerateListContext(push));

		assertEquals(LIST_NEW, context.getLocators("admin.Contact.recent.new").get(0).getLocator());
		assertEquals(LIST_ROWS, context.getLocators("admin.Contact.recent.select").get(0).getLocator());
	}
}
