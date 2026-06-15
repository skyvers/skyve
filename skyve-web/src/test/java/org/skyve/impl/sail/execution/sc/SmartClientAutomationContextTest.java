package org.skyve.impl.sail.execution.sc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.sc.Locator.InputType;
import org.skyve.metadata.sail.language.step.context.PushListContext;

@SuppressWarnings("static-method")
class SmartClientAutomationContextTest {
	@Test
	void locatorStoresSelectorAndOptionalInputType() {
		Locator plain = new Locator("//plain");
		Locator typed = new Locator("//typed", InputType.CHECKBOX);

		assertEquals("//plain", plain.getLocator());
		assertNull(plain.getInputType());
		assertEquals("//typed", typed.getLocator());
		assertEquals(InputType.CHECKBOX, typed.getInputType());
	}

	@Test
	void putAccumulatesLocatorsByIdentifier() {
		SmartClientAutomationContext context = new SmartClientAutomationContext();
		Locator first = new Locator("//first");
		Locator second = new Locator("//second", InputType.TEXT);

		context.put("customer.name", first);
		context.put("customer.name", second);

		assertEquals(List.of(first, second), context.getLocators("customer.name"));
		assertNull(context.getLocators("missing"));
	}

	@Test
	void generateListContextRegistersToolbarAndSelectionLocators() {
		SmartClientAutomationContext context = new SmartClientAutomationContext();
		PushListContext push = new PushListContext();
		push.setModuleName("sales");
		push.setDocumentName("Customer");
		push.setQueryName("activeCustomers");

		context.generate(new SmartClientGenerateListContext(push));

		assertEquals("//:VLayout[ID=\"details\"]//ToolStripButton[name=\"new\"]",
						context.getLocators("sales.activeCustomers.new").get(0).getLocator());
		assertEquals("//:VLayout[ID=\"details\"]//ToolStripButton[name=\"zoom\"]",
						context.getLocators("sales.activeCustomers.zoom").get(0).getLocator());
		assertEquals("//VLayout[ID=\"details\"]/member[Class=VLayout||classIndex=0]/member[Class=BizListGrid||classIndex=0]/member[Class=ListGrid||classIndex=0]/body/row[%%d]",
						context.getLocators("sales.activeCustomers.select").get(0).getLocator());
	}

	@Test
	void generateListContextUsesCurrentContextWhenPushOmitsModuleAndDocument() {
		SmartClientAutomationContext context = new SmartClientAutomationContext();
		context.setModuleName("admin");
		context.setDocumentName("Contact");
		PushListContext push = new PushListContext();
		push.setModelName("recent");

		context.generate(new SmartClientGenerateListContext(push));

		assertEquals("//:VLayout[ID=\"details\"]//ToolStripButton[name=\"new\"]",
						context.getLocators("admin.Contact.recent.new").get(0).getLocator());
		assertEquals("//VLayout[ID=\"details\"]/member[Class=VLayout||classIndex=0]/member[Class=BizListGrid||classIndex=0]/member[Class=ListGrid||classIndex=0]/body/row[%%d]",
						context.getLocators("admin.Contact.recent.select").get(0).getLocator());
	}
}
