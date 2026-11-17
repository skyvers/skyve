package org.skyve.impl.sail.execution.sc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.sail.language.step.context.PushEditContext;

import util.AbstractH2Test;

/**
 * Generates SmartClient locators for a real edit view through {@link SmartClientSAILViewVisitor}.
 */
@SuppressWarnings("static-method")
class SmartClientSAILViewVisitorH2Test extends AbstractH2Test {
	/**
	 * kitchensink.ConditionsFixture's edit view declares named implicit actions (Save1, Cancel1, Defaults1...).
	 */
	@Test
	void namedImplicitActionIsAlsoAddressedByItsName() {
		SmartClientAutomationContext context = conditionsFixtureContext();

		List<Locator> save = context.getLocators("Save");
		List<Locator> save1 = context.getLocators("Save1");
		assertNotNull(save);
		assertNotNull(save1);
		assertEquals(save.get(0).getLocator(), save1.get(0).getLocator());
	}

	@Test
	void namedDefaultsActionIsNotAddressedByItsName() {
		SmartClientAutomationContext context = conditionsFixtureContext();

		assertNull(context.getLocators("Defaults1"));
	}

	private static SmartClientAutomationContext conditionsFixtureContext() {
		SmartClientAutomationContext context = new SmartClientAutomationContext();
		context.setUxui("desktop");
		PushEditContext push = new PushEditContext();
		push.setModuleName("kitchensink");
		push.setDocumentName("ConditionsFixture");
		context.generate(new SmartClientGenerateEditContext(push));
		return context;
	}
}
