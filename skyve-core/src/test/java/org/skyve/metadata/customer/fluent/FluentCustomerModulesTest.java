package org.skyve.metadata.customer.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.customer.CustomerModulesMetaData;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;

class FluentCustomerModulesTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesInstance() {
		FluentCustomerModules m = new FluentCustomerModules();
		assertNotNull(m.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesSupplied() {
		CustomerModulesMetaData meta = new CustomerModulesMetaData();
		FluentCustomerModules m = new FluentCustomerModules(meta);
		assertSame(meta, m.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void homeModuleSetsValue() {
		FluentCustomerModules m = new FluentCustomerModules();
		FluentCustomerModules result = m.homeModule("admin");
		assertSame(m, result);
		assertEquals("admin", m.get().getHomeModule());
	}

	@Test
	@SuppressWarnings("static-method")
	void addModuleByNameAddsEntry() {
		FluentCustomerModules m = new FluentCustomerModules();
		FluentCustomerModules result = m.addModule("admin");
		assertSame(m, result);
		assertEquals(1, m.get().getModules().size());
		assertEquals("admin", m.get().getModules().get(0).getName());
	}

	@Test
	@SuppressWarnings("static-method")
	void addModuleWithLayoutSetsLayout() {
		FluentCustomerModules m = new FluentCustomerModules();
		m.addModule("admin", FormLabelLayout.side);
		assertEquals(FormLabelLayout.side, m.get().getModules().get(0).getFormLabelLayout());
	}

	@Test
	@SuppressWarnings("static-method")
	void removeModuleRemovesEntry() {
		FluentCustomerModules m = new FluentCustomerModules();
		m.addModule("admin");
		FluentCustomerModules result = m.removeModule("admin");
		assertSame(m, result);
		assertTrue(m.get().getModules().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void removeModuleClearsHomeModuleWhenMatching() {
		FluentCustomerModules m = new FluentCustomerModules();
		m.homeModule("admin");
		m.addModule("admin");
		m.removeModule("admin");
		assertNull(m.get().getHomeModule());
	}

	@Test
	@SuppressWarnings("static-method")
	void clearModulesEmptiesCollection() {
		FluentCustomerModules m = new FluentCustomerModules();
		m.addModule("admin");
		m.addModule("contacts");
		FluentCustomerModules result = m.clearModules();
		assertSame(m, result);
		assertTrue(m.get().getModules().isEmpty());
	}
}
