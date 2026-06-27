package org.skyve.metadata.customer.fluent;

import java.util.List;

import org.skyve.impl.metadata.repository.customer.CustomerModuleMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerModulesMetaData;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.metadata.module.Module;

/**
 * Builds customer module membership metadata and home-module selection.
 */
public class FluentCustomerModules {
	private CustomerModulesMetaData modules = null;
	
	/**
	 * Creates a builder with new empty customer-modules metadata.
	 */
	public FluentCustomerModules() {
		modules = new CustomerModulesMetaData();
	}
	
	/**
	 * Creates a builder around existing customer-modules metadata.
	 *
	 * @param modules backing metadata
	 */
	public FluentCustomerModules(CustomerModulesMetaData modules) {
		this.modules = modules;
	}
	
	/**
	 * Copies module entries and home module from a customer contract.
	 *
	 * @param customerModules module collection
	 * @param homeModule home module
	 * @return this builder
	 */
	public FluentCustomerModules from(List<Module> customerModules, Module homeModule) {
		homeModule(homeModule.getName());
		for (Module module : customerModules) {
			addModule(module.getName(), module.getFormLabelLayout());
		}
		return this;
	}
	
	/**
	 * Sets the home module name.
	 *
	 * @param moduleName home module name
	 * @return this builder
	 */
	public FluentCustomerModules homeModule(String moduleName) {
		modules.setHomeModule(moduleName);
		return this;
	}
	
	/**
	 * Adds a module entry using default module layout behaviour.
	 *
	 * @param moduleName module name
	 * @return this builder
	 */
	public FluentCustomerModules addModule(String moduleName) {
		return addModule(moduleName, null);
	}

	/**
	 * Adds a module entry with an optional form label layout override.
	 *
	 * @param moduleName module name
	 * @param layout optional form label layout override
	 * @return this builder
	 */
	public FluentCustomerModules addModule(String moduleName, FormLabelLayout layout) {
		CustomerModuleMetaData module = new CustomerModuleMetaData();
		module.setName(moduleName);
		module.setFormLabelLayout(layout);
		modules.getModules().add(module);
		return this;
	}

	/**
	 * Removes one module entry and clears home-module setting if it matches.
	 *
	 * @param moduleName module name
	 * @return this builder
	 */
	public FluentCustomerModules removeModule(String moduleName) {
		modules.getModules().removeIf(m -> moduleName.equals(m.getName()));
		if (moduleName.equals(modules.getHomeModule())) {
			modules.setHomeModule(null);
		}
		return this;
	}
	
	/**
	 * Removes all module entries.
	 *
	 * @return this builder
	 */
	public FluentCustomerModules clearModules() {
		modules.getModules().clear();
		return this;
	}
	
	/**
	 * Returns the mutable metadata instance being built.
	 *
	 * @return backing customer-modules metadata
	 */
	public CustomerModulesMetaData get() {
		return modules;
	}
}
