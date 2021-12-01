package org.skyve.metadata.customer.fluent;

import java.util.List;

import org.skyve.impl.metadata.repository.customer.CustomerModuleMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerModulesMetaData;
import org.skyve.metadata.module.Module;

public class FluentCustomerModules {
	private CustomerModulesMetaData modules = null;
	
	public FluentCustomerModules() {
		modules = new CustomerModulesMetaData();
	}
	
	public FluentCustomerModules(CustomerModulesMetaData modules) {
		this.modules = modules;
	}
	
	public FluentCustomerModules from(List<Module> customerModules, Module homeModule) {
		homeModule(homeModule.getName());
		for (Module module : customerModules) {
			addModule(module.getName());
		}
		return this;
	}
	
	public FluentCustomerModules homeModule(String moduleName) {
		modules.setHomeModule(moduleName);
		return this;
	}
	
	public FluentCustomerModules addModule(String moduleName) {
		CustomerModuleMetaData module = new CustomerModuleMetaData();
		module.setName(moduleName);
		modules.getModules().add(module);
		return this;
	}

	public CustomerModulesMetaData get() {
		return modules;
	}
}
