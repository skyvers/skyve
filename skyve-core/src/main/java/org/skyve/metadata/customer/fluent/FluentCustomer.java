package org.skyve.metadata.customer.fluent;

import org.skyve.impl.metadata.repository.customer.CustomerFeatureRoleMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerMetaData;
import org.skyve.impl.metadata.repository.customer.HTMLResourcesMetaData;
import org.skyve.impl.metadata.repository.customer.InterceptorMetaDataImpl;
import org.skyve.impl.metadata.repository.customer.ObserverMetaDataImpl;
import org.skyve.impl.metadata.repository.customer.UIResources;
import org.skyve.metadata.ConverterName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.customer.InterceptorMetaData;
import org.skyve.metadata.customer.ObserverMetaData;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public class FluentCustomer {
	private CustomerMetaData customer = null;
	
	public FluentCustomer() {
		customer = new CustomerMetaData();
	}
	
	public FluentCustomer(@Nonnull CustomerMetaData customer) {
		this.customer = customer;
	}

	public @Nonnull FluentCustomer from(@SuppressWarnings("hiding") @Nonnull Customer customer) {
		name(customer.getName());
		language(customer.getLanguageTag());

		// Populate resources
		logoRelativeFileName(customer.getUiResources().getLogoRelativeFileName());
		cssRelativeFileName(customer.getHtmlResources().getCssRelativeFileName());
		loginResource(new FluentLoginResources().from(customer.getLoginResources()));

		// Populate default converters
		defaultDateConverter(ConverterName.valueOf(customer.getDefaultDateConverter()));
		defaultTimeConverter(ConverterName.valueOf(customer.getDefaultTimeConverter()));
		defaultDateTimeConverter(ConverterName.valueOf(customer.getDefaultDateTimeConverter()));
		defaultTimestampConverter(ConverterName.valueOf(customer.getDefaultTimestampConverter()));

		// Populate module names
		modules(new FluentCustomerModules().from(customer.getModules(), customer.getHomeModule()));

		// Populate Roles
		roles(new FluentCustomerRoles().from(customer.getRoles(), customer.isAllowModuleRoles()));

		// Populate Roles
		roles(new FluentCustomerRoles().from(customer.getRoles(), customer.isAllowModuleRoles()));

		// Populate Interceptors
		for (InterceptorMetaData interceptor : customer.getInterceptors()) {
			addInterceptor(interceptor.getClassName());
		}

		// Populate Observers
		for (ObserverMetaData observer : customer.getObservers()) {
			addInterceptor(observer.getClassName());
		}

		// Populate Chart Processors
		jFreeChartPostProcessorClassName(customer.getJFreeChartPostProcessorClassName());
		primeFacesChartPostProcessorClassName(customer.getPrimeFacesChartPostProcessorClassName());
		
		return this;
	}
	
	public @Nonnull FluentCustomer name(@Nonnull String name) {
		customer.setName(name);
		return this;
	}

	public @Nonnull FluentCustomer language(@Nullable String language) {
		customer.setLanguage(language);
		return this;
	}
	
	public @Nonnull FluentCustomer logoRelativeFileName(@Nullable String logo) {
		UIResources resources = new UIResources();
		resources.setLogoRelativeFileName(logo);
		customer.setUiResources(resources);
		return this;
	}
	
	public @Nonnull FluentCustomer cssRelativeFileName(@Nullable String css) {
		HTMLResourcesMetaData resources = new HTMLResourcesMetaData();
		resources.setCssRelativeFileName(css);
		customer.setHtmlResources(resources);
		return this;
	}

	public @Nonnull FluentCustomer loginResource(@Nonnull FluentLoginResources resources) {
		customer.setLoginResources(resources.get());
		return this;
	}
	
	public @Nonnull FluentCustomer defaultDateConverter(@Nonnull ConverterName converter) {
		customer.setDefaultDateConverter(converter);
		return this;
	}
	
	public @Nonnull FluentCustomer defaultDateTimeConverter(@Nonnull ConverterName converter) {
		customer.setDefaultDateTimeConverter(converter);
		return this;
	}
	
	public @Nonnull FluentCustomer defaultTimeConverter(@Nonnull ConverterName converter) {
		customer.setDefaultTimeConverter(converter);
		return this;
	}
	
	public @Nonnull FluentCustomer defaultTimestampConverter(@Nonnull ConverterName converter) {
		customer.setDefaultTimestampConverter(converter);
		return this;
	}
	
	public @Nonnull FluentCustomer modules(@Nonnull FluentCustomerModules modules) {
		customer.setModules(modules.get());
		return this;
	}
	
	public @Nonnull FluentCustomer roles(@Nonnull FluentCustomerRoles roles) {
		customer.setRoles(roles.get());
		return this;
	}

	public @Nonnull FluentCustomer addTextSearchRole(@Nonnull FluentCustomerFeatureRole role) {
		customer.getTextSearchRoles().add(role.get());
		return this;
	}

	public @Nonnull FluentCustomer removeTextSearchModuleRole(@Nonnull String moduleName, @Nonnull String name) {
		customer.getTextSearchRoles().removeIf(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName()));
		return this;
	}

	public @Nonnull FluentCustomer removeTextSearchCustomerRole(@Nonnull String name) {
		customer.getTextSearchRoles().removeIf(r -> (r.getModuleName() == null) && name.equals(r.getName()));
		return this;
	}

	public @Nonnull FluentCustomer clearTextSearchRoles() {
		customer.getTextSearchRoles().clear();
		return this;
	}
	
	public @Nonnull FluentCustomerFeatureRole findTextSearchModuleRole(@Nonnull String moduleName, @Nonnull String name) {
		CustomerFeatureRoleMetaData result = customer.getTextSearchRoles().stream().filter(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCustomerFeatureRole(result);
		}
		return null;
	}

	public @Nonnull FluentCustomerFeatureRole findTextSearchCustomerRole(@Nonnull String name) {
		CustomerFeatureRoleMetaData result = customer.getTextSearchRoles().stream().filter(r -> (r.getModuleName() == null) && name.equals(r.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCustomerFeatureRole(result);
		}
		return null;
	}

	public @Nonnull FluentCustomer addFlagRole(@Nonnull FluentCustomerFeatureRole role) {
		customer.getFlagRoles().add(role.get());
		return this;
	}

	public @Nonnull FluentCustomer removeFlagModuleRole(@Nonnull String moduleName, @Nonnull String name) {
		customer.getFlagRoles().removeIf(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName()));
		return this;
	}

	public @Nonnull FluentCustomer removeFlagCustomerRole(@Nonnull String name) {
		customer.getFlagRoles().removeIf(r -> (r.getModuleName() == null) && name.equals(r.getName()));
		return this;
	}

	public @Nonnull FluentCustomer clearFlagRoles() {
		customer.getFlagRoles().clear();
		return this;
	}
	
	public @Nullable FluentCustomerFeatureRole findFlagModuleRole(@Nonnull String moduleName, @Nonnull String name) {
		CustomerFeatureRoleMetaData result = customer.getFlagRoles().stream().filter(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCustomerFeatureRole(result);
		}
		return null;
	}

	public @Nullable FluentCustomerFeatureRole findFlagCustomerRole(@Nonnull String name) {
		CustomerFeatureRoleMetaData result = customer.getFlagRoles().stream().filter(r -> (r.getModuleName() == null) && name.equals(r.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCustomerFeatureRole(result);
		}
		return null;
	}

	public @Nonnull FluentCustomer addSwitchModeRole(@Nonnull FluentCustomerFeatureRole role) {
		customer.getSwitchModeRoles().add(role.get());
		return this;
	}

	public @Nonnull FluentCustomer removeSwitchModeModuleRole(@Nonnull String moduleName, @Nonnull String name) {
		customer.getSwitchModeRoles().removeIf(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName()));
		return this;
	}

	public @Nonnull FluentCustomer removeSwitchModeCustomerRole(@Nonnull String name) {
		customer.getSwitchModeRoles().removeIf(r -> (r.getModuleName() == null) && name.equals(r.getName()));
		return this;
	}

	public @Nonnull FluentCustomer clearSwitchModeRoles() {
		customer.getSwitchModeRoles().clear();
		return this;
	}
	
	public @Nullable FluentCustomerFeatureRole findSwitchModeModuleRole(@Nonnull String moduleName, @Nonnull String name) {
		CustomerFeatureRoleMetaData result = customer.getSwitchModeRoles().stream().filter(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCustomerFeatureRole(result);
		}
		return null;
	}

	public @Nullable FluentCustomerFeatureRole findSwitchModeCustomerRole(@Nonnull String name) {
		CustomerFeatureRoleMetaData result = customer.getSwitchModeRoles().stream().filter(r -> (r.getModuleName() == null) && name.equals(r.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCustomerFeatureRole(result);
		}
		return null;
	}

	public @Nonnull FluentCustomer addInterceptor(@Nonnull String fullyQualifiedClassName) {
		InterceptorMetaDataImpl interceptor = new InterceptorMetaDataImpl();
		interceptor.setClassName(fullyQualifiedClassName);
		customer.getInterceptors().add(interceptor);
		return this;
	}
	
	public @Nonnull FluentCustomer removeInterceptor(@Nonnull String fullyQualifiedClassName) {
		customer.getInterceptors().removeIf(i -> fullyQualifiedClassName.equals(i.getClassName()));
		return this;
	}
	
	public @Nonnull FluentCustomer clearInterceptors() {
		customer.getInterceptors().clear();
		return this;
	}
	
	public @Nonnull FluentCustomer addObserver(@Nonnull String fullyQualifiedClassName) {
		ObserverMetaDataImpl observer = new ObserverMetaDataImpl();
		observer.setClassName(fullyQualifiedClassName);
		customer.getObservers().add(observer);
		return this;
	}
	
	public @Nonnull FluentCustomer removeObserver(@Nonnull String fullyQualifiedClassName) {
		customer.getObservers().removeIf(o -> fullyQualifiedClassName.equals(o.getClassName()));
		return this;
	}
	
	public @Nonnull FluentCustomer clearObservers() {
		customer.getObservers().clear();
		return this;
	}
	
	public @Nonnull FluentCustomer jFreeChartPostProcessorClassName(@Nonnull String fullyQualifiedClassName) {
		customer.setJFreeChartPostProcessorClassName(fullyQualifiedClassName);
		return this;
	}
	
	public @Nonnull FluentCustomer primeFacesChartPostProcessorClassName(@Nonnull String fullyQualifiedClassName) {
		customer.setPrimeFacesChartPostProcessorClassName(fullyQualifiedClassName);
		return this;
	}

	public @Nonnull CustomerMetaData get() {
		return customer;
	}
}
