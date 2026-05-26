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

/**
 * Builds customer metadata including modules, roles, feature-role policies, and hooks.
 *
 * <p>This builder mutates a single {@link CustomerMetaData} instance and returns
 * itself from each mutator to support fluent chaining.
 */
public class FluentCustomer {
	private CustomerMetaData customer = null;
	
	/**
	 * Creates a builder with new empty customer metadata.
	 */
	public FluentCustomer() {
		customer = new CustomerMetaData();
	}
	
	/**
	 * Creates a builder around existing customer metadata.
	 *
	 * @param customer backing metadata
	 */
	public FluentCustomer(@Nonnull CustomerMetaData customer) {
		this.customer = customer;
	}

	/**
	 * Copies customer metadata into this builder.
	 *
	 * @param customer source customer contract
	 * @return this builder
	 */
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
			addObserver(observer.getClassName());
		}

		// Populate Chart Processors
		jFreeChartPostProcessorClassName(customer.getJFreeChartPostProcessorClassName());
		primeFacesChartPostProcessorClassName(customer.getPrimeFacesChartPostProcessorClassName());
		
		return this;
	}
	
	/**
	 * Sets the customer name.
	 *
	 * @param name customer name
	 * @return this builder
	 */
	public @Nonnull FluentCustomer name(@Nonnull String name) {
		customer.setName(name);
		return this;
	}

	/**
	 * Sets the optional customer language tag.
	 *
	 * @param language BCP47 language tag, or {@code null}
	 * @return this builder
	 */
	public @Nonnull FluentCustomer language(@Nullable String language) {
		customer.setLanguage(language);
		return this;
	}
	
	/**
	 * Sets the logo resource path.
	 *
	 * @param logo logo path relative to customer resources
	 * @return this builder
	 */
	public @Nonnull FluentCustomer logoRelativeFileName(@Nullable String logo) {
		UIResources resources = new UIResources();
		resources.setLogoRelativeFileName(logo);
		customer.setUiResources(resources);
		return this;
	}
	
	/**
	 * Sets the CSS resource path.
	 *
	 * @param css stylesheet path relative to customer resources
	 * @return this builder
	 */
	public @Nonnull FluentCustomer cssRelativeFileName(@Nullable String css) {
		HTMLResourcesMetaData resources = new HTMLResourcesMetaData();
		resources.setCssRelativeFileName(css);
		customer.setHtmlResources(resources);
		return this;
	}

	/**
	 * Sets login-resource metadata.
	 *
	 * @param resources login resource builder
	 * @return this builder
	 */
	public @Nonnull FluentCustomer loginResource(@Nonnull FluentLoginResources resources) {
		customer.setLoginResources(resources.get());
		return this;
	}
	
	/**
	 * Sets the default date converter.
	 *
	 * @param converter converter enum
	 * @return this builder
	 */
	public @Nonnull FluentCustomer defaultDateConverter(@Nonnull ConverterName converter) {
		customer.setDefaultDateConverter(converter);
		return this;
	}
	
	/**
	 * Sets the default date-time converter.
	 *
	 * @param converter converter enum
	 * @return this builder
	 */
	public @Nonnull FluentCustomer defaultDateTimeConverter(@Nonnull ConverterName converter) {
		customer.setDefaultDateTimeConverter(converter);
		return this;
	}
	
	/**
	 * Sets the default time converter.
	 *
	 * @param converter converter enum
	 * @return this builder
	 */
	public @Nonnull FluentCustomer defaultTimeConverter(@Nonnull ConverterName converter) {
		customer.setDefaultTimeConverter(converter);
		return this;
	}
	
	/**
	 * Sets the default timestamp converter.
	 *
	 * @param converter converter enum
	 * @return this builder
	 */
	public @Nonnull FluentCustomer defaultTimestampConverter(@Nonnull ConverterName converter) {
		customer.setDefaultTimestampConverter(converter);
		return this;
	}
	
	/**
	 * Sets module membership metadata.
	 *
	 * @param modules modules wrapper
	 * @return this builder
	 */
	public @Nonnull FluentCustomer modules(@Nonnull FluentCustomerModules modules) {
		customer.setModules(modules.get());
		return this;
	}
	
	/**
	 * Sets role metadata.
	 *
	 * @param roles roles wrapper
	 * @return this builder
	 */
	public @Nonnull FluentCustomer roles(@Nonnull FluentCustomerRoles roles) {
		customer.setRoles(roles.get());
		return this;
	}

	/**
	 * Adds a text-search feature role.
	 *
	 * @param role feature role wrapper
	 * @return this builder
	 */
	public @Nonnull FluentCustomer addTextSearchRole(@Nonnull FluentCustomerFeatureRole role) {
		customer.getTextSearchRoles().add(role.get());
		return this;
	}

	/**
	 * Removes one module-scoped text-search role.
	 *
	 * @param moduleName module name
	 * @param name role name
	 * @return this builder
	 */
	public @Nonnull FluentCustomer removeTextSearchModuleRole(@Nonnull String moduleName, @Nonnull String name) {
		customer.getTextSearchRoles().removeIf(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName()));
		return this;
	}

	/**
	 * Removes one customer-scoped text-search role.
	 *
	 * @param name role name
	 * @return this builder
	 */
	public @Nonnull FluentCustomer removeTextSearchCustomerRole(@Nonnull String name) {
		customer.getTextSearchRoles().removeIf(r -> (r.getModuleName() == null) && name.equals(r.getName()));
		return this;
	}

	/**
	 * Removes all text-search roles.
	 *
	 * @return this builder
	 */
	public @Nonnull FluentCustomer clearTextSearchRoles() {
		customer.getTextSearchRoles().clear();
		return this;
	}
	
	/**
	 * Finds a module-scoped text-search role.
	 *
	 * @param moduleName module name
	 * @param name role name
	 * @return matching role wrapper, or {@code null}
	 */
	public @Nullable FluentCustomerFeatureRole findTextSearchModuleRole(@Nonnull String moduleName, @Nonnull String name) {
		CustomerFeatureRoleMetaData result = customer.getTextSearchRoles().stream().filter(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCustomerFeatureRole(result);
		}
		return null;
	}

	/**
	 * Finds a customer-scoped text-search role.
	 *
	 * @param name role name
	 * @return matching role wrapper, or {@code null}
	 */
	public @Nullable FluentCustomerFeatureRole findTextSearchCustomerRole(@Nonnull String name) {
		CustomerFeatureRoleMetaData result = customer.getTextSearchRoles().stream().filter(r -> (r.getModuleName() == null) && name.equals(r.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCustomerFeatureRole(result);
		}
		return null;
	}

	/**
	 * Adds a flag feature role.
	 *
	 * @param role feature role wrapper
	 * @return this builder
	 */
	public @Nonnull FluentCustomer addFlagRole(@Nonnull FluentCustomerFeatureRole role) {
		customer.getFlagRoles().add(role.get());
		return this;
	}

	/**
	 * Removes one module-scoped flag role.
	 *
	 * @param moduleName module name
	 * @param name role name
	 * @return this builder
	 */
	public @Nonnull FluentCustomer removeFlagModuleRole(@Nonnull String moduleName, @Nonnull String name) {
		customer.getFlagRoles().removeIf(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName()));
		return this;
	}

	/**
	 * Removes one customer-scoped flag role.
	 *
	 * @param name role name
	 * @return this builder
	 */
	public @Nonnull FluentCustomer removeFlagCustomerRole(@Nonnull String name) {
		customer.getFlagRoles().removeIf(r -> (r.getModuleName() == null) && name.equals(r.getName()));
		return this;
	}

	/**
	 * Removes all flag roles.
	 *
	 * @return this builder
	 */
	public @Nonnull FluentCustomer clearFlagRoles() {
		customer.getFlagRoles().clear();
		return this;
	}
	
	/**
	 * Finds a module-scoped flag role.
	 *
	 * @param moduleName module name
	 * @param name role name
	 * @return matching role wrapper, or {@code null}
	 */
	public @Nullable FluentCustomerFeatureRole findFlagModuleRole(@Nonnull String moduleName, @Nonnull String name) {
		CustomerFeatureRoleMetaData result = customer.getFlagRoles().stream().filter(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCustomerFeatureRole(result);
		}
		return null;
	}

	/**
	 * Finds a customer-scoped flag role.
	 *
	 * @param name role name
	 * @return matching role wrapper, or {@code null}
	 */
	public @Nullable FluentCustomerFeatureRole findFlagCustomerRole(@Nonnull String name) {
		CustomerFeatureRoleMetaData result = customer.getFlagRoles().stream().filter(r -> (r.getModuleName() == null) && name.equals(r.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCustomerFeatureRole(result);
		}
		return null;
	}

	/**
	 * Adds a switch-mode feature role.
	 *
	 * @param role feature role wrapper
	 * @return this builder
	 */
	public @Nonnull FluentCustomer addSwitchModeRole(@Nonnull FluentCustomerFeatureRole role) {
		customer.getSwitchModeRoles().add(role.get());
		return this;
	}

	/**
	 * Removes one module-scoped switch-mode role.
	 *
	 * @param moduleName module name
	 * @param name role name
	 * @return this builder
	 */
	public @Nonnull FluentCustomer removeSwitchModeModuleRole(@Nonnull String moduleName, @Nonnull String name) {
		customer.getSwitchModeRoles().removeIf(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName()));
		return this;
	}

	/**
	 * Removes one customer-scoped switch-mode role.
	 *
	 * @param name role name
	 * @return this builder
	 */
	public @Nonnull FluentCustomer removeSwitchModeCustomerRole(@Nonnull String name) {
		customer.getSwitchModeRoles().removeIf(r -> (r.getModuleName() == null) && name.equals(r.getName()));
		return this;
	}

	/**
	 * Removes all switch-mode roles.
	 *
	 * @return this builder
	 */
	public @Nonnull FluentCustomer clearSwitchModeRoles() {
		customer.getSwitchModeRoles().clear();
		return this;
	}
	
	/**
	 * Finds a module-scoped switch-mode role.
	 *
	 * @param moduleName module name
	 * @param name role name
	 * @return matching role wrapper, or {@code null}
	 */
	public @Nullable FluentCustomerFeatureRole findSwitchModeModuleRole(@Nonnull String moduleName, @Nonnull String name) {
		CustomerFeatureRoleMetaData result = customer.getSwitchModeRoles().stream().filter(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCustomerFeatureRole(result);
		}
		return null;
	}

	/**
	 * Finds a customer-scoped switch-mode role.
	 *
	 * @param name role name
	 * @return matching role wrapper, or {@code null}
	 */
	public @Nullable FluentCustomerFeatureRole findSwitchModeCustomerRole(@Nonnull String name) {
		CustomerFeatureRoleMetaData result = customer.getSwitchModeRoles().stream().filter(r -> (r.getModuleName() == null) && name.equals(r.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCustomerFeatureRole(result);
		}
		return null;
	}

	/**
	 * Adds an interceptor class by fully qualified class name.
	 *
	 * @param fullyQualifiedClassName interceptor class name
	 * @return this builder
	 */
	public @Nonnull FluentCustomer addInterceptor(@Nonnull String fullyQualifiedClassName) {
		InterceptorMetaDataImpl interceptor = new InterceptorMetaDataImpl();
		interceptor.setClassName(fullyQualifiedClassName);
		customer.getInterceptors().add(interceptor);
		return this;
	}
	
	/**
	 * Removes an interceptor class by fully qualified class name.
	 *
	 * @param fullyQualifiedClassName interceptor class name
	 * @return this builder
	 */
	public @Nonnull FluentCustomer removeInterceptor(@Nonnull String fullyQualifiedClassName) {
		customer.getInterceptors().removeIf(i -> fullyQualifiedClassName.equals(i.getClassName()));
		return this;
	}
	
	/**
	 * Removes all configured interceptors.
	 *
	 * @return this builder
	 */
	public @Nonnull FluentCustomer clearInterceptors() {
		customer.getInterceptors().clear();
		return this;
	}
	
	/**
	 * Adds an observer class by fully qualified class name.
	 *
	 * @param fullyQualifiedClassName observer class name
	 * @return this builder
	 */
	public @Nonnull FluentCustomer addObserver(@Nonnull String fullyQualifiedClassName) {
		ObserverMetaDataImpl observer = new ObserverMetaDataImpl();
		observer.setClassName(fullyQualifiedClassName);
		customer.getObservers().add(observer);
		return this;
	}
	
	/**
	 * Removes an observer class by fully qualified class name.
	 *
	 * @param fullyQualifiedClassName observer class name
	 * @return this builder
	 */
	public @Nonnull FluentCustomer removeObserver(@Nonnull String fullyQualifiedClassName) {
		customer.getObservers().removeIf(o -> fullyQualifiedClassName.equals(o.getClassName()));
		return this;
	}
	
	/**
	 * Removes all configured observers.
	 *
	 * @return this builder
	 */
	public @Nonnull FluentCustomer clearObservers() {
		customer.getObservers().clear();
		return this;
	}
	
	/**
	 * Sets the optional JFreeChart post-processor class.
	 *
	 * @param fullyQualifiedClassName post-processor class name
	 * @return this builder
	 */
	public @Nonnull FluentCustomer jFreeChartPostProcessorClassName(@Nonnull String fullyQualifiedClassName) {
		customer.setJFreeChartPostProcessorClassName(fullyQualifiedClassName);
		return this;
	}
	
	/**
	 * Sets the optional PrimeFaces chart post-processor class.
	 *
	 * @param fullyQualifiedClassName post-processor class name
	 * @return this builder
	 */
	public @Nonnull FluentCustomer primeFacesChartPostProcessorClassName(@Nonnull String fullyQualifiedClassName) {
		customer.setPrimeFacesChartPostProcessorClassName(fullyQualifiedClassName);
		return this;
	}

	/**
	 * Returns the mutable metadata instance being built.
	 *
	 * @return backing customer metadata
	 */
	public @Nonnull CustomerMetaData get() {
		return customer;
	}
}
