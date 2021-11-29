package org.skyve.metadata.customer.fluent;

import org.skyve.impl.metadata.repository.customer.CustomerMetaData;
import org.skyve.impl.metadata.repository.customer.HTMLResourcesMetaData;
import org.skyve.impl.metadata.repository.customer.InterceptorMetaDataImpl;
import org.skyve.impl.metadata.repository.customer.ObserverMetaDataImpl;
import org.skyve.impl.metadata.repository.customer.UIResources;
import org.skyve.metadata.ConverterName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.customer.InterceptorMetaData;
import org.skyve.metadata.customer.ObserverMetaData;

public class FluentCustomer {
	private CustomerMetaData customer = new CustomerMetaData();
	
	public FluentCustomer() {
		// nothing to see
	}
	
	public FluentCustomer(Customer customer) {
		name(customer.getName());
		language(customer.getLanguageTag());

		// Populate resources
		logoRelativeFileName(customer.getUiResources().getLogoRelativeFileName());
		cssRelativeFileName(customer.getHtmlResources().getCssRelativeFileName());
		loginResource(new FluentLoginResources(customer.getLoginResources()));

		// Populate default converters
		defaultDateConverter(ConverterName.valueOf(customer.getDefaultDateConverter()));
		defaultTimeConverter(ConverterName.valueOf(customer.getDefaultTimeConverter()));
		defaultDateTimeConverter(ConverterName.valueOf(customer.getDefaultDateTimeConverter()));
		defaultTimestampConverter(ConverterName.valueOf(customer.getDefaultTimestampConverter()));

		// Populate module names
		modules(new FluentCustomerModules(customer.getModules(), customer.getHomeModule()));

		// Populate Roles
		roles(new FluentCustomerRoles(customer.getRoles(), customer.isAllowModuleRoles()));

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
	}
	
	public FluentCustomer name(String name) {
		customer.setName(name);
		return this;
	}

	public FluentCustomer language(String language) {
		customer.setLanguage(language);
		return this;
	}
	
	public FluentCustomer logoRelativeFileName(String logo) {
		UIResources resources = new UIResources();
		resources.setLogoRelativeFileName(logo);
		customer.setUiResources(resources);
		return this;
	}
	
	public FluentCustomer cssRelativeFileName(String css) {
		HTMLResourcesMetaData resources = new HTMLResourcesMetaData();
		resources.setCssRelativeFileName(css);
		customer.setHtmlResources(resources);
		return this;
	}

	public FluentCustomer loginResource(FluentLoginResources resources) {
		customer.setLoginResources(resources.get());
		return this;
	}
	
	public FluentCustomer defaultDateConverter(ConverterName converter) {
		customer.setDefaultDateConverter(converter);
		return this;
	}
	
	public FluentCustomer defaultDateTimeConverter(ConverterName converter) {
		customer.setDefaultDateTimeConverter(converter);
		return this;
	}
	
	public FluentCustomer defaultTimeConverter(ConverterName converter) {
		customer.setDefaultTimeConverter(converter);
		return this;
	}
	
	public FluentCustomer defaultTimestampConverter(ConverterName converter) {
		customer.setDefaultTimestampConverter(converter);
		return this;
	}
	
	public FluentCustomer modules(FluentCustomerModules modules) {
		customer.setModules(modules.get());
		return this;
	}
	
	public FluentCustomer addInterceptor(String fullyQualifiedClassName) {
		InterceptorMetaDataImpl interceptor = new InterceptorMetaDataImpl();
		interceptor.setClassName(fullyQualifiedClassName);
		customer.getInterceptors().add(interceptor);
		return this;
	}
	
	public FluentCustomer addObserver(String fullyQualifiedClassName) {
		ObserverMetaDataImpl observer = new ObserverMetaDataImpl();
		observer.setClassName(fullyQualifiedClassName);
		customer.getObservers().add(observer);
		return this;
	}
	
	public FluentCustomer roles(FluentCustomerRoles roles) {
		customer.setRoles(roles.get());
		return this;
	}
	
	public FluentCustomer jFreeChartPostProcessorClassName(String fullyQualifiedClassName) {
		customer.setJFreeChartPostProcessorClassName(fullyQualifiedClassName);
		return this;
	}
	
	public FluentCustomer primeFacesChartPostProcessorClassName(String fullyQualifiedClassName) {
		customer.setPrimeFacesChartPostProcessorClassName(fullyQualifiedClassName);
		return this;
	}

	public CustomerMetaData get() {
		return customer;
	}
}
