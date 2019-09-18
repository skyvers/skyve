package org.skyve.impl.cdi;

import java.util.Collection;
import java.util.List;

import javax.enterprise.inject.Alternative;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.converters.Converter;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.customer.CustomerRole;
import org.skyve.metadata.customer.HTMLResources;
import org.skyve.metadata.customer.InterceptorMetaData;
import org.skyve.metadata.customer.LoginResources;
import org.skyve.metadata.customer.UIResources;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.module.Module;

/**
 * A proxy that can be Serialized with no state and continue to work after deserialization.
 * 
 * @author mike
 */
@Alternative
public class CustomerInjectable implements Customer {
	private static final long serialVersionUID = 8311112604181158312L;

	@Override
	public String getName() {
		return CORE.getCustomer().getName();
	}

	@Override
	public String getLanguageTag() {
		return CORE.getCustomer().getLanguageTag();
	}

	@Override
	public Converter<DateOnly> getDefaultDateConverter() {
		return CORE.getCustomer().getDefaultDateConverter();
	}

	@Override
	public Converter<DateTime> getDefaultDateTimeConverter() {
		return CORE.getCustomer().getDefaultDateTimeConverter();
	}

	@Override
	public Converter<TimeOnly> getDefaultTimeConverter() {
		return CORE.getCustomer().getDefaultTimeConverter();
	}

	@Override
	public Converter<Timestamp> getDefaultTimestampConverter() {
		return CORE.getCustomer().getDefaultTimestampConverter();
	}

	@Override
	public Module getHomeModule() {
		return CORE.getCustomer().getHomeModule();
	}

	@Override
	public Module getModule(String moduleName) {
		return CORE.getCustomer().getModule(moduleName);
	}

	@Override
	public List<Module> getModules() {
		return CORE.getCustomer().getModules();
	}

	@Override
	public Collection<CustomerRole> getRoles() {
		return CORE.getCustomer().getRoles();
	}

	@Override
	public CustomerRole getRole(String roleName) {
		return CORE.getCustomer().getRole(roleName);
	}

	@Override
	public boolean isAllowModuleRoles() {
		return CORE.getCustomer().isAllowModuleRoles();
	}

	@Override
	public Collection<InterceptorMetaData> getInterceptors() {
		return CORE.getCustomer().getInterceptors();
	}

	@Override
	public UIResources getUiResources() {
		return CORE.getCustomer().getUiResources();
	}

	@Override
	public HTMLResources getHtmlResources() {
		return CORE.getCustomer().getHtmlResources();
	}

	@Override
	public LoginResources getLoginResources() {
		return CORE.getCustomer().getLoginResources();
	}

	@Override
	public String getJFreeChartPostProcessorClassName() {
		return CORE.getCustomer().getJFreeChartPostProcessorClassName();
	}

	@Override
	public String getPrimeFacesChartPostProcessorClassName() {
		return CORE.getCustomer().getJFreeChartPostProcessorClassName();
	}

	@Override
	public <T extends Bean> List<DomainValue> getConstantDomainValues(Bizlet<T> bizlet,
																		String moduleName,
																		String documentName,
																		Attribute attribute)
	throws Exception {
		return CORE.getCustomer().getConstantDomainValues(bizlet, moduleName, documentName, attribute);
	}
}
