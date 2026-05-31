package org.skyve.impl.cdi;

import java.util.Collection;
import java.util.List;

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
import org.skyve.metadata.customer.ObserverMetaData;
import org.skyve.metadata.customer.UIResources;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.module.Module;

import jakarta.enterprise.inject.Alternative;

/**
 * Stateless CDI proxy for {@link Customer}.
 *
 * <p>Delegates all calls to {@link CORE#getCustomer()} to avoid serializing live
 * repository/customer state when passivating CDI beans.
 */
@Alternative
public class CustomerInjectable implements Customer {
	private static final long serialVersionUID = 8311112604181158312L;

	/**
	 * Returns the customer name.
	 *
	 * @return the configured customer name.
	 */
	@Override
	public String getName() {
		return CORE.getCustomer().getName();
	}

	/**
	 * Returns the metadata last-modified timestamp.
	 *
	 * @return the last metadata modification time in epoch milliseconds.
	 */
	@Override
	public long getLastModifiedMillis() {
		return CORE.getCustomer().getLastModifiedMillis();
	}
	
	/**
	 * Returns the metadata last-checked timestamp.
	 *
	 * @return the last metadata staleness check time in epoch milliseconds.
	 */
	@Override
	public long getLastCheckedMillis() {
		return CORE.getCustomer().getLastCheckedMillis();
	}
	
	/**
	 * Updates the repository staleness marker used by metadata refresh checks.
	 *
	 * @param lastCheckedMillis the staleness check time in epoch milliseconds.
	 */
	@Override
	public void setLastCheckedMillis(long lastCheckedMillis) {
		CORE.getCustomer().setLastCheckedMillis(lastCheckedMillis);
	}
	
	/**
	 * Returns the default language tag for this customer.
	 *
	 * @return the default language tag.
	 */
	@Override
	public String getLanguageTag() {
		return CORE.getCustomer().getLanguageTag();
	}

	/**
	 * Returns the default converter for date-only values.
	 *
	 * @return the date-only converter.
	 */
	@Override
	public Converter<DateOnly> getDefaultDateConverter() {
		return CORE.getCustomer().getDefaultDateConverter();
	}

	/**
	 * Returns the default converter for date-time values.
	 *
	 * @return the date-time converter.
	 */
	@Override
	public Converter<DateTime> getDefaultDateTimeConverter() {
		return CORE.getCustomer().getDefaultDateTimeConverter();
	}

	/**
	 * Returns the default converter for time-only values.
	 *
	 * @return the time-only converter.
	 */
	@Override
	public Converter<TimeOnly> getDefaultTimeConverter() {
		return CORE.getCustomer().getDefaultTimeConverter();
	}

	/**
	 * Returns the default converter for timestamp values.
	 *
	 * @return the timestamp converter.
	 */
	@Override
	public Converter<Timestamp> getDefaultTimestampConverter() {
		return CORE.getCustomer().getDefaultTimestampConverter();
	}

	/**
	 * Returns the configured home module.
	 *
	 * @return the home module.
	 */
	@Override
	public Module getHomeModule() {
		return CORE.getCustomer().getHomeModule();
	}

	/**
	 * Resolves a module by name.
	 *
	 * @param moduleName the module name.
	 * @return the module if found, otherwise null.
	 */
	@Override
	public Module getModule(String moduleName) {
		return CORE.getCustomer().getModule(moduleName);
	}

	/**
	 * Returns all configured modules.
	 *
	 * @return the module list.
	 */
	@Override
	public List<Module> getModules() {
		return CORE.getCustomer().getModules();
	}

	/**
	 * Returns all customer roles.
	 *
	 * @return the role collection.
	 */
	@Override
	public Collection<CustomerRole> getRoles() {
		return CORE.getCustomer().getRoles();
	}

	/**
	 * Resolves a role by name.
	 *
	 * @param roleName the role name.
	 * @return the role if found, otherwise null.
	 */

	@Override
	public CustomerRole getRole(String roleName) {
		return CORE.getCustomer().getRole(roleName);
	}

	/**
	 * Indicates whether module roles are enabled.
	 *
	 * @return true if module roles are enabled; otherwise false.
	 */
	@Override
	public boolean isAllowModuleRoles() {
		return CORE.getCustomer().isAllowModuleRoles();
	}

	/**
	 * Returns interceptor metadata configured for this customer.
	 *
	 * @return the interceptor metadata collection.
	 */
	@Override
	public Collection<InterceptorMetaData> getInterceptors() {
		return CORE.getCustomer().getInterceptors();
	}

	/**
	 * Returns observer metadata configured for this customer.
	 *
	 * @return the observer metadata collection.
	 */
	@Override
	public Collection<ObserverMetaData> getObservers() {
		return CORE.getCustomer().getObservers();
	}

	/**
	 * Returns UI resource configuration.
	 *
	 * @return the UI resources.
	 */
	@Override
	public UIResources getUiResources() {
		return CORE.getCustomer().getUiResources();
	}

	/**
	 * Returns HTML resource configuration.
	 *
	 * @return the HTML resources.
	 */
	@Override
	public HTMLResources getHtmlResources() {
		return CORE.getCustomer().getHtmlResources();
	}

	/**
	 * Returns login resource configuration.
	 *
	 * @return the login resources.
	 */
	@Override
	public LoginResources getLoginResources() {
		return CORE.getCustomer().getLoginResources();
	}

	/**
	 * Returns the JFreeChart post-processor class name.
	 *
	 * @return the JFreeChart post-processor class name.
	 */
	@Override
	public String getJFreeChartPostProcessorClassName() {
		return CORE.getCustomer().getJFreeChartPostProcessorClassName();
	}

	/**
	 * Returns the PrimeFaces chart post-processor class name.
	 *
	 * @return the PrimeFaces chart post-processor class name.
	 */
	@Override
	public String getPrimeFacesChartPostProcessorClassName() {
		return CORE.getCustomer().getPrimeFacesChartPostProcessorClassName();
	}

	/**
	 * Resolves static domain values for the supplied attribute via the owning bizlet.
	 *
	 * @param bizlet the bizlet that resolves domain values.
	 * @param moduleName the module name containing the document.
	 * @param documentName the document name containing the attribute.
	 * @param attribute the attribute requiring domain values.
	 * @param <T> the bean type handled by the bizlet.
	 * @return the resolved constant domain values.
	 *
	 * @throws Exception if domain value evaluation fails.
	 */
	@Override
	public <T extends Bean> List<DomainValue> getConstantDomainValues(Bizlet<T> bizlet,
																		String moduleName,
																		String documentName,
																		Attribute attribute)
	throws Exception {
		return CORE.getCustomer().getConstantDomainValues(bizlet, moduleName, documentName, attribute);
	}
	
	/**
	 * Rebuilds customer-level metadata dependency graphs.
	 *
	 * <p>Side effects: mutates in-memory dependency indexes used for validation,
	 * navigation and derived metadata traversal.
	 */
	@Override
	public void determineDependencies() {
		CORE.getCustomer().determineDependencies();
	}
}
