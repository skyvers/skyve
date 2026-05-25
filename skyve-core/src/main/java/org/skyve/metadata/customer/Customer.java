package org.skyve.metadata.customer;

import java.util.Collection;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.converters.Converter;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.PersistentMetaData;
import org.skyve.metadata.ReloadableMetaData;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.module.Module;

import jakarta.annotation.Nonnull;

/**
 * The root metadata contract for a Skyve tenant (customer).
 *
 * <p>A {@code Customer} is the top-level unit of multi-tenancy. Each customer has
 * its own set of modules and roles, default date/time converters, UI branding resources,
 * and registered lifecycle hooks (interceptors, observers). The repository loads and
 * caches a {@code Customer} instance for each declared customer directory at startup.
 *
 * <p>Threading: {@code Customer} implementations are long-lived singletons read concurrently
 * from many threads. Implementations must be thread-safe after initialisation.
 *
 * @see org.skyve.CORE#getCustomer()
 */
public interface Customer extends NamedMetaData, PersistentMetaData, ReloadableMetaData {
	/**
	 * Returns the BCP 47 language tag that governs locale-sensitive behaviour for this customer.
	 *
	 * @return a BCP 47 language tag (e.g. {@code "en-AU"}); may be {@code null} if not configured
	 */
	public String getLanguageTag();
	
	/**
	 * Returns the default converter used to format and parse {@link org.skyve.domain.types.DateOnly} values.
	 *
	 * @return the date converter; never {@code null}
	 */
	public Converter<DateOnly> getDefaultDateConverter();

	/**
	 * Returns the default converter used to format and parse {@link org.skyve.domain.types.DateTime} values.
	 *
	 * @return the date-time converter; never {@code null}
	 */
	public Converter<DateTime> getDefaultDateTimeConverter();
	
	/**
	 * Returns the default converter used to format and parse {@link org.skyve.domain.types.TimeOnly} values.
	 *
	 * @return the time converter; never {@code null}
	 */
	public Converter<TimeOnly> getDefaultTimeConverter();
	
	/**
	 * Returns the default converter used to format and parse {@link org.skyve.domain.types.Timestamp} values.
	 *
	 * @return the timestamp converter; never {@code null}
	 */
	public Converter<Timestamp> getDefaultTimestampConverter();
	
	/**
	 * Returns the module that is shown first after a user logs in.
	 *
	 * @return the home module; may be {@code null} if not configured
	 */
	public Module getHomeModule();
	
	/**
	 * Returns the named module, resolving any customer override.
	 *
	 * @param moduleName  the module name; must not be {@code null}
	 * @return the module; never {@code null}
	 * @throws org.skyve.domain.messages.NoResultsException if no module with that name exists
	 */
	public @Nonnull Module getModule(@Nonnull String moduleName);
	
	/**
	 * Returns the ordered list of modules accessible to this customer.
	 *
	 * @return an ordered, unmodifiable list; never {@code null}
	 */
	public List<Module> getModules();
	
	/**
	 * Returns all customer-level roles declared in the customer XML.
	 *
	 * @return a collection of customer roles; never {@code null}
	 */
	public Collection<CustomerRole> getRoles();
	
	/**
	 * Returns the named customer-level role.
	 *
	 * @param roleName  the role name to look up
	 * @return the role, or {@code null} if no role with that name exists
	 */
	public CustomerRole getRole(String roleName);
	
	/**
	 * Returns whether users of this customer may be assigned module-level roles directly,
	 * in addition to customer-level roles.
	 *
	 * @return {@code true} if module roles may be assigned to users
	 */
	public boolean isAllowModuleRoles();

	/**
	 * Returns the interceptor registrations declared in the customer XML, in declaration order.
	 *
	 * @return the interceptor metadata collection; never {@code null}
	 */
	public Collection<InterceptorMetaData> getInterceptors();

	/**
	 * Returns the observer registrations declared in the customer XML.
	 *
	 * @return the observer metadata collection; never {@code null}
	 */
	public Collection<ObserverMetaData> getObservers();

	/**
	 * Returns the customer-specific UI resource overrides (logo path, etc.).
	 *
	 * @return the UI resources; may be {@code null} if not configured
	 */
	public UIResources getUiResources();
	
	/**
	 * Returns the customer-specific HTML resource overrides (CSS path, etc.).
	 *
	 * @return the HTML resources; may be {@code null} if not configured
	 */
	public HTMLResources getHtmlResources();

	/**
	 * Returns the customer-specific login and logout page URL overrides.
	 *
	 * @return the login resources; may be {@code null} if not configured
	 */
	public LoginResources getLoginResources();

	/**
	 * Returns the fully-qualified class name of a JFreeChart post-processor to apply
	 * after chart data is prepared.
	 *
	 * @return the class name, or {@code null} if not configured
	 */
	public String getJFreeChartPostProcessorClassName();

	/**
	 * Returns the fully-qualified class name of a PrimeFaces chart post-processor to apply
	 * after chart model data is prepared.
	 *
	 * @return the class name, or {@code null} if not configured
	 */
	public String getPrimeFacesChartPostProcessorClassName();
	
	/**
	 * Resolves and returns the constant domain values for a document attribute, applying
	 * any customer-specific Bizlet override.
	 *
	 * @param bizlet       the bizlet to invoke; may be {@code null}
	 * @param moduleName   the owning module name
	 * @param documentName the owning document name
	 * @param attribute    the attribute whose domain values are requested
	 * @return the list of domain values; never {@code null}
	 * @throws Exception if the Bizlet call fails
	 */
	public <T extends Bean> List<DomainValue> getConstantDomainValues(Bizlet<T> bizlet,
																		String moduleName,
																		String documentName,
																		Attribute attribute)
	throws Exception;
	
	/**
	 * Initialize the dependencies on this customer given their modules and overrides.
	 */
	public void determineDependencies();
}
