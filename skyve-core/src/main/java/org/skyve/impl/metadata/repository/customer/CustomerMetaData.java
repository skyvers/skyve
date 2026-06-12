package org.skyve.impl.metadata.repository.customer;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.ConvertibleMetaData;
import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.ConverterName;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

// TODO Populate defaultActions property in customer returned by convert
@XmlRootElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "customer")
@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE, 
			name = "customer",
			propOrder = {"language",
							"uiResources",
							"htmlResources", 
							"loginResources", 
							"defaultDateConverter", 
							"defaultTimeConverter",
							"defaultDateTimeConverter", 
							"defaultTimestampConverter", 
							"modules", 
							"roles",
							"textSearchRoles",
							"flagRoles",
							"switchModeRoles",
							"interceptors",
							"observers",
							"JFreeChartPostProcessorClassName",
							"primeFacesChartPostProcessorClassName",
							"properties"})
/**
 * JAXB root element for a customer descriptor ({@code customer.xml}), converted
 * to a runtime {@link Customer} during repository bootstrap.
 *
 * <p>Holds the complete tenant configuration: locale, UI/HTML resource overrides,
 * login resources, module set, composite roles, interceptors, observers, and
 * third-party chart post-processor class names.  Extends {@link NamedMetaData}
 * to bind the descriptor to the customer name.
 *
 * <p>Threading: not thread-safe.  Instances are populated during JAXB unmarshalling
 * and are read-only once converted and placed in the repository cache.
 *
 * @see Customer
 * @see org.skyve.impl.metadata.customer.CustomerImpl
 */
public class CustomerMetaData extends NamedMetaData implements ConvertibleMetaData<Customer>, DecoratedMetaData {
	private static final long serialVersionUID = 4281621343439667457L;

	private String language;
	private UIResources uiResources;
	private HTMLResourcesMetaData htmlResources;
	private LoginResourcesMetaData loginResources;
	private ConverterName defaultDateConverter;
	private ConverterName defaultTimeConverter;
	private ConverterName defaultDateTimeConverter;
	private ConverterName defaultTimestampConverter;
	private CustomerModulesMetaData modules;
	private CustomerRolesMetaData roles;
	private List<CustomerFeatureRoleMetaData> textSearchRoles = new ArrayList<>();
	private List<CustomerFeatureRoleMetaData> flagRoles = new ArrayList<>();
	private List<CustomerFeatureRoleMetaData> switchModeRoles = new ArrayList<>();
	private List<InterceptorMetaDataImpl> interceptors = new ArrayList<>();
	private List<ObserverMetaDataImpl> observers = new ArrayList<>();
	private String fullyQualifiedJFreeChartPostProcessorClassName;
	private String fullyQualifiedPrimeFacesChartPostProcessorClassName;
	private long lastModifiedMillis = Long.MAX_VALUE;
	
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Returns the customer language tag.
	 *
	 * @return BCP-47 language tag, or {@code null}
	 */
	public String getLanguage() {
		return language;
	}

	/**
	 * Sets the customer language tag.
	 *
	 * @param language BCP-47 language tag
	 */
	@XmlAttribute
	public void setLanguage(String language) {
		this.language = Util.processStringValue(language);
	}

	/**
	 * Returns customer-level UI resource overrides.
	 *
	 * @return UI resource metadata, or {@code null}
	 */
	public UIResources getUiResources() {
		return uiResources;
	}

	/**
	 * Sets customer-level UI resource overrides.
	 *
	 * @param uiResources UI resource metadata
	 */
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
	public void setUiResources(UIResources uiResources) {
		this.uiResources = uiResources;
	}

	/**
	 * Returns HTML resource customisation metadata.
	 *
	 * @return HTML resource metadata, or {@code null}
	 */
	public HTMLResourcesMetaData getHtmlResources() {
		return htmlResources;
	}

	/**
	 * Sets HTML resource customisation metadata.
	 *
	 * @param htmlResources HTML resource metadata
	 */
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
	public void setHtmlResources(HTMLResourcesMetaData htmlResources) {
		this.htmlResources = htmlResources;
	}

	/**
	 * Returns login resource customisation metadata.
	 *
	 * @return login resource metadata, or {@code null}
	 */
	public LoginResourcesMetaData getLoginResources() {
		return loginResources;
	}

	/**
	 * Sets login resource customisation metadata.
	 *
	 * @param loginResources login resource metadata
	 */
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
	public void setLoginResources(LoginResourcesMetaData loginResources) {
		this.loginResources = loginResources;
	}

	/**
	 * Returns the default converter for date attributes.
	 *
	 * @return configured date converter name
	 */
	public ConverterName getDefaultDateConverter() {
		return defaultDateConverter;
	}

	/**
	 * Sets the default converter for date attributes.
	 *
	 * @param defaultDateConverter configured date converter name
	 */
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, required = true)
	public void setDefaultDateConverter(ConverterName defaultDateConverter) {
		this.defaultDateConverter = defaultDateConverter;
	}

	/**
	 * Returns the default converter for date-time attributes.
	 *
	 * @return configured date-time converter name
	 */
	public ConverterName getDefaultDateTimeConverter() {
		return defaultDateTimeConverter;
	}

	/**
	 * Sets the default converter for date-time attributes.
	 *
	 * @param defaultDateTimeConverter configured date-time converter name
	 */
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, required = true)
	public void setDefaultDateTimeConverter(ConverterName defaultDateTimeConverter) {
		this.defaultDateTimeConverter = defaultDateTimeConverter;
	}

	/**
	 * Returns the default converter for time attributes.
	 *
	 * @return configured time converter name
	 */
	public ConverterName getDefaultTimeConverter() {
		return defaultTimeConverter;
	}

	/**
	 * Sets the default converter for time attributes.
	 *
	 * @param defaultTimeConverter configured time converter name
	 */
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, required = true)
	public void setDefaultTimeConverter(ConverterName defaultTimeConverter) {
		this.defaultTimeConverter = defaultTimeConverter;
	}

	/**
	 * Returns the default converter for timestamp attributes.
	 *
	 * @return configured timestamp converter name
	 */
	public ConverterName getDefaultTimestampConverter() {
		return defaultTimestampConverter;
	}

	/**
	 * Sets the default converter for timestamp attributes.
	 *
	 * @param defaultTimestampConverter configured timestamp converter name
	 */
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, required = true)
	public void setDefaultTimestampConverter(ConverterName defaultTimestampConverter) {
		this.defaultTimestampConverter = defaultTimestampConverter;
	}

	/**
	 * Returns module configuration metadata for the customer.
	 *
	 * @return customer module metadata, or {@code null}
	 */
	public CustomerModulesMetaData getModules() {
		return modules;
	}

	/**
	 * Sets module configuration metadata for the customer.
	 *
	 * @param modules customer module metadata
	 */
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, required = true)
	public void setModules(CustomerModulesMetaData modules) {
		this.modules = modules;
	}

	/**
	 * Returns composite role definitions for the customer.
	 *
	 * @return customer role metadata, or {@code null}
	 */
	public CustomerRolesMetaData getRoles() {
		return roles;
	}

	/**
	 * Sets composite role definitions for the customer.
	 *
	 * @param roles customer role metadata
	 */
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, required = false)
	public void setRoles(CustomerRolesMetaData roles) {
		this.roles = roles;
	}

	/**
	 * Returns feature roles allowed to perform text search operations.
	 *
	 * @return mutable list of text-search feature roles
	 */
	@XmlElementWrapper(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "textSearchRoles")
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "role", required = true)
	public List<CustomerFeatureRoleMetaData> getTextSearchRoles() {
		return textSearchRoles;
	}
	
	/**
	 * Returns feature roles allowed to access flag operations.
	 *
	 * @return mutable list of flag feature roles
	 */
	@XmlElementWrapper(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "flagRoles")
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "role", required = true)
	public List<CustomerFeatureRoleMetaData> getFlagRoles() {
		return flagRoles;
	}
	
	/**
	 * Returns feature roles allowed to switch mode.
	 *
	 * @return mutable list of switch-mode feature roles
	 */
	@XmlElementWrapper(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "switchModeRoles")
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "role", required = true)
	public List<CustomerFeatureRoleMetaData> getSwitchModeRoles() {
		return switchModeRoles;
	}
	
	/**
	 * Returns interceptor metadata declarations for this customer.
	 *
	 * @return mutable list of interceptor metadata
	 */
	@XmlElementWrapper(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "interceptors")
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "interceptor", required = true)
	public List<InterceptorMetaDataImpl> getInterceptors() {
		return interceptors;
	}

	/**
	 * Returns observer metadata declarations for this customer.
	 *
	 * @return mutable list of observer metadata
	 */
	@XmlElementWrapper(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "observers")
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "observer", required = true)
	public List<ObserverMetaDataImpl> getObservers() {
		return observers;
	}

	/**
	 * Returns the fully-qualified JFreeChart post-processor class name.
	 *
	 * @return JFreeChart post-processor class name, or {@code null}
	 */
	public String getJFreeChartPostProcessorClassName() {
		return fullyQualifiedJFreeChartPostProcessorClassName;
	}

	/**
	 * Sets the fully-qualified JFreeChart post-processor class name.
	 *
	 * @param fullyQualifiedJFreeChartPostProcessorClassName class name to use
	 */
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
	public void setJFreeChartPostProcessorClassName(String fullyQualifiedJFreeChartPostProcessorClassName) {
		this.fullyQualifiedJFreeChartPostProcessorClassName = Util.processStringValue(fullyQualifiedJFreeChartPostProcessorClassName);
	}

	/**
	 * Returns the fully-qualified PrimeFaces chart post-processor class name.
	 *
	 * @return PrimeFaces chart post-processor class name, or {@code null}
	 */
	public String getPrimeFacesChartPostProcessorClassName() {
		return fullyQualifiedPrimeFacesChartPostProcessorClassName;
	}

	/**
	 * Sets the fully-qualified PrimeFaces chart post-processor class name.
	 *
	 * @param fullyQualifiedPrimeFacesChartPostProcessorClassName class name to use
	 */
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
	public void setPrimeFacesChartPostProcessorClassName(String fullyQualifiedPrimeFacesChartPostProcessorClassName) {
		this.fullyQualifiedPrimeFacesChartPostProcessorClassName = Util.processStringValue(fullyQualifiedPrimeFacesChartPostProcessorClassName);
	}

	/**
	 * Returns the source metadata last-modified timestamp used for reload checks.
	 *
	 * @return source metadata last-modified timestamp in milliseconds
	 */
	@Override
	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}

	/**
	 * Sets the source metadata last-modified timestamp used for reload checks.
	 *
	 * @param lastModifiedMillis source metadata last-modified timestamp in milliseconds
	 */
	@XmlTransient
	public void setLastModifiedMillis(long lastModifiedMillis) {
		this.lastModifiedMillis = lastModifiedMillis;
	}
	
	/**
	 * Returns decorator properties defined for this customer descriptor.
	 *
	 * @return mutable customer property map
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}

	/**
	 * Converts this JAXB customer descriptor into a runtime {@link CustomerImpl}.
	 *
	 * <p>Performs structural validation, resolves default converters by type,
	 * flattens customer and feature roles to module-role mappings, and registers
	 * interceptors and observers.
	 *
	 * <p>Side effects: populates dependency metadata on the returned customer.
	 *
	 * @param metaDataName metadata path used in validation error messages
	 * @return the populated runtime customer metadata
	 * @throws MetaDataException if required metadata is missing, duplicated, or invalid
	 */
	@Override
	@SuppressWarnings("java:S3776") // Complexity OK
	public CustomerImpl convert(String metaDataName) {
		CustomerImpl result = new CustomerImpl();
		result.setLastModifiedMillis(getLastModifiedMillis());
		String value = getName();
		if (value == null) {
			throw new MetaDataException(metaDataName + " : The customer [name] is required");
		}
		result.setName(value);

		result.setLanguageTag(getLanguage());
		result.setUiResources((uiResources == null) ? new UIResources() : uiResources);
		result.setHtmlResources((htmlResources == null) ? new HTMLResourcesMetaData() : htmlResources);
		result.setLoginResources((loginResources == null) ? new LoginResourcesMetaData() : loginResources);

		ConverterName converterName = getDefaultDateConverter();
		if (converterName == null) {
			throw new MetaDataException(metaDataName + " : The customer [defaultDateConverter] is required");
		}
		@SuppressWarnings("unchecked")
		Converter<DateOnly> dateConverter = (Converter<DateOnly>) converterName.getConverter();
		if (! AttributeType.date.equals(dateConverter.getAttributeType())) {
			throw new MetaDataException(metaDataName + " : The customer [defaultDateConverter] is not for the date type");
		}
		result.setDefaultDateConverter(dateConverter);

		converterName = getDefaultTimeConverter();
		if (converterName == null) {
			throw new MetaDataException(metaDataName + " : The customer [defaultTimeConverter] is required");
		}
		@SuppressWarnings("unchecked")
		Converter<TimeOnly> timeConverter = (Converter<TimeOnly>) converterName.getConverter();
		if (! AttributeType.time.equals(timeConverter.getAttributeType())) {
			throw new MetaDataException(metaDataName + " : The customer [defaultTimeConverter] is not for the time type");
		}
		result.setDefaultTimeConverter(timeConverter);

		converterName = getDefaultDateTimeConverter();
		if (converterName == null) {
			throw new MetaDataException(metaDataName + " : The customer [defaultDateTimeConverter] is required");
		}
		@SuppressWarnings("unchecked")
		Converter<DateTime> dateTimeConverter = (Converter<DateTime>) converterName.getConverter();
		if (! AttributeType.dateTime.equals(dateTimeConverter.getAttributeType())) {
			throw new MetaDataException(metaDataName + " : The customer [defaultDateTimeConverter] is not for the dateTime type");
		}
		result.setDefaultDateTimeConverter(dateTimeConverter);

		converterName = getDefaultTimestampConverter();
		if (converterName == null) {
			throw new MetaDataException(metaDataName + " : The customer [defaultTimestampConverter] is required");
		}
		@SuppressWarnings("unchecked")
		Converter<Timestamp> timestampConverter = (Converter<Timestamp>) converterName.getConverter();
		if (! AttributeType.timestamp.equals(timestampConverter.getAttributeType())) {
			throw new MetaDataException(metaDataName + " : The customer [defaultTimestampConverter] is not for the timestamp type");
		}
		result.setDefaultTimestampConverter(timestampConverter);

		// NB Entries are in insertion order
		Map<String, FormLabelLayout> moduleEntries = result.getModuleEntries();
		if (modules != null) {
			for (CustomerModuleMetaData module : modules.getModules()) {
				if (module == null) {
					throw new MetaDataException(metaDataName + " : One of the module references is not defined.");
				}
				moduleEntries.put(module.getName(), module.getFormLabelLayout());
			}
	
			value = modules.getHomeModule();
			if (value == null) {
				throw new MetaDataException(metaDataName + " : The customer modules [homeModule] is required");
			}
			result.setHomeModuleName(value);
		}
		
		// Populate Roles
		if (roles != null) {
			Set<String> roleNames = new TreeSet<>();
			for (CustomerRoleMetaData role : roles.getRoles()) {
				value = role.getName();
				if (value == null) {
					throw new MetaDataException(metaDataName + " : The [name] for a role is required");
				}
				if (! roleNames.add(value)) {
					throw new MetaDataException(metaDataName + " : Duplicate role " + value);
				}
				String customerRoleName = value;
				for (CustomerModuleRoleMetaData moduleRole : role.getRoles()) {
					value = moduleRole.getName();
					if (value == null) {
						throw new MetaDataException(metaDataName + " : The [name] of module role for customer role " + 
														customerRoleName + " is required");
					}
					if (! moduleEntries.containsKey(moduleRole.getModuleName())) {
						throw new MetaDataException(metaDataName + " : The [module] of module role " + moduleRole.getModuleName() + 
														" for customer role " + customerRoleName + " is not a valid module");
					}
				}
				result.putRole(role);
			}
			result.setAllowModuleRoles(roles.isAllowModuleRoles());
		}

		// Populate Text Search Roles
		if (textSearchRoles != null) {
			Set<String> textSearchModuleRoles = result.getTextSearchRoles();
			populateFeatureRoles(textSearchRoles, textSearchModuleRoles, metaDataName, roles, moduleEntries);
		}		
		
		// Populate Flag Roles
		if (flagRoles != null) {
			Set<String> flagModuleRoles = result.getFlagRoles();
			populateFeatureRoles(flagRoles, flagModuleRoles, metaDataName, roles, moduleEntries);
		}
		
		// Populate Switch Mode Roles
		if (switchModeRoles != null) {
			Set<String> switchModeModuleRoles = result.getSwitchModeRoles();
			populateFeatureRoles(switchModeRoles, switchModeModuleRoles, metaDataName, roles, moduleEntries);
		}
		
		// Populate Interceptors
		List<InterceptorMetaDataImpl> repositoryInterceptors = getInterceptors();
		if (repositoryInterceptors != null) {
			for (InterceptorMetaDataImpl interceptor : repositoryInterceptors) {
				value = interceptor.getClassName();
				if (value == null) {
					throw new MetaDataException(metaDataName + " : The [className] for an interceptor is required");
				}
				if (! result.putInterceptor(interceptor)) {
					throw new MetaDataException(metaDataName + " : Duplicate interceptor " + value);
				}
			}
		}

		// Populate Observers
		List<ObserverMetaDataImpl> repositoryObservers = getObservers();
		if (repositoryObservers != null) {
			for (ObserverMetaDataImpl observer : repositoryObservers) {
				value = observer.getClassName();
				if (value == null) {
					throw new MetaDataException(metaDataName + " : The [className] for an observer is required");
				}
				if (! result.putObserver(observer)) {
					throw new MetaDataException(metaDataName + " : Duplicate observer " + value);
				}
			}
		}

		result.setJFreeChartPostProcessorClassName(fullyQualifiedJFreeChartPostProcessorClassName);
		result.setPrimeFacesChartPostProcessorClassName(fullyQualifiedPrimeFacesChartPostProcessorClassName);
		
		result.determineDependencies();
		
		return result;
	}
	
	/**
	 * Flattens feature-role declarations into module-role identifiers.
	 *
	 * <p>Feature roles may reference either a direct module role or a composite
	 * customer role. Composite roles are expanded to all underlying module roles.
	 *
	 * @param featureRoles source feature-role declarations
	 * @param featureModuleRoles destination set of {@code module.role} entries
	 * @param metaDataName metadata path used in validation error messages
	 * @param roles available composite customer roles
	 * @param moduleEntries known customer modules
	 * @throws MetaDataException if role/module references are invalid or duplicated
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private static void populateFeatureRoles(List<CustomerFeatureRoleMetaData> featureRoles, Set<String> featureModuleRoles,
			String metaDataName, CustomerRolesMetaData roles, Map<String, FormLabelLayout> moduleEntries) {
		for (CustomerFeatureRoleMetaData featureRole : featureRoles) {
			String roleName = featureRole.getName();
			if (roleName == null) {
				throw new MetaDataException(metaDataName + " : The [name] for a feature role is required");
			}
			String moduleName = featureRole.getModuleName();
			if (moduleName == null) {
				CustomerRoleMetaData customerRole = null;
				if (roles != null) {
					for (CustomerRoleMetaData candidate : roles.getRoles()) {
						if (roleName.equals(candidate.getName())) {
							customerRole = candidate;
							break;
						}
					}
				}
				if (customerRole == null) {
					throw new MetaDataException(metaDataName + " : The [name] of feature role " + roleName + " is not a valid customer role");
				}
				for (CustomerModuleRoleMetaData moduleRole : customerRole.getRoles()) {
					moduleName = moduleRole.getModuleName();
					String value = moduleRole.getName();
					featureModuleRoles.add(moduleName + "." + value);
				}
			}
			else {
				if (! moduleEntries.containsKey(moduleName)) {
					throw new MetaDataException(metaDataName + " : The [module] of module role " + moduleName + " is not a valid module");
				}
				if (! featureModuleRoles.add(moduleName + "." + roleName)) {
					throw new MetaDataException(metaDataName + " : Duplicate role " + roleName);
				}
			}
		}
	}
}
