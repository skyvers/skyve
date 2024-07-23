package org.skyve.impl.metadata.repository.customer;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.ConvertableMetaData;
import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.ConverterName;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

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
							"primeFacesChartPostProcessorClassName"})
public class CustomerMetaData extends NamedMetaData implements ConvertableMetaData<Customer> {
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

	public String getLanguage() {
		return language;
	}

	@XmlAttribute
	public void setLanguage(String language) {
		this.language = Util.processStringValue(language);
	}

	public UIResources getUiResources() {
		return uiResources;
	}

	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
	public void setUiResources(UIResources uiResources) {
		this.uiResources = uiResources;
	}

	public HTMLResourcesMetaData getHtmlResources() {
		return htmlResources;
	}

	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
	public void setHtmlResources(HTMLResourcesMetaData htmlResources) {
		this.htmlResources = htmlResources;
	}

	public LoginResourcesMetaData getLoginResources() {
		return loginResources;
	}

	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
	public void setLoginResources(LoginResourcesMetaData loginResources) {
		this.loginResources = loginResources;
	}

	public ConverterName getDefaultDateConverter() {
		return defaultDateConverter;
	}

	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, required = true)
	public void setDefaultDateConverter(ConverterName defaultDateConverter) {
		this.defaultDateConverter = defaultDateConverter;
	}

	public ConverterName getDefaultDateTimeConverter() {
		return defaultDateTimeConverter;
	}

	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, required = true)
	public void setDefaultDateTimeConverter(ConverterName defaultDateTimeConverter) {
		this.defaultDateTimeConverter = defaultDateTimeConverter;
	}

	public ConverterName getDefaultTimeConverter() {
		return defaultTimeConverter;
	}

	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, required = true)
	public void setDefaultTimeConverter(ConverterName defaultTimeConverter) {
		this.defaultTimeConverter = defaultTimeConverter;
	}

	public ConverterName getDefaultTimestampConverter() {
		return defaultTimestampConverter;
	}

	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, required = true)
	public void setDefaultTimestampConverter(ConverterName defaultTimestampConverter) {
		this.defaultTimestampConverter = defaultTimestampConverter;
	}

	public CustomerModulesMetaData getModules() {
		return modules;
	}

	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, required = true)
	public void setModules(CustomerModulesMetaData modules) {
		this.modules = modules;
	}

	public CustomerRolesMetaData getRoles() {
		return roles;
	}

	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, required = false)
	public void setRoles(CustomerRolesMetaData roles) {
		this.roles = roles;
	}

	@XmlElementWrapper(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "textSearchRoles")
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "role", required = true)
	public List<CustomerFeatureRoleMetaData> getTextSearchRoles() {
		return textSearchRoles;
	}
	
	@XmlElementWrapper(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "flagRoles")
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "role", required = true)
	public List<CustomerFeatureRoleMetaData> getFlagRoles() {
		return flagRoles;
	}
	
	@XmlElementWrapper(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "switchModeRoles")
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "role", required = true)
	public List<CustomerFeatureRoleMetaData> getSwitchModeRoles() {
		return switchModeRoles;
	}
	
	@XmlElementWrapper(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "interceptors")
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "interceptor", required = true)
	public List<InterceptorMetaDataImpl> getInterceptors() {
		return interceptors;
	}

	@XmlElementWrapper(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "observers")
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "observer", required = true)
	public List<ObserverMetaDataImpl> getObservers() {
		return observers;
	}

	public String getJFreeChartPostProcessorClassName() {
		return fullyQualifiedJFreeChartPostProcessorClassName;
	}

	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
	public void setJFreeChartPostProcessorClassName(String fullyQualifiedJFreeChartPostProcessorClassName) {
		this.fullyQualifiedJFreeChartPostProcessorClassName = Util.processStringValue(fullyQualifiedJFreeChartPostProcessorClassName);
	}

	public String getPrimeFacesChartPostProcessorClassName() {
		return fullyQualifiedPrimeFacesChartPostProcessorClassName;
	}

	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
	public void setPrimeFacesChartPostProcessorClassName(String fullyQualifiedPrimeFacesChartPostProcessorClassName) {
		this.fullyQualifiedPrimeFacesChartPostProcessorClassName = Util.processStringValue(fullyQualifiedPrimeFacesChartPostProcessorClassName);
	}

	@Override
	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}

	@XmlTransient
	public void setLastModifiedMillis(long lastModifiedMillis) {
		this.lastModifiedMillis = lastModifiedMillis;
	}

	@Override
	public CustomerImpl convert(String metaDataName, ProvidedRepository repository) {
		CustomerImpl result = new CustomerImpl(repository);
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
	 * Populates and flattens feature roles to module roles. 
	 * Feature roles include text search, flag and switch mode.
	 * 
	 * @param featureRoles
	 * @param featureModuleRoles
	 * @param metaDataName
	 * @param roles
	 * @param moduleEntries
	 */
	private static void populateFeatureRoles(List<CustomerFeatureRoleMetaData> featureRoles, Set<String> featureModuleRoles,
			String metaDataName, CustomerRolesMetaData roles, Map<String, FormLabelLayout> moduleEntries) {
		for (CustomerFeatureRoleMetaData featureRole : featureRoles) {
			String roleName = featureRole.getName();
			if (roleName == null) {
				throw new MetaDataException(metaDataName + " : The [name] for a feature role is required");
			}
			String moduleName = featureRole.getModuleName();
			if (moduleName == null) {
				Optional<CustomerRoleMetaData> oCustomerRole = roles.getRoles().stream().filter(r -> r.getName().equals(roleName)).findAny();
				if (oCustomerRole.isEmpty()) {
					throw new MetaDataException(metaDataName + " : The [name] of feature role " + roleName + " is not a valid customer role");
				}
				CustomerRoleMetaData customerRole = oCustomerRole.get();
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
