package org.skyve.impl.metadata.customer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.customer.CustomerRoleMetaData;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.customer.CustomerRole;
import org.skyve.metadata.customer.HTMLResources;
import org.skyve.metadata.customer.InterceptorMetaData;
import org.skyve.metadata.customer.LoginResources;
import org.skyve.metadata.customer.ObserverMetaData;
import org.skyve.metadata.customer.UIResources;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.util.logging.Category;
import org.skyve.web.WebContext;
import org.slf4j.Logger;

import com.google.common.base.MoreObjects;

import jakarta.annotation.Nonnull;
import jakarta.servlet.http.HttpSession;

/**
 * Runtime implementation of the {@link Customer} contract, populated from a
 * customer XML descriptor and its referenced metadata during repository bootstrap.
 *
 * <p>Holds the customer's module activations, role definitions, converter overrides,
 * UI resource references, interceptors, and observers.  The object graph is fully
 * resolved before being placed in the repository cache; callers may read all
 * accessors without triggering additional I/O.
 *
 * <p>Threading: not thread-safe.  A {@code CustomerImpl} instance is written
 * exclusively during repository loading and is read-only afterwards.  Callers must
 * not mutate the instance after it has been published to the cache.
 *
 * @see Customer
 * @see org.skyve.impl.metadata.repository.FileSystemRepository
 */
public class CustomerImpl implements Customer {
	private static final long serialVersionUID = 2926460705821800439L;
	private static final Logger BIZLET_LOGGER = Category.BIZLET.logger();

	/*
	 * Change derived field groups to "relations". 
	 * Add "action" attribute to DFG to throw error or null out FK's. 
	 * action = (Constrain = throw exception, 
	 * 				Dereference = null out the FK, 
	 * 				DereferenceAll = null out FK and all derived fields) 
	 * What if the nulling of the FK should effect totals or other such crap - eg nulling out 
	 * invoice line FK not allowed when timesheet is submitted.
	 * 
	 * Place refs in another structure. 
	 * Have map of refs keyed by FK field name.
	 * Add refs from derived documents - check for duplicate FK names in ref.
	 * Add child document fields not present from all derived documents. 
	 * Name could be "Object closure" 
	 * admin.Contact -> admin.User.ContactId -> 
	 * (persistentName & doc alias & action) timesheet.Timesheet.RaisedBy ->
	 * (persistentName & doc alias & action) timesheet.Timesheet.ReviewedBy ->
	 * (persistentName & doc alias & action) contacts.ContactHistory.addedBy ->
	 * (Contact, "Contacts") (extraChilds)-> contacts.ContactHistory 
	 * In delete, for each child document field, get all records, for each record, check refs. 
	 * But what if this is overridden.
	 * 
	 * single delete - check locking, fire preDelete, check refs.
	 * 
	 * keep a map String id -> table to delete from - called cascadeDeletePlan.
	 * Parent Key ID -> TableName to delete children from. eg Deleting contact
	 * 1. 1 -> contacthistory has [child] children 3 -> Child 7 -> Child
	 * 
	 * Persisted derived fields???? What if derived field changes? 
	 * propagate the changes to every referencing row.
	 */
	
	/**
	 * Customer name.
	 */
	private String name;
	
	// 64 bit not atomic on some JVM implementations
	private volatile long lastModifiedMillis = Long.MAX_VALUE;
	// 64 bit not atomic on some JVM implementations
	private volatile long lastCheckedMillis = System.currentTimeMillis();

	private String languageTag;
	
	private UIResources uiResources;
	private HTMLResources htmlResources;
	private LoginResources loginResources;
	
	private Converter<DateOnly> defaultDateConverter;
	private Converter<TimeOnly> defaultTimeConverter;
	private Converter<DateTime> defaultDateTimeConverter;
	private Converter<Timestamp> defaultTimestampConverter;
	private LinkedHashMap<String, FormLabelLayout> moduleEntries = new LinkedHashMap<>();
	private String homeModuleName;
	private LinkedHashMap<String, CustomerRoleMetaData> roles = new LinkedHashMap<>();
	private Set<String> textSearchRoles = new TreeSet<>();
	private Set<String> flagRoles = new TreeSet<>();
	private Set<String> switchModeRoles = new TreeSet<>();
	private boolean allowModuleRoles = true;
	private Map<String, InterceptorMetaData> interceptors = new LinkedHashMap<>();
	private List<InterceptorMetaData> reversedInterceptors = new ArrayList<>();
	private Map<String, ObserverMetaData> observers = new LinkedHashMap<>();
	private List<ObserverMetaData> reversedObservers = new ArrayList<>();
	private Map<String, Action> defaultActions = new TreeMap<>();

	private String jFreeChartPostProcessorClassName;
	private String primeFacesChartPostProcessorClassName;
	
	/**
	 * "<module.document>" -> exported reference
	 */
	private Map<String, List<ExportedReference>> exportedReferences = new TreeMap<>();

	/**
	 * "<module.document>" -extends-> "<module.document>".
	 * Bear in mind that a document may have multiple derivations.
	 */
	private Map<String, String> derivations = new TreeMap<>();

	/**
	 * A cache of constant domains.
	 */
	private Map<String, List<DomainValue>> domainValueCache = new TreeMap<>();

	/**
	 * Returns the configured customer name.
	 *
	 * @return the customer name from metadata.
	 */
	@Override
	public String getName() {
		return name;
	}

	/**
	 * Sets the configured customer name.
	 *
	 * @param name the metadata name for this customer.
	 */
	public void setName(String name) {
		this.name = name;
	}
	
	/**
	 * Returns the timestamp of the last metadata modification detected for this customer.
	 *
	 * @return the last modification time in milliseconds since the epoch.
	 */
	@Override
	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}

	/**
	 * Sets the timestamp of the last metadata modification detected for this customer.
	 *
	 * @param lastModifiedMillis the last modification time in milliseconds since the epoch.
	 */
	public void setLastModifiedMillis(long lastModifiedMillis) {
		this.lastModifiedMillis = lastModifiedMillis;
	}

	/**
	 * Returns the timestamp of the last repository validation check for this customer.
	 *
	 * @return the last check time in milliseconds since the epoch.
	 */
	@Override
	public long getLastCheckedMillis() {
		return lastCheckedMillis;
	}

	/**
	 * Sets the timestamp of the last repository validation check for this customer.
	 *
	 * @param lastCheckedMillis the last check time in milliseconds since the epoch.
	 */
	@Override
	public void setLastCheckedMillis(long lastCheckedMillis) {
		this.lastCheckedMillis = lastCheckedMillis;
	}

	/**
	 * Returns the default language tag configured for this customer.
	 *
	 * @return the IETF BCP 47 language tag, or {@code null} if none is configured.
	 */
	@Override
	public String getLanguageTag() {
		return languageTag;
	}

	/**
	 * Sets the default language tag configured for this customer.
	 *
	 * @param languageTag the IETF BCP 47 language tag to persist with the customer metadata.
	 */
	public void setLanguageTag(String languageTag) {
		this.languageTag = languageTag;
	}

	/**
	 * Returns the default converter for {@link DateOnly} values.
	 *
	 * @return the customer-level date converter, or {@code null} if the framework default applies.
	 */
	@Override
	public Converter<DateOnly> getDefaultDateConverter() {
		return defaultDateConverter;
	}

	/**
	 * Sets the default converter for {@link DateOnly} values.
	 *
	 * @param defaultDateConverter the converter to use for customer-level date formatting and parsing.
	 */
	public void setDefaultDateConverter(Converter<DateOnly> defaultDateConverter) {
		this.defaultDateConverter = defaultDateConverter;
	}

	/**
	 * Returns the default converter for {@link DateTime} values.
	 *
	 * @return the customer-level date-time converter, or {@code null} if the framework default applies.
	 */
	@Override
	public Converter<DateTime> getDefaultDateTimeConverter() {
		return defaultDateTimeConverter;
	}

	/**
	 * Sets the default converter for {@link DateTime} values.
	 *
	 * @param defaultDateTimeConverter the converter to use for customer-level date-time formatting and parsing.
	 */
	public void setDefaultDateTimeConverter(Converter<DateTime> defaultDateTimeConverter) {
		this.defaultDateTimeConverter = defaultDateTimeConverter;
	}

	/**
	 * Returns the default converter for {@link TimeOnly} values.
	 *
	 * @return the customer-level time converter, or {@code null} if the framework default applies.
	 */
	@Override
	public Converter<TimeOnly> getDefaultTimeConverter() {
		return defaultTimeConverter;
	}

	/**
	 * Sets the default converter for {@link TimeOnly} values.
	 *
	 * @param defaultTimeConverter the converter to use for customer-level time formatting and parsing.
	 */
	public void setDefaultTimeConverter(Converter<TimeOnly> defaultTimeConverter) {
		this.defaultTimeConverter = defaultTimeConverter;
	}

	/**
	 * Returns the default converter for {@link Timestamp} values.
	 *
	 * @return the customer-level timestamp converter, or {@code null} if the framework default applies.
	 */
	@Override
	public Converter<Timestamp> getDefaultTimestampConverter() {
		return defaultTimestampConverter;
	}

	/**
	 * Sets the default converter for {@link Timestamp} values.
	 *
	 * @param defaultTimestampConverter the converter to use for customer-level timestamp formatting and parsing.
	 */
	public void setDefaultTimestampConverter(Converter<Timestamp> defaultTimestampConverter) {
		this.defaultTimestampConverter = defaultTimestampConverter;
	}

	/**
	 * Returns the default action map keyed by implicit action name.
	 *
	 * <p>The returned map is the live backing map used during metadata loading.
	 * Callers may mutate it only while constructing customer metadata.
	 *
	 * @return the mutable default action registry.
	 */
	public Map<String, Action> getDefaultActions() {
		return defaultActions;
	}

	/**
	 * Returns the enabled module entry map for this customer.
	 *
	 * <p>The map preserves insertion order and associates each module name with its
	 * configured form label layout.
	 *
	 * @return the live module-entry map.
	 */
	public Map<String, FormLabelLayout> getModuleEntries() {
		return moduleEntries;
	}

	/**
	 * Resolves the configured home module for this customer.
	 *
	 * @return the home module metadata, or {@code null} when no home module is configured.
	 */
	@Override
	public Module getHomeModule() {
		return getModule(homeModuleName);
	}

	/**
	 * Resolves a module by name for this customer.
	 *
	 * @param moduleName the module name to resolve.
	 * @return the module metadata, or {@code null} if the repository does not expose it.
	 */
	@Override
	public Module getModule(String moduleName) {
		Module result = ProvidedRepositoryFactory.get().getModule(this, moduleName);
		if (result == null) {
			throw new MetaDataException("No module named '" + moduleName + "' is available to customer '" + name + "'.");
		}
		return result;
	}

	/**
	 * Returns the enabled modules for this customer in configured display order.
	 *
	 * <p>Module names are resolved lazily from {@link #moduleEntries}, preserving the
	 * order declared in the customer metadata.
	 *
	 * @return the ordered list of enabled modules.
	 */
	@Override
	public List<Module> getModules() {
		List<Module> result = new ArrayList<>(moduleEntries.size());

		// NB Keys are in insertion order
		for (String moduleName : moduleEntries.keySet()) {
			result.add(getModule(moduleName));
		}

		return result;
	}

	/**
	 * Registers a role definition for this customer.
	 *
	 * @param role the role metadata to register.
	 * @return {@code true} if the role name was not already registered.
	 */
	public boolean putRole(CustomerRoleMetaData role) {
		return (roles.put(role.getName(), role) == null);
	}

	/**
	 * Returns the registered customer roles.
	 *
	 * @return an unmodifiable view of the configured roles in registration order.
	 */
	@Override
	public java.util.Collection<CustomerRole> getRoles() {
		return Collections.unmodifiableCollection(roles.values());
	}

	/**
	 * Returns the customer role metadata registered under the supplied role name.
	 *
	 * @param roleName the simple customer role name.
	 * @return the matching role metadata, or {@code null} if no such role exists.
	 */
	
	@Override
	public CustomerRole getRole(String roleName) {
		return roles.get(roleName);
	}
	
	/**
	 * Indicates whether module roles are enabled for this customer.
	 *
	 * @return {@code true} when module-scoped roles may be resolved for authorization.
	 */
	@Override
	public boolean isAllowModuleRoles() {
		return allowModuleRoles;
	}

	/**
	 * Sets whether module roles may be used for this customer.
	 *
	 * @param allowModuleRoles {@code true} to allow module roles; {@code false} to disable them.
	 */
	
	public void setAllowModuleRoles(boolean allowModuleRoles) {
		this.allowModuleRoles = allowModuleRoles;
	}
	
	/**
	 * Registers an interceptor and prepends it to the reverse-order chain.
	 *
	 * @param interceptor interceptor metadata.
	 * @return {@code true} when the interceptor class name was not already registered.
	 */
	public boolean putInterceptor(InterceptorMetaData interceptor) {
		boolean result = (interceptors.put(interceptor.getClassName(), interceptor) == null);
		if (result) {
			reversedInterceptors.add(0, interceptor);
		}
		return result;
	}
	

	/**
	 * Returns the interceptor metadata registered for this customer.
	 *
	 * @return an unmodifiable view of interceptors in registration order.
	 */
	@Override
	public java.util.Collection<InterceptorMetaData> getInterceptors() {
		return Collections.unmodifiableCollection(interceptors.values());
	}

	/**
	 * Registers an observer and prepends it to the reverse-order observer chain.
	 *
	 * @param observer observer metadata.
	 * @return {@code true} when the observer class name was not already registered.
	 */
	public boolean putObserver(ObserverMetaData observer) {
		boolean result = (observers.put(observer.getClassName(), observer) == null);
		if (result) {
			reversedObservers.add(0, observer);
		}
		return result;
	}
	

	/**
	 * Returns the observer metadata registered for this customer.
	 *
	 * @return an unmodifiable view of observers in registration order.
	 */
	@Override
	public java.util.Collection<ObserverMetaData> getObservers() {
		return Collections.unmodifiableCollection(observers.values());
	}

	/**
	 * Sets the home-module name used by {@link #getHomeModule()}.
	 *
	 * @param homeModuleName the module name to use as the customer's landing module.
	 */

	public void setHomeModuleName(String homeModuleName) {
		this.homeModuleName = homeModuleName;
	}

	/**
	 * Returns the UI resource metadata for this customer.
	 *
	 * @return the UI resource bundle, or {@code null} when none has been configured.
	 */
	@Override
	public UIResources getUiResources() {
		return uiResources;
	}

	/**
	 * Sets the UI resource bundle for this customer.
	 *
	 * @param uiResources the UI resource metadata.
	 */

	public void setUiResources(UIResources uiResources) {
		this.uiResources = uiResources;
	}

	/**
	 * Returns the HTML resource metadata for this customer.
	 *
	 * @return the HTML resource bundle, or {@code null} when none has been configured.
	 */
	@Override
	public HTMLResources getHtmlResources() {
		return htmlResources;
	}

	/**
	 * Sets the HTML resource bundle for this customer.
	 *
	 * @param htmlResources the HTML resource metadata.
	 */

	public void setHtmlResources(HTMLResources htmlResources) {
		this.htmlResources = htmlResources;
	}

	/**
	 * Returns the login resource metadata for this customer.
	 *
	 * @return the login resource bundle, or {@code null} when none has been configured.
	 */
	@Override
	public LoginResources getLoginResources() {
		return loginResources;
	}

	/**
	 * Sets the login-page resource bundle for this customer.
	 *
	 * @param loginResources the login resource metadata.
	 */

	public void setLoginResources(LoginResources loginResources) {
		this.loginResources = loginResources;
	}

	/**
	 * Returns the configured JFreeChart post-processor implementation class name.
	 *
	 * @return the fully qualified class name, or {@code null} when no override is configured.
	 */
	@Override
	public String getJFreeChartPostProcessorClassName() {
		return jFreeChartPostProcessorClassName;
	}
	
	/**
	 * Sets the JFreeChart post-processor implementation class name.
	 *
	 * <p>Side effects: normalises blank input via {@link UtilImpl#processStringValue(String)}.
	 *
	 * @param jFreeChartPostProcessorClassName the fully qualified class name, or blank to clear it.
	 */
	
	public void setJFreeChartPostProcessorClassName(String jFreeChartPostProcessorClassName) {
		this.jFreeChartPostProcessorClassName = UtilImpl.processStringValue(jFreeChartPostProcessorClassName);
	}
	
	/**
	 * Returns the configured PrimeFaces chart post-processor implementation class name.
	 *
	 * @return the fully qualified class name, or {@code null} when no override is configured.
	 */
	@Override
	public String getPrimeFacesChartPostProcessorClassName() {
		return primeFacesChartPostProcessorClassName;
	}
	
	/**
	 * Sets the PrimeFaces chart post-processor implementation class name.
	 *
	 * <p>Side effects: normalises blank input via {@link UtilImpl#processStringValue(String)}.
	 *
	 * @param primeFacesChartPostProcessorClassName the fully qualified class name, or blank to clear it.
	 */
	public void setPrimeFacesChartPostProcessorClassName(String primeFacesChartPostProcessorClassName) {
		this.primeFacesChartPostProcessorClassName = UtilImpl.processStringValue(primeFacesChartPostProcessorClassName);
	}

	/**
	 * Rebuilds customer-level derivation and exported-reference indexes.
	 *
	 * <p>Side effects: clears and repopulates in-memory dependency structures used for
	 * metadata validation, referential-integrity checks, backup/export, and overridden
	 * domain generation.
	 */
	@Override
	@SuppressWarnings("java:S3776") // Complexity OK
	public void determineDependencies() {
		derivations.clear();
		exportedReferences.clear();
		
		for (Module module : getModules()) {
			for (String documentName : module.getDocumentRefs().keySet()) {
				DocumentRef documentRef = module.getDocumentRefs().get(documentName);
				Document document = module.getDocument(this, documentName);

				// do it for documents defined in this module - not external references
				if (documentRef.getOwningModuleName().equals(module.getName())) {
					Extends inherits = document.getExtends();
					if (inherits != null) {
						Document baseDocument = module.getDocument(this, inherits.getDocumentName());
						derivations.put(module.getName() + '.' + document.getName(),
											baseDocument.getOwningModuleName() + '.' + baseDocument.getName());
					}
				}
				
				// do it for persistent documents defined in this module - not external references
				// NB - References to mapped documents must be included so that document overriding can occur
				Persistent persistent = document.getPersistent();
				if ((persistent != null) && 
						documentRef.getOwningModuleName().equals(module.getName())) {
					for (String referenceName : document.getReferenceNames()) {
						Reference reference = document.getReferenceByName(referenceName);
						Document targetDocument = module.getDocument(this, reference.getDocumentName());
						if (reference.isPersistent()) {
							// Ensure that the document is tagged as ordered, 
							// if it is the target of any ordered collection
							if ((reference instanceof Collection collection) && 
									Boolean.TRUE.equals(collection.getOrdered())) {
								((DocumentImpl) targetDocument).setOrdered(true);
							}
							
							// Embedded associations don't have an exported reference
							if (AssociationType.embedded.equals(reference.getType())) {
								continue;
							}

							// References are required to be able to check FK constraints in HibernatePersistence
							// but are also needed to be able to export the tables - Backup
							// and to generate the overridden domain - OverridableDomainGenerator
							ExportedReference ref = new ExportedReference();
							ref.setModuleName(document.getOwningModuleName());
							ref.setDocumentName(document.getName());
							ref.setDocumentAlias(document.getSingularAlias());
							ref.setPersistent(document.getPersistent());
							ref.setReferenceFieldName(reference.getName());
							ref.setType(reference.getType());
							ref.setRequired(reference.isRequired());
							String documentNameKey = targetDocument.getOwningModuleName() +
														'.' + targetDocument.getName();
							List<ExportedReference> refs = exportedReferences.get(documentNameKey);
							if (refs == null) {
								refs = new ArrayList<>();
								exportedReferences.put(documentNameKey, refs);
							}
							refs.add(ref);
						}
					}
				}
			}
		}
	}

	/**
	 * Returns exported references that point at the supplied document.
	 *
	 * <p>Used when checking referential integrity during delete operations and when
	 * generating overridden ORM metadata.
	 *
	 * @param document the referenced target document.
	 * @return the exported references targeting {@code document}, or {@code null} when none exist.
	 */
	public List<ExportedReference> getExportedReferences(Document document) {
		return exportedReferences.get(document.getOwningModuleName() + '.' + document.getName());
	}

	/**
	 * If the document derives (extends or restricts) another document then the
	 * result will be "<module>.<document>", otherwise if this document isn't a
	 * derivation, then the result is <code>null</code>.
	 * 
	 * @param document
	 * @return The base document name or <code>null</code>
	 */
	public String getBaseDocument(Document document) {
		return derivations.get(document.getOwningModuleName() + '.' + document.getName());
	}

	/**
	 * If the document is a base document (is extended by another document)
	 * then the result will be a list of "<module>.<document>",
	 * otherwise if this document isn't a base, the result is an empty list.
	 * 
	 * @param document
	 * @return The list of derived documents (or an empty list if none exist).
	 */
	public @Nonnull List<String> getDerivedDocuments(Document document) {
		List<String> result = new ArrayList<>();

		String value = document.getOwningModuleName() + '.' + document.getName();
		for (Entry<String, String> entry : derivations.entrySet()) {
			if (value.equals(entry.getValue())) {
				result.add(entry.getKey());
			}
		}

		return result;
	}
	
	/**
	 * Returns constant domain values for the supplied attribute, caching the result by
	 * module, document, and attribute name.
	 *
	 * <p>Side effects: invokes before/after interceptor hooks and populates the customer
	 * domain-value cache on first resolution.
	 *
	 * @param bizlet the bizlet that may supply constant domain values.
	 * @param moduleName the owning module name.
	 * @param documentName the owning document name.
	 * @param attribute the attribute whose constant domain values are required.
	 * @param <T> the bean type handled by the bizlet.
	 * @return the resolved constant domain values, or {@code null} when resolution is vetoed.
	 * @throws Exception if bizlet execution or interceptor processing fails.
	 */
	@Override
	@SuppressWarnings({"unchecked", "java:S3776"}) // Complexity OK
	public synchronized <T extends Bean> List<DomainValue> getConstantDomainValues(Bizlet<T> bizlet, 
																					String moduleName,
																					String documentName, 
																					Attribute attribute)
	throws Exception {
		List<DomainValue> result = null;
		
		String attributeName = attribute.getName();
		boolean vetoed = interceptBeforeGetConstantDomainValues(attributeName);
		if (! vetoed) {
			String key = moduleName + '.' + documentName + '.' + attributeName;
			result = domainValueCache.get(key);
			if (result == null) {
				if ((bizlet != null) && DomainType.constant.equals(attribute.getDomainType())) {
                    if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Entering {}.getConstantDomainValues: {}", bizlet.getClass().getName(), attributeName);
					result = bizlet.getConstantDomainValues(attributeName);
                    if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Exiting {}.getConstantDomainValues: {}", bizlet.getClass().getName(), result);
					domainValueCache.put(key, result);
				}
				if ((result == null) && (attribute instanceof Enumeration e)) {
					e = e.getTarget();
					if (e.isDynamic()) {
						List<EnumeratedValue> values = e.getTarget().getValues();
						result = new ArrayList<>(values.size());
						for (EnumeratedValue value : values) {
							String code = value.getCode();
							String description = value.getDescription();
							if (description == null) {
								description = code;
							}
							result.add(new DomainValue(code, description));
						}
					}
					else { // need to use the method if its available as it could be a hand implemented enumeration.
						Class<?> domainEnum = attribute.getImplementingType();
						result = (List<DomainValue>) domainEnum.getMethod(org.skyve.domain.types.Enumeration.TO_DOMAIN_VALUES_METHOD_NAME).invoke(null);
					}
					
					domainValueCache.put(key, result);
				}
			}

			interceptAfterGetConstantDomainValues(attributeName, result);
		}

		if (result == null) {
			result = Collections.emptyList();
		}
		
		return result;
	}

	/**
	 * Notifies all registered observers that customer startup is beginning.
	 */
	public void notifyStartup() {
		for (ObserverMetaData observer : observers.values()) {
			observer.getObserver().startup(this);
		}
	}

	/**
	 * Notifies all registered observers that customer shutdown is beginning.
	 *
	 * <p>Observers are invoked in reverse registration order.
	 */
	public void notifyShutdown() {
		for (ObserverMetaData observer : reversedObservers) {
			observer.getObserver().shutdown(this);
		}
	}

	/**
	 * Notifies observers before a backup operation starts.
	 */
	public void notifyBeforeBackup() {
		for (ObserverMetaData observer : observers.values()) {
			observer.getObserver().beforeBackup(this);
		}
	}

	/**
	 * Notifies observers after a backup operation completes.
	 *
	 * <p>Observers are invoked in reverse registration order.
	 */
	public void notifyAfterBackup() {
		for (ObserverMetaData observer : reversedObservers) {
			observer.getObserver().afterBackup(this);
		}
	}

	/**
	 * Notifies observers before a restore operation starts.
	 */
	public void notifyBeforeRestore() {
		for (ObserverMetaData observer : observers.values()) {
			observer.getObserver().beforeRestore(this);
		}
	}

	/**
	 * Notifies observers after a restore operation completes.
	 *
	 * <p>Observers are invoked in reverse registration order.
	 */
	public void notifyAfterRestore() {
		for (ObserverMetaData observer : reversedObservers) {
			observer.getObserver().afterRestore(this);
		}
	}

	/**
	 * Notifies observers that a user has logged in.
	 *
	 * @param user the authenticated user.
	 * @param session the active HTTP session for the login.
	 */
	public void notifyLogin(User user, HttpSession session) {
		for (ObserverMetaData observer : observers.values()) {
			observer.getObserver().login(user, session);
		}
	}

	/**
	 * Notifies observers that a user is logging out.
	 *
	 * <p>Observers are invoked in reverse registration order.
	 *
	 * @param user the user being logged out.
	 * @param session the associated HTTP session.
	 */
	public void notifyLogout(User user, HttpSession session) {
		for (ObserverMetaData observer : reversedObservers) {
			observer.getObserver().logout(user, session);
		}
	}

	/**
	 * Invokes interceptors before a new bean instance is created.
	 *
	 * @param bean the bean instance about to be initialised.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforeNewInstance(Bean bean) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforeNewInstance(bean)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Invokes interceptors after a new bean instance has been created.
	 *
	 * <p>Interceptors are invoked in reverse registration order.
	 *
	 * @param bean the newly created bean instance.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterNewInstance(Bean bean) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterNewInstance(bean);
		}
	}

	/**
	 * Invokes interceptors before validation runs for a bean.
	 *
	 * @param bean the bean being validated.
	 * @param e the accumulating validation exception.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforeValidate(Bean bean, ValidationException e) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforeValidate(bean, e)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Invokes interceptors after validation completes for a bean.
	 *
	 * @param bean the validated bean.
	 * @param e the validation result container.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterValidate(Bean bean, ValidationException e) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterValidate(bean, e);
		}
	}

	/**
	 * Invokes interceptors before constant-domain values are resolved.
	 *
	 * @param attributeName the attribute whose domain values are being requested.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforeGetConstantDomainValues(String attributeName) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforeGetConstantDomainValues(attributeName)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Invokes interceptors after constant-domain values have been resolved.
	 *
	 * @param attributeName the attribute whose domain values were requested.
	 * @param result the resolved domain values.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterGetConstantDomainValues(String attributeName, List<DomainValue> result) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterGetConstantDomainValues(attributeName, result);
		}
	}

	/**
	 * Invokes interceptors before variant-domain values are resolved.
	 *
	 * @param attributeName the attribute whose variant domain values are being requested.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforeGetVariantDomainValues(String attributeName) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforeGetVariantDomainValues(attributeName)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Invokes interceptors after variant-domain values have been resolved.
	 *
	 * @param attributeName the attribute whose domain values were requested.
	 * @param result the resolved domain values.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterGetVariantDomainValues(String attributeName, List<DomainValue> result) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterGetVariantDomainValues(attributeName, result);
		}
	}

	/**
	 * Invokes interceptors before dynamic-domain values are resolved.
	 *
	 * @param attributeName the attribute whose dynamic domain values are being requested.
	 * @param bean the contextual bean used for domain evaluation.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforeGetDynamicDomainValues(String attributeName, Bean bean) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforeGetDynamicDomainValues(attributeName, bean)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Invokes interceptors after dynamic-domain values have been resolved.
	 *
	 * @param attributeName the attribute whose domain values were requested.
	 * @param bean the contextual bean used during evaluation.
	 * @param result the resolved domain values.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterGetDynamicDomainValues(String attributeName, Bean bean, List<DomainValue> result) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterGetDynamicDomainValues(attributeName, bean, result);
		}
	}

	/**
	 * Invokes interceptors before an auto-complete request is evaluated.
	 *
	 * @param attributeName the completing attribute.
	 * @param value the user-entered prefix value.
	 * @param bean the contextual bean.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforeComplete(String attributeName, String value, Bean bean)
	throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforeComplete(attributeName, value, bean)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Invokes interceptors after an auto-complete request has been evaluated.
	 *
	 * @param attributeName the completing attribute.
	 * @param value the user-entered prefix value.
	 * @param bean the contextual bean.
	 * @param result the completion candidates.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterComplete(String attributeName, String value, Bean bean, List<String> result)
	throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterComplete(attributeName, value, bean, result);
		}
	}

	/**
	 * Invokes interceptors before a bean save operation.
	 *
	 * @param document the document metadata for the bean.
	 * @param bean the persistent bean about to be saved.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforeSave(Document document, PersistentBean bean) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforeSave(document, bean)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Invokes interceptors after a bean save operation.
	 *
	 * @param document the document metadata for the saved bean.
	 * @param result the saved bean instance.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterSave(Document document, PersistentBean result) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterSave(document, result);
		}
	}

	/**
	 * Invokes interceptors before bean pre-save callbacks run.
	 *
	 * @param bean the bean about to enter pre-save processing.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforePreSave(Bean bean) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforePreSave(bean)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Invokes interceptors after bean pre-save callbacks run.
	 *
	 * @param bean the processed bean.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterPreSave(Bean bean) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterPreSave(bean);
		}
	}

	/**
	 * Invokes interceptors before bean post-save callbacks run.
	 *
	 * @param bean the bean entering post-save processing.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforePostSave(Bean bean) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforePostSave(bean)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Invokes interceptors after bean post-save callbacks run.
	 *
	 * @param bean the processed bean.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterPostSave(Bean bean) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterPostSave(bean);
		}
	}

	/**
	 * Invokes interceptors before a bean delete operation.
	 *
	 * @param document the document metadata for the bean.
	 * @param bean the bean about to be deleted.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforeDelete(Document document, PersistentBean bean) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforeDelete(document, bean)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Invokes interceptors after a bean delete operation.
	 *
	 * @param document the deleted bean's document metadata.
	 * @param bean the deleted bean instance.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterDelete(Document document, PersistentBean bean) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterDelete(document, bean);
		}
	}

	/**
	 * Invokes interceptors before bean pre-delete callbacks run.
	 *
	 * @param bean the bean about to enter pre-delete processing.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforePreDelete(PersistentBean bean) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforePreDelete(bean)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Invokes interceptors after bean pre-delete callbacks run.
	 *
	 * @param bean the processed bean.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterPreDelete(PersistentBean bean) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterPreDelete(bean);
		}
	}

	/**
	 * Invokes interceptors before bean post-delete callbacks run.
	 *
	 * @param bean the bean entering post-delete processing.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforePostDelete(PersistentBean bean) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforePostDelete(bean)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Invokes interceptors after bean post-delete callbacks run.
	 *
	 * @param bean the processed bean.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterPostDelete(PersistentBean bean) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterPostDelete(bean);
		}
	}

	/**
	 * Invokes interceptors before bean post-load callbacks run.
	 *
	 * @param bean the loaded bean.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforePostLoad(PersistentBean bean) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforePostLoad(bean)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Invokes interceptors after bean post-load callbacks run.
	 *
	 * @param bean the loaded bean.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterPostLoad(PersistentBean bean) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterPostLoad(bean);
		}
	}

	/**
	 * Invokes interceptors before implicit pre-execute processing runs.
	 *
	 * @param actionName the implicit action being processed.
	 * @param bean the current bean.
	 * @param parentBean the parent bean context, if any.
	 * @param webContext the active web context.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforePreExecute(ImplicitActionName actionName, Bean bean, Bean parentBean, WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforePreExecute(actionName, bean, parentBean, webContext)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Invokes interceptors after implicit pre-execute processing completes.
	 *
	 * @param actionName the implicit action being processed.
	 * @param result the resulting bean.
	 * @param parentBean the parent bean context, if any.
	 * @param webContext the active web context.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterPreExecute(ImplicitActionName actionName,
											Bean result,
											Bean parentBean,
											WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterPreExecute(actionName, result, parentBean, webContext);
		}
	}

	/**
	 * Invokes interceptors before pre-rerender processing runs.
	 *
	 * @param source the rerender trigger source.
	 * @param bean the current bean.
	 * @param webContext the active web context.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforePreRerender(String source, Bean bean, WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforePreRerender(source, bean, webContext)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Invokes interceptors after pre-rerender processing completes.
	 *
	 * @param source the triggering rerender source.
	 * @param result the resulting bean.
	 * @param webContext the active web context.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterPreRerender(String source,
											Bean result,
											WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterPreRerender(source, result, webContext);
		}
	}

	/**
	 * Invokes interceptors before a server-side action runs.
	 *
	 * @param document the action document.
	 * @param actionName the action name.
	 * @param bean the bean bound to the action.
	 * @param webContext the active web context.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforeServerSideAction(Document document,
													String actionName, 
													Bean bean,
													WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforeServerSideAction(document, actionName, bean, webContext)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Invokes interceptors after a server-side action completes.
	 *
	 * @param document the action document.
	 * @param actionName the action name.
	 * @param result the action result wrapper.
	 * @param webContext the active web context.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterServerSideAction(Document document, 
												String actionName, 
												ServerSideActionResult<Bean> result, 
												WebContext webContext) 
	throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterServerSideAction(document, actionName, result, webContext);
		}
	}

	/**
	 * Invokes interceptors before a download action runs.
	 *
	 * @param document the action document.
	 * @param actionName the action name.
	 * @param bean the bean bound to the action.
	 * @param webContext the active web context.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforeDownloadAction(Document document,
													String actionName,
													Bean bean,
													WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforeDownloadAction(document, actionName, bean, webContext)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Invokes interceptors after a download action completes.
	 *
	 * @param document the action document.
	 * @param actionName the action name.
	 * @param bean the bean bound to the action.
	 * @param download the produced download descriptor.
	 * @param webContext the active web context.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterDownloadAction(Document document,
												String actionName,
												Bean bean,
												Download download,
												WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterDownloadAction(document, actionName, bean, download, webContext);
		}
	}

	/**
	 * Invokes interceptors before an upload action runs.
	 *
	 * @param document the action document.
	 * @param actionName the action name.
	 * @param bean the bean bound to the action.
	 * @param upload the uploaded content descriptor.
	 * @param webContext the active web context.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforeUploadAction(Document document,
												String actionName,
												Bean bean,
												Upload upload,
												WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforeUploadAction(document, actionName, bean, upload, webContext)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Invokes interceptors after an upload action completes.
	 *
	 * @param document the action document.
	 * @param actionName the action name.
	 * @param bean the bean bound to the action.
	 * @param upload the uploaded content descriptor.
	 * @param webContext the active web context.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterUploadAction(Document document,
											String actionName,
											Bean bean,
											Upload upload,
											WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterUploadAction(document, actionName, bean, upload, webContext);
		}
	}
	
	/**
	 * Invokes interceptors before a BizPort import action runs.
	 *
	 * @param document the action document.
	 * @param actionName the action name.
	 * @param bizPortable the workbook being imported.
	 * @param problems the accumulating import problems container.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforeBizImportAction(Document document,
													String actionName,
													BizPortWorkbook bizPortable,
													UploadException problems)
	throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforeBizImportAction(document, actionName, bizPortable, problems)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Invokes interceptors after a BizPort import action completes.
	 *
	 * @param document the action document.
	 * @param actionName the action name.
	 * @param bizPortable the workbook being imported.
	 * @param problems the accumulating import problems container.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterBizImportAction(Document document,
												String actionName,
												BizPortWorkbook bizPortable,
												UploadException problems)
	throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterBizImportAction(document, actionName, bizPortable, problems);
		}
	}

	/**
	 * Invokes interceptors before a BizPort export action runs.
	 *
	 * @param document the action document.
	 * @param actionName the action name.
	 * @param webContext the active web context.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforeBizExportAction(Document document, String actionName, WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforeBizExportAction(document, actionName, webContext)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Invokes interceptors after a BizPort export action completes.
	 *
	 * @param document the action document.
	 * @param actionName the action name.
	 * @param result the exported workbook.
	 * @param webContext the active web context.
	 * @throws Exception if an interceptor fails.
	 */
	public void interceptAfterBizExportAction(Document document,
												String actionName,
												BizPortWorkbook result,
												WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterBizExportAction(document, actionName, result, webContext);
		}
	}

	/**
	 * Invokes interceptors before post-render processing runs.
	 *
	 * @param bean the bean about to be post-rendered.
	 * @param webContext the active web context.
	 * @return {@code true} if any interceptor vetoes further processing.
	 * @throws Exception if an interceptor fails.
	 */
	public boolean interceptBeforePostRender(Bean bean, WebContext webContext) {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor().beforePostRender(bean, webContext)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Invokes interceptors after post-render processing completes.
	 *
	 * @param result the rendered bean result.
	 * @param webContext the active web context.
	 */
	public void interceptAfterPostRender(Bean result, WebContext webContext) {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor().afterPostRender(result, webContext);
		}
	}

	/**
	 * Returns the roles allowed to use text search for this customer.
	 *
	 * @return the live role-name set.
	 */
	public Set<String> getTextSearchRoles() {
		return textSearchRoles;
	}
	
	/**
	 * Returns the roles allowed to flag records for this customer.
	 *
	 * @return the live role-name set.
	 */
	public Set<String> getFlagRoles() {
		return flagRoles;
	}

	/**
	 * Returns the roles allowed to switch UI modes for this customer.
	 *
	 * @return the live role-name set.
	 */
	public Set<String> getSwitchModeRoles() {
		return switchModeRoles;
	}

	/**
	 * Returns a concise string representation containing the customer name.
	 */
    @Override
    public String toString() {
        return MoreObjects.toStringHelper(this)
                          .add("name", name)
                          .toString();
    }
}
       