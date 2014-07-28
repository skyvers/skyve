package org.skyve.wildcat.metadata.customer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.logging.Level;

import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.converters.Converter;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.customer.HTMLResources;
import org.skyve.metadata.customer.LoginResources;
import org.skyve.metadata.customer.Service;
import org.skyve.metadata.customer.UIResources;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Reference.ReferenceType;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.view.Action;
import org.skyve.wildcat.metadata.model.document.DocumentImpl;
import org.skyve.wildcat.metadata.model.document.field.Enumeration;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.util.UtilImpl;

public class CustomerImpl implements Customer {
	public class ExportedReference implements Serializable {
		/**
		 * For Serialization.
		 */
		private static final long serialVersionUID = 3054965257788501970L;

		private String moduleName;
		private String documentName;
		private Persistent persistent;
		private String documentAlias;
		private String referenceFieldName;
		private ReferenceType type;

		public String getDocumentAlias() {
			return documentAlias;
		}

		public void setDocumentAlias(String documentAlias) {
			this.documentAlias = documentAlias;
		}

		public String getReferenceFieldName() {
			return referenceFieldName;
		}

		public void setReferenceFieldName(String referenceFieldName) {
			this.referenceFieldName = referenceFieldName;
		}

		public Persistent getPersistent() {
			return persistent;
		}

		public void setPersistent(Persistent persistent) {
			this.persistent = persistent;
		}

		public String getDocumentName() {
			return documentName;
		}

		public void setDocumentName(String documentName) {
			this.documentName = documentName;
		}

		public String getModuleName() {
			return moduleName;
		}

		public void setModuleName(String moduleName) {
			this.moduleName = moduleName;
		}

		public boolean isCollection() {
			return (type instanceof CollectionType);
		}

		public ReferenceType getType() {
			return type;
		}

		public void setType(ReferenceType type) {
			this.type = type;
		}
	}

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
	 * For Serialization.
	 */
	private static final long serialVersionUID = 2926460705821800439L;

	/**
	 * Customer name.
	 */
	private String name;

	private UIResources uiResources;
	private HTMLResources htmlResources;
	private LoginResources loginResources;
	
	private Converter<DateOnly> defaultDateConverter;
	private Converter<TimeOnly> defaultTimeConverter;
	private Converter<DateTime> defaultDateTimeConverter;
	private Converter<Timestamp> defaultTimestampConverter;
	private List<String> moduleNames = new ArrayList<>();
	private String homeModuleName;
	private Map<String, Service> services = new TreeMap<>();
	private Map<String, Action> defaultActions = new TreeMap<>();

	/**
	 * vtable == fullyQualifiedName -> location 
	 * (later will be - customer defined = TRUE, otherwise FALSE)
	 */
	private Map<String, String> vtable = new TreeMap<>();

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

	@Override
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return Returns the defaultDateConverter.
	 */
	@Override
	public Converter<DateOnly> getDefaultDateConverter() {
		return defaultDateConverter;
	}

	/**
	 * @param defaultDateConverter	The defaultDateConverter to set.
	 */
	public void setDefaultDateConverter(Converter<DateOnly> defaultDateConverter) {
		this.defaultDateConverter = defaultDateConverter;
	}

	/**
	 * @return Returns the defaultDateTimeConverter.
	 */
	@Override
	public Converter<DateTime> getDefaultDateTimeConverter() {
		return defaultDateTimeConverter;
	}

	/**
	 * @param defaultDateTimeConverter	The defaultDateTimeConverter to set.
	 */
	public void setDefaultDateTimeConverter(Converter<DateTime> defaultDateTimeConverter) {
		this.defaultDateTimeConverter = defaultDateTimeConverter;
	}

	/**
	 * @return Returns the defaultTimeConverter.
	 */
	@Override
	public Converter<TimeOnly> getDefaultTimeConverter() {
		return defaultTimeConverter;
	}

	/**
	 * @param defaultTimeConverter	The defaultTimeConverter to set.
	 */
	public void setDefaultTimeConverter(Converter<TimeOnly> defaultTimeConverter) {
		this.defaultTimeConverter = defaultTimeConverter;
	}

	/**
	 * @return Returns the defaultTimestampConverter.
	 */
	@Override
	public Converter<Timestamp> getDefaultTimestampConverter() {
		return defaultTimestampConverter;
	}

	/**
	 * @param defaultTimestampConverter	The defaultTimestampConverter to set.
	 */
	public void setDefaultTimestampConverter(Converter<Timestamp> defaultTimestampConverter) {
		this.defaultTimestampConverter = defaultTimestampConverter;
	}

	public Map<String, String> getVTable() {
		return vtable;
	}

	public Map<String, Action> getDefaultActions() {
		return defaultActions;
	}

	public List<String> getModuleNames() {
		return moduleNames;
	}

	@Override
	public Module getHomeModule() throws MetaDataException {
		return getModule(homeModuleName);
	}

	@Override
	public final Module getModule(String moduleName) throws MetaDataException {
		return AbstractRepository.get().getModule(this, moduleName);
	}

	@Override
	public List<Module> getModules() throws MetaDataException {
		List<Module> result = new ArrayList<>(moduleNames.size());

		for (String moduleName : moduleNames) {
			result.add(getModule(moduleName));
		}

		return result;
	}

	@Override
	public Service getService(String serviceName) {
		return services.get(serviceName);
	}

	public boolean putService(Service service) {
		return (services.put(service.getName(), service) == null);
	}
	
	@Override
	public java.util.Collection<Service> getServices() {
		return Collections.unmodifiableCollection(services.values());
	}

	public void setHomeModuleName(String homeModuleName) {
		this.homeModuleName = homeModuleName;
	}

	@Override
	public UIResources getUiResources() {
		return uiResources;
	}

	public void setUiResources(UIResources uiResources) {
		this.uiResources = uiResources;
	}

	@Override
	public HTMLResources getHtmlResources() {
		return htmlResources;
	}

	public void setHtmlResources(HTMLResources htmlResources) {
		this.htmlResources = htmlResources;
	}

	@Override
	public LoginResources getLoginResources() {
		return loginResources;
	}

	public void setLoginResources(LoginResources loginResources) {
		this.loginResources = loginResources;
	}

	public void determineDependencies() throws MetaDataException {
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
							if ((reference instanceof Collection) && 
									Boolean.TRUE.equals(((Collection) reference).getOrdered())) {
								((DocumentImpl) targetDocument).setOrdered(true);
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
				} // if (persistent docs defined in the module)
			} // for (all docs in this module)
		} // for (all modules for this customer)
	}

	/**
	 * This method returns all exported references - ie references pointing to the document parameter given.
	 * This is used to check referential integrity when deleting, and when generating overridden ORMs.
	 * @param document
	 * @return
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
	 * If the document is a base document (is extended or is restricted) by
	 * another document than the result will be a list of "<module>.<document>",
	 * otherwise if this document isn't a base, then the result is an empty list.
	 * 
	 * @param document
	 * @return The list of derived documents (or an empty list if none exist).
	 */
	public List<String> getDerivedDocuments(Document document) {
		List<String> result = new ArrayList<>();

		String value = document.getOwningModuleName() + '.' + document.getName();
		for (Entry<String, String> entry : derivations.entrySet()) {
			if (value.equals(entry.getValue())) {
				result.add(entry.getKey());
			}
		}

		return result;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public synchronized <T extends Bean> List<DomainValue> getConstantDomainValues(Bizlet<T> bizlet, 
																					String documentName, 
																					Attribute attribute)
	throws Exception {
		String attributeName = attribute.getName();
		String key = documentName + '.' + attributeName;
		List<DomainValue> result = domainValueCache.get(key);
		if (attribute instanceof Enumeration) {
			Class<org.skyve.domain.types.Enumeration> domainEnum = AbstractRepository.get().getEnum((Enumeration) attribute);
			result = (List<DomainValue>) domainEnum.getMethod("toDomainValues").invoke(null);
			domainValueCache.put(key, result);
		}
		else if ((bizlet != null) && (result == null)) {
			if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "getConstantDomainValues", "Entering " + bizlet.getClass().getName() + ".getConstantDomainValues: " + attributeName);
			result = bizlet.getConstantDomainValues(attributeName);
			if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "getConstantDomainValues", "Exiting " + bizlet.getClass().getName() + ".getConstantDomainValues: " + result);
			domainValueCache.put(key, result);
		}

		return result;
	}
}
