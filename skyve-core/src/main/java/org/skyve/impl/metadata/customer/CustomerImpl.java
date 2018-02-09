package org.skyve.impl.metadata.customer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.logging.Level;

import javax.enterprise.inject.Alternative;

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
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.customer.CustomerRoleMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.DownloadAction.Download;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.controller.UploadAction.UploadedFile;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.customer.CustomerRole;
import org.skyve.metadata.customer.HTMLResources;
import org.skyve.metadata.customer.InterceptorMetaData;
import org.skyve.metadata.customer.LoginResources;
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
import org.skyve.web.WebContext;

@Alternative
public class CustomerImpl implements Customer {
	private static final long serialVersionUID = 2926460705821800439L;

	public class ExportedReference implements Serializable {
		private static final long serialVersionUID = 3054965257788501970L;

		private String moduleName;
		private String documentName;
		private Persistent persistent;
		private String documentAlias;
		private String referenceFieldName;
		private ReferenceType type;
		private boolean required;
		
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

		public boolean isRequired() {
			return required;
		}

		public void setRequired(boolean required) {
			this.required = required;
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
	 * Customer name.
	 */
	private String name;

	private String languageTag;
	
	private UIResources uiResources;
	private HTMLResources htmlResources;
	private LoginResources loginResources;
	
	private Converter<DateOnly> defaultDateConverter;
	private Converter<TimeOnly> defaultTimeConverter;
	private Converter<DateTime> defaultDateTimeConverter;
	private Converter<Timestamp> defaultTimestampConverter;
	private List<String> moduleNames = new ArrayList<>();
	private String homeModuleName;
	private List<CustomerRoleMetaData> roles = new ArrayList<>();
	private boolean allowModuleRoles;
	private Map<String, InterceptorMetaData> interceptors = new LinkedHashMap<>();
	private List<InterceptorMetaData> reversedInterceptors = new ArrayList<>();
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
	
	@Override
	public String getLanguageTag() {
		return languageTag;
	}

	public void setLanguageTag(String languageTag) {
		this.languageTag = languageTag;
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
	public Module getHomeModule() {
		return getModule(homeModuleName);
	}

	@Override
	public final Module getModule(String moduleName) {
		return AbstractRepository.get().getModule(this, moduleName);
	}

	@Override
	public List<Module> getModules() {
		List<Module> result = new ArrayList<>(moduleNames.size());

		for (String moduleName : moduleNames) {
			result.add(getModule(moduleName));
		}

		return result;
	}

	public void setRoles(List<CustomerRoleMetaData> roles) {
		this.roles = roles;
	}

	@Override
	public List<CustomerRole> getRoles() {
		return Collections.unmodifiableList(roles);
	}
	
	@Override
	public boolean isAllowModuleRoles() {
		return allowModuleRoles;
	}
	
	public void setAllowModuleRoles(boolean allowModuleRoles) {
		this.allowModuleRoles = allowModuleRoles;
	}
	
	public boolean putInterceptor(InterceptorMetaData interceptor) {
		boolean result = (interceptors.put(interceptor.getClassName(), interceptor) == null);
		if (result) {
			reversedInterceptors.add(0, interceptor);
		}
		return result;
	}
	

	@Override
	public java.util.Collection<InterceptorMetaData> getInterceptors() {
		return Collections.unmodifiableCollection(interceptors.values());
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

	public void determineDependencies() {
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
		List<DomainValue> result = null;
		
		String attributeName = attribute.getName();
		boolean vetoed = interceptBeforeGetConstantDomainValues(attributeName);
		if (! vetoed) {
			String key = documentName + '.' + attributeName;
			result = domainValueCache.get(key);
			if (result == null) {
				if (bizlet != null) {
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "getConstantDomainValues", "Entering " + bizlet.getClass().getName() + ".getConstantDomainValues: " + attributeName);
					result = bizlet.getConstantDomainValues(attributeName);
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "getConstantDomainValues", "Exiting " + bizlet.getClass().getName() + ".getConstantDomainValues: " + result);
					domainValueCache.put(key, result);
				}
				if ((result == null) && (attribute instanceof Enumeration)) {
					Class<org.skyve.domain.types.Enumeration> domainEnum = AbstractRepository.get().getEnum((Enumeration) attribute);
					result = (List<DomainValue>) domainEnum.getMethod(org.skyve.domain.types.Enumeration.TO_DOMAIN_VALUES_METHOD_NAME).invoke(null);
					domainValueCache.put(key, result);
				}
			}

			interceptAfterGetConstantDomainValues(attributeName, result);
		}

		return result;
	}
	
	public boolean interceptBeforeNewInstance(Bean bean) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforeNewInstance(bean)) {
				return true;
			}
		}
		return false;
	}
	
	public void interceptAfterNewInstance(Bean bean) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterNewInstance(bean);
		}
	}
	
	public boolean interceptBeforeValidate(Bean bean, ValidationException e) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforeValidate(bean, e)) {
				return true;
			}
		}
		return false;
	}

	public void interceptAfterValidate(Bean bean, ValidationException e) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterValidate(bean, e);
		}
	}
	
	public boolean interceptBeforeGetConstantDomainValues(String attributeName) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforeGetConstantDomainValues(attributeName)) {
				return true;
			}
		}
		return false;
	}

	public void interceptAfterGetConstantDomainValues(String attributeName, List<DomainValue> result) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterGetConstantDomainValues(attributeName, result);
		}
	}

	public boolean interceptBeforeGetVariantDomainValues(String attributeName) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforeGetVariantDomainValues(attributeName)) {
				return true;
			}
		}
		return false;
	}

	public void interceptAfterGetVariantDomainValues(String attributeName, List<DomainValue> result) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterGetVariantDomainValues(attributeName, result);
		}
	}

	public boolean interceptBeforeGetDynamicDomainValues(String attributeName, Bean bean) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforeGetDynamicDomainValues(attributeName, bean)) {
				return true;
			}
		}
		return false;
	}

	public void interceptAfterGetDynamicDomainValues(String attributeName, Bean bean, List<DomainValue> result) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterGetDynamicDomainValues(attributeName, bean, result);
		}
	}

	public boolean interceptBeforeSave(Document document, PersistentBean bean) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforeSave(document, bean)) {
				return true;
			}
		}
		return false;
	}
	
	public void interceptAfterSave(Document document, PersistentBean result) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterSave(document, result);
		}
	}
	
	public boolean interceptBeforePreSave(Bean bean) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforePreSave(bean)) {
				return true;
			}
		}
		return false;
	}

	public void interceptAfterPreSave(Bean bean) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterPreSave(bean);
		}
	}
	
	public boolean interceptBeforePostSave(Bean bean) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforePostSave(bean)) {
				return true;
			}
		}
		return false;
	}

	public void interceptAfterPostSave(Bean bean) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterPostSave(bean);
		}
	}

	public boolean interceptBeforeDelete(Document document, PersistentBean bean) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforeDelete(document, bean)) {
				return true;
			}
		}
		return false;
	}
	
	public void interceptAfterDelete(Document document, PersistentBean bean) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterDelete(document, bean);
		}
	}

	public boolean interceptBeforePreDelete(PersistentBean bean) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforePreDelete(bean)) {
				return true;
			}
		}
		return false;
	}
	
	public void interceptAfterPreDelete(PersistentBean bean) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterPreDelete(bean);
		}
	}

	public boolean interceptBeforePostLoad(PersistentBean bean) throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforePostLoad(bean)) {
				return true;
			}
		}
		return false;
	}
	
	public void interceptAfterPostLoad(PersistentBean bean) throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterPostLoad(bean);
		}
	}
	
	public boolean interceptBeforePreExecute(ImplicitActionName actionName, Bean bean, Bean parentBean, WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforePreExecute(actionName, bean, parentBean, webContext)) {
				return true;
			}
		}
		return false;
	}

	public void interceptAfterPreExecute(ImplicitActionName actionName,
											Bean result,
											Bean parentBean,
											WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterPreExecute(actionName, result, parentBean, webContext);
		}
	}

	public boolean interceptBeforePreRerender(String source, Bean bean, WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforePreRerender(source, bean, webContext)) {
				return true;
			}
		}
		return false;
	}

	public void interceptAfterPreRerender(String source,
											Bean result,
											WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterPreRerender(source, result, webContext);
		}
	}

	public boolean interceptBeforeServerSideAction(Document document,
													String actionName, 
													Bean bean,
													WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforeServerSideAction(document, actionName, bean, webContext)) {
				return true;
			}
		}
		return false;
	}
	
	public void interceptAfterServerSideAction(Document document, 
												String actionName, 
												ServerSideActionResult<Bean> result, 
												WebContext webContext) 
	throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterServerSideAction(document, actionName, result, webContext);
		}
	}

	public boolean interceptBeforeDownloadAction(Document document,
													String actionName,
													Bean bean,
													WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforeDownloadAction(document, actionName, bean, webContext)) {
				return true;
			}
		}
		return false;
	}
	
	public void interceptAfterDownloadAction(Document document,
												String actionName,
												Bean bean,
												Download download,
												WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterDownloadAction(document, actionName, bean, download, webContext);
		}
	}

	public boolean interceptBeforeUploadAction(Document document,
												String actionName,
												Bean bean,
												UploadedFile file,
												WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforeUploadAction(document, actionName, bean, file, webContext)) {
				return true;
			}
		}
		return false;
	}
	
	public void interceptAfterUploadAction(Document document,
											String actionName,
											Bean bean,
											UploadedFile file,
											WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterUploadAction(document, actionName, bean, file, webContext);
		}
	}
	
	public boolean interceptBeforeBizImportAction(Document document,
													String actionName,
													BizPortWorkbook bizPortable,
													UploadException problems)
	throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforeBizImportAction(document, actionName, bizPortable, problems)) {
				return true;
			}
		}
		return false;
	}
	
	public void interceptAfterBizImportAction(Document document,
												String actionName,
												BizPortWorkbook bizPortable,
												UploadException problems)
	throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterBizImportAction(document, actionName, bizPortable, problems);
		}
	}

	public boolean interceptBeforeBizExportAction(Document document, String actionName, WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : interceptors.values()) {
			if (interceptor.getInterceptor(this).beforeBizExportAction(document, actionName, webContext)) {
				return true;
			}
		}
		return false;
	}
	
	public void interceptAfterBizExportAction(Document document,
												String actionName,
												BizPortWorkbook result,
												WebContext webContext)
	throws Exception {
		for (InterceptorMetaData interceptor : reversedInterceptors) {
			interceptor.getInterceptor(this).afterBizExportAction(document, actionName, result, webContext);
		}
	}
}
