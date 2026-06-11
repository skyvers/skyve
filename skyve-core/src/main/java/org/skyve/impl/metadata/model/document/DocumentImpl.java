package org.skyve.impl.metadata.model.document;

import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.DynamicChildBean;
import org.skyve.domain.DynamicHierarchicalBean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.DynamicPersistentChildBean;
import org.skyve.domain.DynamicPersistentHierarchicalBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.behaviour.ServerSideMetaDataAction;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.ModelImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.persistence.AbstractDocumentQuery;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UUIDv7;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.metadata.controller.BizImportAction;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.metadata.view.model.comparison.ComparisonModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.map.MapModel;
import org.skyve.util.ExpressionEvaluator;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;

/**
 * Runtime implementation of the {@link Document} contract, populated from a
 * document XML descriptor during repository bootstrap.
 *
 * <p>Extends {@link ModelImpl} with document-specific metadata: persistence
 * descriptor, conditions, bizlet class reference, actions, and the complete
 * attribute hierarchy (owned attributes and inherited ones from the parent
 * document).  After loading the fully-resolved instance is placed in the
 * repository cache and shared read-only across all threads.
 *
 * <p>Threading: not thread-safe.  The instance is written during loading and
 * read-only afterwards.
 *
 * @see Document
 * @see ModelImpl
 */
public final class DocumentImpl extends ModelImpl implements Document {
    private static final long serialVersionUID = 9091172268741052691L;
    private static final Logger BIZLET_LOGGER = Category.BIZLET.logger();
    
	private static final String DOCUMENT_PREFIX = "Document ";

	private long lastModifiedMillis = Long.MAX_VALUE;
	private long lastCheckedMillis = System.currentTimeMillis();
	
	private List<UniqueConstraint> uniqueConstraints = new ArrayList<>();

	private Map<String, Reference> referencesByFieldNames = new HashMap<>();

	private Map<String, Relation> relationsByFieldNames = new HashMap<>();
	
	/**
	 * This is this document's master or parent document name. This can be <code>null</code> if no parent document exists.
	 */
	private String parentDocumentName;
	
	/**
	 * This indicates whether a database index should be created on the "parent_id" foreign key column.
	 */
	private Boolean parentDatabaseIndex;
	
	private String bizKeyMethodCode;
	// Although this is code generated into the domain class, we need it here
	// so that it can be checked by the repository implementor, as only then
	// will all the references be resolved enough to check the bindings.
	private String bizKeyExpression;
	
	private Sensitivity bizKeySensitity;

	/**
	 * A map of condition name -> Condition.
	 */
	private Map<String, Condition> conditions = new TreeMap<>();

	/**
	 * Action names defined in module privileges - Used when generating views.
	 */
	private Set<String> definedActionNames = new TreeSet<>();

	/**
	 * Indicates if the document is the target end of some ordered collection.
	 * Any collection type can be ordered.  This just indicates it.
	 */
	private boolean ordered;
	
	private String documentation;
	
	private Map<String, String> properties = new TreeMap<>();
	
	/**
	 * Returns the last metadata modification timestamp recorded for this document.
	 *
	 * @return the last modification time in milliseconds since the epoch.
	 */
	@Override
	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}

	/**
	 * Sets the last metadata modification timestamp recorded for this document.
	 *
	 * @param lastModifiedMillis the last modification time in milliseconds since the epoch.
	 */
	public void setLastModifiedMillis(long lastModifiedMillis) {
		this.lastModifiedMillis = lastModifiedMillis;
	}

	/**
	 * Returns the last repository validation timestamp recorded for this document.
	 *
	 * @return the last check time in milliseconds since the epoch.
	 */
	@Override
	public long getLastCheckedMillis() {
		return lastCheckedMillis;
	}

	/**
	 * Sets the last repository validation timestamp recorded for this document.
	 *
	 * @param lastCheckedMillis the last check time in milliseconds since the epoch.
	 */
	@Override
	public void setLastCheckedMillis(long lastCheckedMillis) {
		this.lastCheckedMillis = lastCheckedMillis;
	}

	/**
	 * Creates a new bean instance for the supplied user context.
	 *
	 * <p>Side effects: performs dependency injection, applies implicit biz identity fields,
	 * executes customer interceptors and the document bizlet {@code newInstance()} hook,
	 * and clears initial dirty-tracking values.
	 *
	 * @param user the active user context used to derive customer and ownership values.
	 * @return a fully initialised bean instance.
	 * @throws Exception if bean creation, interception, or bizlet execution fails.
	 */
	@Override
	public <T extends Bean> T newInstance(User user) throws Exception {
		Customer customer = user.getCustomer();
		T result = newInstance(customer);
		
		// Inject any dependencies
		UtilImpl.inject(result);

		// Set implicit properties
		// NB These properties need to be set before the bizlet.newInstance() is called.
		// For singletons, if we were to set these after the bizlet call, 
		// this could make the object dirty and cause it to be flushed to the datastore
		// (and uprevved resulting in more contention and optimistic locks etc)
		result.setBizCustomer(customer.getName());
		result.setBizDataGroupId(user.getDataGroupId());
		result.setBizUserId(user.getId());

		CustomerImpl internalCustomer = (CustomerImpl) customer;
		boolean vetoed = internalCustomer.interceptBeforeNewInstance(result);
		if (! vetoed) {
			// Run bizlet newInstance()
			Bizlet<T> bizlet = getBizlet(customer);
			if (bizlet != null) {
				if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Entering {}.newInstance: {}", bizlet.getClass().getName(), result);
				result = bizlet.newInstance(result);
				if (result == null) {
					throw new IllegalStateException(bizlet.getClass().getName() + ".newInstance() returned null");
				}
                if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Exiting {}.newInstance: {}", bizlet.getClass().getName(), result);
			}

			internalCustomer.interceptAfterNewInstance(result);
		}

		// clear the object's dirtiness
		result.originalValues().clear();

		return result;
	}

	/**
	 * Resolves the concrete bean class for this document and customer.
	 *
	 * <p>For dynamic documents this returns an in-memory dynamic bean type. For static
	 * documents this resolves generated domain and optional extension classes through the
	 * repository vtable, including customer overrides.
	 *
	 * @param customer the customer context used for repository vtable resolution.
	 * @return the concrete bean class.
	 * @throws ClassNotFoundException if a mapped class cannot be loaded.
	 */
	@SuppressWarnings("unchecked")
	public <T extends Bean> Class<T> getBeanClass(@Nonnull Customer customer)
	throws ClassNotFoundException {
		if (isDynamic()) {
			Persistent persistent = getPersistent();
			if (parentDocumentName != null) {
				if (parentDocumentName.equals(getName())) { // hierarchical
					return (persistent == null) ?
								(Class<T>) DynamicHierarchicalBean.class : 
								(Class<T>) DynamicPersistentHierarchicalBean.class;
				}
				// child
				return (persistent == null) ?
							(Class<T>) DynamicChildBean.class :
							(Class<T>) DynamicPersistentChildBean.class;
			}
			return (persistent == null) ? (Class<T>) DynamicBean.class : (Class<T>) DynamicPersistentBean.class;
		}
		
		Class<T> result = null;
		
		String customerName = customer.getName();
		String documentName = getName();

		ProvidedRepository repository = ProvidedRepositoryFactory.get();
		StringBuilder key = new StringBuilder(128).append(ProvidedRepository.MODULES_NAMESPACE).append(getOwningModuleName()).append('/').append(documentName);
		String packagePath = repository.vtable(customerName, key.toString());
		if (packagePath == null) {
			throw new ClassNotFoundException(key + " not found in the repository vtable");
		}
		
		// Look for a hand-crafted extension first (in module or as a customer override)
		StringBuilder className = new StringBuilder(128);
		try {
			key.append('/').append(documentName).append("Extension");
			String extensionKey = repository.vtable(customerName, key.toString());
			if (extensionKey != null) {
				result = (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(extensionKey.replace('/', '.'));
			}
			else {
				// Convert the vtable key to a package path
				packagePath = packagePath.replace('/', '.');
				int lastDotIndex = packagePath.lastIndexOf('.');
				packagePath = packagePath.substring(0, lastDotIndex + 1);
	
				if (packagePath.startsWith(ProvidedRepository.CUSTOMERS_NAME)) {
					// Look for an override first and if not found look for a domain class
					try {
						className.setLength(0);
						className.append(packagePath).append(ProvidedRepository.DOMAIN_NAME).append('.').append(documentName).append("Ext");
						result = (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(className.toString());
					}
					catch (@SuppressWarnings("unused") ClassNotFoundException e1) { // no override class
						// Look for the domain class in the customer area
						try {
							className.setLength(className.length() - 3); // remove "Ext"
							result = (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(className.toString());
						}
						catch (@SuppressWarnings("unused") ClassNotFoundException e2) { // no override or base class in customer area
							// Look for the domain class in the modules area
							className.setLength(0);
							className.append(ProvidedRepository.MODULES_NAME).append('.').append(getOwningModuleName()).append('.').append(ProvidedRepository.DOMAIN_NAME).append('.').append(documentName);
							result = (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(className.toString());
						}
					}
				}
				else {
					// Look for domain class and if abstract, look for an override
					className.setLength(0);
					className.append(packagePath).append(ProvidedRepository.DOMAIN_NAME).append('.').append(documentName);
					result = (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(className.toString());
					if (Modifier.isAbstract(result.getModifiers())) {
						className.append("Ext");
						try {
							result = (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(className.toString());
						}
						catch (@SuppressWarnings("unused") ClassNotFoundException e2) { // no extension or base class in customer area
							// stick with the domain class
						}
					}
				}
			}
		}
		catch (ClassNotFoundException e) {
			if (UtilImpl.DEV_MODE) {
				throw new ClassNotFoundException("Bean Class " + className.toString() + " not found. Is domain generation required or are there compile errors?", e);
			}
			throw e;
		}
		
		return result;
	}

	/**
	 * Instantiates a base bean without running interceptors or bizlet {@code newInstance()}.
	 *
	 * <p>Creates either a dynamic bean with default framework properties or a compiled
	 * domain bean via its default constructor, then applies dynamic attribute defaults.
	 *
	 * @param customer the customer context used for class resolution and attribute defaults.
	 * @return the newly created base bean.
	 * @throws Exception if class resolution or object construction fails.
	 */
	public <T extends Bean> T newInstance(Customer customer) throws Exception {
		T result = null;
		
		if (isDynamic()) {
			final Map<String, Object> p = new TreeMap<>();
			
			// Bean
			p.put(Bean.DOCUMENT_ID, UUIDv7.create().toString());
			p.put(Bean.BIZ_KEY, null);
			p.put(Bean.CUSTOMER_NAME, null);
			p.put(Bean.DATA_GROUP_ID, null);
			p.put(Bean.USER_ID, null);
			
			// PersistentBean
			boolean isPersistent = false;
			Persistent persistent = getPersistent();
			if (persistent != null) { // includes mapped strategy as these are generated to AbstractPersistentBean extensions
				isPersistent = true;
				p.put(PersistentBean.VERSION_NAME, null);
				p.put(PersistentBean.LOCK_NAME, null);
				p.put(PersistentBean.FLAG_COMMENT_NAME, null);
				p.put(PersistentBean.TAGGED_NAME, null);
			}
			
			boolean isHierarchical = false;
			boolean isChild = false;
			if (parentDocumentName != null) {
				String name = getName();
				// HierarchicalBean
				if (parentDocumentName.equals(name)) {
					isHierarchical = true;
					p.put(HierarchicalBean.PARENT_ID, null);
				}
				// ChildBean
				else {
				 isChild = true;
				 p.put(ChildBean.PARENT_NAME, null);
				 p.put(Bean.ORDINAL_NAME, null);
				}
			}
			
			if (isHierarchical) {
				@SuppressWarnings("unchecked")
				T t = isPersistent ?
						(T) new DynamicPersistentHierarchicalBean(getOwningModuleName(), getName(), p) :
						(T) new DynamicHierarchicalBean(getOwningModuleName(), getName(), p);
				result = t;
			}
			else if (isChild) {
				@SuppressWarnings("unchecked")
				T t = isPersistent ?
						(T) new DynamicPersistentChildBean(getOwningModuleName(), getName(), p) :
						(T) new DynamicChildBean(getOwningModuleName(), getName(), p);
				result = t;
			}
			else {
				@SuppressWarnings("unchecked")
				T t = isPersistent ?
						(T) new DynamicPersistentBean(getOwningModuleName(), getName(), p) :
						(T) new DynamicBean(getOwningModuleName(), getName(), p);
				result = t;
			}
			// default any dynamic values here
			final T t = result; // to get around final required in forEach()
			getAllAttributes(customer).forEach(a -> t.putDynamic(a.getName(), dynamicDefaultValue(a, t)));
		}
		else {
			final Class<T> beanClass = getBeanClass(customer);
			result = beanClass.getDeclaredConstructor().newInstance();
			populateDynamicAttributeDefaults(customer, result);
		}
		
		return result;
	}
	
	/**
	 * Populates dynamic default values for dynamic attributes on a compiled bean instance.
	 */
	public void populateDynamicAttributeDefaults(Customer customer, Bean bean) {
		Module m = customer.getModule(getOwningModuleName());

		getAllAttributes(customer).forEach(a -> {
			if (BindUtil.isDynamic(customer, m, this, a)) {
				bean.putDynamic(a.getName(), dynamicDefaultValue(a, bean));
			}
		});
	}
	
	/**
	 * Computes the framework default value for a dynamic attribute.
	 *
	 * @param attribute the attribute being initialised.
	 * @param bean the bean receiving the default value.
	 * @return the resolved default value, or {@code null} when no default applies.
	 */
	private static Object dynamicDefaultValue(Attribute attribute, Bean bean) {
		Object result = null;
		
		if (attribute instanceof Field field) {
			String defaultValue = field.getDefaultValue();
			if (defaultValue != null) {
				Class<?> implementingType = attribute.getImplementingType();
				if (String.class.equals(implementingType)) {
					if (BindUtil.containsSkyveExpressions(defaultValue)) {
						result = BindUtil.formatMessage(defaultValue, bean);
					}
					else {
						// NB Take care of escaped {
						result = defaultValue.replace("\\{", "{");
					}
				}
				else {
					if (BindUtil.isSkyveExpression(defaultValue)) {
						result = ExpressionEvaluator.evaluate(defaultValue, bean);
					}
					else {
						Converter<?> converter = null;
						if (attribute instanceof Enumeration enumeration) {
							converter = enumeration.getConverter();
						}

						result = BindUtil.fromSerialised(converter, implementingType, defaultValue);
					}
				}
			}
		}
		else if ((attribute instanceof Collection) || (attribute instanceof InverseMany)) {
			result = new ArrayList<>();
		}
		
		return result;
	}

	/**
	 * Returns the named unique constraint declared on this document.
	 *
	 * @param name the unique-constraint name.
	 * @return the matching constraint, or {@code null} if it is not defined.
	 */
	@Override
	public UniqueConstraint getUniqueConstraint(String name) {
		return (UniqueConstraint) getMetaData(name);
	}

	/**
	 * Registers a unique constraint on this document.
	 *
	 * <p>Side effects: stores the constraint in both the general metadata map and the
	 * ordered unique-constraint list.
	 *
	 * @param constraint the constraint metadata to register.
	 */
	public void putUniqueConstraint(UniqueConstraint constraint) {
		putMetaData(constraint.getName(), constraint);
		uniqueConstraints.add(constraint);
	}

	/**
	 * Resolves a dynamic image definition for this document.
	 *
	 * @param customer the customer context used for repository lookup.
	 * @param name the dynamic-image name.
	 * @return the resolved dynamic image, or {@code null} if none is defined.
	 */
	@Override
	public <T extends Bean> DynamicImage<T> getDynamicImage(Customer customer, String name) {
		DynamicImage<T> result = ProvidedRepositoryFactory.get().getDynamicImage(customer, this, name, true);
		if (result == null) {
			throw new MetaDataException(DOCUMENT_PREFIX + getName() + " has no dynamic image defined for " + name);
		}
		return result;
	}

	/**
	 * Returns the unique constraints declared on this document.
	 *
	 * @return an unmodifiable list of unique constraints in declaration order.
	 */
	@Override
	public List<UniqueConstraint> getUniqueConstraints() {
		return Collections.unmodifiableList(uniqueConstraints);
	}

	/**
	 * Returns the reference metadata registered under the supplied field name.
	 *
	 * @param referenceName the reference field name.
	 * @return the matching reference, or {@code null} if the field is not a reference.
	 */
	@Override
	public Reference getReferenceByName(String referenceName) {
		return referencesByFieldNames.get(referenceName);
	}

	/**
	 * Resolves the related document for the named relation, searching up the document
	 * inheritance hierarchy when necessary.
	 *
	 * @param customer the customer context used for document resolution.
	 * @param relationName the relation field name.
	 * @return the related document metadata.
	 * @throws IllegalStateException if the relation or its target document is not defined.
	 */
	@Override
	public Document getRelatedDocument(Customer customer, String relationName) {
		Relation relation = relationsByFieldNames.get(relationName);

		// Find the relation up the document extension hierarchy
		Extends currentExtends = getExtends();
		while ((relation == null) && (currentExtends != null)) {
			Module module = customer.getModule(getOwningModuleName());
			DocumentImpl baseDocument = (DocumentImpl) module.getDocument(customer, currentExtends.getDocumentName());
			relation = baseDocument.relationsByFieldNames.get(relationName);
			currentExtends = baseDocument.getExtends();
		}
		
		if (relation == null) {
			throw new IllegalStateException("Document has no related document defined for " + relationName);
		}

		String relatedDocumentName = relation.getDocumentName();
		if (relatedDocumentName == null) {
			throw new IllegalStateException("Document has no related document defined for " + relationName);
		}

		return customer.getModule(getOwningModuleName()).getDocument(customer, relatedDocumentName);
	}

	/**
	 * Returns the names of reference relations declared on this document.
	 *
	 * @return the live set view of reference field names.
	 */
	@Override
	public Set<String> getReferenceNames() {
		return referencesByFieldNames.keySet();
	}

	/**
	 * Registers a relation on this document.
	 *
	 * <p>Side effects: updates the relation map, optionally records the relation as a
	 * reference, and exposes it through the document attribute registry.
	 *
	 * @param relation the relation metadata to register.
	 */
	public void putRelation(Relation relation) {
		relationsByFieldNames.put(relation.getName(), relation);
		if (relation instanceof Reference reference) {
			referencesByFieldNames.put(reference.getName(), reference);
		}
		putAttribute(relation);
	}

	/**
	 * Returns the parent or master document name for this document.
	 *
	 * @return the parent document name, or {@code null} when this document has no parent.
	 */
	@Override
	public String getParentDocumentName() {
		return parentDocumentName;
	}

	/**
	 * Sets the parent or master document name for this document.
	 *
	 * @param parentDocumentName the parent document name, or {@code null} for a root document.
	 */
	public void setParentDocumentName(String parentDocumentName) {
		this.parentDocumentName = parentDocumentName;
	}
	
	/**
	 * Indicates whether the parent foreign key should be indexed in the database.
	 *
	 * @return {@code Boolean.TRUE} to create the index, {@code Boolean.FALSE} to suppress it,
	 *         or {@code null} to use repository defaults.
	 */
	public Boolean getParentDatabaseIndex() {
		return parentDatabaseIndex;
	}

	/**
	 * Sets whether the parent foreign key should be indexed in the database.
	 *
	 * @param parentDatabaseIndex {@code Boolean.TRUE} to create the index,
	 *        {@code Boolean.FALSE} to suppress it, or {@code null} to use repository defaults.
	 */
	public void setParentDatabaseIndex(Boolean parentDatabaseIndex) {
		this.parentDatabaseIndex = parentDatabaseIndex;
	}

	/**
	 * Returns the generated Java source used to implement the business-key method.
	 *
	 * @return the business-key method source, or {@code null} if none is defined.
	 */
	public String getBizKeyMethodCode() {
		return bizKeyMethodCode;
	}

	/**
	 * Sets the generated Java source used to implement the business-key method.
	 *
	 * @param bizKeyMethodCode the business-key method source.
	 */
	public void setBizKeyMethodCode(String bizKeyMethodCode) {
		this.bizKeyMethodCode = bizKeyMethodCode;
	}

	/**
	 * Returns the declarative business-key expression for this document.
	 *
	 * @return the business-key expression, or {@code null} if none is defined.
	 */
	@Override
	public String getBizKeyExpression() {
		return bizKeyExpression;
	}

	/**
	 * Sets the declarative business-key expression for this document.
	 *
	 * @param bizKeyExpression the expression used to derive the business key.
	 */
	public void setBizKeyExpression(String bizKeyExpression) {
		this.bizKeyExpression = bizKeyExpression;
	}
	
	/**
	 * Returns the sensitivity classification applied to this document's business key.
	 *
	 * @return the business-key sensitivity.
	 */
	@Override
	public Sensitivity getBizKeySensitity() {
		return bizKeySensitity;
	}

	/**
	 * Sets the sensitivity classification applied to this document's business key.
	 *
	 * @param bizKeySensitity the business-key sensitivity.
	 */
	public void setBizKeySensitity(Sensitivity bizKeySensitity) {
		this.bizKeySensitity = bizKeySensitity;
	}

	/**
	 * Sets the business-key value on the supplied persistent bean.
	 *
	 * <p>Side effects: evaluates the dynamic business-key expression when required,
	 * normalises blank values, substitutes {@code "Unknown"} for missing results, and
	 * truncates the final key to the database dialect's maximum supported length.
	 *
	 * @param bean the persistent bean whose business key should be updated.
	 */
	@Override
	public void setBizKey(PersistentBean bean) {
		// Get the bizKey value
		String bizKey = null;
		if (isDynamic()) {
			try {
				if (bizKeyExpression != null) {
					bizKey = BindUtil.formatMessage(bizKeyExpression, bean);
				}
			}
			catch (@SuppressWarnings("unused") Exception e) {
				// nothing to do here
			}
		}
		else {
			bizKey = bean.getBizKey();
		}
		
		// Process the value
		bizKey = UtilImpl.processStringValue(bizKey);
		if (bizKey == null) {
			bizKey = "Unknown";
		}
		else {
			int bizKeyLength = AbstractPersistence.getBizKeyLength();
			if (bizKey.length() > bizKeyLength) {
				bizKey = bizKey.substring(0, bizKeyLength);
			}
		}

		// Set the value
		bean.setBizKey(bizKey);
	}
	
	/**
	 * Indicates whether any collection targeting this document preserves row order.
	 *
	 * @return {@code true} if the document participates in an ordered collection.
	 */
	@Override
	public boolean isOrdered() {
		return ordered;
	}

	/**
	 * Sets whether any collection targeting this document preserves row order.
	 *
	 * @param ordered {@code true} if the document participates in an ordered collection.
	 */
	public void setOrdered(boolean ordered) {
		this.ordered = ordered;
	}

	/**
	 * Returns the condition metadata declared on this document.
	 *
	 * @return the live condition map keyed by condition name.
	 */
	public Map<String, Condition> getConditions() {
		return conditions;
	}

	/**
	 * Resolves the parent document metadata for this document.
	 *
	 * @param customer the customer context used for repository resolution, or {@code null}
	 *        to resolve against the base repository only.
	 * @return the parent document metadata, or {@code null} when this document has no parent.
	 */
	@Override
	public Document getParentDocument(Customer customer) {
		Document result = null;

		if (parentDocumentName != null) {
			String owningModuleName = getOwningModuleName();
			if (customer == null) {
				Module owningModule = ProvidedRepositoryFactory.get().getModule(null, owningModuleName);
				if (owningModule != null) {
					result = owningModule.getDocument(null, parentDocumentName);
				}
				else {
					throw new MetaDataException("Owning module " + owningModuleName + " not found in repository");
				}
			}
			else {
				result = customer.getModule(owningModuleName).getDocument(customer, parentDocumentName);
			}
		}

		return result;
	}

	/**
	 * Resolves the bizlet for this document and attaches any metadata-defined bizlet.
	 *
	 * @param customer the customer context used for repository lookup.
	 * @return the resolved bizlet, a metadata-only adapter, or {@code null} if no bizlet exists.
	 */
	@Override
	public <T extends Bean> Bizlet<T> getBizlet(Customer customer) {
		ProvidedRepository repository = ProvidedRepositoryFactory.get();
		Bizlet<T> result = repository.getBizlet(customer, this, true);
		BizletMetaData metaDataBizlet = repository.getMetaDataBizlet(customer, this);
		if (result != null) {
			result.setMetaDataBizlet(metaDataBizlet);
		}
		else if (metaDataBizlet != null) {
			result = new Bizlet<>();
			result.setMetaDataBizlet(metaDataBizlet);
		}
		return result;
	}

	/**
	 * Resolves the named comparison model for this document.
	 *
	 * @param customer the customer context used for repository lookup.
	 * @param modelName the comparison-model name.
	 * @param runtime whether runtime overrides should be consulted.
	 * @return the resolved comparison model, or {@code null} if none is defined.
	 */
	@Override
	public <T extends Bean, C extends Bean> ComparisonModel<T, C> getComparisonModel(Customer customer, String modelName, boolean runtime) {
		ComparisonModel<T, C> result = ProvidedRepositoryFactory.get().getComparisonModel(customer, this, modelName, runtime);
		if (result == null) {
			throw new MetaDataException(DOCUMENT_PREFIX + getName() + " has no comparison model defined for " + modelName);
		}
		return result;
	}

	/**
	 * Resolves the named map model for this document.
	 *
	 * @param customer the customer context used for repository lookup.
	 * @param modelName the map-model name.
	 * @param runtime whether runtime overrides should be consulted.
	 * @return the resolved map model, or {@code null} if none is defined.
	 */
	@Override
	public <T extends Bean> MapModel<T> getMapModel(Customer customer, String modelName, boolean runtime) {
		MapModel<T> result = ProvidedRepositoryFactory.get().getMapModel(customer, this, modelName, runtime);
		if (result == null) {
			throw new MetaDataException(DOCUMENT_PREFIX + getName() + " has no map model defined for " + modelName);
		}
		return result;
	}

	/**
	 * Resolves the named chart model for this document.
	 *
	 * @param customer the customer context used for repository lookup.
	 * @param modelName the chart-model name.
	 * @param runtime whether runtime overrides should be consulted.
	 * @return the resolved chart model, or {@code null} if none is defined.
	 */
	@Override
	public <T extends Bean> ChartModel<T> getChartModel(Customer customer, String modelName, boolean runtime) {
		ChartModel<T> result = ProvidedRepositoryFactory.get().getChartModel(customer, this, modelName, runtime);
		if (result == null) {
			throw new MetaDataException(DOCUMENT_PREFIX + getName() + " has no chart model defined for " + modelName);
		}
		return result;
	}

	/**
	 * Resolves the named list model for this document.
	 *
	 * @param customer the customer context used for repository lookup.
	 * @param modelName the list-model name.
	 * @param runtime whether runtime overrides should be consulted.
	 * @return the resolved list model, or {@code null} if none is defined.
	 */
	@Override
	public <T extends Bean> ListModel<T> getListModel(Customer customer, String modelName, boolean runtime) {
		ListModel<T> result = ProvidedRepositoryFactory.get().getListModel(customer, this, modelName, runtime);
		if (result == null) {
			throw new MetaDataException(DOCUMENT_PREFIX + getName() + " has no list model defined for " + modelName);
		}
		return result;
	}

	/**
	 * Resolves the named server-side action for this document.
	 *
	 * <p>If the action is declared purely in metadata, this returns a metadata-backed
	 * {@link ServerSideMetaDataAction} wrapper instead of a compiled action class.
	 *
	 * @param customer the customer context used for repository lookup.
	 * @param className the action class or metadata action name.
	 * @param runtime whether runtime overrides should be consulted.
	 * @return the resolved server-side action, or {@code null} if none is defined.
	 */
	@Override
	public ServerSideAction<Bean> getServerSideAction(Customer customer, String className, boolean runtime) {
		ProvidedRepository repository = ProvidedRepositoryFactory.get();
		ActionMetaData metaDataAction = repository.getMetaDataAction(customer, this, className);
		if (metaDataAction != null) {
			return new ServerSideMetaDataAction(metaDataAction);
		}
		
		ServerSideAction<Bean> result = repository.getServerSideAction(customer, this, className, runtime);
		if (result == null) {
			throw new MetaDataException(DOCUMENT_PREFIX + getName() + " has no server-side action defined for " + className);
		}
		return result;
	}

	/**
	 * Resolves the named BizPort export action for this document.
	 *
	 * @param customer the customer context used for repository lookup.
	 * @param className the export action class name.
	 * @param runtime whether runtime overrides should be consulted.
	 * @return the resolved export action, or {@code null} if none is defined.
	 */
	@Override
	public BizExportAction getBizExportAction(Customer customer, String className, boolean runtime) {
		BizExportAction result = ProvidedRepositoryFactory.get().getBizExportAction(customer, this, className, runtime);
		if (result == null) {
			throw new MetaDataException(DOCUMENT_PREFIX + getName() + " has no BizPort export action defined for " + className);
		}
		return result;
	}

	/**
	 * Resolves the named BizPort import action for this document.
	 *
	 * @param customer the customer context used for repository lookup.
	 * @param className the import action class name.
	 * @param runtime whether runtime overrides should be consulted.
	 * @return the resolved import action, or {@code null} if none is defined.
	 */
	@Override
	public BizImportAction getBizImportAction(Customer customer, String className, boolean runtime) {
		BizImportAction result = ProvidedRepositoryFactory.get().getBizImportAction(customer, this, className, runtime);
		if (result == null) {
			throw new MetaDataException(DOCUMENT_PREFIX + getName() + " has no BizPort import action defined for " + className);
		}
		return result;
	}

	/**
	 * Resolves the named download action for this document.
	 *
	 * @param customer the customer context used for repository lookup.
	 * @param className the download action class name.
	 * @param runtime whether runtime overrides should be consulted.
	 * @return the resolved download action, or {@code null} if none is defined.
	 */
	@Override
	public DownloadAction<Bean> getDownloadAction(Customer customer, String className, boolean runtime) {
		DownloadAction<Bean> result = ProvidedRepositoryFactory.get().getDownloadAction(customer, this, className, runtime);
		if (result == null) {
			throw new MetaDataException(DOCUMENT_PREFIX + getName() + " has no download action defined for " + className);
		}
		return result;
	}

	/**
	 * Resolves the named upload action for this document.
	 *
	 * @param customer the customer context used for repository lookup.
	 * @param className the upload action class name.
	 * @param runtime whether runtime overrides should be consulted.
	 * @return the resolved upload action, or {@code null} if none is defined.
	 */
	@Override
	public UploadAction<Bean> getUploadAction(Customer customer, String className, boolean runtime) {
		UploadAction<Bean> result = ProvidedRepositoryFactory.get().getUploadAction(customer, this, className, runtime);
		if (result == null) {
			throw new MetaDataException(DOCUMENT_PREFIX + getName() + " has no upload action defined for " + className);
		}
		return result;
	}

	/**
	 * Resolves domain values for the supplied attribute.
	 *
	 * <p>Delegates to constant, variant, or dynamic bizlet hooks as required and falls
	 * back to reference-query based resolution when business logic does not supply values.
	 *
	 * @param customer the internal customer context used for interception and caching.
	 * @param domainType the domain strategy declared by the attribute.
	 * @param attribute the attribute whose domain values are required.
	 * @param owningBean the current owning bean for dynamic-domain resolution.
	 * @param runtime whether runtime overrides should be consulted.
	 * @param <T> the owning bean type.
	 * @return the resolved domain values, never {@code null}.
	 * @throws MetaDataException if bizlet or query-based resolution fails.
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	public <T extends Bean> List<DomainValue> getDomainValues(CustomerImpl customer,
																DomainType domainType,
																Attribute attribute,
																T owningBean,
																boolean runtime) {
		List<DomainValue> result = null;
		
		if (domainType != null) {
			ProvidedRepository repository = ProvidedRepositoryFactory.get();
			// Note - Can't call this.getBizlet() here as it has no runtime parameter
			Bizlet<T> bizlet = repository.getBizlet(customer, this, runtime);
			BizletMetaData metaDataBizlet = repository.getMetaDataBizlet(customer, this);
			if (bizlet != null) {
				bizlet.setMetaDataBizlet(metaDataBizlet);
			}
			else if (metaDataBizlet != null) {
				bizlet = new Bizlet<>();
				bizlet.setMetaDataBizlet(metaDataBizlet);
			}

			try {
				if (DomainType.constant.equals(domainType)) {
					result = customer.getConstantDomainValues(bizlet,
																getOwningModuleName(),
																getName(),
																attribute);
				}
				else {
					String attributeName = attribute.getName();
					if (DomainType.variant.equals(domainType)) {
						boolean vetoed = customer.interceptBeforeGetVariantDomainValues(attributeName);
						if (! vetoed) {
							if (bizlet != null) {
                                if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Entering {}.getVariantDomainValues: {}", bizlet.getClass().getName(), attributeName);
								result = bizlet.getVariantDomainValues(attributeName);
								if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Exiting {}.getVariantDomainValues: {}", bizlet.getClass().getName(), result);
							}
							customer.interceptAfterGetVariantDomainValues(attributeName, result);
						}
					}
					else if (DomainType.dynamic.equals(domainType)) {
						boolean vetoed = customer.interceptBeforeGetDynamicDomainValues(attributeName, owningBean);
						if (! vetoed) {
							if (bizlet != null) {
								if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Entering {}.getDynamicDomainValues: {}, {}", bizlet.getClass().getName(), attributeName, owningBean);
								result = bizlet.getDynamicDomainValues(attributeName, owningBean);
								if (UtilImpl.BIZLET_TRACE) BIZLET_LOGGER.info("Exiting {}.getDynamicDomainValues: {}", bizlet.getClass().getName(), result);
							}
							customer.interceptAfterGetDynamicDomainValues(attributeName, owningBean, result);
						}
					}
					if (result == null) {
						result = useQuery(customer, attribute);
					}
				}
			}
			catch (Exception e) {
				throw new MetaDataException(e);
			}
		}
		
		if (result == null) {
			result = new ArrayList<>();
		}
		
		return result;
	}

	/**
	 * Resolves domain values from a referenced document query.
	 *
	 * @param customer the customer context used for repository and query resolution.
	 * @param attribute the reference attribute driving the lookup.
	 * @return the resolved query-backed domain values, an empty list for transient targets,
	 *         or {@code null} when the attribute is not query-backed.
	 */
	private List<DomainValue> useQuery(Customer customer, Attribute attribute) {
		List<DomainValue> result = null;
		
		if (attribute instanceof Reference reference) {
			Document referencedDocument = getRelatedDocument(customer, attribute.getName());
			// Query only if persistent
			if (referencedDocument.isPersistable()) { // persistent referenced document
				AbstractDocumentQuery referenceQuery = null;
				String queryName = reference.getQueryName();
				if (queryName != null) {
					Module module = customer.getModule(getOwningModuleName());
					MetaDataQueryDefinition query = module.getNullSafeMetaDataQuery(queryName);
					referenceQuery = (AbstractDocumentQuery) query.constructDocumentQuery(null, null);
					referenceQuery.clearProjections();
					referenceQuery.clearOrderings();
				}
				else {
					referenceQuery = (AbstractDocumentQuery) AbstractPersistence.get().newDocumentQuery(referencedDocument);
				}
				
				referenceQuery.addBoundProjection(Bean.DOCUMENT_ID);
				referenceQuery.addBoundProjection(Bean.BIZ_KEY);
				referenceQuery.addBoundOrdering(Bean.BIZ_KEY);
	
				List<Bean> beans = referenceQuery.projectedResults();
				result = new ArrayList<>(beans.size());
				for (Bean bean : beans) {
					String bizKey = (String) BindUtil.get(bean, Bean.BIZ_KEY);
					if (bizKey == null) {
						bizKey = "<Unknown>";
					}
					result.add(new DomainValue(bean.getBizId(), bizKey));
				}
			}
			else { // transient referenced document
				result = Collections.emptyList();
			}
		}

		return result;
	}
	
	/**
	 * Returns the action names declared for privilege-driven view generation.
	 *
	 * @return the live set of defined action names.
	 */
	@Override
	public Set<String> getDefinedActionNames() {
		return definedActionNames;
	}

	/**
	 * Returns the names of conditions declared on this document.
	 *
	 * @return the live set view of condition names.
	 */
	@Override
	public Set<String> getConditionNames() {
		return conditions.keySet();
	}

	/**
	 * Returns the named condition declared on this document.
	 *
	 * @param conditionName the condition name.
	 * @return the matching condition, or {@code null} if it is not defined.
	 */
	@Override
	public Condition getCondition(String conditionName) {
		return conditions.get(conditionName);
	}

	/**
	 * Resolves a named view for this document and UX/UI combination.
	 *
	 * <p>If no explicit create view exists, this falls back to the edit view.
	 *
	 * @param uxui the UX/UI profile.
	 * @param customer the customer context used for repository lookup.
	 * @param name the view name.
	 * @return the resolved view, or {@code null} if no suitable view exists.
	 */
	@Override
	public View getView(String uxui, Customer customer, String name) {
		ProvidedRepository repository = ProvidedRepositoryFactory.get();
		View view = repository.getView(uxui, customer, this, name);
		// if we want a create view and there isn't one, get the edit view instead
		if ((view == null) && (ViewType.create.toString().equals(name))) {
			view = repository.getView(uxui, customer, this, ViewType.edit.toString());
		}
		
		if (view == null) {
			throw new MetaDataException(DOCUMENT_PREFIX + getName() + " has no view defined for " + name);
		}

		return view;
	}

	/**
	 * Returns the free-form documentation text associated with this document.
	 *
	 * @return the document documentation, or {@code null} if none is defined.
	 */
	@Override
	public String getDocumentation() {
		return documentation;
	}

	/**
	 * Sets the free-form documentation text associated with this document.
	 *
	 * <p>Side effects: normalises blank values via {@link UtilImpl#processStringValue(String)}.
	 *
	 * @param documentation the documentation text.
	 */
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}
	
	/**
	 * Returns the arbitrary property map declared for this document.
	 *
	 * @return the live property map keyed by property name.
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
	
	private static Text bizKeyField = new Text();
	static {
		bizKeyField.setAttributeType(AttributeType.text);
		bizKeyField.setDisplayName("Business Key");
		bizKeyField.setName(Bean.BIZ_KEY);
		bizKeyField.setPersistent(false);
		bizKeyField.setRequired(false);
		bizKeyField.setDescription(null);
		bizKeyField.setDomainType(null);
		bizKeyField.setLength(1024);
	}

	/**
	 * Returns the synthetic business-key attribute exposed by the framework.
	 *
	 * @return the shared business-key attribute metadata.
	 */
	public static Text getBizKeyAttribute() {
		return bizKeyField;
	}
	
	private static org.skyve.impl.metadata.model.document.field.Integer bizOrdinalField = new org.skyve.impl.metadata.model.document.field.Integer();
	static {
		bizOrdinalField.setAttributeType(AttributeType.integer);
		bizOrdinalField.setDisplayName("Order");
		bizOrdinalField.setName(Bean.ORDINAL_NAME);
		bizOrdinalField.setPersistent(true);
		bizOrdinalField.setRequired(false);
		bizOrdinalField.setDescription(null);
		bizOrdinalField.setDomainType(null);
	}

	/**
	 * Returns the synthetic ordinal attribute used for ordered child collections.
	 *
	 * @return the shared ordinal attribute metadata.
	 */
	public static org.skyve.impl.metadata.model.document.field.Integer getBizOrdinalAttribute() {
		return bizOrdinalField;
	}
}
