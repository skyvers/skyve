package org.skyve.impl.metadata.model.document;

import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.UUID;
import java.util.logging.Level;

import javax.annotation.Nonnull;

import org.apache.deltaspike.core.api.provider.BeanProvider;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.DynamicChildBean;
import org.skyve.domain.DynamicHierarchicalBean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.DynamicPersistentChildBean;
import org.skyve.domain.DynamicPersistentHierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.enumeration.DynamicEnumerationConverter;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.ModelImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractDocumentQuery;
import org.skyve.impl.persistence.AbstractPersistence;
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

public final class DocumentImpl extends ModelImpl implements Document {
	private static final long serialVersionUID = 9091172268741052691L;

	private long lastModifiedMillis = Long.MAX_VALUE;
	
	private List<UniqueConstraint> uniqueConstraints = new ArrayList<>();

	/**
	 * This is a map of fieldName -> document's child or detail document names. This can be empty if no detail document exists.
	 */
	private Map<String, Reference> referencesByDocumentNames = new HashMap<>();

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
	
	private transient ProvidedRepository repository;
	
	public DocumentImpl(ProvidedRepository repository) {
		this.repository = repository;
	}
	
	// Required for Serialization
	// NB This class should never be serialized.
	public DocumentImpl() {
		repository = ProvidedRepositoryFactory.get();
	}

	@Override
	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}

	public void setLastModifiedMillis(long lastModifiedMillis) {
		this.lastModifiedMillis = lastModifiedMillis;
	}

	@Override
	public <T extends Bean> T newInstance(User user) throws Exception {
		Customer customer = user.getCustomer();
		T result = newInstance(customer);
		
		// Inject any dependencies
		result = BeanProvider.injectFields(result);
		
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
				if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "newInstance", "Entering " + bizlet.getClass().getName() + ".newInstance: " + result);
				result = bizlet.newInstance(result);
				if (result == null) {
					throw new IllegalStateException(bizlet.getClass().getName() + ".newInstance() returned null");
				}
				if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "newInstance", "Exiting " + bizlet.getClass().getName() + ".newInstance: " + result);
			}

			internalCustomer.interceptAfterNewInstance(result);
		}

		// clear the object's dirtiness
		result.originalValues().clear();

		return result;
	}

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

		StringBuilder key = new StringBuilder(128).append(ProvidedRepository.MODULES_NAMESPACE).append(getOwningModuleName()).append('/').append(documentName);
		String packagePath = repository.vtable(customerName, key.toString());
		if (packagePath == null) {
			throw new ClassNotFoundException(key + " not found in the repository vtable");
		}
		
		// Look for a hand-crafted extension first (in module or as a customer override)
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

			StringBuilder className = new StringBuilder(128);

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
		
		return result;
	}

	/**
	 * Instantiates a static compiled bean by default constructor or a DynamicBean.
	 * This is not the normal bean newInstance() static factory method as it does not call the bizlet or interceptor etc.
	 * This is akin the the vanilla Java default constructor (but also caters for dynamic documents)
	 * @param <T>	The type of bean to produce.
	 * @param customer	The customer.
	 * @return	The bean.
	 * @throws Exception
	 */
	public <T extends Bean> T newInstance(Customer customer) throws Exception {
		T result = null;
		
		if (isDynamic()) {
			final Map<String, Object> p = new TreeMap<>();
			
			// Bean
			p.put(Bean.DOCUMENT_ID, UUID.randomUUID().toString());
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
	
	public void populateDynamicAttributeDefaults(Customer customer, Bean bean) {
		Module m = customer.getModule(getOwningModuleName());

		getAllAttributes(customer).forEach(a -> {
			if (BindUtil.isDynamic(customer, m, this, a)) {
				bean.putDynamic(a.getName(), dynamicDefaultValue(a, bean));
			}
		});
	}
	
	private static Object dynamicDefaultValue(Attribute attribute, Bean bean) {
		Object result = null;
		
		if (attribute instanceof Field) {
			Field field = (Field) attribute;
			String defaultValue = field.getDefaultValue();
			if (defaultValue != null) {
				Class<?> implementingType = attribute.getAttributeType().getImplementingType();
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
						Class<?> type = attribute.getAttributeType().getImplementingType();
						Converter<?> converter = null;
						
						// Cater where a dynamic enum references a generated one, otherwise it stays a string
						if (attribute instanceof Enumeration) {
							Enumeration enumeration = (Enumeration) attribute;
							enumeration = enumeration.getTarget();
							if (enumeration.isDynamic()) {
								type = String.class;
								converter = new DynamicEnumerationConverter(enumeration);
							}
							else {
								type = enumeration.getEnum();
							}
						}

						result = BindUtil.fromSerialised(converter, type, defaultValue);
					}
				}
			}
		}
		else if ((attribute instanceof Collection) || (attribute instanceof InverseMany)) {
			result = new ArrayList<>();
		}
		
		return result;
	}
	
	@Override
	public UniqueConstraint getUniqueConstraint(String name) {
		return (UniqueConstraint) getMetaData(name);
	}

	public void putUniqueConstraint(UniqueConstraint constraint) {
		putMetaData(constraint.getName(), constraint);
		uniqueConstraints.add(constraint);
	}

	@Override
	public <T extends Bean> DynamicImage<T> getDynamicImage(Customer customer, String name) {
		return repository.getDynamicImage(customer, this, name, true);
	}

	@Override
	public List<UniqueConstraint> getUniqueConstraints() {
		return Collections.unmodifiableList(uniqueConstraints);
	}
	
	@Override
	public List<UniqueConstraint> getAllUniqueConstraints(Customer customer) {
		List<UniqueConstraint> result = new ArrayList<>(uniqueConstraints);
		Extends currentExtends = getExtends();
		if (currentExtends != null) {
			while (currentExtends != null) {
				Module module = customer.getModule(getOwningModuleName());
				Document baseDocument = module.getDocument(customer, currentExtends.getDocumentName());
				result.addAll(baseDocument.getUniqueConstraints());
				currentExtends = baseDocument.getExtends();
			}
		}
		
		return Collections.unmodifiableList(result);
	}

	@Override
	public Reference getReferenceByName(String referenceName) {
		return referencesByFieldNames.get(referenceName);
	}

	public Reference getReferenceByDocumentName(String detailDocumentName) {
		return referencesByDocumentNames.get(detailDocumentName);
	}

	@Override
	public org.skyve.metadata.model.document.Document getRelatedDocument(Customer customer, String relationName) {
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

	@Override
	public Set<String> getReferencedDocumentNames() {
		return referencesByDocumentNames.keySet();
	}

	@Override
	public Set<String> getReferenceNames() {
		return referencesByFieldNames.keySet();
	}

	@Override
	public Set<org.skyve.metadata.model.document.Document> getReferencedDocuments(Customer customer) {
		HashSet<org.skyve.metadata.model.document.Document> result = new HashSet<>();

		for (String detailDocumentName : getReferencedDocumentNames()) {
			result.add(customer.getModule(getOwningModuleName()).getDocument(customer, detailDocumentName));
		}

		return result;
	}

	public void putRelation(Relation relation) {
		relationsByFieldNames.put(relation.getName(), relation);
		if (relation instanceof Reference) {
			Reference reference = (Reference) relation;
			referencesByDocumentNames.put(reference.getDocumentName(), reference);
			referencesByFieldNames.put(reference.getName(), reference);
		}
		putAttribute(relation);
	}

	@Override
	public String getParentDocumentName() {
		return parentDocumentName;
	}

	public void setParentDocumentName(String parentDocumentName) {
		this.parentDocumentName = parentDocumentName;
	}
	
	public Boolean getParentDatabaseIndex() {
		return parentDatabaseIndex;
	}

	public void setParentDatabaseIndex(Boolean parentDatabaseIndex) {
		this.parentDatabaseIndex = parentDatabaseIndex;
	}

	public String getBizKeyMethodCode() {
		return bizKeyMethodCode;
	}

	public void setBizKeyMethodCode(String bizKeyMethodCode) {
		this.bizKeyMethodCode = bizKeyMethodCode;
	}

	@Override
	public String getBizKeyExpression() {
		return bizKeyExpression;
	}

	public void setBizKeyExpression(String bizKeyExpression) {
		this.bizKeyExpression = bizKeyExpression;
	}

	/**
	 * Set the bizKey value on the given persistent bean.
	 * Note that Skyve may truncate the bizKey to the particular max data store length base don the Skyve Database Dialect.
	 */
	@Override
	public void setBizKey(PersistentBean bean) {
		// Get the bizKey value
		String bizKey = null;
		if (isDynamic()) {
			try {
				bizKey = BindUtil.formatMessage(getBizKeyExpression(), bean);
			}
			catch (@SuppressWarnings("unused") Exception e) {
				bizKey = null;
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
	
	@Override
	public boolean isOrdered() {
		return ordered;
	}

	public void setOrdered(boolean ordered) {
		this.ordered = ordered;
	}

	public Map<String, Condition> getConditions() {
		return conditions;
	}
	
	@Override
	public org.skyve.metadata.model.document.Document getParentDocument(Customer customer) {
		org.skyve.metadata.model.document.Document result = null;

		if (parentDocumentName != null) {
			if (customer == null) {
				result = repository.getModule(null, getOwningModuleName()).getDocument(null, parentDocumentName);
			}
			else {
				result = customer.getModule(getOwningModuleName()).getDocument(customer, parentDocumentName);
			}
		}

		return result;
	}

	@Override
	public <T extends Bean> Bizlet<T> getBizlet(Customer customer) {
		return repository.getBizlet(customer, this, true);
	}

	@Override
	public <T extends Bean, C extends Bean> ComparisonModel<T, C> getComparisonModel(Customer customer, String modelName, boolean runtime) {
		return repository.getComparisonModel(customer, this, modelName, runtime);
	}
	
	@Override
	public <T extends Bean> MapModel<T> getMapModel(Customer customer, String modelName, boolean runtime) {
		return repository.getMapModel(customer, this, modelName, runtime);
	}

	@Override
	public <T extends Bean> ChartModel<T> getChartModel(Customer customer, String modelName, boolean runtime) {
		return repository.getChartModel(customer, this, modelName, runtime);
	}

	@Override
	public <T extends Bean> ListModel<T> getListModel(Customer customer, String modelName, boolean runtime) {
		return repository.getListModel(customer, this, modelName, runtime);
	}
	
	@Override
	public ServerSideAction<Bean> getServerSideAction(Customer customer, String className, boolean runtime) {
		return repository.getServerSideAction(customer, this, className, runtime);
	}

	@Override
	public BizExportAction getBizExportAction(Customer customer, String className, boolean runtime) {
		return repository.getBizExportAction(customer, this, className, runtime);
	}

	@Override
	public BizImportAction getBizImportAction(Customer customer, String className, boolean runtime) {
		return repository.getBizImportAction(customer, this, className, runtime);
	}

	@Override
	public DownloadAction<Bean> getDownloadAction(Customer customer, String className, boolean runtime) {
		return repository.getDownloadAction(customer, this, className, runtime);
	}

	@Override
	public UploadAction<Bean> getUploadAction(Customer customer, String className, boolean runtime) {
		return repository.getUploadAction(customer, this, className, runtime);
	}

	
	public <T extends Bean> List<DomainValue> getDomainValues(CustomerImpl customer,
																DomainType domainType,
																Attribute attribute,
																T owningBean,
																boolean runtime) {
		List<DomainValue> result = null;
		
		if (domainType != null) {
			Bizlet<T> bizlet = repository.getBizlet(customer, this, runtime);
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
								if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "getVariantDomainValues", "Entering " + bizlet.getClass().getName() + ".getVariantDomainValues: " + attributeName);
								result = bizlet.getVariantDomainValues(attributeName);
								if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "getVariantDomainValues", "Exiting " + bizlet.getClass().getName() + ".getVariantDomainValues: " + result);
							}
							customer.interceptAfterGetVariantDomainValues(attributeName, result);
						}
					}
					else if (DomainType.dynamic.equals(domainType)) {
						boolean vetoed = customer.interceptBeforeGetDynamicDomainValues(attributeName, owningBean);
						if (! vetoed) {
							if (bizlet != null) {
								if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "getDynamicDomainValues", "Entering " + bizlet.getClass().getName() + ".getDynamicDomainValues: " + attributeName + ", " + owningBean);
								result = bizlet.getDynamicDomainValues(attributeName, owningBean);
								if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "getDynamicDomainValues", "Exiting " + bizlet.getClass().getName() + ".getDynamicDomainValues: " + result);
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

	private List<DomainValue> useQuery(Customer customer, Attribute attribute)
	throws Exception {
		List<DomainValue> result = null;
		
		if (attribute instanceof Reference) {
			Reference reference = (Reference) attribute;
			org.skyve.metadata.model.document.Document referencedDocument = getRelatedDocument(customer, attribute.getName());
			// Query only if persistent
			if (referencedDocument.isPersistable()) { // persistent referenced document
				AbstractDocumentQuery referenceQuery = null;
				String queryName = reference.getQueryName();
				if (queryName != null) {
					Module module = customer.getModule(getOwningModuleName());
					MetaDataQueryDefinition query = module.getMetaDataQuery(queryName);
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
					result.add(new DomainValue(bean.getBizId(), (String) BindUtil.get(bean, Bean.BIZ_KEY)));
				}
			}
			else { // transient referenced document
				result = Collections.EMPTY_LIST;
			}
		}

		return result;
	}
	
	@Override
	public Set<String> getDefinedActionNames() {
		return definedActionNames;
	}

	@Override
	public Set<String> getConditionNames() {
		return conditions.keySet();
	}
	
	@Override
	public Condition getCondition(String conditionName) {
		return conditions.get(conditionName);
	}

	@Override
	public View getView(String uxui, Customer customer, String name) {
		View view = repository.getView(uxui, customer, this, name);
		// if we want a create view and there isn't one, get the edit view instead
		if ((view == null) && (ViewType.create.toString().equals(name))) {
			view = repository.getView(uxui, customer, this, ViewType.edit.toString());
		}

		return view;
	}

	@Override
	public String getDocumentation() {
		return documentation;
	}

	public void setDocumentation(String documentation) {
		this.documentation = documentation;
	}
	
	private static Text bizKeyField;
	public static Text getBizKeyAttribute() {
		if (bizKeyField == null) {
			bizKeyField = new Text();
			
			bizKeyField.setAttributeType(AttributeType.text);
			bizKeyField.setDisplayName("Business Key");
			bizKeyField.setName(Bean.BIZ_KEY);
			bizKeyField.setPersistent(false);
			bizKeyField.setRequired(false);
			bizKeyField.setDescription(null);
			bizKeyField.setDomainType(null);
			bizKeyField.setLength(1024);
		}
		
		return bizKeyField;
	}
	
	private static org.skyve.impl.metadata.model.document.field.Integer bizOrdinalField;
	public static org.skyve.impl.metadata.model.document.field.Integer getBizOrdinalAttribute() {
		if (bizOrdinalField == null) {
			bizOrdinalField = new org.skyve.impl.metadata.model.document.field.Integer();
			
			bizOrdinalField.setAttributeType(AttributeType.integer);
			bizOrdinalField.setDisplayName("Order");
			bizOrdinalField.setName(Bean.ORDINAL_NAME);
			bizOrdinalField.setPersistent(true);
			bizOrdinalField.setRequired(false);
			bizOrdinalField.setDescription(null);
			bizOrdinalField.setDomainType(null);
		}
		
		return bizOrdinalField;
	}
}
