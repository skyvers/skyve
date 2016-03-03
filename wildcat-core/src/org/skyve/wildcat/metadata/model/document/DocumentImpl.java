package org.skyve.wildcat.metadata.model.document;

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
import java.util.logging.Level;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.metadata.customer.CustomerImpl;
import org.skyve.wildcat.metadata.flow.Flow;
import org.skyve.wildcat.metadata.model.ModelImpl;
import org.skyve.wildcat.metadata.model.document.field.Text;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.persistence.AbstractDocumentQuery;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.UtilImpl;

public final class DocumentImpl extends ModelImpl implements Document {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 9091172268741052691L;

	private List<UniqueConstraint> uniqueConstraints = new ArrayList<>();

	private Flow flow;

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
	
	@Override
	public <T extends Bean> T newInstance(User user) throws Exception {
		Customer customer = user.getCustomer();
		Class<T> beanClass = getBeanClass(customer);
		T result = beanClass.newInstance();
		
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

		// Set implicit properties
		result.setBizCustomer(customer.getName());
		result.setBizDataGroupId(user.getDataGroupId());
		result.setBizUserId(user.getId());

		return result;
	}

	@SuppressWarnings("unchecked")
	public <T extends Bean> Class<T> getBeanClass(Customer customer)
	throws ClassNotFoundException {
		Class<T> result = null;
		
		AbstractRepository repository = AbstractRepository.get();

		String documentName = getName();
		String packagePath = ((CustomerImpl) customer).getVTable().get(getOwningModuleName() + '.' + documentName);
		packagePath = packagePath.replace('/', '.');
		int lastDotIndex = packagePath.lastIndexOf('.');
		packagePath = packagePath.substring(0, lastDotIndex + 1);

		StringBuilder className = new StringBuilder(128);
		
		// Look for a hand-crafted extension first
		try {
			className.append(packagePath).append(documentName).append('.').append(documentName).append("Extension");
			result = (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(className.toString());
		}
		catch (ClassNotFoundException e) {
			if (packagePath.startsWith(repository.CUSTOMERS_NAME)) {
				// Look for an extension first and if not found look for a base class
				try {
					className.setLength(0);
					className.append(packagePath).append(repository.DOMAIN_NAME).append('.').append(documentName).append("Ext");
					result = (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(className.toString());
				}
				catch (ClassNotFoundException e1) { // no extension class
					// Look for the base class in the customer area
					try {
						className.setLength(className.length() - 3); // remove "Ext"
						result = (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(className.toString());
					}
					catch (ClassNotFoundException e2) { // no extension or base class in customer area
						// Look for the base class in the modules area
						className.setLength(0);
						className.append(repository.MODULES_NAME).append('.').append(getOwningModuleName()).append('.').append(repository.DOMAIN_NAME).append('.').append(documentName);
						result = (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(className.toString());
					}
				}
			}
			else {
				// Look for base class and if abstract, look for an extension
				className.setLength(0);
				className.append(packagePath).append(repository.DOMAIN_NAME).append('.').append(documentName);
				result = (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(className.toString());
				if (Modifier.isAbstract(result.getModifiers())) {
					className.append("Ext");
					result = (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(className.toString());
				}
			}
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
	public <T extends Bean> DynamicImage<T> getDynamicImage(Customer customer, String name) throws MetaDataException {
		return AbstractRepository.get().getDynamicImage(customer, this, name);
	}

	@Override
	public List<UniqueConstraint> getUniqueConstraints() {
		return Collections.unmodifiableList(uniqueConstraints);
	}
	
	@Override
	public List<UniqueConstraint> getAllUniqueConstraints() throws MetaDataException {
		List<UniqueConstraint> result = new ArrayList<>(uniqueConstraints);
		Extends currentExtends = getExtends();
		if (currentExtends != null) {
			Customer customer = CORE.getUser().getCustomer();
			while (currentExtends != null) {
				Module module = customer.getModule(getOwningModuleName());
				Document baseDocument = module.getDocument(customer, currentExtends.getDocumentName());
				result.addAll(baseDocument.getUniqueConstraints());
				currentExtends = baseDocument.getExtends();
			}
		}
		
		return Collections.unmodifiableList(result);
	}

	public Flow getFlow() {
		return flow;
	}

	public void setFlow(Flow flow) {
		this.flow = flow;
	}

	@Override
	public Reference getReferenceByName(String referenceName) {
		return referencesByFieldNames.get(referenceName);
	}

	public Reference getReferenceByDocumentName(String detailDocumentName) {
		return referencesByDocumentNames.get(detailDocumentName);
	}

	@Override
	public org.skyve.metadata.model.document.Document getRelatedDocument(Customer customer, String relationName) 
	throws MetaDataException {
		Relation relation = relationsByFieldNames.get(relationName);
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
	public Set<org.skyve.metadata.model.document.Document> getReferencedDocuments(Customer customer) 
	throws MetaDataException {
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

	public String getBizKeyMethodCode() {
		return bizKeyMethodCode;
	}

	public void setBizKeyMethodCode(String bizKeyMethodCode) {
		this.bizKeyMethodCode = bizKeyMethodCode;
	}

	public String getBizKeyExpression() {
		return bizKeyExpression;
	}

	public void setBizKeyExpression(String bizKeyExpression) {
		this.bizKeyExpression = bizKeyExpression;
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
	public org.skyve.metadata.model.document.Document getParentDocument(Customer customer) throws MetaDataException {
		org.skyve.metadata.model.document.Document result = null;

		if (parentDocumentName != null) {
			if (customer == null) {
				result = AbstractRepository.get().getModule(null, getOwningModuleName()).getDocument(null, parentDocumentName);
			}
			else {
				result = customer.getModule(getOwningModuleName()).getDocument(customer, parentDocumentName);
			}
		}

		return result;
	}

	public <T extends Bean> Bizlet<T> getBizlet(Customer customer) throws MetaDataException {
		return AbstractRepository.get().getBizlet(customer, this);
	}

	public <T extends Bean> List<DomainValue> getDomainValues(CustomerImpl customer,
																DomainType domainType,
																Attribute attribute,
																T owningBean)
	throws MetaDataException {
		List<DomainValue> result = null;
		
		if (domainType != null) {
			Bizlet<T> bizlet = getBizlet(customer);
			try {
				if (DomainType.constant.equals(domainType)) {
					result = customer.getConstantDomainValues(bizlet, getName(), attribute);
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
						result = useQuery(customer, attributeName);
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

	private List<DomainValue> useQuery(Customer customer, String attributeName)
	throws Exception {
		List<DomainValue> result = null;
		
		Attribute attribute = getAttribute(attributeName);
		if (attribute instanceof Reference) {
			Reference reference = (Reference) attribute;
			org.skyve.metadata.model.document.Document referencedDocument = getRelatedDocument(customer, attributeName);
			AbstractDocumentQuery referenceQuery = null;
			String queryName = reference.getQueryName();
			if (queryName != null) {
				Module module = customer.getModule(getOwningModuleName());
				DocumentQueryDefinition query = module.getDocumentQuery(queryName);
				referenceQuery = (AbstractDocumentQuery) query.constructDocumentQuery(null, null);
				referenceQuery.clearProjections();
				referenceQuery.clearOrderings();
			}
			else {
				referenceQuery = (AbstractDocumentQuery) AbstractPersistence.get().newDocumentQuery(referencedDocument);
			}
			
			referenceQuery.addBoundProjection(Bean.DOCUMENT_ID);
			referenceQuery.addBoundProjection(Bean.BIZ_KEY);
			referenceQuery.addOrdering(Bean.BIZ_KEY);

			List<Bean> beans = referenceQuery.projectedResults();
			result = new ArrayList<>(beans.size());
			for (Bean bean : beans) {
				result.add(new DomainValue(bean.getBizId(), (String) BindUtil.get(bean, Bean.BIZ_KEY)));
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
	public View getView(String uxui, Customer customer, ViewType type) throws MetaDataException {
		AbstractRepository repository = AbstractRepository.get();
		View view = repository.getView(uxui, customer, this, type);
		if ((view == null) && (type == ViewType.create)) {
			view = repository.getView(uxui, customer, this, ViewType.edit);
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
			bizKeyField.setName("bizKey");
			bizKeyField.setPersistent(false);
			bizKeyField.setRequired(false);
			bizKeyField.setDescription(null);
			bizKeyField.setDomainType(null);
			bizKeyField.setLength(1024);
		}
		
		return bizKeyField;
	}
}
