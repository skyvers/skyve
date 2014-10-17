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

import org.skyve.domain.Bean;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.metadata.customer.CustomerImpl;
import org.skyve.wildcat.metadata.flow.Flow;
import org.skyve.wildcat.metadata.model.Model;
import org.skyve.wildcat.metadata.model.document.field.Text;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.persistence.DocumentQueryImpl;
import org.skyve.wildcat.util.UtilImpl;

public final class DocumentImpl extends Model implements Document {
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

	/**
	 * This is this document's master or parent document name. This can be <code>null</code> if no parent document exists.
	 */
	private String parentDocumentName;

	private String bizKeyMethodCode;

	/**
	 * A map of condition name -> condition code.
	 */
	private Map<String, String> conditionsCode = new TreeMap<>();

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
		
		// Run bizlet newInstance()
		Bizlet<T> bizlet = getBizlet(customer);
		if (bizlet != null) {
			if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "newInstance", "Entering " + bizlet.getClass().getName() + ".newInstance: " + result);
			result = bizlet.newInstance(result);
			// clear the object's dirtiness
			result.originalValues().clear();
			if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "newInstance", "Exiting " + bizlet.getClass().getName() + ".newInstance: " + result);
		}

		// Set implicit properties
		result.setBizCustomer(customer.getName());
		result.setBizDataGroupId(user.getDataGroupId());
		result.setBizUserId(user.getId());

		return result;
	}

	@SuppressWarnings("unchecked")
	public <T extends Bean> Class<T> getBeanClass(Customer customer) throws ClassNotFoundException {
		Class<T> result = null;
		
		AbstractRepository repository = AbstractRepository.get();

		String dotDocumentName = '.' + getName();
		String packagePath = ((CustomerImpl) customer).getVTable().get(getOwningModuleName() + dotDocumentName);
		packagePath = packagePath.replace('/', '.');
		int lastDotIndex = packagePath.lastIndexOf('.');
		String className = packagePath.substring(0, lastDotIndex + 1) + repository.DOMAIN_NAME + dotDocumentName;
		if (packagePath.startsWith(repository.CUSTOMERS_NAME)) {
			// Look for an extension first and if not found look for a base class
			try {
				result = (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(className + "Ext");
			}
			catch (ClassNotFoundException e) { // no extension class
				// Look for the base class in the customer area
				try {
					result = (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(className);
				}
				catch (ClassNotFoundException e1) { // no extension or base class in customer area
					// Look for the base class in the modules area
					className = new StringBuilder(128).append(repository.MODULES_NAME).append('.').append(getOwningModuleName()).append('.').append(repository.DOMAIN_NAME).append(dotDocumentName).toString();
					result = (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(className);
				}
			}
		}
		else {
			// Look for base class and if abstract, look for an extension
			result = (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(className);
			if (Modifier.isAbstract(result.getModifiers())) {
				result = (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(className + "Ext");
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
	public org.skyve.metadata.model.document.Document getReferencedDocument(Customer customer, String referenceName) 
	throws MetaDataException {
		Reference reference = getReferenceByName(referenceName);
		if ((reference == null) || (reference.getDocumentName() == null)) {
			throw new IllegalStateException("Document has no referenced document defined for " + referenceName);
		}

		return customer.getModule(getOwningModuleName()).getDocument(customer, reference.getDocumentName());
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

	public void putReference(Reference reference) {
		referencesByDocumentNames.put(reference.getDocumentName(), reference);
		referencesByFieldNames.put(reference.getName(), reference);
		putAttribute(reference);
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

	@Override
	public boolean isOrdered() {
		return ordered;
	}

	public void setOrdered(boolean ordered) {
		this.ordered = ordered;
	}

	public Map<String, String> getConditionsCode() {
		return conditionsCode;
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

	public <T extends Bean> List<DomainValue> getDomainValues(Customer customer,
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
					if (bizlet != null) {
						if (DomainType.variant.equals(domainType)) {
							if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "getVariantDomainValues", "Entering " + bizlet.getClass().getName() + ".getVariantDomainValues: " + attributeName);
							result = bizlet.getVariantDomainValues(attributeName);
							if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "getVariantDomainValues", "Exiting " + bizlet.getClass().getName() + ".getVariantDomainValues: " + result);
						}
						else if (DomainType.dynamic.equals(domainType)) {
							if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "getDynamicDomainValues", "Entering " + bizlet.getClass().getName() + ".getDynamicDomainValues: " + attributeName + ", " + owningBean);
							result = bizlet.getDynamicDomainValues(attributeName, owningBean);
							if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "getDynamicDomainValues", "Exiting " + bizlet.getClass().getName() + ".getDynamicDomainValues: " + result);
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
			org.skyve.metadata.model.document.Document referencedDocument = getReferencedDocument(customer, attributeName);
			DocumentQueryImpl referenceQuery = null;
			String queryName = reference.getQueryName();
			if (queryName != null) {
				Module module = customer.getModule(getOwningModuleName());
				referenceQuery = (DocumentQueryImpl) module.getQuery(queryName).constructDocumentQuery(null, null);
				referenceQuery.clearProjections();
				referenceQuery.clearOrderings();
			}
			else {
				referenceQuery = new DocumentQueryImpl(referencedDocument);
			}
			
			referenceQuery.addBoundProjection(Bean.DOCUMENT_ID);
			referenceQuery.addBoundProjection(Bean.BIZ_KEY);
			referenceQuery.addOrdering(Bean.BIZ_KEY);

			List<Bean> beans = AbstractPersistence.get().retrieve(referenceQuery);
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
		return conditionsCode.keySet();
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
			bizKeyField.setShortDescription(null);
			bizKeyField.setDomainType(null);
			bizKeyField.setLength(1024);
		}
		
		return bizKeyField;
	}
}
