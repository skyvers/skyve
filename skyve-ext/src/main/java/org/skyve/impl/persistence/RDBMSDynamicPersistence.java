package org.skyve.impl.persistence;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.UUID;

import org.hibernate.LockMode;
import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;
import org.hibernate.query.Query;
import org.hibernate.type.StringType;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.ExportedReference;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DynamicPersistence;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
import org.skyve.util.BeanVisitor;
import org.skyve.util.Binder;
import org.skyve.util.JSON;

// TODO Need to replicate HibernateListener functions for dynamic beans
public class RDBMSDynamicPersistence implements DynamicPersistence {
	private static final long serialVersionUID = -6445760028486705253L;

	private static final Integer NEW_VERSION = Integer.valueOf(0);

	private Persistence persistence;
	
//	private Map<String, PersistenteBean> dynamicFirstLevelCache = new TreeMap<>();
	
	@Override
	public void postConstruct(@SuppressWarnings("hiding") Persistence persistence) {
		this.persistence = persistence;
	}
	
	@Override
	public void persist(Customer customer, Module module, Document document, PersistentBean bean) {
		if (bean.isPersisted()) {
			delete(customer, document, bean, true);
		}

		new BeanVisitor(false, false, true) {
			@Override
			protected boolean accept(String binding,
										Document visitedDocument,
										Document owningDocument,
										Relation owningRelation,
										Bean visitedBean)
			throws Exception {
				// TODO refine this rule possibly - transient reference to persisted bean and all that crap?
				Persistent persistent = visitedDocument.getPersistent();
				if ((persistent != null) && 
						(persistent.getName() != null)) { // persistent document
					persistOne(customer, visitedDocument, (PersistentBean) visitedBean);
				}
				return true;
			}
		}.visit(document, bean, customer);
	}
	
	private void persistOne(Customer c, Document d, PersistentBean bean) {
		final Map<String, Object> dynamicFields = new TreeMap<>();
		final List<String> dynamicReferenceNames = new ArrayList<>();
		final Module m = c.getModule(d.getOwningModuleName());

		final boolean dynamicDocument = d.isDynamic();

		for (Attribute a : d.getAllAttributes(c)) {
			// if dynamic document or dynamic field or reference to dynamic document
			boolean dynamicAttribute = dynamicDocument;
			if (a instanceof Field) {
				if (! dynamicAttribute) {
					dynamicAttribute = ((Field) a).isDynamic();
				}
				if (dynamicAttribute) {
					String name = a.getName();
					dynamicFields.put(name, Binder.get(bean, name));
				}
			}
			else if (a instanceof Reference) {
				Reference r = (Reference) a;
				if (! dynamicAttribute) {
					dynamicAttribute = BindUtil.isDynamic(c, m, r);
				}
				if (dynamicAttribute) {
					dynamicReferenceNames.add(r.getName());
				}
			}
		}
		
		if (! (dynamicFields.isEmpty() && dynamicReferenceNames.isEmpty())) {
			insertEntity(bean, JSON.marshall(dynamicFields));
			insertReferences(c, bean, dynamicReferenceNames);

			dynamicFields.clear();
			dynamicReferenceNames.clear();
		}
	}

	private void insertEntity(PersistentBean bean, String json) {
		// This is automatically handled by hibernate for static domain beans
		if (bean.getBizVersion() == null) {
			bean.setBizVersion(NEW_VERSION);
		}
		
		String insert = "insert into ADM_DynamicEntity (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizFlagComment, bizDataGroupId, bizUserId, moduleName, documentName, fields) " +
							"values (:bizId, :bizVersion, :bizLock, :bizKey, :bizCustomer, :bizFlagComment, :bizDataGroupId, :bizUserId, :moduleName, :documentName, :fields)";
		SQL sql = persistence.newSQL(insert);
		sql.putParameter(Bean.DOCUMENT_ID, bean.getBizId(), false);
		sql.putParameter(PersistentBean.VERSION_NAME, bean.getBizVersion());
		sql.putParameter(PersistentBean.LOCK_NAME, new OptimisticLock(persistence.getUser().getName(), new Date()).toString(), false);
		sql.putParameter(Bean.BIZ_KEY, bean.getBizKey(), false);
		sql.putParameter(Bean.CUSTOMER_NAME, bean.getBizCustomer(), false);
		sql.putParameter(PersistentBean.FLAG_COMMENT_NAME, bean.getBizFlagComment(), false);
		sql.putParameter(Bean.DATA_GROUP_ID, bean.getBizDataGroupId(), false);
		sql.putParameter(Bean.USER_ID, bean.getBizUserId(), false);
		sql.putParameter("moduleName", bean.getBizModule(), false);
		sql.putParameter("documentName", bean.getBizDocument(), false);
		sql.putParameter("fields", json, true);
		
		sql.execute();
	}
	
	private void insertReferences(Customer c, PersistentBean bean, List<String> referenceNames) {
		String insert = "insert into ADM_DynamicRelation (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizFlagComment, bizDataGroupId, bizUserId, parent_id, relatedModuleName, relatedDocumentName, relatedId, attributeName, ordinal) " + 
							"values (:bizId, 0, :bizLock, :bizKey, :bizCustomer, null, null, :bizUserId, :parent_id, :relatedModuleName, :relatedDocumentName, :relatedId, :attributeName, :ordinal)";
		SQL sql = persistence.newSQL(insert);		
		String bizLock = new OptimisticLock(persistence.getUser().getName(), new Date()).toString();
		sql.putParameter(PersistentBean.LOCK_NAME, bizLock, false);
		sql.putParameter(Bean.CUSTOMER_NAME, bean.getBizCustomer(), false);
		sql.putParameter(Bean.USER_ID, bean.getBizUserId(), false);
		sql.putParameter("parent_id", bean.getBizId(), false);
		
		for (String name : referenceNames) {
			Object value = Binder.get(bean, name);
			if (value == null) {
				sql.putParameter(Bean.DOCUMENT_ID, UUID.randomUUID().toString(), false);
				sql.putParameter(Bean.BIZ_KEY, "->null", false);
				sql.putParameter("relatedModuleName", null, false);
				sql.putParameter("relatedDocumentName", null, false);
				sql.putParameter("relatedId", null, false);
				sql.putParameter("attributeName", name, false);
				sql.putParameter("ordinal", null, false);
				sql.execute();
			}
			else if (value instanceof List<?>) {
				int ordinal = 0;
				@SuppressWarnings("unchecked")
				List<Bean> relatedBeans = (List<Bean>) value;
				for (Bean relatedBean : relatedBeans) {
					sql.putParameter(Bean.DOCUMENT_ID, UUID.randomUUID().toString(), false);
					String relatedId = relatedBean.getBizId();
					sql.putParameter(Bean.BIZ_KEY, "->" + relatedId, false);
					String relatedModuleName = relatedBean.getBizModule();
					String relatedDocumentName = relatedBean.getBizDocument();
					boolean dynnamic = c.getModule(relatedModuleName).getDocument(c, relatedDocumentName).isDynamic();
					sql.putParameter("relatedModuleName", dynnamic ? null : relatedModuleName, false);
					sql.putParameter("relatedDocumentName", dynnamic ? null : relatedDocumentName, false);
					sql.putParameter("relatedId", relatedId, false);
					sql.putParameter("attributeName", name, false);
					sql.putParameter("ordinal", Integer.valueOf(ordinal));
					sql.execute();
					ordinal++;
				}
			}
			else {
				Bean relatedBean = (Bean) value;
				sql.putParameter(Bean.DOCUMENT_ID, UUID.randomUUID().toString(), false);
				String relatedId = relatedBean.getBizId();
				sql.putParameter(Bean.BIZ_KEY, "->" + relatedId, false);
				String relatedModuleName = relatedBean.getBizModule();
				String relatedDocumentName = relatedBean.getBizDocument();
				boolean dynnamic = c.getModule(relatedModuleName).getDocument(c, relatedDocumentName).isDynamic();
				sql.putParameter("relatedModuleName", dynnamic ? null : relatedModuleName, false);
				sql.putParameter("relatedDocumentName", dynnamic ? null : relatedDocumentName, false);
				sql.putParameter("relatedId", relatedId, false);
				sql.putParameter("attributeName", name, false);
				sql.putParameter("ordinal", null, false);
				sql.execute();
			}
		}
	}
	
	@Override
	public void delete(Customer customer, Document document, PersistentBean bean) {
		delete(customer, document, bean, false);
	}
	
	private void delete(Customer customer, Document document, PersistentBean bean, boolean beforeSave) {
		final Set<String> bizIdsToDelete = new TreeSet<>();
		
		new BeanVisitor(false, false, true) {
			@Override
			protected boolean accept(String binding,
										Document visitedDocument,
										Document owningDocument,
										Relation owningRelation,
										Bean visitedBean)
			throws Exception {
				if (owningRelation == null) {
					bizIdsToDelete.add(visitedBean.getBizId());
				}
				else {
					if (beforeSave) {
						bizIdsToDelete.add(visitedBean.getBizId());
					}
					else {
						if (owningRelation instanceof Collection) {
							// cascade
							if (((Collection) owningRelation).getType() != CollectionType.aggregation) {
								bizIdsToDelete.add(visitedBean.getBizId());
							}
						}
						else if (owningRelation instanceof Association) {
							// cascade
							if (((Association) owningRelation).getType() != AssociationType.aggregation) {
								bizIdsToDelete.add(visitedBean.getBizId());
							}
						}
					}
				}
				
				return true;
			}
		}.visit(document, bean, customer);

		// Delete in batches of 100
		int i = 0;
		int l = bizIdsToDelete.size();
		List<String> batch = new ArrayList<>(100);
		for (String bizId : bizIdsToDelete) {
			batch.add(bizId);
			i++;
			if ((i == l) || // last element reached
					((i % 100) == 0)) { // multiple of 100
				// delete all outgoing DynamicRelation for the DynamicEntity where bizId in (bizIdsToDelete)
				// NB Could be extra relations left over from schema evolution
				// NB No need to worry about clashing bizIds as it needs to be a PK in ADM_DynamicEntity (no duplicates)
				SQL sql = persistence.newSQL("delete from ADM_DynamicRelation where parent_id in (:bizId)");
				sql.putParameter(Bean.DOCUMENT_ID, batch, AttributeType.id);
				sql.execute();

				// delete all incoming DynamicRelation for the DynamicEntity where bizId in (bizIdsToDelete)
				// NB Could be extra relations left over from schema evolution
				// NB No need to worry about clashing bizIds as it needs to be a PK in ADM_DynamicEntity (no duplicates)
				sql = persistence.newSQL("delete from ADM_DynamicRelation where relatedId in (:bizId)");
				sql.putParameter(Bean.DOCUMENT_ID, batch, AttributeType.id);
				sql.execute();

				// delete the DynamicEntity
				sql = persistence.newSQL("delete from ADM_DynamicEntity where bizId in (:bizId)");
				sql.putParameter(Bean.DOCUMENT_ID, batch, AttributeType.id);
				sql.execute();
				
				batch.clear();
			}
		}

		batch.clear();
		bizIdsToDelete.clear();
	}

//	Do I need a first level cache to keep all the objects the same?
//	I think I do...
//	Just for dynamic objects though - static ones are cached by hibernate
			
	
	@Override
	public DynamicPersistentBean populate(String bizId) {
		Map<String, PersistentBean> loaded = new TreeMap<>();
		populate(bizId, loaded);
		return null;
	}
	
	private DynamicPersistentBean populate(String bizId, Map<String, PersistentBean> visited) {
		try {
			// select the json by bizId
			String select = "select bizVersion, bizLock, bizKey, bizCustomer, bizFlagComment, bizDataGroupId, bizUserId, fields, moduleName, documentName from ADM_DynamicEntity where bizid = :bizId";
			Object[] tuple = persistence.newSQL(select).putParameter(Bean.DOCUMENT_ID, bizId, false).retrieveTuple();
			
			User u = persistence.getUser();
			Customer c = u.getCustomer();
			Module m = c.getModule((String) tuple[8]);
			Document d = m.getDocument(c, (String) tuple[9]);
			DynamicPersistentBean result = d.newInstance(u);
			
			populate(u, c, m, d, result, tuple, visited);
			visited.clear();
			return result;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}
	
	@Override
	public void populate(PersistentBean bean) {
		try {
			// select the json by bizId
			String select = "select bizVersion, bizLock, bizKey, bizCustomer, bizFlagComment, bizDataGroupId, bizUserId, fields from ADM_DynamicEntity where bizid = :bizId";
			Object[] tuple = persistence.newSQL(select).putParameter(Bean.DOCUMENT_ID, bean.getBizId(), false).retrieveTuple();
	
			User u = persistence.getUser();
			Customer c = u.getCustomer();
			Module m = c.getModule(bean.getBizModule());
			Document d = m.getDocument(c, bean.getBizDocument());
			
			Map<String, PersistentBean> visited = new TreeMap<>();
			populate(u, c, m, d, bean, tuple, visited);
			visited.clear();
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	private void populate(User u,
							Customer c,
							Module m,
							Document d,
							PersistentBean bean,
							Object[] tuple,
							Map<String, PersistentBean> visited) throws Exception {
		bean.setBizVersion(Integer.valueOf(((Number) tuple[0]).intValue()));
		bean.setBizLock(new OptimisticLock((String) tuple[1]));
		bean.setBizKey((String) tuple[2]);
		bean.setBizCustomer((String) tuple[3]);
		bean.setBizFlagComment((String) tuple[4]);
		bean.setBizDataGroupId((String) tuple[5]);
		bean.setBizUserId((String) tuple[6]);
		
		@SuppressWarnings("unchecked")
		Map<String, Object> json = (Map<String, Object>) JSON.unmarshall(u, (String) tuple[7]);
		Set<String> dynamicReferenceNames = new TreeSet<>();
		final boolean dynamicDocument = d.isDynamic();
		for (Attribute a : d.getAllAttributes(c)) {
			// if dynamic document or dynamic field or reference to dynamic document
			boolean dynamicAttribute = dynamicDocument;
			if (a instanceof Field) {
				if (! dynamicAttribute) {
					dynamicAttribute = ((Field) a).isDynamic();
				}
				if (dynamicAttribute) {
					String name = a.getName();
					bean.setDynamic(name, json.get(name));
				}
			}
			else if (a instanceof Reference) {
				Reference r = (Reference) a;
				if (! dynamicAttribute) {
					dynamicAttribute = BindUtil.isDynamic(c, m, r);
				}
				if (dynamicAttribute) {
					dynamicReferenceNames.add(r.getName());
				}
			}
		}
		visited.put(bean.getBizId(), bean);
		
		populateReferences(bean, dynamicReferenceNames);
	}

	private void populateReferences(PersistentBean bean, Set<String> dynamicReferenceNames) throws Exception {
		String select = "select relatedModuleName, relatedDocumentName, relatedId, attributeName from ADM_DynamicRelation where parent_id = :bizId order by attributeName, ordinal";
		try (AutoClosingIterable<Object[]> i = persistence.newSQL(select).putParameter(Bean.DOCUMENT_ID, bean.getBizId(), false).tupleIterable()) {
			for (Object[] tuple : i) {
				String relatedModuleName = (String) tuple[0];
				String relatedDocumentName = (String) tuple[1];
				String relatedId = (String) tuple[2];
				String attributeName = (String) tuple[3];

				if (dynamicReferenceNames.contains(attributeName)) {
					// Find the related bean
					PersistentBean relatedBean = null;
					if (relatedId != null) {
						if ((relatedModuleName != null) && (relatedDocumentName != null)) {
							relatedBean = persistence.retrieve(relatedModuleName, relatedDocumentName, relatedId);
						}
						else {
							populate(relatedId);
						}
					}
					
					// Set the related bean
					Object value = bean.getDynamic(attributeName);
					if (value instanceof List<?>) {
						if (relatedBean != null) {
							@SuppressWarnings("unchecked")
							List<PersistentBean> list = ((List<PersistentBean>) value);
							list.add(relatedBean);
						}
					}
					else {
						bean.setDynamic(attributeName, relatedBean);
					}
				}
			}
		}
	}
	
	@Override
	public boolean hasReferentialIntegrity(Document documentToDelete,
											PersistentBean beanToDelete,
											ExportedReference exportedReference,
											Document referenceDocument,
											Set<Bean> beansToBeExcluded) {
		StringBuilder queryString = new StringBuilder(128);
		queryString.append("select bean from ");
		queryString.append("adminDynamicRelation as bean");
		// Take into account atributeName and the owning dynamic entity matches the reference document.
		queryString.append(" where bean.attributeName = '").append(exportedReference.getReferenceFieldName());
		queryString.append("' and bean.parent.moduleName = '").append(referenceDocument.getOwningModuleName());
		queryString.append("' and bean.parent.documentName = '").append(referenceDocument.getName());
		// Take the relatedModuleName / relatedDocumentName into account if its a static reference
		// as the bizId does not have to be unique across database tables.
		if (! (referenceDocument.isDynamic() || documentToDelete.isDynamic())) { // static reference
			queryString.append("' and bean.relatedModuleName = '").append(referenceDocument.getOwningModuleName());
			queryString.append("' and bean.relatedDocumentName = '").append(referenceDocument.getName());
		}
		queryString.append("' and bean.relatedId = :referencedBeanId");

		if (beansToBeExcluded != null) {
			int i = 0;
			for (@SuppressWarnings("unused") Bean beanToBeExcluded : beansToBeExcluded) {
				queryString.append(" and bean.relatedId != :deletedBeanId").append(i++);
			}
		}
		if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info("FK check : " + queryString);

		@SuppressWarnings("resource")
		Query<?> query = ((AbstractHibernatePersistence) persistence).getSession().createQuery(queryString.toString());
		query.setLockMode("bean", LockMode.READ); // read lock required for referential integrity

		// Set timeout if applicable
		int timeout = UtilImpl.DATA_STORE.getOltpConnectionTimeoutInSeconds();
		if (timeout > 0) {
			query.setTimeout(timeout);
		}

		query.setParameter("referencedBeanId", beanToDelete.getBizId(), StringType.INSTANCE);
		if (beansToBeExcluded != null) {
			int i = 0;
			for (Bean thisBeanToBeCascaded : beansToBeExcluded) {
				// Use the id, not the entity as hibernate cannot resolve the entity mapping of the parameter under some circumstances.
				query.setParameter("deletedBeanId" + i++, thisBeanToBeCascaded.getBizId(), StringType.INSTANCE);
			}
		}

		try (ScrollableResults results = query.scroll(ScrollMode.FORWARD_ONLY)) {
			return (! results.next());
		}
	}
}
