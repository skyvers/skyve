package org.skyve.impl.persistence;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.UUID;
import java.util.logging.Level;

import org.skyve.domain.Bean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ReferentialConstraintViolationException;
import org.skyve.domain.messages.SkyveException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.ValidationUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Reference.ReferenceType;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DynamicPersistence;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
import org.skyve.util.BeanVisitor;
import org.skyve.util.JSON;
import org.skyve.util.Util;

// TODO Need to replicate HibernateListener functions for dynamic beans
// TODO Need to treat bizVersion and bizLock which requires change detection in DynamicBean.
// The idea here is to completely persist all beans reachable, no matter the relationship.
public class RDBMSDynamicPersistence implements DynamicPersistence {
	private static final long serialVersionUID = -6445760028486705253L;

	private static final Integer NEW_VERSION = Integer.valueOf(0);

	public static final String DYNAMIC_ENTITY_TABLE_NAME = "ADM_DynamicEntity";
	
	protected Persistence persistence;
	
	// Cache that points to each DynamicBean ever populated.
	// This cache is not concerned with dynamic static beans as hibernate caching controls them.
	// There is no merge function implemented. It is expected that an existing persisted instance was gotten from populate()
	// and any non-persistent instances saved will be put into the cache.
	private final Map<String, DynamicPersistentBean> dynamicFirstLevelCache = new TreeMap<>();
	
	@Override
	public void postConstruct(@SuppressWarnings("hiding") Persistence persistence) {
		this.persistence = persistence;
	}
	
	@Override
	public void persist(PersistentBean bean) {
		Customer c = persistence.getUser().getCustomer();
		Module m = c.getModule(bean.getBizModule());
		Document d = m.getDocument(c, bean.getBizDocument());
		
		// Do this even if bean is transient as there might be some persistent part in the graph somewhere
		delete(c, d, bean, true);

		new BeanVisitor(false, false, false) {
			@Override
			protected boolean accept(String binding,
										Document visitedDocument,
										Document owningDocument,
										Relation owningRelation,
										Bean visitedBean)
			throws Exception {
//System.out.println("Visit " + binding + " = " + visitedBean.getBizId() + " / " + visitedBean.getClass());
				// Use our own cascade rules for dynamic beans
				if (visitedDocument.isDynamic()) {
					// Persist persistent documents
					if (visitedDocument.isPersistable()) {
						persistOne(c, visitedDocument, (PersistentBean) visitedBean);
					}
					else {
						// Persist persistent embedded associations as its own DynamicEntity
						if ((owningRelation instanceof Association) && owningRelation.isPersistent()) {
							Association association = (Association) owningRelation;
							if (association.getType() == AssociationType.embedded) {
								persistOne(c, visitedDocument, (PersistentBean) visitedBean);
							}
						}
					}
				}
				// Follow hibernate's lead and persist only persistent beans.
				else if (visitedDocument.hasDynamic()) {
					// Persist if hibernate has it persisted
					if (visitedBean.isPersisted()) {
						persistOne(c, visitedDocument, (PersistentBean) visitedBean);
					}
					else {
						// Persist persistent embedded associations as its own DynamicEntity if the owner is persisted
						if ((owningRelation instanceof Association) && owningRelation.isPersistent()) {
							Association association = (Association) owningRelation;
							if (association.getType() == AssociationType.embedded) {
								// Get the owning bean
								Bean owningBean = null;
								int lastDotIndex = binding.lastIndexOf('.');
								if (lastDotIndex > 0) {
									owningBean = (Bean) BindUtil.get(bean, binding.substring(0, lastDotIndex));
								}
								else {
									owningBean = bean;
								}
								// If the owningBean is persisted then the embedded object has been persisted also
								if (owningBean.isPersisted()) {
									persistOne(c, visitedDocument, (PersistentBean) visitedBean);
								}
							}
						}
					}
				}
				
				return true;
			}
		}.visit(d, bean, c);
	}
	
	private void persistOne(Customer c, Document d, PersistentBean bean) {
		final Map<String, Object> dynamicFields = new TreeMap<>();
		// Reference name -> emebdded association indictor
		Map<String, Boolean> dynamicReferences = new TreeMap<>();
		final Module m = c.getModule(d.getOwningModuleName());

		final boolean dynamicDocument = d.isDynamic();

		for (Attribute a : d.getAllAttributes(c)) {
			if (! a.isPersistent()) {
				continue;
			}
			
			// if dynamic document or dynamic field or reference to dynamic document
			boolean dynamicAttribute = dynamicDocument;
			if (a instanceof Field) {
				if (! dynamicAttribute) {
					dynamicAttribute = ((Field) a).isDynamic();
				}
				if (dynamicAttribute) {
					String name = a.getName();
					dynamicFields.put(name, BindUtil.get(bean, name));
				}
			}
			else if (a instanceof Reference) {
				Reference r = (Reference) a;
				if (! dynamicAttribute) {
					dynamicAttribute = BindUtil.isDynamic(c, m, r);
				}
				if (dynamicAttribute) {
					dynamicReferences.put(r.getName(), Boolean.valueOf(r.getType() == AssociationType.embedded));
				}
			}
		}
		
		if (! (dynamicFields.isEmpty() && dynamicReferences.isEmpty())) {
			insertEntity(bean, JSON.marshall(dynamicFields));
			insertReferences(c, bean, dynamicReferences);

			// Cache the flushed instances
			if (dynamicDocument) {
				dynamicFirstLevelCache.put(bean.getBizId(), (DynamicPersistentBean) bean);
			}
			
			dynamicFields.clear();
			dynamicReferences.clear();
		}
	}

	private void insertEntity(PersistentBean bean, String json) {
		// This is automatically handled by hibernate for static domain beans
		if (bean.getBizVersion() == null) {
			bean.setBizVersion(NEW_VERSION);
		}
		
//System.out.println("insert entity " + bean.getBizDocument() + " with bizId " + bean.getBizId() + " with json " + json);
		String insert = "insert into ADM_DynamicEntity (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizFlagComment, bizDataGroupId, bizUserId, moduleName, documentName, fields) " +
							"values (:bizId, :bizVersion, :bizLock, :bizKey, :bizCustomer, :bizFlagComment, :bizDataGroupId, :bizUserId, :moduleName, :documentName, :fields)";
		SQL sql = persistence.newSQL(insert);
		sql.putParameter(Bean.DOCUMENT_ID, bean.getBizId(), false);
		sql.putParameter(PersistentBean.VERSION_NAME, bean.getBizVersion());
		sql.putParameter(PersistentBean.LOCK_NAME, new OptimisticLock(persistence.getUser().getName(), new Date()).toString(), false);
		String bizKey = bean.getBizKey();
		sql.putParameter(Bean.BIZ_KEY, (bizKey == null) ? "Unkown" : bizKey, false);
		sql.putParameter(Bean.CUSTOMER_NAME, bean.getBizCustomer(), false);
		sql.putParameter(PersistentBean.FLAG_COMMENT_NAME, bean.getBizFlagComment(), false);
		sql.putParameter(Bean.DATA_GROUP_ID, bean.getBizDataGroupId(), false);
		sql.putParameter(Bean.USER_ID, bean.getBizUserId(), false);
		sql.putParameter("moduleName", bean.getBizModule(), false);
		sql.putParameter("documentName", bean.getBizDocument(), false);
		sql.putParameter("fields", json, true);
		
		sql.execute();
	}
	
	private void insertReferences(Customer c, PersistentBean bean, Map<String, Boolean> references) {
		String insert = "insert into ADM_DynamicRelation (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizFlagComment, bizDataGroupId, bizUserId, parent_id, relatedModuleName, relatedDocumentName, relatedId, attributeName, ordinal) " + 
							"values (:bizId, 0, :bizLock, :bizKey, :bizCustomer, null, null, :bizUserId, :parent_id, :relatedModuleName, :relatedDocumentName, :relatedId, :attributeName, :ordinal)";
		SQL sql = persistence.newSQL(insert);		
		String bizLock = new OptimisticLock(persistence.getUser().getName(), new Date()).toString();
		sql.putParameter(PersistentBean.LOCK_NAME, bizLock, false);
		sql.putParameter(Bean.CUSTOMER_NAME, bean.getBizCustomer(), false);
		sql.putParameter(Bean.USER_ID, bean.getBizUserId(), false);
		String parentId = bean.getBizId();
		sql.putParameter("parent_id", parentId, false);
		
		for (String name : references.keySet()) {
			Object value = BindUtil.get(bean, name);
			// NB don't insert null references
			if (value != null) {
				if (value instanceof List<?>) {
					int ordinal = 0;
					@SuppressWarnings("unchecked")
					List<Bean> relatedBeans = (List<Bean>) value;
					for (Bean relatedBean : relatedBeans) {
						sql.putParameter(Bean.DOCUMENT_ID, UUID.randomUUID().toString(), false);
						String relatedId = relatedBean.getBizId();
//System.out.println("insert collection element for " + bean.getBizDocument() + " with bizId " + bean.getBizId() + " for attribute " + name + " = " + relatedId);
						sql.putParameter(Bean.BIZ_KEY, parentId + "->" + relatedId, false);
						String relatedModuleName = relatedBean.getBizModule();
						String relatedDocumentName = relatedBean.getBizDocument();
						boolean dynamic = c.getModule(relatedModuleName).getDocument(c, relatedDocumentName).isDynamic();
						sql.putParameter("relatedModuleName", dynamic ? null : relatedModuleName, false);
						sql.putParameter("relatedDocumentName", dynamic ? null : relatedDocumentName, false);
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
//System.out.println("insert association element for " + bean.getBizDocument() + " with bizId " + bean.getBizId() + " for attribute " + name + " = " + relatedId);
					sql.putParameter(Bean.BIZ_KEY, parentId + "->" + relatedId, false);
					String relatedModuleName = relatedBean.getBizModule();
					String relatedDocumentName = relatedBean.getBizDocument();
					boolean dynamic = references.get(name).booleanValue() || // an embedded association 
											c.getModule(relatedModuleName).getDocument(c, relatedDocumentName).isDynamic(); // or dynamic
					sql.putParameter("relatedModuleName", dynamic ? null : relatedModuleName, false);
					sql.putParameter("relatedDocumentName", dynamic ? null : relatedDocumentName, false);
					sql.putParameter("relatedId", relatedId, false);
					sql.putParameter("attributeName", name, false);
					sql.putParameter("ordinal", null, false);
					sql.execute();
				}
			}
		}
	}
	
	@Override
	public void delete(PersistentBean bean) {
		Customer c = persistence.getUser().getCustomer();
		Module m = c.getModule(bean.getBizModule());
		Document d = m.getDocument(c, bean.getBizDocument());
		delete(c, d, bean, false);
	}
	
	// Remove the lot before save, we'll put it all back there if its required
	// Otherwise if its an actual delete, just cascade all but aggregations.
	// NB We don't check that the document hasDynamic here so we can clean up anything that has gone from dynamic to static
	private void delete(Customer customer, Document document, PersistentBean bean, boolean beforeSave) {
		final Map<String, Bean> bizIdsToDelete = new TreeMap<>();
		
		// Call Bizlet.preDelete() on anything that will cascade delete
		new BeanVisitor(false, false, false) {
			@Override
			protected boolean accept(String binding,
										Document visitedDocument,
										Document owningDocument,
										Relation owningRelation,
										Bean visitedBean)
			throws Exception {
				if (owningRelation == null) {
					if (visitedBean.isPersisted()) {
						bizIdsToDelete.put(visitedBean.getBizId(), visitedBean);
					}
					return true;
				}

				if (beforeSave) {
					if (visitedBean.isPersisted()) {
						bizIdsToDelete.put(visitedBean.getBizId(), visitedBean);
					}
					return true;
				}

				if (owningRelation instanceof Reference) {
					Reference reference = (Reference) owningRelation;
					ReferenceType type = reference.getType();
					// Requires cascading
					if (! (AssociationType.aggregation.equals(type) || CollectionType.aggregation.equals(type))) {
						if (visitedBean.isPersisted()) {
							if (visitedDocument.isDynamic()) {
								callBizletPreDelete(customer, visitedDocument, (PersistentBean) visitedBean);
							}
							bizIdsToDelete.put(visitedBean.getBizId(), visitedBean);
							return true;
						}
					}
				}
				
				return false;
			}
		}.visit(document, bean, customer);

		// Delete in batches of 100
//System.out.println("delete entity (and relations) for " + JSON.marshall(bizIdsToDelete));
		int i = 0;
		int l = bizIdsToDelete.size();
		List<String> batch = new ArrayList<>(100);
		for (String bizId : bizIdsToDelete.keySet()) {
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

				// delete the DynamicEntity
				sql = persistence.newSQL("delete from ADM_DynamicEntity where bizId in (:bizId)");
				sql.putParameter(Bean.DOCUMENT_ID, batch, AttributeType.id);
				sql.execute();
				
				batch.clear();
			}
		}

		if (! beforeSave) {
			// Check referential integrity
			i = 0;
			for (String bizId : bizIdsToDelete.keySet()) {
				batch.add(bizId);
				i++;
				if ((i == l) || // last element reached
						((i % 100) == 0)) { // multiple of 100
					SQL sql = persistence.newSQL("select de.moduleName, de.documentName, dr.relatedId, de.bizId, dr.attributeName from ADM_DynamicRelation dr inner join ADM_DynamicEntity de on dr.parent_id = de.bizId where dr.relatedId in (:bizId)");
					sql.putParameter(Bean.DOCUMENT_ID, batch, AttributeType.id);
					Object[] result = sql.tupleResult();
					if (result != null) {
						Bean missingBean = bizIdsToDelete.get(result[2]);
						Module m = customer.getModule(missingBean.getBizModule());
						Document d = m.getDocument(customer, missingBean.getBizDocument());
						String documentAlias = d.getLocalisedSingularAlias();
						m = customer.getModule((String) result[0]);
						d = m.getDocument(customer, (String) result[1]);
						String referencingDocumentAlias = d.getLocalisedSingularAlias();
						throw new ReferentialConstraintViolationException(documentAlias, bean.getBizKey(), referencingDocumentAlias);
					}
					batch.clear();
				}
			}
			
			// Call Bizlet.postDelete() on all the deleted beans (except the bean being deleted)
			for (Bean deletedBean : bizIdsToDelete.values()) {
				if (! deletedBean.equals(bean)) {
					final Module m = customer.getModule(deletedBean.getBizModule());
					final Document d = m.getDocument(customer, deletedBean.getBizDocument());
					if (d.isDynamic()) {
						callBizletPostDelete(customer, d, (PersistentBean) deletedBean);
					}
				}
			}
			
			// Flush above was successful, remove from the first level cache now
			for (String bizId : bizIdsToDelete.keySet()) {
				dynamicFirstLevelCache.remove(bizId);
			}
		}

		batch.clear();
		bizIdsToDelete.clear();
	}

	private static void callBizletPreDelete(Customer customer, Document document, PersistentBean bean) {
		try {
			CustomerImpl internalCustomer = (CustomerImpl) customer;
			boolean vetoed = internalCustomer.interceptBeforePreDelete(bean);
			if (! vetoed) {
				Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
				if (bizlet != null) {
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preDelete", "Entering " + bizlet.getClass().getName() + ".preDelete: " + bean);
					bizlet.preDelete(bean);
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "preDelete", "Exiting " + bizlet.getClass().getName() + ".preDelete");
				}
				internalCustomer.interceptAfterPreDelete(bean);
			}
		}
		catch (ValidationException e) {
			for (Message message : e.getMessages()) {
				ValidationUtil.processMessageBindings(customer, message, bean, bean);
			}
			throw e;
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}
	
	private static void callBizletPostDelete(Customer customer, Document document, PersistentBean bean) {
		try {
			CustomerImpl internalCustomer = (CustomerImpl) customer;
			boolean vetoed = internalCustomer.interceptBeforePostDelete(bean);
			if (! vetoed) {
				Bizlet<Bean> bizlet = ((DocumentImpl) document).getBizlet(customer);
				if (bizlet != null) {
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "postDelete", "Entering " + bizlet.getClass().getName() + ".postDelete: " + bean);
					bizlet.postDelete(bean);
					if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "postDelete", "Exiting " + bizlet.getClass().getName() + ".postDelete");
				}
				internalCustomer.interceptAfterPostDelete(bean);
			}
		}
		catch (ValidationException e) {
			for (Message message : e.getMessages()) {
				ValidationUtil.processMessageBindings(customer, message, bean, bean);
			}
			throw e;
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	/**
	 * Called by Skyve Persistence for a static bean with some dynamic properties
	 */
	@Override
	public void populate(PersistentBean bean) {
		try {
//System.out.println("populate document for " + bean.getBizDocument() + " with bizId " + bean.getBizId());
			// Note that caching of these mixed beans is handled by hibernate so if AbstractHibernatePersistence.postLoad() calls this method, we don't ask questions.

			// select the json by bizId (no assertion that the row exists since this is a hybrid static/dynamic bean and is driven by the static select)
			String select = "select bizVersion, bizLock, bizKey, bizCustomer, bizFlagComment, bizDataGroupId, bizUserId, fields from ADM_DynamicEntity where bizid = :bizId";
			Object[] tuple = persistence.newSQL(select).putParameter(Bean.DOCUMENT_ID, bean.getBizId(), false).tupleResult();
			if (tuple != null) { // dynamic properties exist
				User u = persistence.getUser();
				Customer c = u.getCustomer();
				Module m = c.getModule(bean.getBizModule());
				Document d = m.getDocument(c, bean.getBizDocument());
				
				populate(u, c, m, d, bean, tuple);
			}
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	/**
	 * Called by Skyve Persistence for a totally dynamic bean.
	 */
	@Override
	public DynamicPersistentBean populate(String bizId) {
		try {
//System.out.println("populate entity with bizId " + bizId);
			if (dynamicFirstLevelCache.containsKey(bizId)) {
				return dynamicFirstLevelCache.get(bizId);
			}

			// select the json by bizId (assert that the row exists since this is a totally dynamic bean)
			String select = "select bizVersion, bizLock, bizKey, bizCustomer, bizFlagComment, bizDataGroupId, bizUserId, fields, moduleName, documentName from ADM_DynamicEntity where bizid = :bizId";
			Object[] tuple = persistence.newSQL(select).putParameter(Bean.DOCUMENT_ID, bizId, false).retrieveTuple();
			
			User u = persistence.getUser();
			Customer c = u.getCustomer();
			Module m = c.getModule((String) tuple[8]);
			Document d = m.getDocument(c, (String) tuple[9]);
			DynamicPersistentBean result = d.newInstance(u);
			result.setDynamic(Bean.DOCUMENT_ID, bizId); // set the new bean's bizId
			
			// Cache the newly created bean
			// NB Do this before calling populate to short circuit cyclic references
			dynamicFirstLevelCache.put(bizId, result);
			
			populate(u, c, m, d, result, tuple);

			return result;
		}
		catch (SkyveException e) {
			throw e;
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
							Object[] tuple)
	throws Exception {
		// only populate the biz stuff if this is a dynamic bean, otherwise its in the static bean
		if (d.isDynamic()) {
			bean.setBizVersion(Integer.valueOf(((Number) tuple[0]).intValue()));
			bean.setBizLock(new OptimisticLock((String) tuple[1]));
			bean.setBizKey((String) tuple[2]);
			bean.setBizCustomer((String) tuple[3]);
			bean.setBizFlagComment((String) tuple[4]);
			bean.setBizDataGroupId((String) tuple[5]);
			bean.setBizUserId((String) tuple[6]);
		}
		
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
					Object value = json.get(name);
					Class<?> type = a.getAttributeType().getImplementingType();
					if ((value != null) && (! type.equals(value.getClass()))) {
						try {
							value = BindUtil.fromSerialised(type, value.toString());
						}
						catch (Exception e) {
							Util.LOGGER.warning("RDBMSDynamicPersistence: Schema evolution problem on populate of " + d.getOwningModuleName() + "." + d.getName() + "#" + bean.getBizId() + " :- [" + value + "] cannot be coerced to type " + type);
							e.printStackTrace();
						}
					}
					bean.setDynamic(name, value);
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
		
		populateReferences(bean, dynamicReferenceNames);
	}

	private void populateReferences(PersistentBean bean, Set<String> dynamicReferenceNames) throws Exception {
		String select = "select relatedModuleName, relatedDocumentName, relatedId, attributeName from ADM_DynamicRelation where parent_id = :bizId order by attributeName, ordinal";
		// Note - this following SQL gets a list instead of iterating as this method is recursive (through the populate() call for relatedBean).
		// Hibernate can't manage multiple nested ScrollableResults for certain databases (MySQL) and closes the encapsulated ResultSet of the outer ScrollableResults prematurely.
		for (Object[] tuple : persistence.newSQL(select).putParameter(Bean.DOCUMENT_ID, bean.getBizId(), false).tupleResults()) {
			String relatedModuleName = (String) tuple[0];
			String relatedDocumentName = (String) tuple[1];
			String relatedId = (String) tuple[2];
			String attributeName = (String) tuple[3];

			if (dynamicReferenceNames.contains(attributeName)) {
				// Find the related bean
				PersistentBean relatedBean = null;
				if (relatedId != null) {
					// static document has related module and document name
					if ((relatedModuleName != null) && (relatedDocumentName != null)) {
						relatedBean = persistence.retrieve(relatedModuleName, relatedDocumentName, relatedId);
					}
					// otherwise dynamic document
					else {
						relatedBean = populate(relatedId);
					}
				}
				
				// Set the related bean
				Object value = bean.getDynamic(attributeName);
				if (value instanceof List<?>) {
					if (relatedBean != null) {
						BindUtil.addElementToCollection(bean, attributeName, relatedBean);
					}
				}
				else {
					BindUtil.setAssociation(bean, attributeName, relatedBean);
				}
			}
		}
	}

	@Override
	public void evictAllCached() {
		dynamicFirstLevelCache.clear();
	}
	
	@Override
	public void evictCached(Bean bean) {
		dynamicFirstLevelCache.remove(bean.getBizId());
	}
	
	@Override
	public boolean cached(Bean bean) {
		return dynamicFirstLevelCache.containsKey(bean.getBizId());
	}

	@Override
	public void begin() {
		// nothing to do as we use the parent persistence's connection
	}

	@Override
	public void rollback() {
		// nothing to do as we use the parent persistence's connection
	}

	@Override
	public void commit() {
		// nothing to do as we use the parent persistence's connection
	}
	
	@Override
	public void close() {
		// nothing to do as we use the parent persistence's connection
	}
}
