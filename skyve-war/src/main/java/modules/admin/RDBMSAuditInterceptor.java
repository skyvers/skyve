package modules.admin;

import java.util.HashMap;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.Interceptor;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;

import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;
import modules.admin.domain.UserLoginRecord;

public class RDBMSAuditInterceptor extends Interceptor {
	private static final ThreadLocal<Map<String, Operation>> BIZ_ID_TO_OPERATION = new ThreadLocal<>();

	@Override
	public boolean beforeSave(Document document, PersistentBean bean) throws Exception {
		if (! (UserLoginRecord.DOCUMENT_NAME.equals(document.getName()) && 
				UserLoginRecord.MODULE_NAME.equals(document.getOwningModuleName()))) {
			if (bean.isPersisted()) {
				ensureOriginalInsertAuditExists(bean);
				setThreadLocalOperation(bean.getBizId(), Operation.update);
			}
			else {
				setThreadLocalOperation(bean.getBizId(), Operation.insert);
			}
		}
		
		return false;
	}

	@Override
	public void afterSave(Document document, final PersistentBean result) throws Exception {
		Operation operation = getThreadLocalOperation(result.getBizId());
		if (operation != null) {
			audit(result, operation, false);
		}
		removeThreadLocalOperation(result.getBizId());
	}

	@Override
	public void afterDelete(Document document, PersistentBean bean) throws Exception {
		if (bean instanceof Audit){
			// do not audit removal of audits
		}
		else {
			audit(bean, Operation.delete, false);
		}
	}
	
	// Ensure an insert audit either exists already or is inserted
	private static void ensureOriginalInsertAuditExists(PersistentBean bean) throws Exception {
		Persistence p = CORE.getPersistence();
		Customer c = p.getUser().getCustomer();

		// check to see if an audit is required
		Module am = c.getModule(bean.getBizModule());
		Document ad = am.getDocument(c, bean.getBizDocument());
		if (ad.isAudited()) {
			// Check if there exists an insert audit record.
			Module m = c.getModule(Audit.MODULE_NAME);
			@SuppressWarnings("null")
			String persistentIdentifier = m.getDocument(c, Audit.DOCUMENT_NAME).getPersistent().getPersistentIdentifier();

			// Cater for multi-tenancy
			String sql = (UtilImpl.CUSTOMER == null) ?
							String.format("select %s from %s where %s = :%s and %s = :%s and %s = :%s", 
											Bean.DOCUMENT_ID, 
											persistentIdentifier,
											Audit.auditBizIdPropertyName,
											Audit.auditBizIdPropertyName,
											Bean.CUSTOMER_NAME,
											Bean.CUSTOMER_NAME,
											Audit.operationPropertyName,
											Audit.operationPropertyName) :
							String.format("select %s from %s where %s = :%s and %s = :%s", 
											Bean.DOCUMENT_ID, 
											persistentIdentifier,
											Audit.auditBizIdPropertyName,
											Audit.auditBizIdPropertyName,
											Audit.operationPropertyName,
											Audit.operationPropertyName);
												
			SQL q = p.newSQL(sql);
			q.putParameter(Audit.auditBizIdPropertyName, bean.getBizId(), false);
			if (UtilImpl.CUSTOMER == null) { // multi-tenant
				q.putParameter(Bean.CUSTOMER_NAME, c.getName(), false);
			}
			q.putParameter(Audit.operationPropertyName, Operation.insert);
	
	
			// if not we need to create one
			if (q.scalarResults(String.class).isEmpty()) {
				// To do this we need to get the database state before this update operation
				// We can do this by getting a new persistence and loading the record,
				// getting the JSON for it and then inserting it in our current thread's persistence.
				AbstractHibernatePersistence tempP = (AbstractHibernatePersistence) AbstractPersistence.newInstance();
				try {
					tempP.setUser(p.getUser());
					tempP.begin();
					PersistentBean oldBean = tempP.retrieve(bean.getBizModule(), bean.getBizDocument(), bean.getBizId());
					// oldBean can be null when the bean was inserted and updated within this transaction but not yet committed
					// ie tempP can't see the bean yet on another DB connection
					if (oldBean == null) {
						audit(bean, Operation.insert, true);
					}
					else {
						audit(oldBean, Operation.insert, true);
					}
				}
				finally {
					try {
						tempP.rollback();
					}
					finally {
						// Can't call tempP.commit(true) here as it would remove the current thread's Persistence as well
						tempP.close();
					}
				}
			}
		}
	}
	
	private static void audit(PersistentBean bean, Operation operation, boolean originalInsert) throws Exception {
		Persistence p = CORE.getPersistence();
		User u = p.getUser();
		Customer c = u.getCustomer();
		
		// check to see if an audit is required
		Module am = c.getModule(bean.getBizModule());
		Document ad = am.getDocument(c, bean.getBizDocument());
		if (ad.isAudited()) {
			Audit a = Audit.newInstance();

			AuditJSONGenerator generator = new AuditJSONGenerator(c);
			generator.visit(ad, bean, c);
			a.setAuditDetail(generator.toJSON());
			
			a.setAuditModuleName(bean.getBizModule());
			a.setAuditDocumentName(bean.getBizDocument());
			a.setAuditBizId(bean.getBizId());
			
			int bizKeyLength = AbstractPersistence.getBizKeyLength();
			String bizKey = bean.getBizKey();
			if (bizKey.length() > bizKeyLength) {
				bizKey = bizKey.substring(0, bizKeyLength);
			}
			a.setAuditBizKey(bizKey);
			
			if (originalInsert) {
				OptimisticLock lock = bean.getBizLock();
				long millis = lock.getTimestamp().getTime();
				a.setMillis(Long.valueOf(millis));
				a.setTimestamp(new Timestamp(millis));
				a.setUserName(lock.getUsername());
				a.setOperation(Operation.insert);
			}
			else {
				long millis = System.currentTimeMillis();
				a.setMillis(Long.valueOf(millis));
				a.setTimestamp(new Timestamp(millis));
				a.setUserName(u.getName());
				a.setOperation(operation);
			}
			p.upsertBeanTuple(a);
		}
	}
	
	public static void audit(PersistentBean bean, Operation operation) throws Exception {
		ensureOriginalInsertAuditExists(bean);
		audit(bean, operation, false);
	}
	
	private static void setThreadLocalOperation(String bizId, Operation operation) {
		Map<String, Operation> map = BIZ_ID_TO_OPERATION.get();
		if (map == null) {
			map = new HashMap<>();
			BIZ_ID_TO_OPERATION.set(map);
		}
		map.put(bizId, operation);
	}
	
	private static Operation getThreadLocalOperation(String bizId) {
		Map<String, Operation> map = BIZ_ID_TO_OPERATION.get();
		return (map == null) ? null : map.get(bizId);
	}
	
	private static void removeThreadLocalOperation(String bizId) {
		Map<String, Operation> map = BIZ_ID_TO_OPERATION.get();
		if (map != null) {
			map.remove(bizId);
			if (map.isEmpty()) {
				BIZ_ID_TO_OPERATION.remove();
			}
		}
	}
}
