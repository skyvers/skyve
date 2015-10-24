package modules;

import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;
import modules.admin.domain.UserLoginRecord;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.controller.Interceptor;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.wildcat.persistence.hibernate.AbstractHibernatePersistence;

public class RDBMSAuditInterceptor extends Interceptor {
	private static final long serialVersionUID = 8133933539853560711L;

	private static final ThreadLocal<Operation> OPERATION = new ThreadLocal<>();

	@Override
	public boolean beforeSave(Document document, PersistentBean bean) throws Exception {
		if (! (UserLoginRecord.DOCUMENT_NAME.equals(document.getName()) && 
				UserLoginRecord.MODULE_NAME.equals(document.getOwningModuleName()))) {
			if (bean.isPersisted()) {
				ensureOriginalInsertAuditExists(bean);
				OPERATION.set(Operation.update);
			}
			else {
				OPERATION.set(Operation.insert);
			}
		}
		
		return false;
	}

	@Override
	public void afterSave(Document document, final PersistentBean result) throws Exception {
		Operation operation = OPERATION.get();
		if (operation != null) {
			audit(result, operation, false);
		}
		OPERATION.remove();
	}

	@Override
	public void afterDelete(Document document, PersistentBean bean) throws Exception {
		audit(bean, Operation.delete, false);
	}
	
	// Ensure an insert audit either exists already or is inserted
	private static void ensureOriginalInsertAuditExists(PersistentBean bean) throws Exception {
		// Check if we have an insert audit record.
		Persistence p = CORE.getPersistence();
		DocumentQuery q = p.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q.addBoundProjection(Bean.DOCUMENT_ID);
		DocumentFilter f = q.getFilter();
		f.addEquals(Audit.auditBizIdPropertyName, bean.getBizId());
		f.addEquals(Audit.operationPropertyName, Operation.insert);

		// if not we need to create one
		if (q.scalarResults(String.class).isEmpty()) {
			// To do this we need to get the database state before this update operation
			// We can do this by getting a new persistence and loading the record,
			// getting the JSON for it and then inserting it in our current thread's persistence.
			AbstractHibernatePersistence tempP = (AbstractHibernatePersistence) p.getClass().newInstance();
			try {
				tempP.setUser(p.getUser());
				PersistentBean oldBean = tempP.retrieve(bean.getBizModule(), bean.getBizDocument(), bean.getBizId(), false);
				audit(oldBean, Operation.insert, true);
			}
			finally {
				// Can't call tempP.commit(true) here as it would remove the current thread's Persistence as well
				tempP.getEntityManager().close();
			}
		}
	}
	
	private static void audit(PersistentBean bean, Operation operation, boolean originalInsert) 
	throws Exception {
		Persistence p = CORE.getPersistence();
		User u = p.getUser();
		Customer c = u.getCustomer();
		Audit a = Audit.newInstance();
		
		Module am = c.getModule(bean.getBizModule());
		Document ad = am.getDocument(c, bean.getBizDocument());
		AuditJSONGenerator generator = new AuditJSONGenerator(c);
		generator.visit(ad, bean, c);
		a.setAudit(generator.toJSON());
		a.setAuditModuleName(bean.getBizModule());
		a.setAuditDocumentName(bean.getBizDocument());
		a.setAuditBizId(bean.getBizId());
		a.setAuditBizKey(bean.getBizKey());
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
	
	public static void audit(PersistentBean bean, Operation operation)
	throws Exception {
		ensureOriginalInsertAuditExists(bean);
		audit(bean, operation, false);
	}
}
