package org.skyve.app.interceptor;

import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;
import modules.admin.domain.UserLoginRecord;

import org.skyve.CORE;
import org.skyve.app.interceptor.AuditJSONGenerator;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.controller.Interceptor;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;

public class RDBMSAuditInterceptor extends Interceptor {
	private static final long serialVersionUID = 8133933539853560711L;

	private static final ThreadLocal<Operation> OPERATION = new ThreadLocal<>();

	@Override
	public boolean beforeSave(Document document, PersistentBean bean) throws Exception {
		if (! (UserLoginRecord.DOCUMENT_NAME.equals(document.getName()) && 
				UserLoginRecord.MODULE_NAME.equals(document.getOwningModuleName()))) {
			if (bean.isPersisted()) {
				if (Util.hasChanged(bean)) {
					OPERATION.set(Operation.update);
				}
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
			audit(result, operation);
		}
		OPERATION.remove();
	}

	@Override
	public void afterDelete(Document document, PersistentBean bean) throws Exception {
		audit(bean, Operation.delete);
	}
	
	private static void audit(PersistentBean bean, Operation operation) 
	throws Exception {
		Persistence p = CORE.getPersistence();
		User u = p.getUser();
		Customer c = u.getCustomer();
		Module m = c.getModule(Audit.MODULE_NAME);
		Document d = m.getDocument(c, Audit.DOCUMENT_NAME);
		Audit a = d.newInstance(u);
		
		Module am = c.getModule(bean.getBizModule());
		Document ad = am.getDocument(c, bean.getBizDocument());
		AuditJSONGenerator generator = new AuditJSONGenerator(c);
		generator.visit(ad, bean, c);
		a.setAudit(generator.toJSON());
		a.setAuditModuleName(bean.getBizModule());
		a.setAuditDocumentName(bean.getBizDocument());
		a.setAuditBizId(bean.getBizId());
		a.setAuditBizKey(bean.getBizKey());
		a.setAuditBizVersion(bean.getBizVersion());
		a.setTimestamp(new Timestamp());
		a.setOperation(operation);
		a.setUser(u.getName());
		p.upsertBeanTuple(a);
	}
}
