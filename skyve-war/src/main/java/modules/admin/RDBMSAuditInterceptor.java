package modules.admin;

import java.util.HashMap;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.controller.Interceptor;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

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
			audit(result, operation);
		}
		removeThreadLocalOperation(result.getBizId());
	}

	@Override
	public void afterDelete(Document document, PersistentBean bean) throws Exception {
		if (bean instanceof Audit){
			// do not audit removal of audits
		}
		else {
			audit(bean, Operation.delete);
		}
	}
	
	public static void audit(PersistentBean bean, Operation operation) throws Exception {
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
			
			long millis = System.currentTimeMillis();
			a.setMillis(Long.valueOf(millis));
			a.setTimestamp(new Timestamp(millis));
			a.setUserName(u.getName());
			a.setOperation(operation);

			p.upsertBeanTuple(a);
		}
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
