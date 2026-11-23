package modules.admin;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.ModuleDocument;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

import modules.admin.domain.AdminDocument;
import modules.admin.domain.Audit;

@SuppressWarnings("static-method")
class RDBMSAuditInterceptorTest {
	@SuppressWarnings("unchecked")
	private static ThreadLocal<AbstractPersistence> persistenceThreadLocal() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		return (ThreadLocal<AbstractPersistence>) field.get(null);
	}

	private static Document document(ModuleDocument reference) {
		Document result = mock(Document.class);
		when(result.getOwningModuleName()).thenReturn(reference.moduleName());
		when(result.getName()).thenReturn(reference.documentName());
		return result;
	}

	private static PersistentBean bean(String bizId, Boolean persisted) {
		PersistentBean result = mock(PersistentBean.class);
		when(result.getBizId()).thenReturn(bizId);
		doReturn(persisted).when(result).isPersisted();
		when(result.getBizModule()).thenReturn("admin");
		when(result.getBizDocument()).thenReturn("DataGroup");
		return result;
	}

	private static AbstractPersistence bindNonAuditedPersistence() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document metadataDocument = mock(Document.class);
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "DataGroup")).thenReturn(metadataDocument);
		doReturn(Boolean.FALSE).when(metadataDocument).isAudited();
		persistenceThreadLocal().set(persistence);
		return persistence;
	}

	@Test
	void beforeSaveIgnoresUserLoginRecordDocuments() throws Exception {
		RDBMSAuditInterceptor interceptor = new RDBMSAuditInterceptor();
		PersistentBean bean = bean("login-1", Boolean.FALSE);

		boolean veto = interceptor.beforeSave(document(AdminDocument.USER_LOGIN_RECORD), bean);
		interceptor.afterSave(document(AdminDocument.USER_LOGIN_RECORD), bean);

		assertFalse(veto);
	}

	@Test
	void afterSaveAuditsNonLoginDocumentOnceAndThenClearsPendingOperation() throws Exception {
		RDBMSAuditInterceptor interceptor = new RDBMSAuditInterceptor();
		PersistentBean bean = bean("bean-1", Boolean.TRUE);
		AbstractPersistence persistence = bindNonAuditedPersistence();
		try {
			boolean veto = interceptor.beforeSave(document(AdminDocument.DATA_GROUP), bean);
			interceptor.afterSave(document(AdminDocument.DATA_GROUP), bean);
			interceptor.afterSave(document(AdminDocument.DATA_GROUP), bean);

			assertFalse(veto);
			verify(persistence).getUser();
			verify(persistence, never()).upsertBeanTuple(org.mockito.ArgumentMatchers.any(Audit.class));
		}
		finally {
			persistenceThreadLocal().remove();
		}
	}

	@Test
	void afterDeleteSkipsAuditRecords() {
		RDBMSAuditInterceptor interceptor = new RDBMSAuditInterceptor();
		assertDoesNotThrow(() -> interceptor.afterDelete(document(AdminDocument.AUDIT), new Audit()));
	}

	@Test
	void afterDeleteAuditsNonAuditBeansWhenDocumentIsNotAudited() throws Exception {
		RDBMSAuditInterceptor interceptor = new RDBMSAuditInterceptor();
		PersistentBean bean = bean("bean-2", Boolean.FALSE);
		AbstractPersistence persistence = bindNonAuditedPersistence();
		try {
			interceptor.afterDelete(document(AdminDocument.DATA_GROUP), bean);

			verify(persistence).getUser();
			verify(persistence, never()).upsertBeanTuple(org.mockito.ArgumentMatchers.any(Audit.class));
		}
		finally {
			persistenceThreadLocal().remove();
		}
	}
}
