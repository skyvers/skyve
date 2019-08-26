package org.skyve.impl.cdi;

import java.util.List;

import javax.enterprise.inject.Alternative;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.persistence.BizQL;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;

/**
 * A proxy that can be Serialized with no state and continue to work after deserialization.
 * 
 * @author mike
 */
@Alternative
public class PersistenceInjectable implements Persistence {
	private static final long serialVersionUID = -780973092711075624L;

	@Override
	public User getUser() {
		return CORE.getPersistence().getUser();
	}

	@Override
	public boolean isPersisted(Bean bean) {
		return CORE.getPersistence().isPersisted(bean);
	}

	@Override
	public void begin() {
		CORE.getPersistence().begin();
	}

	@Override
	public void rollback() {
		CORE.getPersistence().rollback();
	}

	@Override
	public void setRollbackOnly() {
		CORE.getPersistence().setRollbackOnly();
	}

	@Override
	public void commit(boolean close) {
		CORE.getPersistence().commit(close);
	}

	@Override
	public void evictAllCached() {
		CORE.getPersistence().evictAllCached();
	}

	@Override
	public void evictCached(Bean bean) {
		CORE.getPersistence().evictCached(bean);
	}

	@Override
	public void refresh(Bean bean) {
		CORE.getPersistence().refresh(bean);
	}

	@Override
	public void preMerge(Document document, Bean beanToSave) {
		CORE.getPersistence().preMerge(document, beanToSave);
	}

	@Override
	public <T extends PersistentBean> T save(Document document, T bean) {
		return CORE.getPersistence().save(document, bean);
	}

	@Override
	public <T extends PersistentBean> T save(T bean) {
		return CORE.getPersistence().save(bean);
	}

	@Override
	public <T extends PersistentBean> List<T> save(List<T> beans) {
		return CORE.getPersistence().save(beans);
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T extends PersistentBean> List<T> save(T... beans) {
		return CORE.getPersistence().save(beans);
	}

	@Override
	public <T extends PersistentBean> T merge(Document document, T bean) {
		return CORE.getPersistence().merge(document, bean);
	}

	@Override
	public <T extends PersistentBean> T merge(T bean) {
		return CORE.getPersistence().merge(bean);
	}

	@Override
	public <T extends PersistentBean> List<T> merge(List<T> beans) {
		return CORE.getPersistence().merge(beans);
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T extends PersistentBean> List<T> merge(T... beans) {
		return CORE.getPersistence().merge(beans);
	}

	@Override
	public void postMerge(Document document, Bean beanToSave) {
		CORE.getPersistence().postMerge(document, beanToSave);
	}

	@Override
	public void flush() {
		CORE.getPersistence().flush();
	}

	@Override
	public <T extends PersistentBean> void delete(Document document, T bean) {
		CORE.getPersistence().delete(document, bean);
	}

	@Override
	public <T extends PersistentBean> void delete(T bean) {
		CORE.getPersistence().delete(bean);
	}

	@Override
	public void upsertBeanTuple(PersistentBean bean) {
		CORE.getPersistence().upsertBeanTuple(bean);
	}

	@Override
	public void upsertCollectionTuples(PersistentBean owningBean, String collectionName) {
		CORE.getPersistence().upsertCollectionTuples(owningBean, collectionName);
	}

	@Override
	public void insertCollectionTuples(PersistentBean owningBean, String collectionName) {
		CORE.getPersistence().insertCollectionTuples(owningBean, collectionName);
	}

	@Override
	public <T extends Bean> T retrieve(Document document, String id) {
		return CORE.getPersistence().retrieve(document, id);
	}

	@Override
	public <T extends Bean> T retrieveAndLock(Document document, String id) {
		return CORE.getPersistence().retrieveAndLock(document, id);
	}

	@Override
	public <T extends Bean> T retrieve(String moduleName, String documentName, String id) {
		return CORE.getPersistence().retrieve(moduleName, documentName, id);
	}

	@Override
	public <T extends Bean> T retrieveAndLock(String moduleName, String documentName, String id) {
		return CORE.getPersistence().retrieveAndLock(moduleName, documentName, id);
	}

	@Override
	public void reindex(PersistentBean beanToReindex) throws Exception {
		CORE.getPersistence().reindex(beanToReindex);
	}

	@Override
	public void setDocumentPermissionScopes(DocumentPermissionScope scope) {
		CORE.getPersistence().setDocumentPermissionScopes(scope);
	}

	@Override
	public void resetDocumentPermissionScopes() {
		CORE.getPersistence().resetDocumentPermissionScopes();
	}

	@Override
	public SQL newSQL(String query) {
		return CORE.getPersistence().newSQL(query);
	}

	@Override
	public SQL newNamedSQL(String moduleName, String queryName) {
		return CORE.getPersistence().newNamedSQL(moduleName, queryName);
	}

	@Override
	public SQL newNamedSQL(Module module, String queryName) {
		return CORE.getPersistence().newNamedSQL(module, queryName);
	}

	@Override
	public SQL newSQL(String moduleName, String documentName, String query) {
		return CORE.getPersistence().newSQL(moduleName, documentName, query);
	}

	@Override
	public SQL newNamedSQL(String moduleName, String documentName, String queryName) {
		return CORE.getPersistence().newNamedSQL(moduleName, documentName, queryName);
	}

	@Override
	public SQL newSQL(Document document, String query) {
		return CORE.getPersistence().newSQL(document, query);
	}

	@Override
	public SQL newNamedSQL(Document document, String queryName) {
		return CORE.getPersistence().newNamedSQL(document, queryName);
	}

	@Override
	public BizQL newBizQL(String query) {
		return CORE.getPersistence().newBizQL(query);
	}

	@Override
	public BizQL newNamedBizQL(String moduleName, String queryName) {
		return CORE.getPersistence().newNamedBizQL(moduleName, queryName);
	}

	@Override
	public BizQL newNamedBizQL(Module module, String queryName) {
		return CORE.getPersistence().newNamedBizQL(module, queryName);
	}

	@Override
	public DocumentQuery newNamedDocumentQuery(String moduleName, String queryName) {
		return CORE.getPersistence().newNamedDocumentQuery(moduleName, queryName);
	}

	@Override
	public DocumentQuery newNamedDocumentQuery(Module module, String queryName) {
		return CORE.getPersistence().newNamedDocumentQuery(module, queryName);
	}

	@Override
	public DocumentQuery newDocumentQuery(Document document) {
		return CORE.getPersistence().newDocumentQuery(document);
	}

	@Override
	public DocumentQuery newDocumentQuery(String moduleName, String documentName) {
		return CORE.getPersistence().newDocumentQuery(moduleName, documentName);
	}

	@Override
	public DocumentQuery newDocumentQuery(Document document, String fromClause, String filterClause) {
		return CORE.getPersistence().newDocumentQuery(document, fromClause, filterClause);
	}

	@Override
	public DocumentQuery newDocumentQuery(Bean queryByExampleBean) throws Exception {
		return CORE.getPersistence().newDocumentQuery(queryByExampleBean);
	}
}
