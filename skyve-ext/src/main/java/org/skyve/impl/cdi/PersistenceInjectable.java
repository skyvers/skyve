package org.skyve.impl.cdi;

import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.enterprise.inject.Alternative;
import jakarta.persistence.EntityManager;

/**
 * A proxy that can be Serialized with no state and continue to work after deserialization.
 * 
 * @author mike
 */
@Alternative
public class PersistenceInjectable implements Persistence {
	private static final long serialVersionUID = -780973092711075624L;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull User getUser() {
		return CORE.getPersistence().getUser();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean isPersisted(@Nonnull Bean bean) {
		return CORE.getPersistence().isPersisted(bean);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void begin() {
		CORE.getPersistence().begin();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void rollback() {
		CORE.getPersistence().rollback();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setRollbackOnly() {
		CORE.getPersistence().setRollbackOnly();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void commit(boolean close) {
		CORE.getPersistence().commit(close);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void evictAllCached() {
		CORE.getPersistence().evictAllCached();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void evictCached(@Nonnull Bean bean) {
		CORE.getPersistence().evictCached(bean);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean cached(@Nonnull Bean bean) {
		return CORE.getPersistence().cached(bean);
	}

	@Override
	public boolean sharedCacheCollection(@Nonnull String moduleName,
											@Nonnull String documentName,
											@Nonnull String collectionName,
											@Nonnull String ownerBizId) {
		return CORE.getPersistence().sharedCacheCollection(moduleName, documentName, collectionName, ownerBizId);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean sharedCacheCollection(@Nonnull Bean owner, @Nonnull String collectionName) {
		return CORE.getPersistence().sharedCacheCollection(owner, collectionName);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean sharedCacheBean(@Nonnull String moduleName, @Nonnull String documentName, @Nonnull String bizId) {
		return CORE.getPersistence().sharedCacheBean(moduleName, documentName, bizId);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean sharedCacheBean(@Nonnull Bean bean) {
		return CORE.getPersistence().sharedCacheBean(bean);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void evictAllSharedCache() {
		CORE.getPersistence().evictAllSharedCache();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void evictSharedCacheCollections() {
		CORE.getPersistence().evictSharedCacheCollections();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void evictSharedCacheCollections(@Nonnull String moduleName, @Nonnull String documentName, @Nonnull String collectionName) {
		CORE.getPersistence().evictSharedCacheCollections(moduleName, documentName, collectionName);
	}

	@Override
	public void evictSharedCacheCollection(@Nonnull String moduleName,
											@Nonnull String documentName,
											@Nonnull String collectionName,
											@Nonnull String ownerBizId) {
		CORE.getPersistence().evictSharedCacheCollection(moduleName, documentName, collectionName, ownerBizId);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void evictSharedCacheCollection(@Nonnull Bean owner, @Nonnull String collectionName) {
		CORE.getPersistence().evictSharedCacheCollection(owner, collectionName);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void evictSharedCacheBeans() {
		CORE.getPersistence().evictSharedCacheBeans();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void evictSharedCacheBeans(@Nonnull String moduleName, @Nonnull String documentName) {
		CORE.getPersistence().evictSharedCacheBeans(moduleName, documentName);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void evictSharedCachedBean(@Nonnull String moduleName, @Nonnull String documentName, @Nonnull String bizId) {
		CORE.getPersistence().evictSharedCachedBean(moduleName, documentName, bizId);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void evictSharedCachedBean(@Nonnull Bean bean) {
		CORE.getPersistence().evictSharedCachedBean(bean);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void preMerge(@Nonnull Document document, @Nonnull PersistentBean beanToSave) {
		CORE.getPersistence().preMerge(document, beanToSave);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull <T extends PersistentBean> T save(@Nonnull Document document, @Nonnull T bean) {
		return CORE.getPersistence().save(document, bean);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull <T extends PersistentBean> T save(@Nonnull T bean) {
		return CORE.getPersistence().save(bean);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull <T extends PersistentBean> List<T> save(@Nonnull List<T> beans) {
		return CORE.getPersistence().save(beans);
	}

	@Override
	@SuppressWarnings("unchecked")
	/**
	 * Performs save.
	 */
	public @Nonnull <T extends PersistentBean> List<T> save(@Nonnull T... beans) {
		return CORE.getPersistence().save(beans);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull <T extends PersistentBean> T merge(@Nonnull Document document, @Nonnull T bean) {
		return CORE.getPersistence().merge(document, bean);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull <T extends PersistentBean> T merge(@Nonnull T bean) {
		return CORE.getPersistence().merge(bean);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull <T extends PersistentBean> List<T> merge(@Nonnull List<T> beans) {
		return CORE.getPersistence().merge(beans);
	}

	@Override
	@SuppressWarnings("unchecked")
	/**
	 * Performs merge.
	 */
	public @Nonnull <T extends PersistentBean> List<T> merge(@Nonnull T... beans) {
		return CORE.getPersistence().merge(beans);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void postMerge(@Nonnull Document document, @Nonnull PersistentBean beanToSave) {
		CORE.getPersistence().postMerge(document, beanToSave);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void flush() {
		CORE.getPersistence().flush();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public <T extends PersistentBean> void delete(@Nonnull Document document, @Nonnull T bean) {
		CORE.getPersistence().delete(document, bean);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public <T extends PersistentBean> void delete(@Nonnull T bean) {
		CORE.getPersistence().delete(bean);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void upsertBeanTuple(@Nonnull PersistentBean bean) {
		CORE.getPersistence().upsertBeanTuple(bean);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void upsertCollectionTuples(@Nonnull PersistentBean owningBean, @Nonnull String collectionName) {
		CORE.getPersistence().upsertCollectionTuples(owningBean, collectionName);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void insertCollectionTuples(@Nonnull PersistentBean owningBean, @Nonnull String collectionName) {
		CORE.getPersistence().insertCollectionTuples(owningBean, collectionName);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nullable <T extends Bean> T retrieve(@Nonnull Document document, @Nonnull String id) {
		return CORE.getPersistence().retrieve(document, id);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull <T extends Bean> T retrieveAndLock(@Nonnull Document document, @Nonnull String id) {
		return CORE.getPersistence().retrieveAndLock(document, id);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nullable <T extends Bean> T retrieve(@Nonnull String moduleName, @Nonnull String documentName, @Nonnull String id) {
		return CORE.getPersistence().retrieve(moduleName, documentName, id);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull <T extends Bean> T retrieveAndLock(@Nonnull String moduleName, @Nonnull String documentName, @Nonnull String id) {
		return CORE.getPersistence().retrieveAndLock(moduleName, documentName, id);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void reindex(@Nonnull PersistentBean beanToReindex) throws Exception {
		CORE.getPersistence().reindex(beanToReindex);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nullable <R> R withDocumentPermissionScopes(@Nonnull DocumentPermissionScope scope, @Nonnull Function<Persistence, R> function) {
		return CORE.getPersistence().withDocumentPermissionScopes(scope, function);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void withDocumentPermissionScopes(@Nonnull DocumentPermissionScope scope, @Nonnull Consumer<Persistence> consumer) {
		CORE.getPersistence().withDocumentPermissionScopes(scope, consumer);
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull SQL newSQL(@Nonnull String query) {
		return CORE.getPersistence().newSQL(query);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull SQL newNamedSQL(@Nonnull String moduleName, @Nonnull String queryName) {
		return CORE.getPersistence().newNamedSQL(moduleName, queryName);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull SQL newNamedSQL(@Nonnull Module module, @Nonnull String queryName) {
		return CORE.getPersistence().newNamedSQL(module, queryName);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull SQL newSQL(@Nonnull String moduleName, @Nonnull String documentName, @Nonnull String query) {
		return CORE.getPersistence().newSQL(moduleName, documentName, query);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull SQL newNamedSQL(@Nonnull String moduleName, @Nonnull String documentName, @Nonnull String queryName) {
		return CORE.getPersistence().newNamedSQL(moduleName, documentName, queryName);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull SQL newSQL(@Nonnull Document document, @Nonnull String query) {
		return CORE.getPersistence().newSQL(document, query);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull SQL newNamedSQL(@Nonnull Document document, @Nonnull String queryName) {
		return CORE.getPersistence().newNamedSQL(document, queryName);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull BizQL newBizQL(@Nonnull String query) {
		return CORE.getPersistence().newBizQL(query);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull BizQL newNamedBizQL(@Nonnull String moduleName, @Nonnull String queryName) {
		return CORE.getPersistence().newNamedBizQL(moduleName, queryName);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull BizQL newNamedBizQL(@Nonnull Module module, @Nonnull String queryName) {
		return CORE.getPersistence().newNamedBizQL(module, queryName);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull DocumentQuery newNamedDocumentQuery(@Nonnull String moduleName, @Nonnull String queryName) {
		return CORE.getPersistence().newNamedDocumentQuery(moduleName, queryName);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull DocumentQuery newNamedDocumentQuery(@Nonnull Module module, @Nonnull String queryName) {
		return CORE.getPersistence().newNamedDocumentQuery(module, queryName);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull DocumentQuery newDocumentQuery(@Nonnull Document document) {
		return CORE.getPersistence().newDocumentQuery(document);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull DocumentQuery newDocumentQuery(@Nonnull String moduleName, @Nonnull String documentName) {
		return CORE.getPersistence().newDocumentQuery(moduleName, documentName);
	}

	@Override
	public @Nonnull DocumentQuery newDocumentQuery(@Nonnull Document document,
													@Nullable String fromClause,
													@Nullable String filterClause,
													@Nullable String groupClause,
													@Nullable String orderClause) {
		return CORE.getPersistence().newDocumentQuery(document, fromClause, filterClause, groupClause, orderClause);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull DocumentQuery newDocumentQuery(@Nonnull Bean queryByExampleBean) {
		return CORE.getPersistence().newDocumentQuery(queryByExampleBean);
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull EntityManager getEntityManager() {
		return CORE.getPersistence().getEntityManager();
	}
}
