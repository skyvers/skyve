package org.skyve.persistence;

import java.io.Serializable;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;

/**
 * 
 */
public interface Persistence extends Serializable {
	/**
	 * 
	 * @return
	 */
	public User getUser();
	
	/**
	 * 
	 * @param bean
	 * @return
	 */
	public boolean isPersisted(Bean bean);
	
	/**
	 * Begin a transaction.
	 */
	public void begin();
	
	/**
	 * Rollback immediately.
	 */
	public void rollback();
	
	/**
	 * Rollback at the end of the transaction.
	 */
	public void setRollbackOnly();
	
	/**
	 * Commit the transaction (and optionally close the associated resources).
	 * @param close
	 */
	public void commit(boolean close);
	
	/**
	 * 
	 */
	public void evictAllCached();
	
	/**
	 * 
	 * @param bean
	 */
	public void evictCached(Bean bean);

	public boolean cached(Bean bean);
	
	public boolean sharedCacheCollection(String moduleName, String documentName, String collectionName, String ownerBizId);
	
	public boolean sharedCacheCollection(Bean owner, String collectionName);
	
	public boolean sharedCacheBean(String moduleName, String documentName, String bizId);

	public boolean sharedCacheBean(Bean bean);
	
	public void evictAllSharedCache();
	
	public void evictSharedCacheCollections();
	
	public void evictSharedCacheCollections(String moduleName, String documentName, String collectionName);
	
	public void evictSharedCacheCollection(String moduleName, String documentName, String collectionName, String ownerBizId);
	
	public void evictSharedCacheCollection(Bean owner, String collectionName);
	
	public void evictSharedCacheBeans();
	
	public void evictSharedCacheBeans(String moduleName, String documentName);
	
	public void evictSharedCachedBean(String moduleName, String documentName, String bizId);
	
	public void evictSharedCachedBean(Bean bean);
	
	/**
	 * Processing that occurs before the merging of beans into the persistent context.
	 * This method can either be hijacked for a Persistence implementation or used in 
	 * application code to enable the same behaviour as skyve applies pre save/merge - eg before an upsert.
	 * @param document
	 * @param beanToSave
	 */
	public void preMerge(Document document, PersistentBean beanToSave);
	
	/**
	 * Merge and flush.
	 * @param document
	 * @param bean
	 * @return
	 */
	public <T extends PersistentBean> T save(Document document, T bean);
	
	/**
	 * Merge and flush.
	 * @param bean
	 * @return
	 */
	public <T extends PersistentBean> T save(T bean);

	/**
	 * Merge and flush.
	 * @param beans
	 * @return
	 */
	public <T extends PersistentBean> List<T> save(List<T> beans);

	/**
	 * Merge and flush.
	 * @param beans
	 * @return
	 */
	public <T extends PersistentBean> List<T> save(@SuppressWarnings("unchecked") T... beans);

	/**
	 * Merge the bean into the persistent content (no flush).
	 * @param document
	 * @param bean
	 * @return
	 */
	public <T extends PersistentBean> T merge(Document document, T bean);
	
	/**
	 * Merge the bean into the persistent content (no flush).
	 * @param bean
	 * @return
	 */
	public <T extends PersistentBean> T merge(T bean);

	/**
	 * Merge the bean into the persistent content (no flush).
	 * @param beans
	 * @return
	 */
	public <T extends PersistentBean> List<T> merge(List<T> beans);

	/**
	 * Merge the bean into the persistent content (no flush).
	 * @param beans
	 * @return
	 */
	public <T extends PersistentBean> List<T> merge(@SuppressWarnings("unchecked") T... beans);

	/**
	 * Processing that occurs after the merging of beans into the persistent context.
	 * This method can either be hijacked for a Persistence implementation or used in 
	 * application code to enable the same behaviour as skyve applies post save/merge - eg after an upsert.
	 * @param document
	 * @param beanToSave
	 */
	public void postMerge(Document document, PersistentBean beanToSave);
	
	/**
	 * Execute the DML to synchronize the persistent context with the data store.
	 */
	public void flush();
	
	/**
	 * 
	 * @param document
	 * @param bean
	 */
	public <T extends PersistentBean> void delete(Document document, T bean);
	
	/**
	 * 
	 * @param bean
	 */
	public <T extends PersistentBean> void delete(T bean);

	/**
	 * 
	 * @param bean
	 */
	public void upsertBeanTuple(PersistentBean bean);

	/**
	 * 
	 * @param owningBean
	 * @param collectionName
	 */
	public void upsertCollectionTuples(PersistentBean owningBean, String collectionName);

	/**
	 * 
	 * @param owningBean
	 * @param collectionName
	 */
	public void insertCollectionTuples(PersistentBean owningBean, String collectionName);

	/**
	 * 
	 * @param document
	 * @param id
	 * @return
	 */
	public <T extends Bean> T retrieve(Document document, String id);

	/**
	 * Retrieve and place a write lock on the data store.
	 * @param document
	 * @param id
	 * @return
	 */
	public <T extends Bean> T retrieveAndLock(Document document, String id);

	/**
	 * 
	 * @param moduleName
	 * @param documentName
	 * @param id
	 * @return
	 */
	public <T extends Bean> T retrieve(String moduleName, String documentName, String id);

	/**
	 * Retrieve and place a write lock on the data store.
	 * @param moduleName
	 * @param documentName
	 * @param id
	 * @return
	 */
	public <T extends Bean> T retrieveAndLock(String moduleName, String documentName, String id);


	/**
	 * 
	 * @param beanToReindex
	 * @throws Exception
	 */
	public void reindex(PersistentBean beanToReindex) throws Exception;

	/**
	 * Sets all document scopes for this persistence instance.
	 * @param scope	The scope to use for ALL documents.
	 */
	public void setDocumentPermissionScopes(DocumentPermissionScope scope);

	/**
	 * Resets ALL the document scopes to what the user is allowed to access.
	 */
	public void resetDocumentPermissionScopes();
	
	/**
	 * 
	 * @param query
	 * @return
	 */
	public SQL newSQL(String query);	
	
	public SQL newNamedSQL(String moduleName, String queryName);	

	public SQL newNamedSQL(Module module, String queryName);

	/**
	 * 
	 * @param query
	 * @return
	 */
	public SQL newSQL(String moduleName, String documentName, String query);

	public SQL newNamedSQL(String moduleName, String documentName, String queryName);
	
	/**
	 * 
	 * @param query
	 * @return
	 */
	public SQL newSQL(Document document, String query);	

	public SQL newNamedSQL(Document document, String queryName);	

	/**
	 * 
	 * @param query
	 * @return
	 */
	public BizQL newBizQL(String query);	
	
	public BizQL newNamedBizQL(String moduleName, String queryName);	

	public BizQL newNamedBizQL(Module module, String queryName);

	public DocumentQuery newNamedDocumentQuery(String moduleName, String queryName);

	public DocumentQuery newNamedDocumentQuery(Module module, String queryName);

	/**
	 * 
	 * @param document
	 * @return
	 */
	public DocumentQuery newDocumentQuery(Document document);
	
	/**
	 * 
	 * @param moduleName
	 * @param documentName
	 * @return
	 */
	public DocumentQuery newDocumentQuery(String moduleName, String documentName);	
	
	/**
	 * 
	 * @param document
	 * @param fromClause
	 * @param filterClause
	 * @return
	 */
	public DocumentQuery newDocumentQuery(Document document, String fromClause, String filterClause);

	/**
	 * 
	 * @param queryByExampleBean	A bean which will be used to create a document query based on the values
	 * 								in the scalar attributes only.
	 * @return
	 * @throws Exception
	 */
	public DocumentQuery newDocumentQuery(Bean queryByExampleBean)
	throws Exception;
}
