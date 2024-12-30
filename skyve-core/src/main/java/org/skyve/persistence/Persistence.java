package org.skyve.persistence;

import java.io.Serializable;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.ReferentialConstraintViolationException;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * 
 */
public interface Persistence extends Serializable {
	/**
	 * 
	 * @return
	 */
	@Nonnull User getUser();
	
	/**
	 * 
	 * @param bean
	 * @return
	 */
	boolean isPersisted(@Nonnull Bean bean);
	
	/**
	 * Begin a transaction.
	 */
	void begin();
	
	/**
	 * Rollback immediately.
	 */
	void rollback();
	
	/**
	 * Rollback at the end of the transaction.
	 */
	void setRollbackOnly();
	
	/**
	 * Commit the transaction (and optionally close the associated resources).
	 * @param close
	 */
	void commit(boolean close);
	
	/**
	 * 
	 */
	void evictAllCached();
	
	/**
	 * 
	 * @param bean
	 */
	void evictCached(@Nonnull Bean bean);

	boolean cached(@Nonnull Bean bean);
	
	boolean sharedCacheCollection(@Nonnull String moduleName, 
									@Nonnull String documentName,
									@Nonnull String collectionName,
									@Nonnull String ownerBizId);
	
	boolean sharedCacheCollection(@Nonnull Bean owner, @Nonnull String collectionName);
	
	boolean sharedCacheBean(@Nonnull String moduleName, @Nonnull String documentName, @Nonnull String bizId);

	boolean sharedCacheBean(@Nonnull Bean bean);
	
	void evictAllSharedCache();
	
	void evictSharedCacheCollections();
	
	void evictSharedCacheCollections(@Nonnull String moduleName,
										@Nonnull String documentName,
										@Nonnull String collectionName);
	
	void evictSharedCacheCollection(@Nonnull String moduleName,
										@Nonnull String documentName,
										@Nonnull String collectionName,
										@Nonnull String ownerBizId);
	
	void evictSharedCacheCollection(@Nonnull Bean owner, @Nonnull String collectionName);
	
	void evictSharedCacheBeans();
	
	void evictSharedCacheBeans(@Nonnull String moduleName, @Nonnull String documentName);
	
	void evictSharedCachedBean(@Nonnull String moduleName, @Nonnull String documentName, @Nonnull String bizId);
	
	void evictSharedCachedBean(@Nonnull Bean bean);
	
	/**
	 * Processing that occurs before the merging of beans into the persistent context.
	 * This method can either be hijacked for a Persistence implementation or used in 
	 * application code to enable the same behaviour as skyve applies pre save/merge - eg before an upsert.
	 * @param document
	 * @param beanToSave
	 */
	void preMerge(@Nonnull Document document, @Nonnull PersistentBean beanToSave);
	
	/**
	 * Merge and flush.
	 * @param document
	 * @param bean
	 * @return
	 */
	@Nonnull <T extends PersistentBean> T save(@Nonnull Document document, @Nonnull T bean);
	
	/**
	 * Merge and flush.
	 * @param bean
	 * @return
	 */
	@Nonnull <T extends PersistentBean> T save(@Nonnull T bean);

	/**
	 * Merge and flush.
	 * @param beans
	 * @return
	 */
	@Nonnull <T extends PersistentBean> List<T> save(@Nonnull List<T> beans);

	/**
	 * Merge and flush.
	 * @param beans
	 * @return
	 */
	@Nonnull <T extends PersistentBean> List<T> save(@SuppressWarnings("unchecked") @Nonnull T... beans);

	/**
	 * Merge the bean into the persistent content (no flush).
	 * @param document
	 * @param bean
	 * @return
	 */
	@Nonnull <T extends PersistentBean> T merge(@Nonnull Document document, @Nonnull T bean);
	
	/**
	 * Merge the bean into the persistent content (no flush).
	 * @param bean
	 * @return
	 */
	@Nonnull <T extends PersistentBean> T merge(@Nonnull T bean);

	/**
	 * Merge the bean into the persistent content (no flush).
	 * @param beans
	 * @return
	 */
	@Nonnull <T extends PersistentBean> List<T> merge(@Nonnull List<T> beans);

	/**
	 * Merge the bean into the persistent content (no flush).
	 * @param beans
	 * @return
	 */
	@Nonnull <T extends PersistentBean> List<T> merge(@SuppressWarnings("unchecked") @Nonnull T... beans);

	/**
	 * Processing that occurs after the merging of beans into the persistent context.
	 * This method can either be hijacked for a Persistence implementation or used in 
	 * application code to enable the same behaviour as skyve applies post save/merge - eg after an upsert.
	 * @param document
	 * @param beanToSave
	 */
	public void postMerge(@Nonnull Document document, @Nonnull PersistentBean beanToSave);
	
	/**
	 * Execute the DML to synchronize the persistent context with the data store.
	 */
	public void flush();
	
	/**
	 * 
	 * @param document
	 * @param bean
	 * @throws ReferentialConstraintViolationException	When the bean cannot be deleted because something is referencing it.
	 */
	<T extends PersistentBean> void delete(@Nonnull Document document, @Nonnull T bean)
	throws ReferentialConstraintViolationException;
	
	/**
	 * 
	 * @param bean
	 * @throws ReferentialConstraintViolationException	When the bean cannot be deleted because something is referencing it.
	 */
	<T extends PersistentBean> void delete(@Nonnull T bean)
	throws ReferentialConstraintViolationException;

	/**
	 * 
	 * @param bean
	 */
	void upsertBeanTuple(@Nonnull PersistentBean bean);

	/**
	 * 
	 * @param owningBean
	 * @param collectionName
	 */
	void upsertCollectionTuples(@Nonnull PersistentBean owningBean, @Nonnull String collectionName);

	/**
	 * 
	 * @param owningBean
	 * @param collectionName
	 */
	void insertCollectionTuples(@Nonnull PersistentBean owningBean, @Nonnull String collectionName);

	/**
	 * 
	 * @param document
	 * @param id
	 * @return
	 */
	@Nullable <T extends Bean> T retrieve(@Nonnull Document document, @Nonnull String id);

	/**
	 * Retrieve and place a write lock on the data store.
	 * @param document
	 * @param id
	 * @return
	 */
	@Nonnull <T extends Bean> T retrieveAndLock(@Nonnull Document document, @Nonnull String id);

	/**
	 * 
	 * @param moduleName
	 * @param documentName
	 * @param id
	 * @return
	 */
	@Nullable <T extends Bean> T retrieve(@Nonnull String moduleName,
											@Nonnull String documentName,
											@Nonnull String id);

	/**
	 * Retrieve and place a write lock on the data store.
	 * @param moduleName
	 * @param documentName
	 * @param id
	 * @return
	 */
	@Nonnull <T extends Bean> T retrieveAndLock(@Nonnull String moduleName,
													@Nonnull String documentName,
													@Nonnull String id);


	/**
	 * 
	 * @param beanToReindex
	 * @throws Exception
	 */
	void reindex(@Nonnull PersistentBean beanToReindex) throws Exception;

	/**
	 * Sets all document scopes for this persistence instance and applies the function.
	 * It is guaranteed that the document scopes are reset at the end of the method call.
	 * @param scope	The scope to use for ALL documents.
	 * @param function	The function to apply.
	 */
	@Nullable <R> R withDocumentPermissionScopes(@Nonnull DocumentPermissionScope scope,
													@Nonnull Function<Persistence, R> function);

	/**
	 * Sets all document scopes for this persistence instance and accepts the consumer.
	 * It is guaranteed that the document scopes are reset at the end of the method call.
	 * @param scope	The scope to use for ALL documents.
	 * @param function	The function to apply.
	 */
	void withDocumentPermissionScopes(@Nonnull DocumentPermissionScope scope,
										@Nonnull Consumer<Persistence> consumer);

	/**
	 * 
	 * @param query
	 * @return
	 */
	@Nonnull SQL newSQL(@Nonnull String query);	
	
	@Nonnull SQL newNamedSQL(@Nonnull String moduleName, @Nonnull String queryName);	

	@Nonnull SQL newNamedSQL(@Nonnull Module module, @Nonnull String queryName);

	/**
	 * 
	 * @param query
	 * @return
	 */
	@Nonnull SQL newSQL(@Nonnull String moduleName, @Nonnull String documentName, @Nonnull String query);

	@Nonnull SQL newNamedSQL(@Nonnull String moduleName, @Nonnull String documentName, @Nonnull String queryName);
	
	/**
	 * 
	 * @param query
	 * @return
	 */
	@Nonnull SQL newSQL(@Nonnull Document document, @Nonnull String query);	

	@Nonnull SQL newNamedSQL(@Nonnull Document document, @Nonnull String queryName);	

	/**
	 * 
	 * @param query
	 * @return
	 */
	@Nonnull BizQL newBizQL(@Nonnull String query);	
	
	@Nonnull BizQL newNamedBizQL(@Nonnull String moduleName, @Nonnull String queryName);	

	@Nonnull BizQL newNamedBizQL(@Nonnull Module module, @Nonnull String queryName);

	@Nonnull DocumentQuery newNamedDocumentQuery(@Nonnull String moduleName, @Nonnull String queryName);

	@Nonnull DocumentQuery newNamedDocumentQuery(@Nonnull Module module, @Nonnull String queryName);

	/**
	 * 
	 * @param document
	 * @return
	 */
	@Nonnull DocumentQuery newDocumentQuery(@Nonnull Document document);
	
	/**
	 * 
	 * @param moduleName
	 * @param documentName
	 * @return
	 */
	@Nonnull DocumentQuery newDocumentQuery(@Nonnull String moduleName, @Nonnull String documentName);	
	
	/**
	 * 
	 * @param document
	 * @param fromClause
	 * @param filterClause
	 * @param groupClause
	 * @param orderClause
	 * @return
	 */
	@Nonnull DocumentQuery newDocumentQuery(@Nonnull Document document,
												@Nullable String fromClause,
												@Nullable String filterClause,
												@Nullable String groupClause,
												@Nullable String orderClause);

	/**
	 * 
	 * @param queryByExampleBean	A bean which will be used to create a document query based on the values
	 * 								in the scalar attributes only.
	 * @return
	 * @throws Exception
	 */
	@Nonnull DocumentQuery newDocumentQuery(@Nonnull Bean queryByExampleBean)
	throws Exception;
}
