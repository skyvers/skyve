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
import jakarta.persistence.EntityManager;

/**
 * Provides access to the underlying persistence mechanism, allowing application code to
 * manage transactions, query data, save and delete domain beans, manage caches, and
 * execute SQL, BizQL, and document queries.
 * <p>
 * An instance of {@code Persistence} is scoped to the current user and is obtained via
 * {@link org.skyve.CORE#getPersistence()}.
 */
public interface Persistence extends Serializable {
	/**
	 * Returns the user associated with this persistence instance.
	 * @return The current user.
	 */
	@Nonnull User getUser();
	
	/**
	 * Returns whether the given bean has been persisted to the data store (i.e. has a non-null bizId
	 * and exists in the database).
	 * @param bean The bean to check.
	 * @return {@code true} if the bean has been persisted.
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
	 * Commit the transaction and optionally close the associated persistence resources.
	 * @param close If {@code true}, the persistence resources are closed after the commit.
	 */
	void commit(boolean close);
	
	/**
	 * Evicts all beans from the first-level (session) cache.
	 */
	void evictAllCached();
	
	/**
	 * Evicts the given bean from the first-level (session) cache.
	 * @param bean The bean to evict.
	 */
	void evictCached(@Nonnull Bean bean);

	/**
	 * Returns whether the given bean is currently held in the first-level (session) cache.
	 * @param bean The bean to check.
	 * @return {@code true} if the bean is cached.
	 */
	boolean cached(@Nonnull Bean bean);

	/**
	 * Returns whether the specified collection is present in the second-level (shared) cache
	 * for the given owner identified by bizId.
	 * @param moduleName The module name of the owning document.
	 * @param documentName The document name of the owning document.
	 * @param collectionName The collection attribute name.
	 * @param ownerBizId The bizId of the owning bean.
	 * @return {@code true} if the collection is in the shared cache.
	 */
	boolean sharedCacheCollection(@Nonnull String moduleName, 
									@Nonnull String documentName,
									@Nonnull String collectionName,
									@Nonnull String ownerBizId);
	
	/**
	 * Returns whether the specified collection of the given owner bean is present in the
	 * second-level (shared) cache.
	 * @param owner The owning bean.
	 * @param collectionName The collection attribute name.
	 * @return {@code true} if the collection is in the shared cache.
	 */
	boolean sharedCacheCollection(@Nonnull Bean owner, @Nonnull String collectionName);

	/**
	 * Returns whether a bean identified by module, document, and bizId is present in the
	 * second-level (shared) cache.
	 * @param moduleName The module name.
	 * @param documentName The document name.
	 * @param bizId The bizId of the bean.
	 * @return {@code true} if the bean is in the shared cache.
	 */
	boolean sharedCacheBean(@Nonnull String moduleName, @Nonnull String documentName, @Nonnull String bizId);

	/**
	 * Returns whether the given bean is present in the second-level (shared) cache.
	 * @param bean The bean to check.
	 * @return {@code true} if the bean is in the shared cache.
	 */
	boolean sharedCacheBean(@Nonnull Bean bean);

	/**
	 * Evicts all entries from the second-level (shared) cache.
	 */
	void evictAllSharedCache();

	/**
	 * Evicts all collection entries from the second-level (shared) cache.
	 */
	void evictSharedCacheCollections();

	/**
	 * Evicts all cached entries for the specified collection across all owning beans.
	 * @param moduleName The module name of the owning document.
	 * @param documentName The document name of the owning document.
	 * @param collectionName The collection attribute name.
	 */
	void evictSharedCacheCollections(@Nonnull String moduleName,
										@Nonnull String documentName,
										@Nonnull String collectionName);

	/**
	 * Evicts a specific collection entry from the second-level (shared) cache.
	 * @param moduleName The module name of the owning document.
	 * @param documentName The document name of the owning document.
	 * @param collectionName The collection attribute name.
	 * @param ownerBizId The bizId of the owning bean.
	 */
	void evictSharedCacheCollection(@Nonnull String moduleName,
										@Nonnull String documentName,
										@Nonnull String collectionName,
										@Nonnull String ownerBizId);

	/**
	 * Evicts a specific collection of the given owner bean from the second-level (shared) cache.
	 * @param owner The owning bean.
	 * @param collectionName The collection attribute name.
	 */
	void evictSharedCacheCollection(@Nonnull Bean owner, @Nonnull String collectionName);

	/**
	 * Evicts all bean entries from the second-level (shared) cache.
	 */
	void evictSharedCacheBeans();

	/**
	 * Evicts all cached beans for the given module and document from the second-level (shared) cache.
	 * @param moduleName The module name.
	 * @param documentName The document name.
	 */
	void evictSharedCacheBeans(@Nonnull String moduleName, @Nonnull String documentName);

	/**
	 * Evicts a specific bean identified by module, document, and bizId from the second-level (shared) cache.
	 * @param moduleName The module name.
	 * @param documentName The document name.
	 * @param bizId The bizId of the bean.
	 */
	void evictSharedCachedBean(@Nonnull String moduleName, @Nonnull String documentName, @Nonnull String bizId);

	/**
	 * Evicts the given bean from the second-level (shared) cache.
	 * @param bean The bean to evict.
	 */
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
	 * Merges the bean into the persistent context and flushes to the data store.
	 * @param document The document metadata for the bean.
	 * @param bean The bean to save.
	 * @return The merged bean instance.
	 */
	@Nonnull <T extends PersistentBean> T save(@Nonnull Document document, @Nonnull T bean);

	/**
	 * Merges the bean into the persistent context and flushes to the data store.
	 * The document metadata is resolved automatically from the bean type.
	 * @param bean The bean to save.
	 * @return The merged bean instance.
	 */
	@Nonnull <T extends PersistentBean> T save(@Nonnull T bean);

	/**
	 * Merges each bean in the list into the persistent context and flushes to the data store.
	 * @param beans The list of beans to save.
	 * @return The list of merged bean instances.
	 */
	@Nonnull <T extends PersistentBean> List<T> save(@Nonnull List<T> beans);

	/**
	 * Merges each bean into the persistent context and flushes to the data store.
	 * @param beans The beans to save.
	 * @return The list of merged bean instances.
	 */
	@Nonnull <T extends PersistentBean> List<T> save(@SuppressWarnings("unchecked") @Nonnull T... beans);

	/**
	 * Merges the bean into the persistent context without flushing to the data store.
	 * @param document The document metadata for the bean.
	 * @param bean The bean to merge.
	 * @return The merged bean instance.
	 */
	@Nonnull <T extends PersistentBean> T merge(@Nonnull Document document, @Nonnull T bean);

	/**
	 * Merges the bean into the persistent context without flushing to the data store.
	 * The document metadata is resolved automatically from the bean type.
	 * @param bean The bean to merge.
	 * @return The merged bean instance.
	 */
	@Nonnull <T extends PersistentBean> T merge(@Nonnull T bean);

	/**
	 * Merges each bean in the list into the persistent context without flushing to the data store.
	 * @param beans The list of beans to merge.
	 * @return The list of merged bean instances.
	 */
	@Nonnull <T extends PersistentBean> List<T> merge(@Nonnull List<T> beans);

	/**
	 * Merges each bean into the persistent context without flushing to the data store.
	 * @param beans The beans to merge.
	 * @return The list of merged bean instances.
	 */
	@Nonnull <T extends PersistentBean> List<T> merge(@SuppressWarnings("unchecked") @Nonnull T... beans);

	/**
	 * Processing that occurs after the merging of beans into the persistent context.
	 * This method can either be hijacked for a Persistence implementation or used in
	 * application code to enable the same behaviour as skyve applies post save/merge — e.g. after an upsert.
	 * @param document The document metadata for the bean.
	 * @param beanToSave The bean that was saved.
	 */
	public void postMerge(@Nonnull Document document, @Nonnull PersistentBean beanToSave);
	
	/**
	 * Execute the DML to synchronize the persistent context with the data store.
	 */
	public void flush();
	
	/**
	 * Deletes the given bean from the data store.
	 * @param document The document metadata for the bean.
	 * @param bean The bean to delete.
	 * @throws ReferentialConstraintViolationException When the bean cannot be deleted because another record references it.
	 */
	<T extends PersistentBean> void delete(@Nonnull Document document, @Nonnull T bean)
	throws ReferentialConstraintViolationException;

	/**
	 * Deletes the given bean from the data store.
	 * The document metadata is resolved automatically from the bean type.
	 * @param bean The bean to delete.
	 * @throws ReferentialConstraintViolationException When the bean cannot be deleted because another record references it.
	 */
	<T extends PersistentBean> void delete(@Nonnull T bean)
	throws ReferentialConstraintViolationException;

	/**
	 * Performs a low-level upsert of the scalar (non-collection) attributes of the given bean
	 * directly to the underlying table row, bypassing the JPA lifecycle.
	 * @param bean The bean whose tuple (row) is to be upserted.
	 */
	void upsertBeanTuple(@Nonnull PersistentBean bean);

	/**
	 * Performs a low-level upsert of the join-table rows for the specified collection of the
	 * given owning bean, bypassing the JPA lifecycle.
	 * @param owningBean The bean that owns the collection.
	 * @param collectionName The name of the collection attribute to upsert.
	 */
	void upsertCollectionTuples(@Nonnull PersistentBean owningBean, @Nonnull String collectionName);

	/**
	 * Performs a low-level insert of the join-table rows for the specified collection of the
	 * given owning bean, bypassing the JPA lifecycle.
	 * @param owningBean The bean that owns the collection.
	 * @param collectionName The name of the collection attribute to insert.
	 */
	void insertCollectionTuples(@Nonnull PersistentBean owningBean, @Nonnull String collectionName);

	/**
	 * Retrieves a bean by its bizId for the given document.
	 * @param document The document metadata.
	 * @param id The bizId of the bean to retrieve.
	 * @return The bean, or {@code null} if not found.
	 */
	@Nullable <T extends Bean> T retrieve(@Nonnull Document document, @Nonnull String id);

	/**
	 * Retrieves a bean by its bizId for the given document and places a pessimistic write lock on it.
	 * @param document The document metadata.
	 * @param id The bizId of the bean to retrieve.
	 * @return The bean (never {@code null}).
	 */
	@Nonnull <T extends Bean> T retrieveAndLock(@Nonnull Document document, @Nonnull String id);

	/**
	 * Retrieves a bean by its bizId for the given module and document.
	 * @param moduleName The module name.
	 * @param documentName The document name.
	 * @param id The bizId of the bean to retrieve.
	 * @return The bean, or {@code null} if not found.
	 */
	@Nullable <T extends Bean> T retrieve(@Nonnull String moduleName,
											@Nonnull String documentName,
											@Nonnull String id);

	/**
	 * Retrieves a bean by its bizId for the given module and document and places a pessimistic write lock on it.
	 * @param moduleName The module name.
	 * @param documentName The document name.
	 * @param id The bizId of the bean to retrieve.
	 * @return The bean (never {@code null}).
	 */
	@Nonnull <T extends Bean> T retrieveAndLock(@Nonnull String moduleName,
													@Nonnull String documentName,
													@Nonnull String id);


	/**
	 * Reindexes the given bean in the content/search index.
	 * @param beanToReindex The bean to reindex.
	 * @throws Exception If reindexing fails.
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
	 * @param scope The scope to use for ALL documents.
	 * @param consumer The consumer to accept.
	 */
	void withDocumentPermissionScopes(@Nonnull DocumentPermissionScope scope,
										@Nonnull Consumer<Persistence> consumer);

	/**
	 * Creates a new {@link SQL} wrapper for the given native SQL query string.
	 * @param query The SQL query string.
	 * @return The SQL wrapper.
	 */
	@Nonnull SQL newSQL(@Nonnull String query);

	/**
	 * Creates a new {@link SQL} wrapper from a named SQL query defined in the given module.
	 * @param moduleName The module name containing the named SQL definition.
	 * @param queryName The name of the SQL query.
	 * @return The SQL wrapper.
	 */
	@Nonnull SQL newNamedSQL(@Nonnull String moduleName, @Nonnull String queryName);

	/**
	 * Creates a new {@link SQL} wrapper from a named SQL query defined in the given module.
	 * @param module The module containing the named SQL definition.
	 * @param queryName The name of the SQL query.
	 * @return The SQL wrapper.
	 */
	@Nonnull SQL newNamedSQL(@Nonnull Module module, @Nonnull String queryName);

	/**
	 * Creates a new {@link SQL} wrapper for the given SQL query string with module and document context
	 * applied for row-level security filtering.
	 * @param moduleName The module name for security context.
	 * @param documentName The document name for security context.
	 * @param query The SQL query string.
	 * @return The SQL wrapper.
	 */
	@Nonnull SQL newSQL(@Nonnull String moduleName, @Nonnull String documentName, @Nonnull String query);

	/**
	 * Creates a new {@link SQL} wrapper from a named SQL query with module and document context
	 * applied for row-level security filtering.
	 * @param moduleName The module name for security context.
	 * @param documentName The document name for security context.
	 * @param queryName The name of the SQL query.
	 * @return The SQL wrapper.
	 */
	@Nonnull SQL newNamedSQL(@Nonnull String moduleName, @Nonnull String documentName, @Nonnull String queryName);

	/**
	 * Creates a new {@link SQL} wrapper for the given SQL query string with document context
	 * applied for row-level security filtering.
	 * @param document The document metadata for security context.
	 * @param query The SQL query string.
	 * @return The SQL wrapper.
	 */
	@Nonnull SQL newSQL(@Nonnull Document document, @Nonnull String query);

	/**
	 * Creates a new {@link SQL} wrapper from a named SQL query with document context
	 * applied for row-level security filtering.
	 * @param document The document metadata for security context.
	 * @param queryName The name of the SQL query.
	 * @return The SQL wrapper.
	 */
	@Nonnull SQL newNamedSQL(@Nonnull Document document, @Nonnull String queryName);

	/**
	 * Creates a new {@link BizQL} wrapper for the given BizQL query string.
	 * BizQL is Skyve's object-oriented query language that operates over domain beans.
	 * @param query The BizQL query string.
	 * @return The BizQL wrapper.
	 */
	@Nonnull BizQL newBizQL(@Nonnull String query);

	/**
	 * Creates a new {@link BizQL} wrapper from a named BizQL query defined in the given module.
	 * @param moduleName The module name containing the named BizQL definition.
	 * @param queryName The name of the BizQL query.
	 * @return The BizQL wrapper.
	 */
	@Nonnull BizQL newNamedBizQL(@Nonnull String moduleName, @Nonnull String queryName);

	/**
	 * Creates a new {@link BizQL} wrapper from a named BizQL query defined in the given module.
	 * @param module The module containing the named BizQL definition.
	 * @param queryName The name of the BizQL query.
	 * @return The BizQL wrapper.
	 */
	@Nonnull BizQL newNamedBizQL(@Nonnull Module module, @Nonnull String queryName);

	/**
	 * Creates a new {@link DocumentQuery} from a named document query defined in the given module.
	 * @param moduleName The module name containing the named document query definition.
	 * @param queryName The name of the document query.
	 * @return The document query.
	 */
	@Nonnull DocumentQuery newNamedDocumentQuery(@Nonnull String moduleName, @Nonnull String queryName);

	/**
	 * Creates a new {@link DocumentQuery} from a named document query defined in the given module.
	 * @param module The module containing the named document query definition.
	 * @param queryName The name of the document query.
	 * @return The document query.
	 */
	@Nonnull DocumentQuery newNamedDocumentQuery(@Nonnull Module module, @Nonnull String queryName);

	/**
	 * Creates a new {@link DocumentQuery} for the given document.
	 * @param document The document metadata to query.
	 * @return The document query.
	 */
	@Nonnull DocumentQuery newDocumentQuery(@Nonnull Document document);

	/**
	 * Creates a new {@link DocumentQuery} for the given module and document.
	 * @param moduleName The module name.
	 * @param documentName The document name.
	 * @return The document query.
	 */
	@Nonnull DocumentQuery newDocumentQuery(@Nonnull String moduleName, @Nonnull String documentName);

	/**
	 * Creates a new {@link DocumentQuery} for the given document with explicit JPQL clauses.
	 * @param document The document metadata to query.
	 * @param fromClause An optional additional FROM clause fragment, or {@code null}.
	 * @param filterClause An optional WHERE clause fragment, or {@code null}.
	 * @param groupClause An optional GROUP BY clause fragment, or {@code null}.
	 * @param orderClause An optional ORDER BY clause fragment, or {@code null}.
	 * @return The document query.
	 */
	@Nonnull DocumentQuery newDocumentQuery(@Nonnull Document document,
												@Nullable String fromClause,
												@Nullable String filterClause,
												@Nullable String groupClause,
												@Nullable String orderClause);

	/**
	 * Creates a new {@link DocumentQuery} based on the non-null scalar attribute values of the
	 * given bean, constructing an equality filter for each populated attribute.
	 * @param queryByExampleBean A bean whose scalar attribute values are used as query filters.
	 * @return The document query.
	 */
	@Nonnull DocumentQuery newDocumentQuery(@Nonnull Bean queryByExampleBean);
	
	/**
	 * In case of emergency, break glass
	 */
	public @Nonnull EntityManager getEntityManager();
}
