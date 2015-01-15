package org.skyve.persistence;

import java.io.Serializable;
import java.sql.Connection;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.document.Document;
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
	 * 
	 */
	public void begin();
	
	/**
	 * 
	 */
	public void rollback();
	
	/**
	 * 
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
	
	/**
	 * 
	 * @param document
	 * @param beanToSave
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public void preFlush(Document document, Bean beanToSave) throws DomainException, MetaDataException;
	
	/**
	 * 
	 * @param document
	 * @param bean
	 * @return
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public <T extends PersistentBean> T save(Document document, T bean) throws DomainException, MetaDataException;
	
	/**
	 * 
	 * @param bean
	 * @return
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public <T extends PersistentBean> T save(T bean) throws DomainException, MetaDataException;

	/**
	 * 
	 * @param document
	 * @param beanToSave
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public void postFlush(Document document, Bean beanToSave) throws DomainException, MetaDataException;
	
	/**
	 * 
	 * @param document
	 * @param bean
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public <T extends PersistentBean> void delete(Document document, T bean) throws DomainException, MetaDataException;
	
	/**
	 * 
	 * @param bean
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public <T extends PersistentBean> void delete(T bean) throws DomainException, MetaDataException;

	/**
	 * 
	 * @param dml
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public void executeDML(BizQL dml) throws DomainException, MetaDataException;

	/**
	 * Need to take into account the customer/user you are running as.
	 * 
	 * @param dml
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public void executeInsecureSQLDML(SQL dml) 
	throws DomainException, MetaDataException;

	/**
	 * 
	 * @param bean
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public void upsertBeanTuple(PersistentBean bean) 
	throws DomainException, MetaDataException;

	/**
	 * 
	 * @param owningBean
	 * @param collectionName
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public void upsertCollectionTuples(PersistentBean owningBean, String collectionName)
	throws DomainException, MetaDataException;

	/**
	 * 
	 * @param sql
	 * @return
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public List<Object> retrieveInsecureSQL(SQL sql) 
	throws DomainException, MetaDataException;

	/**
	 * 
	 * @param document
	 * @param id
	 * @param forUpdate
	 * @return
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public <T extends Bean> T retrieve(Document document, String id, boolean forUpdate) 
	throws DomainException, MetaDataException;

	/**
	 * 
	 * @param moduleName
	 * @param documentName
	 * @param id
	 * @param forUpdate
	 * @return
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public <T extends Bean> T retrieve(String moduleName, String documentName, String id, boolean forUpdate) 
	throws DomainException, MetaDataException;

	/**
	 * 
	 * @param query
	 * @return
	 * @throws DomainException
	 */
	public <T extends Bean> List<T> retrieve(Query query)
	throws DomainException;
	
	/**
	 * 
	 * @param query
	 * @param firstResult
	 * @param maxResults
	 * @return
	 * @throws DomainException
	 */
	public <T extends Bean> List<T> retrieve(Query query, Integer firstResult, Integer maxResults)
	throws DomainException;

	/**
	 * Use a scrollable result set to iterate over a query, thus not instantiating thousands of objects up front.
	 * 
	 * Use like this
	 * <code><pre>
	 * try (AutoClosingBeanIterable&lt;MyBean&gt; beans = persistence.iterate(beansQuery)) {
	 *     for (MyBean bean : beans) {
	 *     }
	 * }
	 * </pre></code>
	 * 
	 * @param query
	 * @return
	 * @throws DomainException
	 */
	public <T extends Bean> AutoClosingBeanIterable<T> iterate(Query query)
	throws DomainException;
	
	/**
	 * Use a scrollable result set to iterate over a query, thus not instantiating thousands of objects up front.
	 * 
	 * Use like this
	 * <code><pre>
	 * try (AutoClosingBeanIterable&lt;MyBean&gt; beans = persistence.iterate(beansQuery, Integer.valueOf(0), Integer.valueOf(100)) {
	 *     for (MyBean bean : beans) {
	 *     }
	 * }
	 * </pre></code>
	 * 
	 * @param <T> extends Bean. The type of bean the iterable will yield.
	 * @param query The query to run.
	 * @param firstResult For paged querying.
	 * @param maxResults For paged querying.
	 * @return An Iterable<T>.
	 * @throws DomainException
	 */
	public <T extends Bean> AutoClosingBeanIterable<T> iterate(Query query, Integer firstResult, Integer maxResults)
	throws DomainException;

	/**
	 * 
	 * @param beanToReindex
	 * @throws Exception
	 */
	public void reindex(PersistentBean beanToReindex) throws Exception;

	/**
	 * Sets all document scopes for this persistence instance.
	 * @param scope	The scope to use for ALL documents.
	 * @throws MetaDataException	When the user modules and documents cannot be accessed.
	 */
	public void setDocumentPermissionScopes(DocumentPermissionScope scope) throws MetaDataException;

	/**
	 * Resets ALL the document scopes to what the user is allowed to access.
	 * @throws MetaDataException	When the user modules and documents cannot be accessed.
	 */
	public void resetDocumentPermissionScopes() throws MetaDataException;
	
	/**
	 * 
	 * @param query
	 * @return
	 */
	public SQL newSQL(String query);	
	
	/**
	 * 
	 * @param query
	 * @return
	 */
	public BizQL newBizQL(String query);	
	
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
	 * @throws MetaDataException
	 */
	public DocumentQuery newDocumentQuery(String moduleName, String documentName)
	throws MetaDataException;	
	
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
	
	/**
	 * Allow access to the SQL connection.
	 * If an emergency, break the glass.  Good for calling stored procedures etc.
	 * Use the Persistence API to retrieve SQL and execute DML.
	 * @return
	 */
	public Connection getConnection();
}
