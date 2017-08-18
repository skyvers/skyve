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
	
	/**
	 * Refresh the bean's state from the database.
	 */
	public void refresh(Bean bean);
	
	/**
	 * 
	 * @param document
	 * @param beanToSave
	 */
	public void preFlush(Document document, Bean beanToSave);
	
	/**
	 * 
	 * @param document
	 * @param bean
	 * @return
	 */
	public <T extends PersistentBean> T save(Document document, T bean);
	
	/**
	 * 
	 * @param bean
	 * @return
	 */
	public <T extends PersistentBean> T save(T bean);

	/**
	 * 
	 * @param beans
	 * @return
	 */
	public <T extends PersistentBean> List<T> save(List<T> beans);

	/**
	 * 
	 * @param beans
	 * @return
	 */
	public <T extends PersistentBean> List<T> save(@SuppressWarnings("unchecked") T... beans);

	/**
	 * 
	 * @param document
	 * @param beanToSave
	 */
	public void postFlush(Document document, Bean beanToSave);
	
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
	 * @param forUpdate
	 * @return
	 */
	public <T extends Bean> T retrieve(Document document, String id, boolean forUpdate);

	/**
	 * 
	 * @param moduleName
	 * @param documentName
	 * @param id
	 * @param forUpdate
	 * @return
	 */
	public <T extends Bean> T retrieve(String moduleName, String documentName, String id, boolean forUpdate);


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
