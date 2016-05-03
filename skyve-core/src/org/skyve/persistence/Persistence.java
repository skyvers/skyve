package org.skyve.persistence;

import java.io.Serializable;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.MetaDataException;
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
	 * Refresh the bean's state from the database.
	 */
	public void refresh(Bean bean) throws DomainException;
	
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
	 * @param owningBean
	 * @param collectionName
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public void insertCollectionTuples(PersistentBean owningBean, String collectionName)
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
	
	public SQL newNamedSQL(String moduleName, String queryName) throws MetaDataException;	

	public SQL newNamedSQL(Module module, String queryName) throws MetaDataException;

	/**
	 * 
	 * @param query
	 * @return
	 */
	public SQL newSQL(String moduleName, String documentName, String query);

	public SQL newNamedSQL(String moduleName, String documentName, String queryName)
	throws MetaDataException;
	
	/**
	 * 
	 * @param query
	 * @return
	 */
	public SQL newSQL(Document document, String query);	

	public SQL newNamedSQL(Document document, String queryName) throws MetaDataException;	

	/**
	 * 
	 * @param query
	 * @return
	 */
	public BizQL newBizQL(String query);	
	
	public BizQL newNamedBizQL(String moduleName, String queryName) throws MetaDataException;	

	public BizQL newNamedBizQL(Module module, String queryName) throws MetaDataException;

	public DocumentQuery newNamedDocumentQuery(String moduleName, String queryName)
	throws MetaDataException;

	public DocumentQuery newNamedDocumentQuery(Module module, String queryName) throws MetaDataException;

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
}
