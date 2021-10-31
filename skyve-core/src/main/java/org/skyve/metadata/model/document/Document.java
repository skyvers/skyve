package org.skyve.metadata.model.document;

import java.util.List;
import java.util.Set;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Model;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;

/**
 * 
 */
public interface Document extends Model {
	/**
	 * 
	 * @param user
	 * @return
	 * @throws Exception
	 */
	public <T extends Bean> T newInstance(User user) throws Exception;
	
	public String getBizKeyExpression();
	
	/**
	 * Set the bizKey for static/dynamic beans.
	 * 
	 * @param bean
	 */
	public void setBizKey(PersistentBean bean);
	
	/**
	 * 
	 * @param name
	 * @return
	 */
	public UniqueConstraint getUniqueConstraint(String name);
	
	/**
	 * 
	 * @param customer
	 * @param name
	 * @return
	 */
	public <T extends Bean> DynamicImage<T> getDynamicImage(Customer customer, String name);

	/**
	 * Get the unique constraints for this document only - no super-documents.
	 * @return
	 */
	public List<UniqueConstraint> getUniqueConstraints();

	/**
	 * Get the unique constraints for this document and any super-documents.
	 * @return
	 */
	public List<UniqueConstraint> getAllUniqueConstraints();

	/**
	 * 
	 * @param referenceName
	 * @return
	 */
	public Reference getReferenceByName(String referenceName);

	/**
	 * 
	 * @param customer
	 * @param relationName
	 * @return
	 */
	public Document getRelatedDocument(Customer customer, String relationName);
	
	/**
	 * 
	 * @return
	 */
	public Set<String> getReferencedDocumentNames();
	
	/**
	 * 
	 * @return
	 */
	public Set<String> getReferenceNames();
	
	/**
	 * 
	 * @return
	 */
	public Set<String> getConditionNames();
	
	/**
	 * 
	 * @return
	 */
	public Condition getCondition(String conditionName);

	/**
	 * 
	 * @return
	 */
	public Set<String> getDefinedActionNames();

	/**
	 * 
	 * @param customer
	 * @return
	 */
	public Set<Document> getReferencedDocuments(Customer customer);
	
	/**
	 * 
	 * @return
	 */
	public String getParentDocumentName();
	
	/**
	 * 
	 * @return
	 */
	public boolean isOrdered();
	
	/**
	 * 
	 * @param customer
	 * @return
	 */
	public Document getParentDocument(Customer customer);
	
	/**
	 * 
	 * @param uxui
	 * @param customer
	 * @param name
	 * @return
	 */
	public View getView(String uxui, Customer customer, String name);
	
	/**
	 * 
	 * @return
	 */
	public String getDocumentation();
}
