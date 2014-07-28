package org.skyve.metadata.model.document;

import java.util.List;
import java.util.Set;

import org.skyve.domain.Bean;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Model;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;

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
	 * @throws MetaDataException
	 */
	public <T extends Bean> DynamicImage<T> getDynamicImage(Customer customer, String name)
	throws MetaDataException;

	/**
	 * 
	 * @return
	 */
	public List<UniqueConstraint> getUniqueConstraints();

	/**
	 * 
	 * @param referenceName
	 * @return
	 */
	public Reference getReferenceByName(String referenceName);

	/**
	 * 
	 * @param customer
	 * @param referenceName
	 * @return
	 * @throws MetaDataException
	 */
	public Document getReferencedDocument(Customer customer, String referenceName) 
	throws MetaDataException;
	
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
	public Set<String> getDefinedActionNames();

	/**
	 * 
	 * @param customer
	 * @return
	 * @throws MetaDataException
	 */
	public Set<Document> getReferencedDocuments(Customer customer) 
	throws MetaDataException;
	
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
	 * @throws MetaDataException
	 */
	public Document getParentDocument(Customer customer) throws MetaDataException;
	
	/**
	 * 
	 * @param uxui
	 * @param customer
	 * @param type
	 * @return
	 * @throws MetaDataException
	 */
	public View getView(String uxui, Customer customer, ViewType type) throws MetaDataException;
	
	/**
	 * 
	 * @return
	 */
	public String getDocumentation();
}
