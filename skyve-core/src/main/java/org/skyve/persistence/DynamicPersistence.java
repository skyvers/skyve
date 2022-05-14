package org.skyve.persistence;

import java.io.Serializable;
import java.util.Set;

import org.skyve.domain.Bean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.metadata.customer.ExportedReference;
import org.skyve.metadata.model.document.Document;

/**
 * Implemented for various types of data stores that can support dynamic domain persistence.
 */
public interface DynamicPersistence extends Serializable {
	/**
	 * Hook used to setup the state of this DynamicPersistence in relation to the Persistence which created it.
	 */
	void postConstruct(Persistence persistence);

	/**
	 * Recursively persist the given bean which may be totally dynamic (PersistentDynamicBean) or have some dynamic attributes (AbstractPersistentBean).
	 */
	void persist(PersistentBean bean);
	
	/**
	 * This method should recursively delete the entire bean graph (including any detritus from schema evolution).
	 * ie Any incoming/outgoing relations/edges that are no longer expressed in metadata because the schema has evolved.
	 * @param bean
	 */
	void delete(PersistentBean bean);

	/**
	 * This method should recursively retrieve a DynamicPersistentBean and its entire bean graph (excluding any detritus from schema evolution).
	 * ie Excluding any outgoing relations/edges that are no longer expressed in metadata because the schema has evolved.
	 * NB The module and document is derived from the data store to allow polymorphism.
	 * NB Calling populate with the same bizId should yield exactly the same bean instance for this DynamicPersistence instance (through a first level cache)
	 * @param bizId
	 */
	DynamicPersistentBean populate(String bizId);
	
	/**
	 * This method should recursively retrieve the entire dynamic bean graph required to populate the given bean (excluding any detritus from schema evolution).
	 * ie Excluding any outgoing relations/edges that are no longer expressed in metadata because the schema has evolved.
	 * NB The module and document is derived from the bean to allow polymorphism.
	 * NB Calling populate with the same bean should yield exactly the same bean instance for this DynamicPersistence instance (through a first level cache)
	 * @param bean
	 */
	void populate(PersistentBean bean);
	
	/**
	 * Return false if anything is referencing a bean that is about to be deleted, otherwise true.
	 * 
	 * This method can't just find any incoming relations/edges and spew as 
	 * 1) some relations will be cascaded on delete
	 * 2) schema evolution could leave detritus relations that should not be considered.
	 * This is why this method gets called for every relation that SHOULD be checked from the metadata.
	 * 
	 * @param documentToDelete
	 * @param beanToDelete
	 * @param exportedReference
	 * @param referenceDocument
	 * @param beansToBeExcluded
	 * @return false if anything is referencing a bean that is about to be deleted, otherwise true.
	 */
	boolean hasReferentialIntegrity(Document documentToDelete,
										PersistentBean beanToDelete,
										ExportedReference exportedReference,
										Document referenceDocument,
										Set<Bean> beansToBeExcluded);
	
	// Persistence first level cache methods
	public void evictAllCached();
	public void evictCached(Bean bean);
	public boolean cached(Bean bean);
}
