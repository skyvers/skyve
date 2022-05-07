package org.skyve.persistence;

import java.io.Serializable;
import java.util.Set;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.metadata.customer.ExportedReference;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

/**
 * Implemented for various types of data stores that can support dynamic domain persistence.
 */
public interface DynamicPersistence extends Serializable {
	/**
	 * Hook used to setup the state of this DynamicPersistence in relation to the Persistence which created it.
	 */
	void postConstruct(Persistence persistence);

	/**
	 * Convenience persist.
	 */
	default void persist(PersistentBean bean) {
		Customer c = CORE.getCustomer();
		Module m = c.getModule(bean.getBizModule());
		Document d = m.getDocument(c, bean.getBizDocument());
		persist(c, m, d, bean);
	}
	
	/**
	 * Recursively persist the given bean which may be totally dynamic (PersistentDynamicBean) or have some dynamic attributes (AbstractPersistentBean).
	 */
	void persist(Customer customer, Module module, Document document, PersistentBean bean);

	/**
	 * Convenience delete.
	 */
	default void delete(PersistentBean bean) {
		Customer c = CORE.getCustomer();
		Module m = c.getModule(bean.getBizModule());
		Document d = m.getDocument(c, bean.getBizDocument());
		delete(c, d, bean);
	}

	/**
	 * This method should recursively delete the entire bean graph (including any detritus from schema evolution).
	 * ie Any incoming/outgoing relations/edges that are no longer expressed in metadata because the schema has evolved.
	 * 
	 * @param customer
	 * @param document
	 * @param bean
	 */
	void delete(Customer customer, Document document, PersistentBean bean);

	/**
	 * This method should recursively retrieve the entire bean graph (excluding any detritus from schema evolution).
	 * ie Excluding any outgoing relations/edges that are no longer expressed in metadata because the schema has evolved.
	 * NB The module and document is derived from the data store to allow polymorphism.
	 * @param bizId
	 */
	DynamicPersistentBean populate(String bizId);
	
	/**
	 * This method should recursively retrieve the entire bean graph (excluding any detritus from schema evolution).
	 * ie Excluding any outgoing relations/edges that are no longer expressed in metadata because the schema has evolved.
	 * NB The module and document is derived from the bean to allow polymorphism.
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
}
