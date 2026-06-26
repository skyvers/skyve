package org.skyve.persistence;

import java.io.Closeable;
import java.io.Serializable;

import org.skyve.domain.Bean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.ReferentialConstraintViolationException;

/**
 * Implemented for various types of data stores that can support dynamic domain persistence.
 */
public interface DynamicPersistence extends Serializable, Closeable {
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
	 * @throws ReferentialConstraintViolationException	When the bean cannot be deleted because something is referencing it.
	 */
	void delete(PersistentBean bean) throws ReferentialConstraintViolationException;

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
		
	// Persistence first level cache methods

	/**
	 * Executes evictAllCached.
	 */
	public void evictAllCached();
	
	/**
	 * Executes evictCached.
	 * @param bean the bean
	 */
	public void evictCached(Bean bean);
	
	/**
	 * Executes cached.
	 * @param bean the bean
	 * @return the result
	 */
	public boolean cached(Bean bean);
	
	// Persistence Transaction methods
	
	/**
	 * Executes begin.
	 */
	public void begin();
	
	/**
	 * Executes rollback.
	 */
	public void rollback();
	
	/**
	 * Commits pending dynamic-persistence work for the current unit of work.
	 *
	 * <p>Idempotency: implementations must tolerate repeated calls after the owning
	 * {@link Persistence} has already completed its transaction. A repeated call without
	 * intervening dynamic work must not duplicate writes or fail solely because there is
	 * no active work to commit.
	 */
	public void commit();
	
	/**
	 * Releases dynamic-persistence resources for this instance.
	 *
	 * <p>Idempotency: implementations must tolerate repeated calls. A repeated call must
	 * not fail solely because resources have already been released.
	 */
	@Override
	public void close();
}
