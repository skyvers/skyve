package org.skyve.metadata.repository;

import javax.annotation.Nonnull;

import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerMetaData;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.View;

public interface MutableRepository {
	/**
	 * Set's the router in this repository.
	 * @param router	The router to set.
	 * @return	The converted router.
	 */
	@Nonnull Router setRouter(@Nonnull Router router);
	
	/**
	 * Puts the customer meta-data in this repository.
	 * @param customer	The customer meta-data to put.
	 * @return	The converted customer.
	 */
	@Nonnull Customer putCustomer(@Nonnull CustomerMetaData customer);
	
	/**
	 * Puts a customer overridden module meta-data into this repository.
	 * @param customer	The customer the module is for.
	 * @param module	The module meta-data to put.
	 * @return	The converted module.
	 */
	@Nonnull Module putModule(@Nonnull Customer customer, @Nonnull ModuleMetaData module);
	
	/**
	 * Puts a vanilla module meta-data into this repository.
	 * @param module	The module meta-data to put.
	 * @return	The converted module.
	 */
	@Nonnull Module putModule(@Nonnull ModuleMetaData module);
	
	/**
	 * Puts a customer overridden document meta-data into this repository.
	 * @param customer	The customer the document is for.
	 * @param module	The module the document is for.
	 * @param document	The document meta-data to put.
	 * @return	The converted document.
	 */
	@Nonnull Document putDocument(@Nonnull Customer customer, @Nonnull Module module, @Nonnull DocumentMetaData document);

	/**
	 * Puts a vanilla document meta-data into this repository.
	 * @param module	The module the document is for.
	 * @param document	The document meta-data to put.
	 * @return	The converted document.
	 */
	@Nonnull Document putDocument(@Nonnull Module module, @Nonnull DocumentMetaData document);

	/**
	 * Puts a customer overridden UX/UI specific view meta-data into this repository.
	 * @param customer	The customer the view is for.
	 * @param uxui	The UX/UI the view is for.
	 * @param document	The document the view is for.
	 * @param view	The view meta-data to put.
	 * @return	The converted view.
	 */
	@Nonnull View putView(@Nonnull Customer customer, @Nonnull String uxui, @Nonnull Document document, @Nonnull ViewMetaData view); 

	/**
	 * Puts a UX/UI specific view meta-data into this repository.
	 * @param uxui	The UX/UI the view is for.
	 * @param document	The document the view is for.
	 * @param view	The view meta-data to put.
	 * @return	The converted view.
	 */
	@Nonnull View putView(@Nonnull String uxui, @Nonnull Document document, @Nonnull ViewMetaData view); 

	/**
	 * Puts a customer overridden UX/UI agnostic view meta-data into this repository.
	 * @param customer	The customer the view is for.
	 * @param document	The document the view is for.
	 * @param view	The view meta-data to put.
	 * @return	The converted view.
	 */
	@Nonnull View putView(@Nonnull Customer customer, @Nonnull Document document, @Nonnull ViewMetaData view); 

	/**
	 * Puts a UX/UI agnostic view meta-data into this repository.
	 * @param document	The document the document is for.
	 * @param view	The view meta-data to put.
	 * @return	The converted view.
	 */
	@Nonnull View putView(@Nonnull Document document, @Nonnull ViewMetaData view); 

	/**
	 * Puts a customer overridden action meta-data into this repository.
	 * @param customer	The customer the view is for.
	 * @param document	The document the action is for.
	 * @param action	The action meta-data to put.
	 * @return	The converted action.
	 */
	@Nonnull ActionMetaData putMetaDataAction(@Nonnull Customer customer, @Nonnull Document document, @Nonnull ActionMetaData action); 

	/**
	 * Puts an action meta-data into this repository.
	 * @param document	The document the action is for.
	 * @param action	The action meta-data to put.
	 * @return	The converted action.
	 */
	@Nonnull ActionMetaData putMetaDataAction(@Nonnull Document document, @Nonnull ActionMetaData action); 


	/**
	 * Puts a customer overridden bizlet meta-data into this repository.
	 * @param customer	The customer the view is for.
	 * @param document	The document the action is for.
	 * @param bizlet	The bizlet meta-data to put.
	 * @return	The converted bizlet.
	 */
	@Nonnull BizletMetaData putMetaDataBizlet(@Nonnull Customer customer, @Nonnull Document document, @Nonnull BizletMetaData bizlet); 

	/**
	 * Puts a bizlet meta-data into this repository.
	 * @param document	The document the bizlet is for.
	 * @param action	The bizlet meta-data to put.
	 * @return	The converted bizlet.
	 */
	@Nonnull BizletMetaData putMetaDataBizlet(@Nonnull Document document, @Nonnull BizletMetaData bizlet); 
}
