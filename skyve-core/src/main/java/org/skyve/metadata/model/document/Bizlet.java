package org.skyve.metadata.model.document;

import java.io.Serializable;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

/**
 * A bizlet allows callbacks to be made at pertinent times in the Skyve server processing.
 * 
 * Event timing is as follows
 * 
 * Bean level -
 * 
 * newInstance() - called after instantiation through the document.newInstance().
 * postLoad() - called after instantiation and population of data store values.
 * preSave() - called (recursively for the object graph) before flushing the values to the data store (before document validation).
 * postSave() - called (recursively for the object graph) after flushing the values to the data store (after document validation).
 * validate() - called after preSave() (and after document validation) but before flushing the values to the data store.
 * preDelete() - called before deletion from the data store.
 * postDelete() - called after deletion from the data store.
 * 
 * Form level -
 * 
 * getDomainValues() - called when rendering the pertinent field (including in list view).
 * complete() - called when rendering view widgets with complete mechanisms.
 * resolve() - called when we have a bizId from a view (combo or list membership) and we need a bean
 * preExecute() - called before an implicit action is executed.
 * 					parentBean parameter is not null when adding or editing (zooming) in a grid or lookup
 * 					(ie when ImplicitActionName.Add or ImplicitActionName.Edit is used).
 * preRerender() - called before a rerender is performed.
 * 					This occurs on zoom out operation (data/list/tree/map/calendar/lookup) and during rerender event action.
 * postRender() - called after a HTML or JSON response has been sent and committed. This can be used to reset state set for the response (ie Tab Change).
 * @param <T>	The type of document bean we want to process with this Bizlet.
 */
public class Bizlet<T extends Bean> implements MetaData {
	/**
	 * Key/Value pairs for domains defined.
	 */
	public static class DomainValue implements Serializable {
		private static final long serialVersionUID = -7737192861504224293L;

		private String code;
		private String description;
		
		/**
		 * 
		 * @param codeAndDescription
		 */
		public DomainValue(String codeAndDescription) {
			this(codeAndDescription, codeAndDescription);
		}
		
		/**
		 * 
		 * @param code
		 * @param description
		 */
		public DomainValue(String code, String description) {
			this.code = code;
			this.description = description;
		}
		
		/**
		 * 
		 * @return
		 */
		public String getCode() {
			return code;
		}
		
		/**
		 * 
		 * @return
		 */
		public String getLocalisedDescription() {
			return Util.i18n(description);
		}
		
		@Override
		public String toString() {
			return super.toString() + '(' + code + "->" + description + ')';
		}
	}
	
	private BizletMetaData metaDataBizlet = null;
	
	/**
	 * This method is used by Skyve to implement the default behaviour.
	 */
	public final void setMetaDataBizlet(BizletMetaData metaDataBizlet) {
		this.metaDataBizlet = metaDataBizlet;
	}
	
	/**
	 * Called when a document is instantiated into a bean.
	 * Use this method to run constructor-style initialization.
	 * 
	 * @param bean	The bean constructed.
	 * @return	The new bean.  This can be an entirely different instance if required.
	 * @throws Exception
	 */
	public T newInstance(T bean) throws Exception {
		// Execute the metaDataBizlet if one exists
		if (metaDataBizlet != null) {
			metaDataBizlet.newInstance(bean);
		}
		return bean;
	}
	
	/**
	 * Called on flush of dirty objects on a Persistence.save().
	 * @param bean	The bean to validate
	 * @param e
	 * @throws Exception
	 */
	public void validate(T bean, ValidationException e) throws Exception {
		// Execute the metaDataBizlet if one exists
		if (metaDataBizlet != null) {
			metaDataBizlet.validate(bean, e);
		}
	}

	/**
	 * NOTE - Use customer.getConstantDomainValues() as this will used cached values.
	 * 
	 * This method is called when domain values are required that will never change.
	 * These values are evaluated once and lazily for a customer in a thin client
	 * and at startup for a fat client.
	 *
	 * @param attributeName	The name of the attribute to get the domain for.
	 * @return the meta-data bizlet's domain values or null.
	 */
	public List<DomainValue> getConstantDomainValues(String attributeName) throws Exception {
		// Execute the metaDataBizlet if one exists
		if (metaDataBizlet != null) {
			return metaDataBizlet.getConstantDomainValues(attributeName);
		}
		return null;
	}

	/**
	 * This method is called when domain values are required that will potentially change.
	 * They are in the database (and are updated).
	 * These values are evaluated once for a render response for a thin client
	 * and once per conversation in a fat client.
	 * 
	 * @param attributeName	The name of the attribute to get the domain for.
	 * @return the meta-data bizlet's domain values or null.
	 */
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		// Execute the metaDataBizlet if one exists
		if (metaDataBizlet != null) {
			return metaDataBizlet.getVariantDomainValues(attributeName);
		}
		return null;
	}	
	
	/**
	 * This method is called when domain values are required that will potentially change.
	 * They are based on values from within the containing bean.
	 * They are evaluated once per bean in the render response phase for a thin client
	 * and once per bean per conversation for a fat client.
	 * 
	 * @param attributeName	The name of the attribute to get the domain for.
	 * @param bean	This bean to use to derive the values.
	 * @return the meta-data bizlet's domain values or null.
	 */
	public List<DomainValue> getDynamicDomainValues(String attributeName, T bean) throws Exception {
		// Execute the metaDataBizlet if one exists
		if (metaDataBizlet != null) {
			return metaDataBizlet.getDynamicDomainValues(attributeName, bean);
		}
		return null;
	}

	/**
	 * This method is called when auto-complete values are required from a view widget.
	 * They can be based on values from within the containing bean.
	 * 
	 * @param attributeName	The name of the attribute to get the domain for.
	 * @param value The value typed into the view widget used to complete upon.
	 * @param bean	This bean to use to derive the values.
	 * @return the meta-data bizlet's completions or null.
	 */
	public List<String> complete(String attributeName, String value, T bean) throws Exception {
		// Execute the metaDataBizlet if one exists
		if (metaDataBizlet != null) {
			return metaDataBizlet.complete(attributeName, value, bean);
		}
		return null;
	}

	/**
	 * This method is called when a view sends a bizId representing the object and the object
	 * needs to be supplied and set on a binding.
	 * For example, this occurs in combos on associations or in list memberships on collections.
	 * If the resolve method returns null, Skyve will try to retrieve the bean from the first level cache
	 * and then from the database.
	 * @param bizId	The ID of the bean to resolve.
	 * @param conversationBean	The conversation bean for the current view, regardless of the zoom level.
	 * @param webContext	The webContext can be used to get the current bean if required.
	 * @return	The bean or null if it can't be resolved.
	 * @throws Exception
	 */
	public T resolve(String bizId, Bean conversationBean, WebContext webContext) throws Exception {
		// Execute the metaDataBizlet if one exists
		if (metaDataBizlet != null) {
			@SuppressWarnings("unchecked")
			T result = (T) metaDataBizlet.resolve(bizId, conversationBean);
			return result;
		}
		return null;
	}
	
	/**
	 * Called before a bean is flushed to the data-store on Persistence.save().
	 * This is the place to ensure that transient derived data is saved with the bean,
	 * or that conversions are processed.
	 * 
	 * @param bean	The bean to be saved (persisted/updated)
	 * @throws Exception
	 */
	public void preSave(T bean) throws Exception {
		// Execute the metaDataBizlet if one exists
		if (metaDataBizlet != null) {
			metaDataBizlet.preSave(bean);
		}
	}
	
	/**
	 * Called after a bean is flushed to the data-store on Persistence.save().
	 * This is the place to ensure that other documents/data are saved or update with the bean.
	 * 
	 * @param bean	The bean to be saved (persisted/updated)
	 * @throws Exception
	 */
	public void postSave(T bean) throws Exception {
		// Execute the metaDataBizlet if one exists
		if (metaDataBizlet != null) {
			metaDataBizlet.postSave(bean);
		}
	}

	/**
	 * Called before a bean is deleted on Persitence.delete().
	 * This is the place to stop a delete (throw ValidationException)
	 * due to logical FK constraints or whatever.
	 * 
	 * @param bean	The bean to be deleted.
	 * @throws Exception
	 */
	public void preDelete(T bean) throws Exception {
		// Execute the metaDataBizlet if one exists
		if (metaDataBizlet != null) {
			metaDataBizlet.preDelete(bean);
		}
	}
	
	/**
	 * Called after a bean is deleted on Persitence.delete().
	 * This is the place to cleanup after a delete has successfully completed.
	 * This method is called after flush and can still cause a rollback 
	 * when an exception is thrown but usually preDelete() is the place for validation.
	 * 
	 * @param bean	The bean that has been deleted.
	 * @throws Exception
	 */
	public void postDelete(T bean) throws Exception {
		// Execute the metaDataBizlet if one exists
		if (metaDataBizlet != null) {
			metaDataBizlet.postDelete(bean);
		}
	}

	/**
	 * Called after the bean is loaded (in view or edit) in Persistence.retrieve().
	 * Don't do too much in this method as it is called alot.
	 * This is the place to ensure that transient derived fields are populated
	 * or that conversions are executed.
	 * 
	 * @param bean	The bean that was loaded.
	 * @throws Exception
	 */
	public void postLoad(T bean) throws Exception {
		// Execute the metaDataBizlet if one exists
		if (metaDataBizlet != null) {
			metaDataBizlet.postLoad(bean);
		}
	}
	
	/**
	 * Provides a point to do form level operations before implicit actions get run.
	 * @param actionName	The name of the action invoked.
	 * @param bean	The bean that the action pertains to.
	 * @param parentBean	The bean of the parent view (the view navigating from) for an Add or Edit action.
	 * @param webContext	The web context.
	 * @throws Exception
	 */
	public T preExecute(ImplicitActionName actionName, T bean, Bean parentBean, WebContext webContext) throws Exception {
		return bean;
	}
	
	/**
	 * Provides a point to manipulate the parent bean post zoom out or before the rerender event action.
	 * @param source	The binding of the control that triggered the rerender 
	 * 					or the query/document/model of the list/tree/map/calendar etc.
	 * @param bean	The bean that is being rerendered.
	 * @param webContext	The web context.
	 * @throws Exception
	 */
	public void preRerender(String source, T bean, WebContext webContext) throws Exception {
		// Execute the metaDataBizlet if one exists
		if (metaDataBizlet != null) {
			metaDataBizlet.preRerender(source, bean);
		}
	}
	
	/**
	 * Provides a point to manipulate the bean after the client response payload has been rendered and sent but before it is cached in its conversation.
	 * Note that this method should not throw any exceptions and should always succeed to enable correct state management after the render response.
	 * @param bean	The bean rendered in the view.
	 * @param webContext	The web context.
	 */
	public void postRender(T bean, WebContext webContext) {
		// Execute the metaDataBizlet if one exists
		if (metaDataBizlet != null) {
			metaDataBizlet.postRender(bean);
		}
	}
}
