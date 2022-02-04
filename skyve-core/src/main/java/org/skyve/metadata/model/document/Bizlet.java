package org.skyve.metadata.model.document;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

/**
 * A bizlet allows callbacks to be made at pertinent times in the Bizhub server processing.
 * 
 * Event timing is as follows
 * 
 * Bean level -
 * 
 * newInstance() - called after instantiation through the document.newInstance().
 * postLoad() - called after instantiation and population of data store values.
 * preSave() - called before flushing the values to the data store or User press of [Change] button (before document validation).
 * validate() - called after preSave() (and after document validation) but before flushing the values to the data store.
 * preDelete() - called before deletion from the data store.
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
 * @param <T>	The type of document bean we want to process with this Bizlet.
 */
public abstract class Bizlet<T extends Bean> implements MetaData {
	/**
	 * Key/Value pairs for domains defined.
	 */
	public static class DomainValue {
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
			this.description = Util.i18n(description);
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
		public String getDescription() {
			return description;
		}
		
		@Override
		public String toString() {
			return super.toString() + '(' + code + "->" + description + ')';
		}
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
		return bean;
	}
	
	/**
	 * Called on flush of dirty objects on a Persistence.save().
	 * @param bean	The bean to validate
	 * @param e
	 * @throws Exception
	 */
	public void validate(T bean, ValidationException e) throws Exception {
		// do nothing
	}

	/**
	 * NOTE - Use customer.getConstantDomainValues() as this will used cached values.
	 * 
	 * This method is called when domain values are required that will never change.
	 * These values are evaluated once and lazily for a customer in a thin client
	 * and at startup for a fat client.
	 *
	 * @param attributeName	The name of the attribute to get the domain for.
	 * @return An empty List of {@link DomainValue}s.
	 */
	@SuppressWarnings("static-method")
	public List<DomainValue> getConstantDomainValues(String attributeName) throws Exception {
		return null;
	}

	/**
	 * This method is called when domain values are required that will potentially change.
	 * They are in the database (and are updated).
	 * These values are evaluated once for a render response for a thin client
	 * and once per conversation in a fat client.
	 * 
	 * @param attributeName	The name of the attribute to get the domain for.
	 * @return An empty List of {@link DomainValue}s.
	 */
	@SuppressWarnings("static-method")
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
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
	 * @return An empty List of {@link DomainValue}s.
	 */
	public List<DomainValue> getDynamicDomainValues(String attributeName, T bean) throws Exception {
		return null;
	}

	/**
	 * This method is called when auto-complete values are required from a view widget.
	 * They can be based on values from within the containing bean.
	 * 
	 * @param attributeName	The name of the attribute to get the domain for.
	 * @param value The value typed into the view widget used to complete upon.
	 * @param bean	This bean to use to derive the values.
	 * @return null.
	 */
	public List<String> complete(String attributeName, String value, T bean) throws Exception {
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
		// do nothing
	}
	
	/**
	 * Called after a bean is flushed to the data-store on Persistence.save().
	 * This is the place to ensure that other documents/data are saved or update with the bean.
	 * 
	 * @param bean	The bean to be saved (persisted/updated)
	 * @throws Exception
	 */
	public void postSave(T bean) throws Exception {
		// do nothing
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
		// do nothing
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
		// do nothing
	}
	
	/**
	 * Provides a point to do form level operations before implicit actions get run.
	 * @param actionName	The name of the action invoked.
	 * @param bean	The bean that the action pertains to.
	 * @param parentBean	The parent of the bean for an Add or Edit action.
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
		// do nothing
	}
}
