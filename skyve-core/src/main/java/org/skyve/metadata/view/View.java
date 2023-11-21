package org.skyve.metadata.view;

import java.util.Collection;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.model.ModelMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.PersistentMetaData;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.util.Util;

/**
 * 
 */
public interface View extends NamedMetaData, PersistentMetaData, DecoratedMetaData {
	/**
	 * 
	 */
	@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
	public static enum ViewType {
		/**
		 * 
		 */
		list,
		
		/**
		 * 
		 */
		create,
		
		/**
		 * 
		 */
		edit, 
		
		/**
		 * 
		 */
		pick,
		
		/**
		 * 
		 */
		params
	}

	@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
	public static class ViewParameter implements SerializableMetaData {
		private static final long serialVersionUID = 2845518081930588156L;

		private String fromBinding;
		private String boundTo;
		
		public String getFromBinding() {
			return fromBinding;
		}
		@XmlAttribute(required = true)
		public void setFromBinding(String fromBinding) {
			this.fromBinding = UtilImpl.processStringValue(fromBinding);
		}
		
		public String getBoundTo() {
			return boundTo;
		}
		@XmlAttribute(required = true)
		public void setBoundTo(String boundTo) {
			this.boundTo = UtilImpl.processStringValue(boundTo);
		}
	}
	
	/**
	 * 
	 * @return
	 */
	public String getRefreshConditionName();
	
	/**
	 * 
	 * @return
	 */
	public Integer getRefreshTimeInSeconds();
	
	/**
	 * 
	 * @return
	 */
	public String getRefreshActionName();
	
	/**
	 * 
	 * @return
	 */
	public String getTitle();

	public default String getLocalisedTitle() {
		return Util.i18n(getTitle());
	}
	
	/**
	 * 
	 * @return
	 */
	public String getIcon32x32RelativeFileName();
	
	/**
	 * 
	 * @return
	 */
	public String getIconStyleClass();
	
	public String getHelpRelativeFileName();

	public String getHelpURL();

	public Sidebar getSidebar();
	
	/**
	 * The widgetId of the actions panel for the renderer to point at if needed in isolation.
	 * 
	 * @return
	 */
	public String getActionsWidgetId();

	/**
	 * 
	 * @param actionName
	 * @return
	 */
	public Action getAction(String actionName);
	
	/**
	 * 
	 * @return
	 */
	public Collection<Action> getActions();

	/**
	 * Get an implicit inlined model - ie a chart model from a given modelName.
	 * 
	 * @param modelName	The name of the model to get
	 * @return	Model meta data.
	 */
	public ModelMetaData getInlineModel(String modelName);
	
	/**
	 * These represent parameters that are allowed to be populated when creating a new record.
	 */
	public List<ViewParameter> getParameters();
	
	/**
	 * 
	 * @return
	 */
	public String getDocumentation();
	
	/**
	 * The customer name for this overridden view, or null if not overridden.
	 * @return The overridden customer name or null. 
	 */
	public String getOverriddenCustomerName();
	
	/**
	 * The ux/ui name for this overridden view, or null if not overridden.
	 * @return The overridden ux/ui name or null. 
	 */
	public String getOverriddenUxUiName();
}
