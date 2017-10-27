package org.skyve.metadata.view;

import java.util.Collection;
import java.util.List;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.view.widget.bound.Parameter;

/**
 * 
 */
public interface View extends NamedMetaData, Parameterizable, DecoratedMetaData {
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
	 * These represent parameters that are allowed to be populated when creating a new record.
	 */
	@Override
	public List<Parameter> getParameters();
	
	/**
	 * 
	 * @return
	 */
	public String getDocumentation();
}
