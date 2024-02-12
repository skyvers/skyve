package org.skyve.metadata.view;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlType;

/**
 * 
 */
public interface Action extends NamedMetaData, Disableable, Invisible, Parameterizable, DecoratedMetaData {
	/**
	 * 
	 */
	@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
	public enum ActionShow {
		/**
		 * Show icon only
		 */
		icon,
		
		/**
		 * Show text only
		 */
		text,
		
		/**
		 * Show icon and text
		 */
		both;
	}

	/**
	 * 
	 * @return
	 */
	public ImplicitActionName getImplicitName();
	
	/**
	 * 
	 * @return
	 */
	public String getResourceName();
	
	public Boolean getClientValidation();
	
	/**
	 * 
	 * @return
	 */
	public String getDisplayName();

	public default String getLocalisedDisplayName() {
		return Util.i18n(getDisplayName());
	}

	/**
	 * 
	 * @return
	 */
	public String getRelativeIconFileName();
	
	/**
	 * 
	 * @return
	 */
	public String getIconStyleClass();

	/**
	 * 
	 * @return
	 */
	public String getConfirmationText();
	
	public default String getLocalisedConfirmationText() {
		return Util.i18n(getConfirmationText());
	}
	
	/**
	 * 
	 * @return
	 */
	public String getToolTip();
	
	public default String getLocalisedToolTip() {
		return Util.i18n(getToolTip());
	}
	
	/**
	 * 
	 * @return
	 */
	public Boolean getInActionPanel();
	
	/**
	 * Whether to show text or icon or both.
	 * @return
	 */
	public ActionShow getShow();
	
	/**
	 * 
	 * @param customer
	 * @param document
	 * @return
	 */
	public ServerSideAction<?> getServerSideAction(Customer customer, Document document);
}
