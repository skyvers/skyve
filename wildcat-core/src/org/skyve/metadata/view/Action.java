package org.skyve.metadata.view;

import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.wildcat.util.XMLUtil;

/**
 * 
 */
public interface Action extends NamedMetaData, Disableable, Invisible, Parameterizable {
	/**
	 * 
	 */
	@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
	public enum RenderHint {
		/**
		 * 
		 */
		button,
		
		/**
		 * 
		 */
		tool;
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
	
	/**
	 * 
	 * @return
	 */
	public String getRelativeIconFileName();
	
	/**
	 * 
	 * @return
	 */
	public String getConfirmationText();
	
	/**
	 * 
	 * @return
	 */
	public String getToolTip();
	
	/**
	 * 
	 * @return
	 */
	public Boolean getInActionPanel();
	
	/**
	 * 
	 * @return
	 */
	public RenderHint getRenderHint();
	
	/**
	 * 
	 * @param customer
	 * @param document
	 * @return
	 * @throws MetaDataException
	 */
	public ServerSideAction<?> getServerSideAction(Customer customer, Document document)
	throws MetaDataException;
}
