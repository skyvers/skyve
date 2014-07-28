package org.skyve.wildcat.metadata.repository.router;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.MetaData;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.web.WebAction;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.ROUTER_NAMESPACE)
public class RouteCriteria implements MetaData {
	private static final long serialVersionUID = 7017356339189117479L;

	private ViewType viewType;
	private WebAction webAction;
	private String moduleName;
	private String documentName;
	private String queryName;
	private String customerName;
	private String dataGroupId;
	private String userId;

	public ViewType getViewType() {
		return viewType;
	}
	@XmlAttribute
	public void setViewType(ViewType viewType) {
		this.viewType = viewType;
	}
	
	public WebAction getWebAction() {
		return webAction;
	}
	@XmlAttribute
	public void setWebAction(WebAction webAction) {
		this.webAction = webAction;
	}
	
	public String getModuleName() {
		return moduleName;
	}
	@XmlAttribute(name = "module")
	public void setModuleName(String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}

	public String getDocumentName() {
		return documentName;
	}
	@XmlAttribute(name = "document")
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	public String getQueryName() {
		return queryName;
	}
	@XmlAttribute(name = "query")
	public void setQueryName(String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}

	public String getCustomerName() {
		return customerName;
	}
	@XmlAttribute(name = "customer")
	public void setCustomerName(String customerName) {
		this.customerName = UtilImpl.processStringValue(customerName);
	}

	public String getDataGroupId() {
		return dataGroupId;
	}
	@XmlAttribute
	public void setDataGroupId(String dataGroupId) {
		this.dataGroupId = UtilImpl.processStringValue(dataGroupId);
	}
	
	public String getUserId() {
		return userId;
	}
	@XmlAttribute
	public void setUserId(String userId) {
		this.userId = UtilImpl.processStringValue(userId);
	}

	public boolean matches(RouteCriteria criteria) {
		if ((customerName != null) && (! customerName.equals(criteria.customerName))) {
			return false;
		}
		if ((moduleName != null) && (! moduleName.equals(criteria.moduleName))) {
			return false;
		}
		if ((documentName != null) && (! documentName.equals(criteria.documentName))) {
			return false;
		}
		if ((queryName != null) && (! queryName.equals(criteria.queryName))) {
			return false;
		}
		if ((dataGroupId != null) && (! dataGroupId.equals(criteria.dataGroupId))) {
			return false;
		}
		if ((userId != null) && (! userId.equals(criteria.userId))) {
			return false;
		}
		if ((viewType != null) && (! viewType.equals(criteria.viewType))) {
			return false;
		}
		if ((webAction != null) && (! webAction.equals(criteria.webAction))) {
			return false;
		}

		return true;
	}
	
	@Override
	public String toString() {
		StringBuilder result = new StringBuilder(64);
		result.append('{');
		boolean something = false;
		if (customerName != null) {
			result.append("customerName=").append(customerName).append(',');
			something = true;
		}
		if (moduleName != null) {
			result.append("moduleName=").append(moduleName).append(',');
			something = true;
		}
		if (documentName != null) {
			result.append("documentName=").append(documentName).append(',');
			something = true;
		}
		if (queryName != null) {
			result.append("queryName=").append(queryName).append(',');
			something = true;
		}
		if (dataGroupId != null) {
			result.append("dataGroupId=").append(dataGroupId).append(',');
			something = true;
		}
		if (userId != null) {
			result.append("userId=").append(userId).append(',');
			something = true;
		}
		if (viewType != null) {
			result.append("viewType=").append(viewType).append(',');
			something = true;
		}
		if (webAction != null) {
			result.append("webAction=").append(webAction).append(',');
			something = true;
		}
		if (something) {
			result.setLength(result.length() - 1); // remove last comma
		}
		result.append('}');
		
		return result.toString();
	}
}
