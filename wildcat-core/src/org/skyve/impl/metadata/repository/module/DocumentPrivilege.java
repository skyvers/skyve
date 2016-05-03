package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLUtil;
import org.skyve.metadata.user.DocumentPermission;

@XmlType(namespace = XMLUtil.MODULE_NAMESPACE, 
			propOrder = {"documentName", 
							"permission", 
							"actions",
							"contentRestrictions", 
							"contentPermissions"})
public class DocumentPrivilege {
	private String documentName;
	private DocumentPermission permission;
	private List<ActionPrivilege> actions = new ArrayList<>();
	private List<ContentRestriction> contentRestrictions = new ArrayList<>();
	private List<ContentPermission> contentPermissions = new ArrayList<>();

	public String getDocumentName() {
		return documentName;
	}

	@XmlAttribute(name = "name", required = true)
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	public DocumentPermission getPermission() {
		return permission;
	}

	@XmlAttribute(required = true)
	public void setPermission(DocumentPermission permission) {
		this.permission = permission;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE, name = "action", required = false)
	public List<ActionPrivilege> getActions() {
		return actions;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE, name = "contentRestriction", required = false)
	public List<ContentRestriction> getContentRestrictions() {
		return contentRestrictions;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE, name = "contentPermission", required = false)
	public List<ContentPermission> getContentPermissions() {
		return contentPermissions;
	}
}
