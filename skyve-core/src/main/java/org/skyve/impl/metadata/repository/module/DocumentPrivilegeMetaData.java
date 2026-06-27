package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.user.DocumentPermission;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated descriptor for a document-level CRUD privilege within a module
 * role.
 *
 * <p>Specifies the create/read/update/delete flags and an optional scope
 * ({@link org.skyve.metadata.MetaData.DocumentPermissionScope}) for a named
 * document.  Also carries the list of action privileges and the set of roles
 * that are granted access.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ModuleRoleMetaData
 * @see ActionPrivilegeMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, 
			propOrder = {"documentName", 
							"permission", 
							"actions",
							"contentRestrictions", 
							"contentPermissions",
							"properties"})
public class DocumentPrivilegeMetaData implements DecoratedMetaData {
	private static final long serialVersionUID = -300528846187141003L;

	private String documentName;
	private DocumentPermission permission;
	private List<ActionPrivilegeMetaData> actions = new ArrayList<>();
	private List<ContentRestriction> contentRestrictions = new ArrayList<>();
	private List<ContentPermission> contentPermissions = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

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

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "action", required = false)
	public List<ActionPrivilegeMetaData> getActions() {
		return actions;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "contentRestriction", required = false)
	public List<ContentRestriction> getContentRestrictions() {
		return contentRestrictions;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "contentPermission", required = false)
	public List<ContentPermission> getContentPermissions() {
		return contentPermissions;
	}
	
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
