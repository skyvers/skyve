package org.skyve.wildcat.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.metadata.repository.NamedMetaData;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.MODULE_NAMESPACE, 
			name = "query",
			propOrder = {"documentation", "documentName", "displayName", "description", "from", "filter", "columns"})
public class QueryMetaData extends NamedMetaData {
	private String documentName;
	private String from;
	private String filter;
	private String displayName;
	private String description;
	private List<Column> columns = new ArrayList<>();
	private String documentation;

	public String getDocumentName() {
		return documentName;
	}

	@XmlAttribute(required = true)
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	public String getFrom() {
		return from;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE)
	public void setFrom(String from) {
		this.from =  UtilImpl.processStringValue(from);
	}

	public String getFilter() {
		return filter;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE)
	public void setFilter(String filter) {
		this.filter =  UtilImpl.processStringValue(filter);
	}

	public String getDisplayName() {
		return displayName;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE, required = true)
	public void setDisplayName(String displayName) {
		this.displayName = UtilImpl.processStringValue(displayName);
	}

	public String getDescription() {
		return description;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE, required = true)
	public void setDescription(String description) {
		this.description = UtilImpl.processStringValue(description);
	}

	@XmlElementWrapper(namespace = XMLUtil.MODULE_NAMESPACE, name = "columns")
	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE, name = "column", required = true)
	public List<Column> getColumns() {
		return columns;
	}
	
	public String getDocumentation() {
		return documentation;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE)
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}
}
