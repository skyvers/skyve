package org.skyve.wildcat.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.MODULE_NAMESPACE, name = "documentQuery")
@XmlType(namespace = XMLUtil.MODULE_NAMESPACE, 
			name = "documentQuery",
			propOrder = {"documentName", "from", "filter", "columns"})
public class DocumentQueryMetaData extends QueryMetaData {
	private String documentName;
	private String from;
	private String filter;
	private List<Column> columns = new ArrayList<>();

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

	@XmlElementWrapper(namespace = XMLUtil.MODULE_NAMESPACE, name = "columns")
	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE, name = "column", required = true)
	public List<Column> getColumns() {
		return columns;
	}
}
