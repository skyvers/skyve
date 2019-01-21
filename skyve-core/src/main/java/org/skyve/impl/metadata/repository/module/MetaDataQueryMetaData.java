package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "query")
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, 
			name = "query",
			propOrder = {"documentName", "polymorphic", "aggregate", "from", "filter", "columns"})
public class MetaDataQueryMetaData extends QueryMetaData {
	private static final long serialVersionUID = -7717015766195112054L;

	private String documentName;
	private Boolean polymorphic;
	private Boolean aggregate;
	private String from;
	private String filter;
	private List<MetaDataQueryColumnMetaData> columns = new ArrayList<>();

	public String getDocumentName() {
		return documentName;
	}

	@XmlAttribute(required = true)
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	public Boolean getPolymorphic() {
		return polymorphic;
	}

	@XmlAttribute
	public void setPolymorphic(Boolean polymorphic) {
		this.polymorphic = polymorphic;
	}

	public Boolean getAggregate() {
		return aggregate;
	}

	@XmlAttribute
	public void setAggregate(Boolean aggregate) {
		this.aggregate = aggregate;
	}

	public String getFrom() {
		return from;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	public void setFrom(String from) {
		this.from =  UtilImpl.processStringValue(from);
	}

	public String getFilter() {
		return filter;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	public void setFilter(String filter) {
		this.filter =  UtilImpl.processStringValue(filter);
	}

	@XmlElementWrapper(namespace = XMLMetaData.MODULE_NAMESPACE, name = "columns")
	@XmlElementRefs({@XmlElementRef(type = MetaDataQueryProjectedColumnMetaData.class),
						@XmlElementRef(type = MetaDataQueryContentColumnMetaData.class)})
	public List<MetaDataQueryColumnMetaData> getColumns() {
		return columns;
	}
}
