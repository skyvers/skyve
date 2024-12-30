package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "query")
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, 
			name = "query",
			propOrder = {"documentName", "polymorphic", "aggregate", "from", "filter", "grouping", "ordering", "columns"})
public class MetaDataQueryMetaData extends QueryMetaData {
	private static final long serialVersionUID = -7717015766195112054L;

	private String documentName;
	private Boolean polymorphic;
	private Boolean aggregate;
	private String from;
	private String filter;
	private String grouping;
	private String ordering;
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
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setFrom(String from) {
		this.from =  UtilImpl.processStringValue(from);
	}

	public String getFilter() {
		return filter;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setFilter(String filter) {
		this.filter =  UtilImpl.processStringValue(filter);
	}

	public String getGrouping() {
		return grouping;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setGrouping(String grouping) {
		this.grouping =  UtilImpl.processStringValue(grouping);
	}

	public String getOrdering() {
		return ordering;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setOrdering(String ordering) {
		this.ordering =  UtilImpl.processStringValue(ordering);
	}

	@XmlElementWrapper(namespace = XMLMetaData.MODULE_NAMESPACE, name = "columns")
	@XmlElementRefs({@XmlElementRef(type = MetaDataQueryProjectedColumnMetaData.class),
						@XmlElementRef(type = MetaDataQueryContentColumnMetaData.class)})
	public List<MetaDataQueryColumnMetaData> getColumns() {
		return columns;
	}
}
