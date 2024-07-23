package org.skyve.impl.metadata.view.widget.bound.tabular;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"showColumnHeaders",
							"showGrid",
							"columns",
							"properties"})
public class DataRepeater extends AbstractDataWidget implements DecoratedMetaData {
	private static final long serialVersionUID = 8324176787351810791L;

	private Boolean showColumnHeaders;
	private Boolean showGrid;

	private List<DataGridColumn> columns = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	@XmlElementRefs({@XmlElementRef(type = DataGridBoundColumn.class),
						@XmlElementRef(type = DataGridContainerColumn.class)})
	public List<DataGridColumn> getColumns() {
		return columns;
	}

	public Boolean getShowColumnHeaders() {
		return showColumnHeaders;
	}

	@XmlAttribute(name = "showColumnHeaders", required = false)
	public void setShowColumnHeaders(Boolean showColumnHeaders) {
		this.showColumnHeaders = showColumnHeaders;
	}

	public Boolean getShowGrid() {
		return showGrid;
	}

	@XmlAttribute(name = "showGrid", required = false)
	public void setShowGrid(Boolean showGrid) {
		this.showGrid = showGrid;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
