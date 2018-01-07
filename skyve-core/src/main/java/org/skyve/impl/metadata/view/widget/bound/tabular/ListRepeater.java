package org.skyve.impl.metadata.view.widget.bound.tabular;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.widget.bound.FilterParameterImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.view.Filterable;
import org.skyve.metadata.view.widget.bound.FilterParameter;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"showColumnHeaders",
							"showGrid",
							"properties"})
public class ListRepeater extends AbstractListWidget implements DecoratedMetaData, Filterable {
	private static final long serialVersionUID = 7695416579584031128L;

	private Boolean showColumnHeaders;
	private Boolean showGrid;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

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
