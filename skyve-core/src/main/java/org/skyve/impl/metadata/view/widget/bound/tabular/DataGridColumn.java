package org.skyve.impl.metadata.view.widget.bound.tabular;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Abstract JAXB base for all column types in a {@link DataGrid} widget.
 *
 * <p>Provides title, horizontal alignment, pixel width, and decorator
 * properties shared by bound and container columns.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see DataGridBoundColumn
 * @see DataGridContainerColumn
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, propOrder = {"title", "alignment", "pixelWidth", "properties"})
public abstract class DataGridColumn implements TabularColumn, AbsoluteWidth, DecoratedMetaData {
	private static final long serialVersionUID = -5532364729219436008L;

	private String title;
	private Boolean escapeTitle;
	private HorizontalAlignment alignment;
	private Integer pixelWidth;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public String getTitle() {
		return title;
	}

	/**
	 * Returns whether the column title text should be escaped before rendering.
	 *
	 * @return {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 */
	public Boolean getEscapeTitle() {
		return escapeTitle;
	}

	@Override
	@XmlAttribute
	public void setTitle(String title) {
		this.title = UtilImpl.processStringValue(title);
	}

	/**
	 * Sets whether the column title text should be escaped before rendering.
	 *
	 * @param escapeTitle {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 */
	@XmlAttribute
	public void setEscapeTitle(Boolean escapeTitle) {
		this.escapeTitle = escapeTitle;
	}

	@Override
	public HorizontalAlignment getAlignment() {
		return alignment;
	}

	@Override
	@XmlAttribute
	public void setAlignment(HorizontalAlignment alignment) {
		this.alignment = alignment;
	}

	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	@Override
	@XmlAttribute
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
