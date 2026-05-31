package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Popup geometry editor bound to a geometry attribute.
 *
 * <p>Opens a map-based editor for creating or updating point, line, or polygon
 * values depending on {@link GeometryInputType}.
 *
 * <p>Threading: not thread-safe. Read-only after JAXB unmarshalling.
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class Geometry extends ChangeableInputWidget implements AbsoluteWidth, FormItemWidget {
	private static final long serialVersionUID = 7902784327466913291L;
	
	private GeometryInputType type;
	
	private Integer pixelWidth;
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Indicates that this widget renders its label by default when placed in a form.
	 *
	 * @return {@code true} because popup geometry editors behave as labelled form items
	 */
	@Override
	public boolean showsLabelByDefault() {
		return true;
	}
	
	/**
	 * Returns the geometry capture mode allowed by the popup editor.
	 *
	 * @return the permitted geometry input type, or {@code null} when the client
	 *         should use its default mode
	 */
	public GeometryInputType getType() {
		return type;
	}

	/**
	 * Sets the geometry capture mode allowed by the popup editor.
	 *
	 * @param type the permitted geometry input type, or {@code null} to defer to
	 *        the client default
	 */
	@XmlAttribute(required = false)
	public void setType(GeometryInputType type) {
		this.type = type;
	}

	/**
	 * Returns the configured popup-editor width in pixels.
	 *
	 * @return the explicit pixel width, or {@code null} when the client default width applies
	 */
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the popup-editor width in pixels.
	 *
	 * @param pixelWidth the explicit pixel width, or {@code null} to use the
	 *        client default width
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the implementation-specific widget properties.
	 *
	 * <p>The returned map is mutable and live, allowing callers or JAXB to attach
	 * vendor-specific client options directly to this widget definition.
	 *
	 * @return a mutable property map, never {@code null}
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
