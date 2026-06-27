package org.skyve.impl.metadata.view.widget;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Invisible;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated spacer widget that inserts blank space in a form layout.
 *
 * <p>Occupies a form item cell with configurable pixel dimensions and
 * optional invisible condition.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"pixelWidth", "pixelHeight", "invisibleConditionName", "visibleConditionName", "properties"})
public class Spacer implements AbsoluteSize, FormItemWidget, Invisible {
	private static final long serialVersionUID = -3535525299887373582L;

	private Integer pixelWidth;
	private Integer pixelHeight;
	private String invisibleConditionName;
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Indicates that spacer widgets do not render a form label by default.
	 *
	 * @return {@code false} always
	 */
	@Override
	public boolean showsLabelByDefault() {
		return false;
	}
	
	/**
	 * Returns the spacer width in pixels.
	 *
	 * @return the pixel width, or {@code null} when not set
	 */
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the spacer width in pixels.
	 *
	 * @param pixelWidth the pixel width
	 */
	@Override
	@XmlAttribute
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the spacer height in pixels.
	 *
	 * @return the pixel height, or {@code null} when not set
	 */
	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	/**
	 * Sets the spacer height in pixels.
	 *
	 * @param pixelHeight the pixel height
	 */
	@Override
	@XmlAttribute
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	/**
	 * Returns the mutable decorator property map for this spacer widget.
	 *
	 * @return the mutable decorator property map keyed by property name
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}

	/**
	 * Returns the invisible condition expression.
	 *
	 * @return the invisible condition expression, or {@code null} when not set
	 */
	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	/**
	 * Sets the invisible condition expression directly.
	 *
	 * @param invisibleConditionName the invisible condition expression
	 */
	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
	}

	/**
	 * JAXB placeholder for the visible condition attribute.
	 *
	 * @return always {@code null}; the visible condition is derived from the invisible condition
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getVisibleConditionName() {
		return null;
	}

	/**
	 * Sets the visible condition by storing its negation as the internal invisible condition.
	 *
	 * @param visibleConditionName the visible condition expression
	 */
	@Override
	@XmlAttribute(name = "visible", required = false)
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
	}
}
