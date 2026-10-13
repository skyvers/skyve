package org.skyve.impl.metadata.view.widget.bound;

import java.util.LinkedHashMap;
import java.util.Map;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.ConstrainableSize;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Invisible;

import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

// TODO Implement the rendering of this
/**
 * JAXB-annotated progress-bar widget bound to a numeric document attribute.
 *
 * <p>Renders a horizontal progress bar whose fill percentage is driven by the
 * bound attribute value relative to optional {@code min} and {@code max}
 * bounds.  Supports absolute and constrained sizes and visibility conditions.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"pixelWidth", 
							"minPixelWidth",
							"maxPixelWidth",
							"pixelHeight",
							"minPixelHeight",
							"maxPixelHeight",
							"xmlInvisibleCondition", 
							"xmlVisibleCondition",
							"properties"})
public class ProgressBar extends AbstractBound implements Invisible, AbsoluteSize, ConstrainableSize, FormItemWidget {
	private static final long serialVersionUID = 4360024000275982927L;

	private Integer pixelWidth;
	private Integer minPixelWidth;
	private Integer maxPixelWidth;
	private Integer pixelHeight;
	private Integer minPixelHeight;
	private Integer maxPixelHeight;
	private String invisibleCondition;
	private String visibleCondition;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new LinkedHashMap<>();

	/**
	 * Indicates progress bars render a label by default.
	 *
	 * @return {@code true} because progress bars show a field label by default
	 */
	@Override
	public boolean showsLabelByDefault() {
		return true;
	}
	
	/**
	 * Returns the absolute pixel width, or {@code null} when renderer defaults apply.
	 *
	 * @return the configured pixel width
	 */
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the absolute pixel width.
	 *
	 * @param pixelWidth the width in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the minimum pixel width constraint.
	 *
	 * @return the minimum width in pixels, or {@code null} when unconstrained
	 */
	@Override
	public Integer getMinPixelWidth() {
		return minPixelWidth;
	}

	/**
	 * Sets the minimum pixel width constraint.
	 *
	 * @param minPixelWidth the minimum width in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMinPixelWidth(Integer minPixelWidth) {
		this.minPixelWidth = minPixelWidth;
	}

	/**
	 * Returns the maximum pixel width constraint.
	 *
	 * @return the maximum width in pixels, or {@code null} when unconstrained
	 */
	@Override
	public Integer getMaxPixelWidth() {
		return maxPixelWidth;
	}

	/**
	 * Sets the maximum pixel width constraint.
	 *
	 * @param maxPixelWidth the maximum width in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelWidth(Integer maxPixelWidth) {
		this.maxPixelWidth = maxPixelWidth;
	}

	/**
	 * Returns the absolute pixel height, or {@code null} when renderer defaults apply.
	 *
	 * @return the configured pixel height
	 */
	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	/**
	 * Sets the absolute pixel height.
	 *
	 * @param pixelHeight the height in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	/**
	 * Returns the minimum pixel height constraint.
	 *
	 * @return the minimum height in pixels, or {@code null} when unconstrained
	 */
	@Override
	public Integer getMinPixelHeight() {
		return minPixelHeight;
	}

	/**
	 * Sets the minimum pixel height constraint.
	 *
	 * @param minPixelHeight the minimum height in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMinPixelHeight(Integer minPixelHeight) {
		this.minPixelHeight = minPixelHeight;
	}

	/**
	 * Returns the maximum pixel height constraint.
	 *
	 * @return the maximum height in pixels, or {@code null} when unconstrained
	 */
	@Override
	public Integer getMaxPixelHeight() {
		return maxPixelHeight;
	}

	/**
	 * Sets the maximum pixel height constraint.
	 *
	 * @param maxPixelHeight the maximum height in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelHeight(Integer maxPixelHeight) {
		this.maxPixelHeight = maxPixelHeight;
	}

	/**
	 * Returns the derived {@code invisible} condition that renderers and other consumers read.
	 *
	 * @return the {@code invisible} attribute as authored, or else the negation of the authored
	 * 			{@code visible} attribute; {@code null} when neither was authored
	 */
	@Override
	@XmlTransient
	public @Nullable String getInvisibleCondition() {
		return (invisibleCondition != null) ? invisibleCondition : BindUtil.negateCondition(visibleCondition);
	}

	/**
	 * Sets the {@code invisible} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code visible} attribute.
	 *
	 * @param invisibleCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setXmlInvisibleCondition(@Nullable String invisibleCondition) {
		this.invisibleCondition = UtilImpl.processStringValue(invisibleCondition);
		this.visibleCondition = null;
	}

	/**
	 * Returns the {@code invisible} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code visible} or neither was authored
	 */
	@Override
	public @Nullable String getXmlInvisibleCondition() {
		return invisibleCondition;
	}

	/**
	 * Returns the {@code visible} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code invisible} or neither was authored
	 */
	@Override
	public @Nullable String getXmlVisibleCondition() {
		return visibleCondition;
	}

	/**
	 * Sets the {@code visible} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code invisible} attribute, so that
	 * {@link #getInvisibleCondition()} returns the negation of this condition.
	 *
	 * @param visibleCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "visible", required = false)
	public void setXmlVisibleCondition(@Nullable String visibleCondition) {
		this.visibleCondition = UtilImpl.processStringValue(visibleCondition);
		this.invisibleCondition = null;
	}

	/**
	 * Returns the mutable decorator property map for this widget.
	 *
	 * @return mutable decorator property map keyed by property name
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
