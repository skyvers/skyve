package org.skyve.impl.metadata.view.widget;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Invisible;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated static-image widget that renders an image from a relative
 * file path.
 *
 * <p>Supports relative sizing and visibility conditions and can be placed
 * directly in forms as a form-item widget.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"relativeFile", 
						"pixelWidth", 
						"responsiveWidth",
						"sm",
						"md",
						"lg",
						"xl",
						"percentageWidth",
						"minPixelWidth", 
						"maxPixelWidth", 
						"pixelHeight", 
						"percentageHeight",
						"minPixelHeight", 
						"maxPixelHeight", 
						"invisibleConditionName",
						"visibleConditionName",
						"properties"})
public class StaticImage implements RelativeSize, Invisible, FormItemWidget {
	private static final long serialVersionUID = 314857374179338882L;

	private String relativeFile;
	
	private Integer pixelWidth;
	private Integer responsiveWidth;
	private Integer sm;
	private Integer md;
	private Integer lg;
	private Integer xl;
	private Integer percentageWidth;
	private Integer minPixelWidth;
	private Integer maxPixelWidth;

	private Integer pixelHeight;
	private Integer percentageHeight;
	private Integer minPixelHeight;
	private Integer maxPixelHeight;
	
	private String invisibleConditionName;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Indicates that static-image widgets do not render a form label by default.
	 *
	 * @return {@code false} always
	 */
	@Override
	public boolean showsLabelByDefault() {
		return false;
	}
	
	/**
	 * Returns the relative file path of the static image resource.
	 *
	 * @return the relative image file path
	 */
	public String getRelativeFile() {
		return relativeFile;
	}

	/**
	 * Sets the relative file path of the static image resource.
	 *
	 * @param relativeFile the relative image file path
	 */
	@XmlAttribute(required = false)
	public void setRelativeFile(String relativeFile) {
		this.relativeFile = UtilImpl.processStringValue(relativeFile);
	}

	/**
	 * Returns the absolute width of the widget in pixels.
	 *
	 * @return the pixel width, or {@code null} when width is not fixed
	 */
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the absolute width of the widget in pixels.
	 *
	 * @param pixelWidth the pixel width, or {@code null} to clear the fixed width
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the responsive width used by the renderer.
	 *
	 * @return the responsive width, or {@code null} when not set
	 */
	@Override
	public Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	/**
	 * Sets the responsive width used by the renderer.
	 *
	 * @param responsiveWidth the responsive width, or {@code null} to clear it
	 */
	@Override
	@XmlAttribute(required = false)
	public void setResponsiveWidth(Integer responsiveWidth) {
		this.responsiveWidth = responsiveWidth;
	}

	/**
	 * Returns the small-breakpoint width allocation.
	 *
	 * @return the small-breakpoint width, or {@code null} when not set
	 */
	@Override
	public Integer getSm() {
		return sm;
	}
	
	/**
	 * Sets the small-breakpoint width allocation.
	 *
	 * @param sm the small-breakpoint width, or {@code null} to clear it
	 */
	@Override
	@XmlAttribute(required = false)
	public void setSm(Integer sm) {
		this.sm = sm;
	}

	/**
	 * Returns the medium-breakpoint width allocation.
	 *
	 * @return the medium-breakpoint width, or {@code null} when not set
	 */
	@Override
	public Integer getMd() {
		return md;
	}
	
	/**
	 * Sets the medium-breakpoint width allocation.
	 *
	 * @param md the medium-breakpoint width, or {@code null} to clear it
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMd(Integer md) {
		this.md = md;
	}
	
	/**
	 * Returns the large-breakpoint width allocation.
	 *
	 * @return the large-breakpoint width, or {@code null} when not set
	 */
	@Override
	public Integer getLg() {
		return lg;
	}
	
	/**
	 * Sets the large-breakpoint width allocation.
	 *
	 * @param lg the large-breakpoint width, or {@code null} to clear it
	 */
	@Override
	@XmlAttribute(required = false)
	public void setLg(Integer lg) {
		this.lg = lg;
	}

	/**
	 * Returns the extra-large-breakpoint width allocation.
	 *
	 * @return the extra-large-breakpoint width, or {@code null} when not set
	 */
	@Override
	public Integer getXl() {
		return xl;
	}
	
	/**
	 * Sets the extra-large-breakpoint width allocation.
	 *
	 * @param xl the extra-large-breakpoint width, or {@code null} to clear it
	 */
	@Override
	@XmlAttribute(required = false)
	public void setXl(Integer xl) {
		this.xl = xl;
	}

	/**
	 * Returns the width as a percentage of the available space.
	 *
	 * @return the percentage width, or {@code null} when not set
	 */
	@Override
	public Integer getPercentageWidth() {
		return percentageWidth;
	}

	/**
	 * Sets the width as a percentage of the available space.
	 *
	 * @param percentageWidth the percentage width, or {@code null} to clear it
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPercentageWidth(Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	/**
	 * Returns the minimum widget width in pixels.
	 *
	 * @return the minimum pixel width, or {@code null} when no minimum is enforced
	 */
	@Override
	public Integer getMinPixelWidth() {
		return minPixelWidth;
	}

	/**
	 * Sets the minimum widget width in pixels.
	 *
	 * @param minPixelWidth the minimum pixel width, or {@code null} to clear the minimum
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMinPixelWidth(Integer minPixelWidth) {
		this.minPixelWidth = minPixelWidth;
	}

	/**
	 * Returns the maximum widget width in pixels.
	 *
	 * @return the maximum pixel width, or {@code null} when no maximum is enforced
	 */
	@Override
	public Integer getMaxPixelWidth() {
		return maxPixelWidth;
	}

	/**
	 * Sets the maximum widget width in pixels.
	 *
	 * @param maxPixelWidth the maximum pixel width, or {@code null} to clear the maximum
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelWidth(Integer maxPixelWidth) {
		this.maxPixelWidth = maxPixelWidth;
	}

	/**
	 * Returns the absolute height of the widget in pixels.
	 *
	 * @return the pixel height, or {@code null} when height is not fixed
	 */
	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	/**
	 * Sets the absolute height of the widget in pixels.
	 *
	 * @param pixelHeight the pixel height, or {@code null} to clear the fixed height
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	/**
	 * Returns the height as a percentage of the available space.
	 *
	 * @return the percentage height, or {@code null} when not set
	 */
	@Override
	public Integer getPercentageHeight() {
		return percentageHeight;
	}

	/**
	 * Sets the height as a percentage of the available space.
	 *
	 * @param percentageHeight the percentage height, or {@code null} to clear it
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPercentageHeight(Integer percentageHeight) {
		this.percentageHeight = percentageHeight;
	}

	/**
	 * Returns the minimum widget height in pixels.
	 *
	 * @return the minimum pixel height, or {@code null} when no minimum is enforced
	 */
	@Override
	public Integer getMinPixelHeight() {
		return minPixelHeight;
	}

	/**
	 * Sets the minimum widget height in pixels.
	 *
	 * @param minPixelHeight the minimum pixel height, or {@code null} to clear the minimum
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMinPixelHeight(Integer minPixelHeight) {
		this.minPixelHeight = minPixelHeight;
	}

	/**
	 * Returns the maximum widget height in pixels.
	 *
	 * @return the maximum pixel height, or {@code null} when no maximum is enforced
	 */
	@Override
	public Integer getMaxPixelHeight() {
		return maxPixelHeight;
	}

	/**
	 * Sets the maximum widget height in pixels.
	 *
	 * @param maxPixelHeight the maximum pixel height, or {@code null} to clear the maximum
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelHeight(Integer maxPixelHeight) {
		this.maxPixelHeight = maxPixelHeight;
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
	
	/**
	 * Returns the mutable decorator property map for this static-image widget.
	 *
	 * @return the mutable decorator property map keyed by property name
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
