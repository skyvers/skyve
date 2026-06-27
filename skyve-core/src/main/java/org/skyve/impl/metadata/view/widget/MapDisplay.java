package org.skyve.impl.metadata.view.widget;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.LoadingType;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.view.Invisible;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated map widget that renders map markers from a named map model.
 *
 * <p>Supports lazy/eager loading strategies, optional auto-refresh interval,
 * refresh controls, relative sizing, and visibility conditions.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "map")
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			name = "map",
			propOrder = {"modelName",
							"loading",
							"refreshTimeInSeconds",
							"showRefreshControls",
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
public class MapDisplay implements DecoratedMetaData, RelativeSize, Invisible {
	private static final long serialVersionUID = 6664085314805510891L;

	private String modelName;
	private LoadingType loading;
	private Integer refreshTimeInSeconds;
	private Boolean showRefreshControls;
	
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
	 * Returns the name of the map model used to supply markers and state.
	 *
	 * @return the map model name
	 */
	public String getModelName() {
		return modelName;
	}

	/**
	 * Sets the name of the map model used to supply markers and state.
	 *
	 * @param modelName the map model name
	 */
	@XmlAttribute(required = true)
	public void setModelName(String modelName) {
		this.modelName = UtilImpl.processStringValue(modelName);
	}

	/**
	 * Returns the loading strategy for retrieving map data.
	 *
	 * @return the loading strategy
	 */
	public LoadingType getLoading() {
		return loading;
	}

	/**
	 * Sets the loading strategy for retrieving map data.
	 *
	 * @param loading the loading strategy
	 */
	@XmlAttribute
	public void setLoading(LoadingType loading) {
		this.loading = loading;
	}

	/**
	 * Returns the automatic refresh interval in seconds.
	 *
	 * @return the refresh interval in seconds, or {@code null} when auto-refresh is disabled
	 */
	public Integer getRefreshTimeInSeconds() {
		return refreshTimeInSeconds;
	}

	/**
	 * Sets the automatic refresh interval in seconds.
	 *
	 * @param refreshTimeInSeconds the refresh interval in seconds, or {@code null} to disable auto-refresh
	 */
	@XmlAttribute(required = false)
	public void setRefreshTimeInSeconds(Integer refreshTimeInSeconds) {
		this.refreshTimeInSeconds = refreshTimeInSeconds;
	}

	/**
	 * Returns whether refresh controls are shown for the map widget.
	 *
	 * @return {@code true} if refresh controls are shown, otherwise {@code false}, or {@code null} when unspecified
	 */
	public Boolean getShowRefreshControls() {
		return showRefreshControls;
	}

	/**
	 * Sets whether refresh controls are shown for the map widget.
	 *
	 * @param showRefreshControls {@code true} to show refresh controls, {@code false} to hide them
	 */
	@XmlAttribute(required = false)
	public void setShowRefreshControls(Boolean showRefreshControls) {
		this.showRefreshControls = showRefreshControls;
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
	 * Returns the mutable decorator property map for this map widget.
	 *
	 * @return the mutable decorator property map keyed by property name
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
