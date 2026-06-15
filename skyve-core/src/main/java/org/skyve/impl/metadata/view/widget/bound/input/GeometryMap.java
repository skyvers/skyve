package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.metadata.view.event.Changeable;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Inline geometry map editor bound to a geometry attribute.
 *
 * <p>Renders the map widget directly in the form and supports geometry type
 * constraints, responsive sizing, and change-event handlers.
 *
 * <p>Threading: not thread-safe. Read-only after JAXB unmarshalling.
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"type",
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
							"changedActions",
							"properties"})
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class GeometryMap extends InputWidget implements RelativeSize, Changeable, FormItemWidget {
	private static final long serialVersionUID = 26075963141105360L;

	private GeometryInputType type;
	
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
	
	private List<EventAction> changedActions = new ArrayList<>();
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Indicates whether this widget renders a label by default.
	 *
	 * @return {@code true} because geometry map widgets are form items with labels.
	 */
	@Override
	public boolean showsLabelByDefault() {
		return true;
	}
	
	/**
	 * Returns the allowed geometry input type for this map editor.
	 *
	 * @return the configured geometry type constraint, or {@code null} to allow any type.
	 */
	public GeometryInputType getType() {
		return type;
	}

	/**
	 * Sets the allowed geometry input type for this map editor.
	 *
	 * @param type the geometry type constraint, or {@code null} for no explicit type restriction.
	 */
	@XmlAttribute(required = false)
	public void setType(GeometryInputType type) {
		this.type = type;
	}

	/**
	 * Returns the fixed width in pixels.
	 *
	 * @return the pixel width, or {@code null} when not explicitly set.
	 */
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the fixed width in pixels.
	 *
	 * @param pixelWidth the pixel width, or {@code null} to clear the fixed-width setting.
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the responsive grid width for default breakpoints.
	 *
	 * @return the responsive width value, or {@code null} when unset.
	 */
	@Override
	public Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	/**
	 * Sets the responsive grid width for default breakpoints.
	 *
	 * @param responsiveWidth the responsive width value, or {@code null} to clear it.
	 */
	@Override
	@XmlAttribute(required = false)
	public void setResponsiveWidth(Integer responsiveWidth) {
		this.responsiveWidth = responsiveWidth;
	}

	/**
	 * Returns the small-breakpoint width value.
	 *
	 * @return the {@code sm} width, or {@code null} when unset.
	 */
	@Override
	public Integer getSm() {
		return sm;
	}
	
	/**
	 * Sets the small-breakpoint width value.
	 *
	 * @param sm the {@code sm} width, or {@code null} to clear it.
	 */
	@Override
	@XmlAttribute(required = false)
	public void setSm(Integer sm) {
		this.sm = sm;
	}

	/**
	 * Returns the medium-breakpoint width value.
	 *
	 * @return the {@code md} width, or {@code null} when unset.
	 */
	@Override
	public Integer getMd() {
		return md;
	}
	
	/**
	 * Sets the medium-breakpoint width value.
	 *
	 * @param md the {@code md} width, or {@code null} to clear it.
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMd(Integer md) {
		this.md = md;
	}
	
	/**
	 * Returns the large-breakpoint width value.
	 *
	 * @return the {@code lg} width, or {@code null} when unset.
	 */
	@Override
	public Integer getLg() {
		return lg;
	}
	
	/**
	 * Sets the large-breakpoint width value.
	 *
	 * @param lg the {@code lg} width, or {@code null} to clear it.
	 */
	@Override
	@XmlAttribute(required = false)
	public void setLg(Integer lg) {
		this.lg = lg;
	}

	/**
	 * Returns the extra-large-breakpoint width value.
	 *
	 * @return the {@code xl} width, or {@code null} when unset.
	 */
	@Override
	public Integer getXl() {
		return xl;
	}
	
	/**
	 * Sets the extra-large-breakpoint width value.
	 *
	 * @param xl the {@code xl} width, or {@code null} to clear it.
	 */
	@Override
	@XmlAttribute(required = false)
	public void setXl(Integer xl) {
		this.xl = xl;
	}

	/**
	 * Returns the width as a percentage value.
	 *
	 * @return the percentage width, or {@code null} when unset.
	 */
	@Override
	public Integer getPercentageWidth() {
		return percentageWidth;
	}

	/**
	 * Sets the width as a percentage value.
	 *
	 * @param percentageWidth the percentage width, or {@code null} to clear it.
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPercentageWidth(Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	/**
	 * Returns the minimum width in pixels.
	 *
	 * @return the minimum pixel width, or {@code null} when unset.
	 */
	@Override
	public Integer getMinPixelWidth() {
		return minPixelWidth;
	}

	/**
	 * Sets the minimum width in pixels.
	 *
	 * @param minPixelWidth the minimum pixel width, or {@code null} to clear it.
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMinPixelWidth(Integer minPixelWidth) {
		this.minPixelWidth = minPixelWidth;
	}

	/**
	 * Returns the maximum width in pixels.
	 *
	 * @return the maximum pixel width, or {@code null} when unset.
	 */
	@Override
	public Integer getMaxPixelWidth() {
		return maxPixelWidth;
	}

	/**
	 * Sets the maximum width in pixels.
	 *
	 * @param maxPixelWidth the maximum pixel width, or {@code null} to clear it.
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelWidth(Integer maxPixelWidth) {
		this.maxPixelWidth = maxPixelWidth;
	}

	/**
	 * Returns the fixed height in pixels.
	 *
	 * @return the pixel height, or {@code null} when not explicitly set.
	 */
	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	/**
	 * Sets the fixed height in pixels.
	 *
	 * @param pixelHeight the pixel height, or {@code null} to clear the fixed-height setting.
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	/**
	 * Returns the height as a percentage value.
	 *
	 * @return the percentage height, or {@code null} when unset.
	 */
	@Override
	public Integer getPercentageHeight() {
		return percentageHeight;
	}

	/**
	 * Sets the height as a percentage value.
	 *
	 * @param percentageHeight the percentage height, or {@code null} to clear it.
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPercentageHeight(Integer percentageHeight) {
		this.percentageHeight = percentageHeight;
	}

	/**
	 * Returns the minimum height in pixels.
	 *
	 * @return the minimum pixel height, or {@code null} when unset.
	 */
	@Override
	public Integer getMinPixelHeight() {
		return minPixelHeight;
	}

	/**
	 * Sets the minimum height in pixels.
	 *
	 * @param minPixelHeight the minimum pixel height, or {@code null} to clear it.
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMinPixelHeight(Integer minPixelHeight) {
		this.minPixelHeight = minPixelHeight;
	}

	/**
	 * Returns the maximum height in pixels.
	 *
	 * @return the maximum pixel height, or {@code null} when unset.
	 */
	@Override
	public Integer getMaxPixelHeight() {
		return maxPixelHeight;
	}

	/**
	 * Sets the maximum height in pixels.
	 *
	 * @param maxPixelHeight the maximum pixel height, or {@code null} to clear it.
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelHeight(Integer maxPixelHeight) {
		this.maxPixelHeight = maxPixelHeight;
	}

	/**
	 * Returns the ordered list of event actions fired after a geometry value change.
	 *
	 * @return the mutable change handler list, never {@code null}.
	 */
	@Override
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "onChangedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class),
						@XmlElementRef(type = ToggleDisabledEventAction.class),
						@XmlElementRef(type = ToggleVisibilityEventAction.class)})
	public List<EventAction> getChangedActions() {
		return changedActions;
	}
	
	/**
	 * Returns additional renderer-specific properties for this widget.
	 *
	 * @return the mutable property map, never {@code null}.
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
