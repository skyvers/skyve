package org.skyve.impl.metadata.view.widget.bound.tabular;

import java.util.List;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.Identifiable;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.metadata.view.widget.bound.AbstractBound;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Invisible;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Abstract JAXB base for data-bound tabular widgets ({@link DataGrid},
 * {@link DataRepeater}) that render rows from an in-memory bean collection.
 *
 * <p>Provides relative sizing, visibility conditions, and a widget identifier.
 * The collection is supplied by the binding inherited from {@link AbstractBound}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see DataGrid
 * @see DataRepeater
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"widgetId",
							"title", 
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
							"visibleConditionName"})
public abstract class AbstractDataWidget extends AbstractBound implements RelativeSize, Invisible, Identifiable {
	private static final long serialVersionUID = 8143928198512212919L;

	private String widgetId;
	private String title;
	
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

	/**
	 * Returns the ordered column definitions rendered for each collection element.
	 *
	 * <p>Implementations return a live, mutable list used directly by metadata consumers.
	 *
	 * @return ordered mutable column metadata
	 */
	public abstract List<? extends TabularColumn> getColumns();

	/**
	 * Returns the optional widget identifier used for client-side targeting.
	 *
	 * @return the widget identifier, or {@code null} when not set
	 */
	@Override
	public String getWidgetId() {
		return widgetId;
	}

	/**
	 * Sets the optional widget identifier after trimming and empty-string normalisation.
	 *
	 * @param widgetId the widget identifier
	 */
	@XmlAttribute(required = false)
	public void setWidgetId(String widgetId) {
		this.widgetId = UtilImpl.processStringValue(widgetId);
	}

	/**
	 * Returns the configured tabular title expression.
	 *
	 * @return the configured title text
	 */
	public String getTitle() {
		return title;
	}
	
	/**
	 * Returns the title localised for the current user locale.
	 *
	 * @return the localised title text
	 */
	public String getLocalisedTitle() {
		return Util.i18n(title);
	}

	/**
	 * Sets the title after trimming and empty-string normalisation.
	 *
	 * @param title the title text to set
	 */
	@XmlAttribute(required = false)
	public void setTitle(String title) {
		this.title = UtilImpl.processStringValue(title);
	}

	/**
	 * Returns the absolute pixel width, or {@code null} when not specified.
	 *
	 * @return configured pixel width
	 */
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the absolute pixel width.
	 *
	 * @param pixelWidth width in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the responsive width unit allocation.
	 *
	 * @return responsive width units
	 */
	@Override
	public Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	/**
	 * Sets the responsive width unit allocation.
	 *
	 * @param responsiveWidth responsive width units
	 */
	@Override
	@XmlAttribute(required = false)
	public void setResponsiveWidth(Integer responsiveWidth) {
		this.responsiveWidth = responsiveWidth;
	}

	/**
	 * Returns the small-breakpoint width override.
	 *
	 * @return small-breakpoint width units
	 */
	@Override
	public Integer getSm() {
		return sm;
	}
	
	/**
	 * Sets the small-breakpoint width override.
	 *
	 * @param sm small-breakpoint width units
	 */
	@Override
	@XmlAttribute(required = false)
	public void setSm(Integer sm) {
		this.sm = sm;
	}

	/**
	 * Returns the medium-breakpoint width override.
	 *
	 * @return medium-breakpoint width units
	 */
	@Override
	public Integer getMd() {
		return md;
	}
	
	/**
	 * Sets the medium-breakpoint width override.
	 *
	 * @param md medium-breakpoint width units
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMd(Integer md) {
		this.md = md;
	}
	
	/**
	 * Returns the large-breakpoint width override.
	 *
	 * @return large-breakpoint width units
	 */
	@Override
	public Integer getLg() {
		return lg;
	}
	
	/**
	 * Sets the large-breakpoint width override.
	 *
	 * @param lg large-breakpoint width units
	 */
	@Override
	@XmlAttribute(required = false)
	public void setLg(Integer lg) {
		this.lg = lg;
	}

	/**
	 * Returns the extra-large-breakpoint width override.
	 *
	 * @return extra-large-breakpoint width units
	 */
	@Override
	public Integer getXl() {
		return xl;
	}
	
	/**
	 * Sets the extra-large-breakpoint width override.
	 *
	 * @param xl extra-large-breakpoint width units
	 */
	@Override
	@XmlAttribute(required = false)
	public void setXl(Integer xl) {
		this.xl = xl;
	}

	/**
	 * Returns the percentage width allocation.
	 *
	 * @return width percentage
	 */
	@Override
	public Integer getPercentageWidth() {
		return percentageWidth;
	}

	/**
	 * Sets the percentage width allocation.
	 *
	 * @param percentageWidth width percentage
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPercentageWidth(Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	/**
	 * Returns the minimum pixel width constraint.
	 *
	 * @return minimum width in pixels
	 */
	@Override
	public Integer getMinPixelWidth() {
		return minPixelWidth;
	}

	/**
	 * Sets the minimum pixel width constraint.
	 *
	 * @param minPixelWidth minimum width in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMinPixelWidth(Integer minPixelWidth) {
		this.minPixelWidth = minPixelWidth;
	}

	/**
	 * Returns the maximum pixel width constraint.
	 *
	 * @return maximum width in pixels
	 */
	@Override
	public Integer getMaxPixelWidth() {
		return maxPixelWidth;
	}

	/**
	 * Sets the maximum pixel width constraint.
	 *
	 * @param maxPixelWidth maximum width in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelWidth(Integer maxPixelWidth) {
		this.maxPixelWidth = maxPixelWidth;
	}

	/**
	 * Returns the absolute pixel height, or {@code null} when not specified.
	 *
	 * @return configured pixel height
	 */
	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	/**
	 * Sets the absolute pixel height.
	 *
	 * @param pixelHeight height in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	/**
	 * Returns the percentage height allocation.
	 *
	 * @return height percentage
	 */
	@Override
	public Integer getPercentageHeight() {
		return percentageHeight;
	}

	/**
	 * Sets the percentage height allocation.
	 *
	 * @param percentageHeight height percentage
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPercentageHeight(Integer percentageHeight) {
		this.percentageHeight = percentageHeight;
	}

	/**
	 * Returns the minimum pixel height constraint.
	 *
	 * @return minimum height in pixels
	 */
	@Override
	public Integer getMinPixelHeight() {
		return minPixelHeight;
	}

	/**
	 * Sets the minimum pixel height constraint.
	 *
	 * @param minPixelHeight minimum height in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMinPixelHeight(Integer minPixelHeight) {
		this.minPixelHeight = minPixelHeight;
	}

	/**
	 * Returns the maximum pixel height constraint.
	 *
	 * @return maximum height in pixels
	 */
	@Override
	public Integer getMaxPixelHeight() {
		return maxPixelHeight;
	}

	/**
	 * Sets the maximum pixel height constraint.
	 *
	 * @param maxPixelHeight maximum height in pixels
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelHeight(Integer maxPixelHeight) {
		this.maxPixelHeight = maxPixelHeight;
	}

	/**
	 * Returns the internal invisible condition expression.
	 *
	 * @return invisible condition expression
	 */
	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	/**
	 * Sets the invisible condition expression directly.
	 *
	 * @param invisibleConditionName invisible condition expression
	 */
	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
	}

	/**
	 * JAXB-only placeholder for {@code visible}; value is derived via {@link #setVisibleConditionName(String)}.
	 *
	 * @return always {@code null}; JAXB writes via {@code setVisibleConditionName}
	 */
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getVisibleConditionName() {
		return null;
	}

	/**
	 * Sets the visible condition by storing its negation as the internal invisible condition.
	 *
	 * @param visibleConditionName a condition expression that enables visibility
	 */
	@Override
	@XmlAttribute(name = "visible", required = false)
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
	}
}
