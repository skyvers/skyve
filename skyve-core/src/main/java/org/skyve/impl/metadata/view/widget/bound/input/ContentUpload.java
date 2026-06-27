package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Editable;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Defines the XML {@code <content/>} widget for managed-content upload attributes.
 *
 * <p>The widget can present content as a link, image, video, or renderer-selected
 * automatic mode. Threading: instances are mutable during metadata loading and
 * fluent construction, and should be treated as thread-confined until the
 * metadata model is published.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "content")
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			name = "contentUpload",
			propOrder = {"pixelWidth", 
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
							"editable",
							"properties"})
public class ContentUpload extends InputWidget implements Editable, RelativeSize, FormItemWidget {
	private static final long serialVersionUID = 4479941440259945333L;

	private ContentDisplay display;
	private ContentCapture capture;
	private Boolean editable;
	private Boolean showMarkup;
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

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Returns whether forms should show this widget's label by default.
	 *
	 * @return {@code true}; content uploads are labelled unless a renderer overrides
	 *         label placement
	 */
	@Override
	public boolean showsLabelByDefault() {
		return true;
	}

	/**
	 * Returns the explicit presentation mode from metadata.
	 *
	 * @return the configured display mode, or {@code null} when metadata omitted the
	 *         attribute
	 * @see #getResolvedDisplay()
	 */
	public @Nullable ContentDisplay getDisplay() {
		return display;
	}

	/**
	 * Sets the explicit presentation mode from metadata.
	 *
	 * @param display the display mode, or {@code null} to use the default
	 *        {@link ContentDisplay#auto}
	 */
	@XmlAttribute(required = false)
	public void setDisplay(@Nullable ContentDisplay display) {
		this.display = display;
	}

	/**
	 * Returns the explicit capture affordance from metadata.
	 *
	 * @return the configured capture affordance, or {@code null} when metadata omitted
	 *         the attribute
	 * @see #getResolvedCapture()
	 */
	public @Nullable ContentCapture getCapture() {
		return capture;
	}

	/**
	 * Sets the explicit capture affordance from metadata.
	 *
	 * @param capture the capture affordance, or {@code null} to use the default
	 *        {@link ContentCapture#none}
	 */
	@XmlAttribute(required = false)
	public void setCapture(@Nullable ContentCapture capture) {
		this.capture = capture;
	}

	/**
	 * Returns the effective presentation mode used for validation and rendering.
	 *
	 * @return the configured display mode, or {@link ContentDisplay#auto} when the
	 *         XML attribute is absent
	 */
	public @Nonnull ContentDisplay getResolvedDisplay() {
		return (display == null) ? ContentDisplay.auto : display;
	}

	/**
	 * Returns the effective capture affordance used for validation and rendering.
	 *
	 * @return the configured capture affordance, or {@link ContentCapture#none} when
	 *         the XML attribute is absent
	 */
	public @Nonnull ContentCapture getResolvedCapture() {
		return (capture == null) ? ContentCapture.none : capture;
	}

	/**
	 * Returns the explicit editability override.
	 *
	 * @return {@link Boolean#TRUE} or {@link Boolean#FALSE} when configured, otherwise
	 *         {@code null} to inherit renderer defaults
	 */
	@Override
	public @Nullable Boolean getEditable() {
		return editable;
	}

	/**
	 * Sets the explicit editability override.
	 *
	 * @param editable {@link Boolean#TRUE} or {@link Boolean#FALSE} to override
	 *        editability, or {@code null} to inherit renderer defaults
	 */
	@Override
	@XmlAttribute(name = "editable", required = false)
	public void setEditable(@Nullable Boolean editable) {
		this.editable = editable;
	}

	/**
	 * Returns whether image markup controls are requested.
	 *
	 * @return {@link Boolean#TRUE} to request markup controls, {@link Boolean#FALSE}
	 *         to suppress them, or {@code null} when metadata omitted the attribute
	 */
	public @Nullable Boolean getShowMarkup() {
		return showMarkup;
	}

	/**
	 * Sets whether image markup controls are requested.
	 *
	 * @param showMarkup the explicit markup-control flag, or {@code null} when absent
	 */
	@XmlAttribute(name = "showMarkup", required = false)
	public void setShowMarkup(@Nullable Boolean showMarkup) {
		this.showMarkup = showMarkup;
	}

	/**
	 * Returns the fixed pixel width requested by metadata.
	 *
	 * @return the pixel width, or {@code null} when unspecified
	 */
	@Override
	public @Nullable Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the fixed pixel width requested by metadata.
	 *
	 * @param pixelWidth the pixel width, or {@code null} when unspecified
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(@Nullable Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the responsive grid width requested by metadata.
	 *
	 * @return the responsive width, or {@code null} when unspecified
	 */
	@Override
	public @Nullable Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	/**
	 * Sets the responsive grid width requested by metadata.
	 *
	 * @param responsiveWidth the responsive width, or {@code null} when unspecified
	 */
	@Override
	@XmlAttribute(required = false)
	public void setResponsiveWidth(@Nullable Integer responsiveWidth) {
		this.responsiveWidth = responsiveWidth;
	}

	/**
	 * Returns the small-breakpoint responsive width.
	 *
	 * @return the small-breakpoint width, or {@code null} when unspecified
	 */
	@Override
	public @Nullable Integer getSm() {
		return sm;
	}

	/**
	 * Sets the small-breakpoint responsive width.
	 *
	 * @param sm the small-breakpoint width, or {@code null} when unspecified
	 */
	@Override
	@XmlAttribute(required = false)
	public void setSm(@Nullable Integer sm) {
		this.sm = sm;
	}

	/**
	 * Returns the medium-breakpoint responsive width.
	 *
	 * @return the medium-breakpoint width, or {@code null} when unspecified
	 */
	@Override
	public @Nullable Integer getMd() {
		return md;
	}

	/**
	 * Sets the medium-breakpoint responsive width.
	 *
	 * @param md the medium-breakpoint width, or {@code null} when unspecified
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMd(@Nullable Integer md) {
		this.md = md;
	}

	/**
	 * Returns the large-breakpoint responsive width.
	 *
	 * @return the large-breakpoint width, or {@code null} when unspecified
	 */
	@Override
	public @Nullable Integer getLg() {
		return lg;
	}

	/**
	 * Sets the large-breakpoint responsive width.
	 *
	 * @param lg the large-breakpoint width, or {@code null} when unspecified
	 */
	@Override
	@XmlAttribute(required = false)
	public void setLg(@Nullable Integer lg) {
		this.lg = lg;
	}

	/**
	 * Returns the extra-large-breakpoint responsive width.
	 *
	 * @return the extra-large-breakpoint width, or {@code null} when unspecified
	 */
	@Override
	public @Nullable Integer getXl() {
		return xl;
	}

	/**
	 * Sets the extra-large-breakpoint responsive width.
	 *
	 * @param xl the extra-large-breakpoint width, or {@code null} when unspecified
	 */
	@Override
	@XmlAttribute(required = false)
	public void setXl(@Nullable Integer xl) {
		this.xl = xl;
	}

	/**
	 * Returns the percentage width requested by metadata.
	 *
	 * @return the percentage width, or {@code null} when unspecified
	 */
	@Override
	public @Nullable Integer getPercentageWidth() {
		return percentageWidth;
	}

	/**
	 * Sets the percentage width requested by metadata.
	 *
	 * @param percentageWidth the percentage width, or {@code null} when unspecified
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPercentageWidth(@Nullable Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	/**
	 * Returns the minimum pixel width requested by metadata.
	 *
	 * @return the minimum pixel width, or {@code null} when unspecified
	 */
	@Override
	public @Nullable Integer getMinPixelWidth() {
		return minPixelWidth;
	}

	/**
	 * Sets the minimum pixel width requested by metadata.
	 *
	 * @param minPixelWidth the minimum pixel width, or {@code null} when unspecified
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMinPixelWidth(@Nullable Integer minPixelWidth) {
		this.minPixelWidth = minPixelWidth;
	}

	/**
	 * Returns the maximum pixel width requested by metadata.
	 *
	 * @return the maximum pixel width, or {@code null} when unspecified
	 */
	@Override
	public @Nullable Integer getMaxPixelWidth() {
		return maxPixelWidth;
	}

	/**
	 * Sets the maximum pixel width requested by metadata.
	 *
	 * @param maxPixelWidth the maximum pixel width, or {@code null} when unspecified
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelWidth(@Nullable Integer maxPixelWidth) {
		this.maxPixelWidth = maxPixelWidth;
	}

	/**
	 * Returns the fixed pixel height requested by metadata.
	 *
	 * @return the pixel height, or {@code null} when unspecified
	 */
	@Override
	public @Nullable Integer getPixelHeight() {
		return pixelHeight;
	}

	/**
	 * Sets the fixed pixel height requested by metadata.
	 *
	 * @param pixelHeight the pixel height, or {@code null} when unspecified
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(@Nullable Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	/**
	 * Returns the percentage height requested by metadata.
	 *
	 * @return the percentage height, or {@code null} when unspecified
	 */
	@Override
	public @Nullable Integer getPercentageHeight() {
		return percentageHeight;
	}

	/**
	 * Sets the percentage height requested by metadata.
	 *
	 * @param percentageHeight the percentage height, or {@code null} when unspecified
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPercentageHeight(@Nullable Integer percentageHeight) {
		this.percentageHeight = percentageHeight;
	}

	/**
	 * Returns the minimum pixel height requested by metadata.
	 *
	 * @return the minimum pixel height, or {@code null} when unspecified
	 */
	@Override
	public @Nullable Integer getMinPixelHeight() {
		return minPixelHeight;
	}

	/**
	 * Sets the minimum pixel height requested by metadata.
	 *
	 * @param minPixelHeight the minimum pixel height, or {@code null} when unspecified
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMinPixelHeight(@Nullable Integer minPixelHeight) {
		this.minPixelHeight = minPixelHeight;
	}

	/**
	 * Returns the maximum pixel height requested by metadata.
	 *
	 * @return the maximum pixel height, or {@code null} when unspecified
	 */
	@Override
	public @Nullable Integer getMaxPixelHeight() {
		return maxPixelHeight;
	}

	/**
	 * Sets the maximum pixel height requested by metadata.
	 *
	 * @param maxPixelHeight the maximum pixel height, or {@code null} when unspecified
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelHeight(@Nullable Integer maxPixelHeight) {
		this.maxPixelHeight = maxPixelHeight;
	}

	/**
	 * Returns renderer-specific extension properties.
	 *
	 * @return the mutable property map owned by this metadata instance; never
	 *         {@code null}
	 */
	@Override
	public @Nonnull Map<String, String> getProperties() {
		return properties;
	}
}
