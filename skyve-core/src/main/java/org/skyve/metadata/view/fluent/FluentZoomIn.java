package org.skyve.metadata.view.fluent;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.metadata.view.Action.ActionShow;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds {@link ZoomIn} action metadata using a fluent API.
 */
public class FluentZoomIn extends FluentBoundWidget<FluentZoomIn>
		implements FluentAbsoluteSize<FluentZoomIn>, FluentConstrainableHeight<FluentZoomIn> {
	private ZoomIn zoom = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link ZoomIn} metadata instance.
	 */
	public FluentZoomIn() {
		zoom = new ZoomIn();
	}
	
	/**
	 * Creates a fluent builder backed by the supplied {@link ZoomIn} metadata instance.
	 *
	 * @param zoom the metadata instance to mutate
	 */
	public FluentZoomIn(ZoomIn zoom) {
		this.zoom = zoom;
	}

	/**
	 * Copies zoom-in metadata into this fluent builder.
	 *
	 * @param zoom
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentZoomIn from(@SuppressWarnings("hiding") ZoomIn zoom) {
		displayName(zoom.getDisplayName());
		escapeDisplayName(zoom.getEscapeDisplayName());
		relativeIconFileName(zoom.getRelativeIconFileName());
		iconStyleClass(zoom.getIconStyleClass());
		toolTip(zoom.getToolTip());
		escapeToolTip(zoom.getEscapeToolTip());
		BindUtil.copyAuthoredCondition(zoom.getXmlDisabledCondition(), zoom.getXmlEnabledCondition(), this::disabledCondition, this::enabledCondition);
		BindUtil.copyAuthoredCondition(zoom.getXmlInvisibleCondition(), zoom.getXmlVisibleCondition(), this::invisibleCondition, this::visibleCondition);
		show(zoom.getShow());
		absoluteSize(zoom, this);
		constrainableHeight(zoom, this);

		super.from(zoom);
		zoom.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

		return this;
	}

	/**
	 * Sets the display name for this zoom-in action.
	 *
	 * @param displayName the label text
	 * @return this builder
	 */
	public FluentZoomIn displayName(String displayName) {
		zoom.setDisplayName(displayName);
		return this;
	}

	/**
	 * Sets whether the display name should be escaped before rendering.
	 *
	 * @param escapeDisplayName {@code false} to allow trusted markup; {@code true} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentZoomIn escapeDisplayName(boolean escapeDisplayName) {
		return escapeDisplayName(escapeDisplayName ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Sets whether the display name should be escaped before rendering.
	 *
	 * @param escapeDisplayName {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentZoomIn escapeDisplayName(Boolean escapeDisplayName) {
		zoom.setEscapeDisplayName(escapeDisplayName);
		return this;
	}

	/**
	 * Sets the relative icon file name for this zoom-in action.
	 *
	 * @param relativeIconFileName the relative path to the icon file
	 * @return this builder
	 */
	public FluentZoomIn relativeIconFileName(String relativeIconFileName) {
		zoom.setRelativeIconFileName(relativeIconFileName);
		return this;
	}

	/**
	 * Sets the CSS style class for the icon of this zoom-in action.
	 *
	 * @param iconStyleClass the CSS class name
	 * @return this builder
	 */
	public FluentZoomIn iconStyleClass(String iconStyleClass) {
		zoom.setIconStyleClass(iconStyleClass);
		return this;
	}

	/**
	 * Sets the tooltip text for this zoom-in action.
	 *
	 * @param toolTip the tooltip string
	 * @return this builder
	 */
	public FluentZoomIn toolTip(String toolTip) {
		zoom.setToolTip(toolTip);
		return this;
	}

	/**
	 * Sets whether the tooltip should be escaped before rendering.
	 *
	 * @param escapeToolTip {@code false} to allow trusted markup; {@code true} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentZoomIn escapeToolTip(boolean escapeToolTip) {
		return escapeToolTip(escapeToolTip ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Sets whether the tooltip should be escaped before rendering.
	 *
	 * @param escapeToolTip {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentZoomIn escapeToolTip(Boolean escapeToolTip) {
		zoom.setEscapeToolTip(escapeToolTip);
		return this;
	}

	/**
	 * Sets the {@code disabled} condition as authored; the opposite sense of {@link #enabledCondition(String)}.
	 *
	 * @param disabledCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentZoomIn disabledCondition(@Nullable String disabledCondition) {
		zoom.setXmlDisabledCondition(disabledCondition);
		return this;
	}

	/**
	 * Sets the {@code enabled} condition as authored; the opposite sense of {@link #disabledCondition(String)}.
	 *
	 * @param enabledCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentZoomIn enabledCondition(@Nullable String enabledCondition) {
		zoom.setXmlEnabledCondition(enabledCondition);
		return this;
	}

	/**
	 * Sets the {@code invisible} condition as authored; the opposite sense of {@link #visibleCondition(String)}.
	 *
	 * @param invisibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentZoomIn invisibleCondition(@Nullable String invisibleCondition) {
		zoom.setXmlInvisibleCondition(invisibleCondition);
		return this;
	}

	/**
	 * Sets the {@code visible} condition as authored; the opposite sense of {@link #invisibleCondition(String)}.
	 *
	 * @param visibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentZoomIn visibleCondition(@Nullable String visibleCondition) {
		zoom.setXmlVisibleCondition(visibleCondition);
		return this;
	}

	/**
	 * Sets the display mode for this zoom-in action.
	 *
	 * @param show the show mode
	 * @return this builder
	 */
	public FluentZoomIn show(ActionShow show) {
		zoom.setShow(show);
		return this;
	}

	/**
	 * Sets the pixel width of this zoom-in widget.
	 *
	 * @param width the pixel width
	 * @return this builder
	 */
	@Override
	public FluentZoomIn pixelWidth(int width) {
		zoom.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the pixel height of this zoom-in widget.
	 *
	 * @param height the pixel height
	 * @return this builder
	 */
	@Override
	public FluentZoomIn pixelHeight(int height) {
		zoom.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Sets the minimum pixel height of this zoom-in widget.
	 *
	 * @param minPixelHeight the minimum pixel height
	 * @return this builder
	 */
	@Override
	public FluentZoomIn minPixelHeight(int minPixelHeight) {
		zoom.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	/**
	 * Sets the maximum pixel height of this zoom-in widget.
	 *
	 * @param maxPixelHeight the maximum pixel height
	 * @return this builder
	 */
	@Override
	public FluentZoomIn maxPixelHeight(int maxPixelHeight) {
		zoom.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	/**
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public @Nonnull FluentZoomIn putProperty(@Nonnull String k, @Nonnull String v) {
		zoom.getProperties().put(k, v);
		return this;
	}

	/**
	 * Returns the wrapped {@link ZoomIn} metadata instance.
	 *
	 * @return the mutable zoom-in metadata being configured
	 */
	@Override
	public ZoomIn get() {
		return zoom;
	}
}
