package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.metadata.view.Action.ActionShow;

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
		disabledConditionName(zoom.getDisabledConditionName());
		invisibleConditionName(zoom.getInvisibleConditionName());
		show(zoom.getShow());
		absoluteSize(zoom, this);
		constrainableHeight(zoom, this);

		super.from(zoom);
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
	 * Sets the condition name that disables this zoom-in action.
	 *
	 * @param disabledConditionName the condition name
	 * @return this builder
	 */
	public FluentZoomIn disabledConditionName(String disabledConditionName) {
		zoom.setDisabledConditionName(disabledConditionName);
		return this;
	}

	/**
	 * Sets the condition name that hides this zoom-in action.
	 *
	 * @param invisibleConditionName the condition name
	 * @return this builder
	 */
	public FluentZoomIn invisibleConditionName(String invisibleConditionName) {
		zoom.setInvisibleConditionName(invisibleConditionName);
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
	 * Returns the wrapped {@link ZoomIn} metadata instance.
	 *
	 * @return the mutable zoom-in metadata being configured
	 */
	@Override
	public ZoomIn get() {
		return zoom;
	}
}
