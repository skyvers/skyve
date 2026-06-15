package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.ShrinkWrap;
import org.skyve.impl.metadata.view.VerticalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.container.VBox;

/**
 * Fluent builder for vertical box container metadata.
 */
public class FluentVBox extends FluentContainer<FluentVBox> implements FluentBox<FluentVBox> {
	private VBox box = null;
	
	/**
	 * Creates a builder backed by a new {@link VBox} metadata instance.
	 */
	public FluentVBox() {
		this.box = new VBox();
	}
	
	/**
	 * Creates a builder backed by the supplied vertical-box metadata instance.
	 *
	 * @param box
	 *            the vertical-box metadata to mutate
	 */
	public FluentVBox(VBox box) {
		this.box = box;
	}
	
	/**
	 * Copies vertical-box metadata into this builder.
	 *
	 * <p>Side effects: replaces layout, alignment, visibility condition, and contained widgets.
	 *
	 * @param box
	 *            the source vertical box metadata
	 * @return this builder
	 */
	public FluentVBox from(@SuppressWarnings("hiding") VBox box) {
		widgetId(box.getWidgetId());
		border(Boolean.TRUE.equals(box.getBorder()));
		borderTitle(box.getBorderTitle());
		
		box(box, this);
		
		horizontalAlignment(box.getHorizontalAlignment());
		verticalAlignment(box.getVerticalAlignment());

		invisibleConditionName(box.getInvisibleConditionName());
		
		super.from(box);
		
		return this;
	}
	
	/**
	 * Sets the widget id for this vertical box.
	 *
	 * @param widgetId
	 *            the widget identifier
	 * @return this builder
	 */
	public FluentVBox widgetId(String widgetId) {
		box.setWidgetId(widgetId);
		return this;
	}
	
	/**
	 * Enables or disables the border around this box.
	 *
	 * @param border
	 *            whether the border is shown
	 * @return this builder
	 */
	public FluentVBox border(boolean border) {
		box.setBorder(border ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}
	
	/**
	 * Sets the border title.
	 *
	 * @param borderTitle
	 *            border caption text
	 * @return this builder
	 */
	public FluentVBox borderTitle(String borderTitle) {
		box.setBorderTitle(borderTitle);
		return this;
	}
	
	/**
	 * Sets whether this vbox is collapsible. If true, a collapse/expand icon will be shown in the border
	 * (if border is true) and the user can click on it to collapse/expand the box. If false, the box will
	 * not be collapsible. The default value is false.
	 * 
	 * If collapsible is defined, the box must have a border, a border title and a pixel or percentage height
	 * defined. If any of these are missing, an exception will be thrown at runtime.
	 */
	public FluentVBox collapsible(Collapsible collapsible) {
		box.setCollapsible(collapsible);
		return this;
	}

	/**
	 * Sets the vertical box width in pixels.
	 *
	 * @param pixelWidth
	 *            width in pixels
	 * @return this builder
	 */
	@Override
	public FluentVBox pixelWidth(int pixelWidth) {
		box.setPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	/**
	 * Sets the responsive width value.
	 *
	 * @param responsiveWidth
	 *            responsive width setting
	 * @return this builder
	 */
	@Override
	public FluentVBox responsiveWidth(int responsiveWidth) {
		box.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	/**
	 * Sets the small breakpoint width.
	 *
	 * @param sm
	 *            small breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentVBox sm(int sm) {
		box.setSm(Integer.valueOf(sm));
		return this;
	}
	
	/**
	 * Sets the medium breakpoint width.
	 *
	 * @param md
	 *            medium breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentVBox md(int md) {
		box.setMd(Integer.valueOf(md));
		return this;
	}
	
	/**
	 * Sets the large breakpoint width.
	 *
	 * @param lg
	 *            large breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentVBox lg(int lg) {
		box.setLg(Integer.valueOf(lg));
		return this;
	}

	/**
	 * Sets the extra-large breakpoint width.
	 *
	 * @param xl
	 *            extra-large breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentVBox xl(int xl) {
		box.setXl(Integer.valueOf(xl));
		return this;
	}

	/**
	 * Sets width as a percentage of available space.
	 *
	 * @param percentageWidth
	 *            width percentage
	 * @return this builder
	 */
	@Override
	public FluentVBox percentageWidth(int percentageWidth) {
		box.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	/**
	 * Sets minimum width in pixels.
	 *
	 * @param minPixelWidth
	 *            minimum width in pixels
	 * @return this builder
	 */
	@Override
	public FluentVBox minPixelWidth(int minPixelWidth) {
		box.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	/**
	 * Sets maximum width in pixels.
	 *
	 * @param maxPixelWidth
	 *            maximum width in pixels
	 * @return this builder
	 */
	@Override
	public FluentVBox maxPixelWidth(int maxPixelWidth) {
		box.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	/**
	 * Sets height in pixels.
	 *
	 * @param pixelHeight
	 *            height in pixels
	 * @return this builder
	 */
	@Override
	public FluentVBox pixelHeight(int pixelHeight) {
		box.setPixelHeight(Integer.valueOf(pixelHeight));
		return this;
	}

	/**
	 * Sets height as a percentage of available space.
	 *
	 * @param percentageHeight
	 *            height percentage
	 * @return this builder
	 */
	@Override
	public FluentVBox percentageHeight(int percentageHeight) {
		box.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	/**
	 * Sets minimum height in pixels.
	 *
	 * @param minPixelHeight
	 *            minimum height in pixels
	 * @return this builder
	 */
	@Override
	public FluentVBox minPixelHeight(int minPixelHeight) {
		box.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	/**
	 * Sets maximum height in pixels.
	 *
	 * @param maxPixelHeight
	 *            maximum height in pixels
	 * @return this builder
	 */
	@Override
	public FluentVBox maxPixelHeight(int maxPixelHeight) {
		box.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}
	
	/**
	 * Sets container padding in pixels.
	 *
	 * @param pixelPadding
	 *            padding in pixels
	 * @return this builder
	 */
	@Override
	public FluentVBox pixelPadding(int pixelPadding) {
		box.setPixelPadding(Integer.valueOf(pixelPadding));
		return this;
	}

	/**
	 * Sets spacing between contained members in pixels.
	 *
	 * @param pixelMemberPadding
	 *            member spacing in pixels
	 * @return this builder
	 */
	@Override
	public FluentVBox pixelMemberPadding(int pixelMemberPadding) {
		box.setPixelMemberPadding(Integer.valueOf(pixelMemberPadding));
		return this;
	}
	
	/**
	 * Sets shrink-wrap behaviour for this box.
	 *
	 * @param shrinkWrap
	 *            shrink-wrap mode
	 * @return this builder
	 */
	@Override
	public FluentVBox shrinkWrap(ShrinkWrap shrinkWrap) {
		box.setShrinkWrap(shrinkWrap);
		return this;
	}

	/**
	 * Sets horizontal alignment for members in this box.
	 *
	 * @param horizontalAlignment
	 *            horizontal alignment mode
	 * @return this builder
	 */
	public FluentVBox horizontalAlignment(HorizontalAlignment horizontalAlignment) {
		box.setHorizontalAlignment(horizontalAlignment);
		return this;
	}

	/**
	 * Sets vertical alignment for members in this box.
	 *
	 * @param verticalAlignment
	 *            vertical alignment mode
	 * @return this builder
	 */
	public FluentVBox verticalAlignment(VerticalAlignment verticalAlignment) {
		box.setVerticalAlignment(verticalAlignment);
		return this;
	}
	
	/**
	 * Sets the condition name that hides this box when true.
	 *
	 * @param invisibleConditionName
	 *            invisibility condition identifier
	 * @return this builder
	 */
	public FluentVBox invisibleConditionName(String invisibleConditionName) {
		box.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	/**
	 * Returns the backing metadata instance.
	 *
	 * @return the mutable vertical box metadata
	 */
	@Override
	public VBox get() {
		return box;
	}
}
