package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.ShrinkWrap;
import org.skyve.impl.metadata.view.VerticalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.container.HBox;

/**
 * Fluent builder for horizontal box container metadata.
 */
public class FluentHBox extends FluentContainer<FluentHBox> implements FluentBox<FluentHBox> {
	private HBox box = null;
	
	/**
	 * Creates a builder backed by a new {@link HBox} metadata instance.
	 */
	public FluentHBox() {
		this.box = new HBox();
	}
	
	/**
	 * Creates a builder backed by the supplied horizontal-box metadata instance.
	 *
	 * @param box
	 *            the horizontal-box metadata to mutate
	 */
	public FluentHBox(HBox box) {
		this.box = box;
	}
	
	/**
	 * Copies horizontal-box metadata into this builder.
	 *
	 * <p>Side effects: replaces layout, alignment, visibility condition, and contained widgets.
	 *
	 * @param box
	 *            the source horizontal box metadata
	 * @return this builder
	 */
	public FluentHBox from(@SuppressWarnings("hiding") HBox box) {
		widgetId(box.getWidgetId());
		border(Boolean.TRUE.equals(box.getBorder()));
		borderTitle(box.getBorderTitle());
		escapeBorderTitle(box.getEscapeBorderTitle());
		
		box(box, this);
		
		horizontalAlignment(box.getHorizontalAlignment());
		verticalAlignment(box.getVerticalAlignment());

		invisibleConditionName(box.getInvisibleConditionName());
		
		super.from(box);
		
		return this;
	}
	
	/**
	 * Sets the widget id for this horizontal box.
	 *
	 * @param widgetId
	 *            the widget identifier
	 * @return this builder
	 */
	public FluentHBox widgetId(String widgetId) {
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
	public FluentHBox border(boolean border) {
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
	public FluentHBox borderTitle(String borderTitle) {
		box.setBorderTitle(borderTitle);
		return this;
	}
	
	/**
	 * Sets whether the border title should be escaped before rendering.
	 *
	 * @param escapeBorderTitle {@code false} to allow trusted markup; {@code true} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentHBox escapeBorderTitle(boolean escapeBorderTitle) {
		return escapeBorderTitle(escapeBorderTitle ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Sets whether the border title should be escaped before rendering.
	 *
	 * @param escapeBorderTitle {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentHBox escapeBorderTitle(Boolean escapeBorderTitle) {
		box.setEscapeBorderTitle(escapeBorderTitle);
		return this;
	}

	/**
	 * Sets whether this hbox is collapsible. If true, a collapse/expand icon will be shown in the border
	 * (if border is true) and the user can click on it to collapse/expand the box. If false, the box will
	 * not be collapsible. The default value is false.
	 * 
	 * If collapsible is defined, the box must have a border, a border title and a pixel or percentage height
	 * defined. If any of these are missing, an exception will be thrown at runtime.
	 */
	public FluentHBox collapsible(Collapsible collapsible) {
		box.setCollapsible(collapsible);
		return this;
	}

	/**
	 * Sets the horizontal box width in pixels.
	 *
	 * @param pixelWidth
	 *            width in pixels
	 * @return this builder
	 */
	@Override
	public FluentHBox pixelWidth(int pixelWidth) {
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
	public FluentHBox responsiveWidth(int responsiveWidth) {
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
	public FluentHBox sm(int sm) {
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
	public FluentHBox md(int md) {
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
	public FluentHBox lg(int lg) {
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
	public FluentHBox xl(int xl) {
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
	public FluentHBox percentageWidth(int percentageWidth) {
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
	public FluentHBox minPixelWidth(int minPixelWidth) {
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
	public FluentHBox maxPixelWidth(int maxPixelWidth) {
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
	public FluentHBox pixelHeight(int pixelHeight) {
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
	public FluentHBox percentageHeight(int percentageHeight) {
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
	public FluentHBox minPixelHeight(int minPixelHeight) {
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
	public FluentHBox maxPixelHeight(int maxPixelHeight) {
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
	public FluentHBox pixelPadding(int pixelPadding) {
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
	public FluentHBox pixelMemberPadding(int pixelMemberPadding) {
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
	public FluentHBox shrinkWrap(ShrinkWrap shrinkWrap) {
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
	public FluentHBox horizontalAlignment(HorizontalAlignment horizontalAlignment) {
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
	public FluentHBox verticalAlignment(VerticalAlignment verticalAlignment) {
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
	public FluentHBox invisibleConditionName(String invisibleConditionName) {
		box.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	/**
	 * Returns the backing metadata instance.
	 *
	 * @return the mutable horizontal box metadata
	 */
	@Override
	public HBox get() {
		return box;
	}
}
