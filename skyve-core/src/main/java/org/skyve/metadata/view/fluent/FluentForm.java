package org.skyve.metadata.view.fluent;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.view.widget.bound.Bound;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Fluent builder for form container metadata.
 */
public class FluentForm extends FluentWidget implements FluentRelativeSize<FluentForm> {
	private Form form = null;

	/**
	 * Creates a builder backed by a new {@link Form} metadata instance.
	 */
	public FluentForm() {
		this.form = new Form();
	}

	/**
	 * Creates a builder backed by the supplied form metadata instance.
	 *
	 * @param form
	 *            the form metadata to mutate
	 */
	public FluentForm(Form form) {
		this.form = form;
	}

	/**
	 * Copies form metadata into this builder.
	 *
	 * <p>Side effects: replaces dimensions, label layout settings, visibility/disabled conditions,
	 * columns, and rows.
	 *
	 * @param form
	 *            the source form metadata
	 * @return this builder
	 */
	public FluentForm from(@SuppressWarnings("hiding") Form form) {
		widgetId(form.getWidgetId());
		border(Boolean.TRUE.equals(form.getBorder()));
		borderTitle(form.getBorderTitle());
		escapeBorderTitle(form.getEscapeBorderTitle());

		relativeSize(form, this);

		labelDefaultHorizontalAlignment(form.getLabelDefaultHorizontalAlignment());
		collapsible(form.getCollapsible());
		labelLayout(form.getLabelLayout());

		BindUtil.copyAuthoredCondition(form.getXmlDisabledCondition(), form.getXmlEnabledCondition(), this::disabledCondition, this::enabledCondition);
		BindUtil.copyAuthoredCondition(form.getXmlInvisibleCondition(), form.getXmlVisibleCondition(), this::invisibleCondition, this::visibleCondition);

		form.getColumns().forEach(c -> addColumn(new FluentFormColumn().from(c)));

		form.getRows().forEach(r -> addRow(new FluentFormRow().from(r)));

		return this;
	}

	/**
	 * Sets the widget id for this form.
	 *
	 * @param widgetId
	 *            the widget identifier
	 * @return this builder
	 */
	public FluentForm widgetId(String widgetId) {
		form.setWidgetId(widgetId);
		return this;
	}

	/**
	 * Enables or disables the form border.
	 *
	 * @param border
	 *            whether the border is shown
	 * @return this builder
	 */
	public FluentForm border(boolean border) {
		form.setBorder(border ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the form border title.
	 *
	 * @param borderTitle
	 *            the border caption text
	 * @return this builder
	 */
	public FluentForm borderTitle(String borderTitle) {
		form.setBorderTitle(borderTitle);
		return this;
	}

	/**
	 * Sets whether the form border title should be escaped before rendering.
	 *
	 * @param escapeBorderTitle {@code false} to allow trusted markup; {@code true} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentForm escapeBorderTitle(boolean escapeBorderTitle) {
		return escapeBorderTitle(escapeBorderTitle ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Sets whether the form border title should be escaped before rendering.
	 *
	 * @param escapeBorderTitle {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentForm escapeBorderTitle(Boolean escapeBorderTitle) {
		form.setEscapeBorderTitle(escapeBorderTitle);
		return this;
	}

	/**
	 * Sets the form width in pixels.
	 *
	 * @param pixelWidth
	 *            width in pixels
	 * @return this builder
	 */
	@Override
	public FluentForm pixelWidth(int pixelWidth) {
		form.setPixelWidth(Integer.valueOf(pixelWidth));
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
	public FluentForm responsiveWidth(int responsiveWidth) {
		form.setResponsiveWidth(Integer.valueOf(responsiveWidth));
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
	public FluentForm sm(int sm) {
		form.setSm(Integer.valueOf(sm));
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
	public FluentForm md(int md) {
		form.setMd(Integer.valueOf(md));
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
	public FluentForm lg(int lg) {
		form.setLg(Integer.valueOf(lg));
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
	public FluentForm xl(int xl) {
		form.setXl(Integer.valueOf(xl));
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
	public FluentForm percentageWidth(int percentageWidth) {
		form.setPercentageWidth(Integer.valueOf(percentageWidth));
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
	public FluentForm minPixelWidth(int minPixelWidth) {
		form.setMinPixelWidth(Integer.valueOf(minPixelWidth));
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
	public FluentForm maxPixelWidth(int maxPixelWidth) {
		form.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	/**
	 * Sets the form height in pixels.
	 *
	 * @param pixelHeight
	 *            height in pixels
	 * @return this builder
	 */
	@Override
	public FluentForm pixelHeight(int pixelHeight) {
		form.setPixelHeight(Integer.valueOf(pixelHeight));
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
	public FluentForm percentageHeight(int percentageHeight) {
		form.setPercentageHeight(Integer.valueOf(percentageHeight));
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
	public FluentForm minPixelHeight(int minPixelHeight) {
		form.setMinPixelHeight(Integer.valueOf(minPixelHeight));
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
	public FluentForm maxPixelHeight(int maxPixelHeight) {
		form.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	/**
	 * Sets the default horizontal alignment for labels in this form.
	 *
	 * @param alignment
	 *            default label alignment
	 * @return this builder
	 */
	public FluentForm labelDefaultHorizontalAlignment(HorizontalAlignment alignment) {
		form.setLabelDefaultHorizontalAlignment(alignment);
		return this;
	}

	/**
	 * Sets whether this form is collapsible. If true, a collapse/expand icon will be shown in the border
	 * (if border is true) and the user can click on it to collapse/expand the form. If false, the form will
	 * not be collapsible. The default value is false.
	 * 
	 * If collapsible is defined, the form must have a border, a border title and a pixel or percentage height
	 * defined. If any of these are missing, an exception will be thrown at runtime.
	 */
	public FluentForm collapsible(Collapsible collapsible) {
		form.setCollapsible(collapsible);
		return this;
	}

	/**
	 * Sets the label layout mode for this form.
	 *
	 * @param layout
	 *            label layout mode
	 * @return this builder
	 */
	public FluentForm labelLayout(FormLabelLayout layout) {
		form.setLabelLayout(layout);
		return this;
	}
	
	/**
	 * Sets the {@code disabled} condition as authored; the opposite sense of {@link #enabledCondition(String)}.
	 *
	 * @param disabledCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentForm disabledCondition(@Nullable String disabledCondition) {
		form.setXmlDisabledCondition(disabledCondition);
		return this;
	}

	/**
	 * Sets the {@code enabled} condition as authored; the opposite sense of {@link #disabledCondition(String)}.
	 *
	 * @param enabledCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentForm enabledCondition(@Nullable String enabledCondition) {
		form.setXmlEnabledCondition(enabledCondition);
		return this;
	}

	/**
	 * Sets the {@code invisible} condition as authored; the opposite sense of {@link #visibleCondition(String)}.
	 *
	 * @param invisibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentForm invisibleCondition(@Nullable String invisibleCondition) {
		form.setXmlInvisibleCondition(invisibleCondition);
		return this;
	}

	/**
	 * Sets the {@code visible} condition as authored; the opposite sense of {@link #invisibleCondition(String)}.
	 *
	 * @param visibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentForm visibleCondition(@Nullable String visibleCondition) {
		form.setXmlVisibleCondition(visibleCondition);
		return this;
	}

	/**
	 * Appends a column definition.
	 *
	 * @param column
	 *            the column builder
	 * @return this builder
	 */
	public FluentForm addColumn(FluentFormColumn column) {
		form.getColumns().add(column.get());
		return this;
	}

	/**
	 * Inserts a column definition at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param column
	 *            the column builder
	 * @return this builder
	 */
	public FluentForm addColumn(int index, FluentFormColumn column) {
		form.getColumns().add(index, column.get());
		return this;
	}

	/**
	 * Returns the column at the supplied index.
	 *
	 * @param index
	 *            zero-based index
	 * @return the column wrapper
	 */
	public FluentFormColumn getColumn(int index) {
		return new FluentFormColumn(form.getColumns().get(index));
	}
	
	/**
	 * Removes the column at the supplied index.
	 *
	 * @param index
	 *            zero-based column index
	 * @return this builder
	 */
	public FluentForm removeColumn(int index) {
		form.getColumns().remove(index);
		return this;
	}

	/**
	 * Removes all columns from this form.
	 *
	 * @return this builder
	 */
	public FluentForm clearColumns() {
		form.getColumns().clear();
		return this;
	}

	/**
	 * Appends a row definition.
	 *
	 * @param row
	 *            the row builder
	 * @return this builder
	 */
	public FluentForm addRow(FluentFormRow row) {
		form.getRows().add(row.get());
		return this;
	}
	
	/**
	 * Inserts a row definition at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param row
	 *            the row builder
	 * @return this builder
	 */
	public FluentForm addRow(int index, FluentFormRow row) {
		form.getRows().add(index, row.get());
		return this;
	}

	/**
	 * Returns the row at the supplied index.
	 *
	 * @param index
	 *            zero-based index
	 * @return the row wrapper
	 */
	public FluentFormRow getRow(int index) {
		return new FluentFormRow(form.getRows().get(index));
	}
	
	/**
	 * Removes the row at the supplied index.
	 *
	 * @param index
	 *            zero-based row index
	 * @return this builder
	 */
	public FluentForm removeRow(int index) {
		form.getRows().remove(index);
		return this;
	}

	/**
	 * Removes all rows from this form.
	 *
	 * @return this builder
	 */
	public FluentForm clearRows() {
		form.getRows().clear();
		return this;
	}

	/**
	 * Finds all form items bound to the supplied binding.
	 *
	 * <p>Complexity: O(r * i) where r is the row count and i is the average item count per row.
	 *
	 * @param binding
	 *            the binding expression to match
	 * @return matching items in row traversal order; never {@code null}
	 */
	public List<FluentFormItem> findItems(String binding) {
		List<FluentFormItem> result = new ArrayList<>();
		for (FormRow row : form.getRows()) {
			for (FormItem item : row.getItems()) {
				MetaData widget = item.getWidget();
				if ((widget instanceof Bound bound) && binding.equals(bound.getBinding())) {
					result.add(new FluentFormItem(item));
				}
			}
		}
		return result;
	}

	/**
	 * Returns the backing metadata instance.
	 *
	 * @return the mutable form metadata
	 */
	@Override
	public Form get() {
		return form;
	}
}
