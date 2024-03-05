package org.skyve.metadata.view.fluent;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.view.widget.bound.Bound;

public class FluentForm extends FluentWidget implements FluentRelativeSize<FluentForm> {
	private Form form = null;

	public FluentForm() {
		this.form = new Form();
	}

	public FluentForm(Form form) {
		this.form = form;
	}

	public FluentForm from(@SuppressWarnings("hiding") Form form) {
		widgetId(form.getWidgetId());
		border(Boolean.TRUE.equals(form.getBorder()));
		borderTitle(form.getBorderTitle());

		relativeSize(form, this);

		labelDefaultHorizontalAlignment(form.getLabelDefaultHorizontalAlignment());
		collapsible(form.getCollapsible());
		labelLayout(form.getLabelLayout());

		disabledConditionName(form.getDisabledConditionName());
		invisibleConditionName(form.getInvisibleConditionName());

		form.getColumns().forEach(c -> addColumn(new FluentFormColumn().from(c)));

		form.getRows().forEach(r -> addRow(new FluentFormRow().from(r)));

		return this;
	}

	public FluentForm widgetId(String widgetId) {
		form.setWidgetId(widgetId);
		return this;
	}

	public FluentForm border(boolean border) {
		form.setBorder(border ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentForm borderTitle(String borderTitle) {
		form.setBorderTitle(borderTitle);
		return this;
	}

	@Override
	public FluentForm pixelWidth(int pixelWidth) {
		form.setPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	@Override
	public FluentForm responsiveWidth(int responsiveWidth) {
		form.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	@Override
	public FluentForm sm(int sm) {
		form.setSm(Integer.valueOf(sm));
		return this;
	}

	@Override
	public FluentForm md(int md) {
		form.setMd(Integer.valueOf(md));
		return this;
	}

	@Override
	public FluentForm lg(int lg) {
		form.setLg(Integer.valueOf(lg));
		return this;
	}

	@Override
	public FluentForm xl(int xl) {
		form.setXl(Integer.valueOf(xl));
		return this;
	}

	@Override
	public FluentForm percentageWidth(int percentageWidth) {
		form.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	@Override
	public FluentForm minPixelWidth(int minPixelWidth) {
		form.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	@Override
	public FluentForm maxPixelWidth(int maxPixelWidth) {
		form.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	@Override
	public FluentForm pixelHeight(int pixelHeight) {
		form.setPixelHeight(Integer.valueOf(pixelHeight));
		return this;
	}

	@Override
	public FluentForm percentageHeight(int percentageHeight) {
		form.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	@Override
	public FluentForm minPixelHeight(int minPixelHeight) {
		form.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentForm maxPixelHeight(int maxPixelHeight) {
		form.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	public FluentForm labelDefaultHorizontalAlignment(HorizontalAlignment alignment) {
		form.setLabelDefaultHorizontalAlignment(alignment);
		return this;
	}

	public FluentForm collapsible(Collapsible collapsible) {
		form.setCollapsible(collapsible);
		return this;
	}

	public FluentForm labelLayout(FormLabelLayout layout) {
		form.setLabelLayout(layout);
		return this;
	}
	
	public FluentForm disabledConditionName(String disabledConditionName) {
		form.setDisabledConditionName(disabledConditionName);
		return this;
	}

	public FluentForm invisibleConditionName(String invisibleConditionName) {
		form.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	public FluentForm addColumn(FluentFormColumn column) {
		form.getColumns().add(column.get());
		return this;
	}

	public FluentForm addColumn(int index, FluentFormColumn column) {
		form.getColumns().add(index, column.get());
		return this;
	}

	public FluentFormColumn getColumn(int index) {
		return new FluentFormColumn(form.getColumns().get(index));
	}
	
	public FluentForm removeColumn(int index) {
		form.getColumns().remove(index);
		return this;
	}

	public FluentForm clearColumns() {
		form.getColumns().clear();
		return this;
	}

	public FluentForm addRow(FluentFormRow row) {
		form.getRows().add(row.get());
		return this;
	}
	
	public FluentForm addRow(int index, FluentFormRow row) {
		form.getRows().add(index, row.get());
		return this;
	}

	public FluentFormRow getRow(int index) {
		return new FluentFormRow(form.getRows().get(index));
	}
	
	public FluentForm removeRow(int index) {
		form.getRows().remove(index);
		return this;
	}

	public FluentForm clearRows() {
		form.getRows().clear();
		return this;
	}

	public List<FluentFormItem> findItems(String binding) {
		List<FluentFormItem> result = new ArrayList<>();
		for (FormRow row : form.getRows()) {
			for (FormItem item : row.getItems()) {
				MetaData widget = item.getWidget();
				if (widget instanceof Bound) {
					if (binding.equals(((Bound) widget).getBinding())) {
						result.add(new FluentFormItem(item));
					}
				}
			}
		}
		return result;
	}

	@Override
	public Form get() {
		return form;
	}
}
