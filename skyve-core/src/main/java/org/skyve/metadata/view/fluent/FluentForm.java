package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.container.form.Form;

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
	
	public FluentForm addRow(FluentFormRow row) {
		form.getRows().add(row.get());
		return this;
	}
	
	@Override
	public Form get() {
		return form;
	}
}
