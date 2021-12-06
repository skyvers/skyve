package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.metadata.view.TextOutput.Sanitisation;

public class FluentDataGridBoundColumn extends FluentDataGridColumn<FluentDataGridBoundColumn> {
	private DataGridBoundColumn column = null;
	
	public FluentDataGridBoundColumn() {
		column = new DataGridBoundColumn();
	}

	public FluentDataGridBoundColumn(DataGridBoundColumn column) {
		this.column = column;
	}

	public FluentDataGridBoundColumn from(@SuppressWarnings("hiding") DataGridBoundColumn column) {
		super.from(column);

		// TODO widget reference
		Boolean b = column.getEditable();
		if (b != null) {
			editable(b.booleanValue());
		}
		b = column.getEscape();
		if (b != null) {
			escape(b.booleanValue());
		}
		sanitise(column.getSanitise());

		WidgetReference widget = column.getInputWidget();
		if (widget != null) {
			InputWidget input = widget.getWidget();
			if (input instanceof ContentImage) {
				inputWidget(new FluentContentImage().from(input));
			}
			// TODO
		}
		
		return this;
	}
	
	public FluentDataGridBoundColumn binding(String binding) {
		column.setBinding(binding);
		return this;
	}

	public FluentDataGridBoundColumn editable(boolean editable) {
		column.setEditable(editable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}
	
	public FluentDataGridBoundColumn escape(boolean escape) {
		column.setEscape(escape ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentDataGridBoundColumn sanitise(Sanitisation sanitise) {
		column.setSanitise(sanitise);
		return this;
	}

	public FluentDataGridBoundColumn inputWidget(FluentContentImage image) {
		WidgetReference widget = new WidgetReference();
		widget.setWidget(image.get());
		column.setInputWidget(widget);
		return this;
	}

	public FluentDataGridBoundColumn inputWidget(FluentContentLink link) {
		WidgetReference widget = new WidgetReference();
		widget.setWidget(link.get());
		column.setInputWidget(widget);
		return this;
	}

	public FluentDataGridBoundColumn inputWidget(FluentCheckBox check) {
		WidgetReference widget = new WidgetReference();
		widget.setWidget(check.get());
		column.setInputWidget(widget);
		return this;
	}

	public FluentDataGridBoundColumn inputWidget(FluentColourPicker colour) {
		WidgetReference widget = new WidgetReference();
		widget.setWidget(colour.get());
		column.setInputWidget(widget);
		return this;
	}

	public FluentDataGridBoundColumn inputWidget(FluentCombo combo) {
		WidgetReference widget = new WidgetReference();
		widget.setWidget(combo.get());
		column.setInputWidget(widget);
		return this;
	}

	public FluentDataGridBoundColumn inputWidget(FluentGeometry geometry) {
		WidgetReference widget = new WidgetReference();
		widget.setWidget(geometry.get());
		column.setInputWidget(widget);
		return this;
	}

	public FluentDataGridBoundColumn inputWidget(FluentHTML html) {
		WidgetReference widget = new WidgetReference();
		widget.setWidget(html.get());
		column.setInputWidget(widget);
		return this;
	}

	public FluentDataGridBoundColumn inputWidget(FluentLookupDescription lookup) {
		WidgetReference widget = new WidgetReference();
		widget.setWidget(lookup.get());
		column.setInputWidget(widget);
		return this;
	}

	/* TODO
		Password 
		Radio
		RichText 
		Slider
		Spinner
		TextArea
		TextField
	 */
	
	@Override
	public DataGridBoundColumn get() {
		return column;
	}
}
