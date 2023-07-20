package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.metadata.FormatterName;
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

		Boolean b = column.getEditable();
		if (b != null) {
			editable(b.booleanValue());
		}
		b = column.getEscape();
		if (b != null) {
			escape(b.booleanValue());
		}
		sanitise(column.getSanitise());
		formatter(column.getFormatterName());
		customFormatter(column.getCustomFormatterName());

		WidgetReference widget = column.getInputWidget();
		if (widget != null) {
			InputWidget input = widget.getWidget();
			if (input instanceof ContentImage) {
				inputWidget(new FluentContentImage().from(input));
			}
			else if (input instanceof ContentLink) {
				inputWidget(new FluentContentLink().from(input));
			}
			else if (input instanceof CheckBox) {
				inputWidget(new FluentCheckBox().from(input));
			}
			else if (input instanceof ColourPicker) {
				inputWidget(new FluentColourPicker().from(input));
			}
			else if (input instanceof Combo) {
				inputWidget(new FluentCombo().from(input));
			}
			else if (input instanceof Geometry) {
				inputWidget(new FluentGeometry().from(input));
			}
			else if (input instanceof HTML) {
				inputWidget(new FluentHTML().from(input));
			}
			else if (input instanceof LookupDescription) {
				inputWidget(new FluentLookupDescription().from(input));
			}
			else if (input instanceof Password) {
				inputWidget(new FluentPassword().from(input));
			}
			else if (input instanceof Radio) {
				inputWidget(new FluentRadio().from(input));
			}
			else if (input instanceof RichText) {
				inputWidget(new FluentRichText().from(input));
			}
			else if (input instanceof Slider) {
				inputWidget(new FluentSlider().from(input));
			}
			else if (input instanceof Spinner) {
				inputWidget(new FluentSpinner().from(input));
			}
			else if (input instanceof TextField) {
				inputWidget(new FluentTextField().from(input));
			}
			else if (input instanceof TextArea) {
				inputWidget(new FluentTextArea().from(input));
			}
			else {
				throw new IllegalStateException(widget + " is not catered for");
			}
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

	public FluentDataGridBoundColumn formatter(FormatterName formatterName) {
		column.setFormatterName(formatterName);
		return this;
	}

	public FluentDataGridBoundColumn customFormatter(String customFormatterName) {
		column.setCustomFormatterName(customFormatterName);
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

	public FluentDataGridBoundColumn inputWidget(FluentPassword password) {
		WidgetReference widget = new WidgetReference();
		widget.setWidget(password.get());
		column.setInputWidget(widget);
		return this;
	}

	public FluentDataGridBoundColumn inputWidget(FluentRadio radio) {
		WidgetReference widget = new WidgetReference();
		widget.setWidget(radio.get());
		column.setInputWidget(widget);
		return this;
	}

	public FluentDataGridBoundColumn inputWidget(FluentRichText richText) {
		WidgetReference widget = new WidgetReference();
		widget.setWidget(richText.get());
		column.setInputWidget(widget);
		return this;
	}

	public FluentDataGridBoundColumn inputWidget(FluentSlider slider) {
		WidgetReference widget = new WidgetReference();
		widget.setWidget(slider.get());
		column.setInputWidget(widget);
		return this;
	}

	public FluentDataGridBoundColumn inputWidget(FluentSpinner spinner) {
		WidgetReference widget = new WidgetReference();
		widget.setWidget(spinner.get());
		column.setInputWidget(widget);
		return this;
	}

	public FluentDataGridBoundColumn inputWidget(FluentTextArea textArea) {
		WidgetReference widget = new WidgetReference();
		widget.setWidget(textArea.get());
		column.setInputWidget(widget);
		return this;
	}

	public FluentDataGridBoundColumn inputWidget(FluentTextField textField) {
		WidgetReference widget = new WidgetReference();
		widget.setWidget(textField.get());
		column.setInputWidget(widget);
		return this;
	}

	@Override
	public DataGridBoundColumn get() {
		return column;
	}
}
