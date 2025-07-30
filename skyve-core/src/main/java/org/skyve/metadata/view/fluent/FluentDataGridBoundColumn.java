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
			if (input instanceof ContentImage image) {
				inputWidget(new FluentContentImage().from(image));
			}
			else if (input instanceof ContentLink link) {
				inputWidget(new FluentContentLink().from(link));
			}
			else if (input instanceof CheckBox box) {
				inputWidget(new FluentCheckBox().from(box));
			}
			else if (input instanceof ColourPicker colour) {
				inputWidget(new FluentColourPicker().from(colour));
			}
			else if (input instanceof Combo combo) {
				inputWidget(new FluentCombo().from(combo));
			}
			else if (input instanceof Geometry geometry) {
				inputWidget(new FluentGeometry().from(geometry));
			}
			else if (input instanceof HTML html) {
				inputWidget(new FluentHTML().from(html));
			}
			else if (input instanceof LookupDescription lookup) {
				inputWidget(new FluentLookupDescription().from(lookup));
			}
			else if (input instanceof Password password) {
				inputWidget(new FluentPassword().from(password));
			}
			else if (input instanceof Radio radio) {
				inputWidget(new FluentRadio().from(radio));
			}
			else if (input instanceof RichText text) {
				inputWidget(new FluentRichText().from(text));
			}
			else if (input instanceof Slider slider) {
				inputWidget(new FluentSlider().from(slider));
			}
			else if (input instanceof Spinner spinner) {
				inputWidget(new FluentSpinner().from(spinner));
			}
			else if (input instanceof TextField text) {
				inputWidget(new FluentTextField().from(text));
			}
			else if (input instanceof TextArea text) {
				inputWidget(new FluentTextArea().from(text));
			}
			else {
				throw new IllegalStateException(input + " is not catered for");
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
		return inputWidgetReference(image);
	}

	public FluentDataGridBoundColumn inputWidget(FluentContentLink link) {
		return inputWidgetReference(link);
	}

	public FluentDataGridBoundColumn inputWidget(FluentCheckBox check) {
		return inputWidgetReference(check);
	}

	public FluentDataGridBoundColumn inputWidget(FluentColourPicker colour) {
		return inputWidgetReference(colour);
	}

	public FluentDataGridBoundColumn inputWidget(FluentCombo combo) {
		return inputWidgetReference(combo);
	}

	public FluentDataGridBoundColumn inputWidget(FluentGeometry geometry) {
		return inputWidgetReference(geometry);
	}

	public FluentDataGridBoundColumn inputWidget(FluentHTML html) {
		return inputWidgetReference(html);
	}

	public FluentDataGridBoundColumn inputWidget(FluentLookupDescription lookup) {
		return inputWidgetReference(lookup);
	}

	public FluentDataGridBoundColumn inputWidget(FluentPassword password) {
		return inputWidgetReference(password);
	}

	public FluentDataGridBoundColumn inputWidget(FluentRadio radio) {
		return inputWidgetReference(radio);
	}

	public FluentDataGridBoundColumn inputWidget(FluentRichText richText) {
		return inputWidgetReference(richText);
	}

	public FluentDataGridBoundColumn inputWidget(FluentSlider slider) {
		return inputWidgetReference(slider);
	}

	public FluentDataGridBoundColumn inputWidget(FluentSpinner spinner) {
		return inputWidgetReference(spinner);
	}

	public FluentDataGridBoundColumn inputWidget(FluentTextArea textArea) {
		return inputWidgetReference(textArea);
	}

	public FluentDataGridBoundColumn inputWidget(FluentTextField textField) {
		return inputWidgetReference(textField);
	}

	private FluentDataGridBoundColumn inputWidgetReference(FluentInputWidget<?> widget) {
		WidgetReference reference = null;
		if (widget != null) {
			reference = new WidgetReference();
			reference.setWidget(widget.get());
		}
		column.setInputWidget(reference);
		return this;
	}

	@Override
	public DataGridBoundColumn get() {
		return column;
	}
}
