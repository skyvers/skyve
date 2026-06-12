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

/**
 * Builds bound-column {@link DataGridBoundColumn} metadata.
 */
public class FluentDataGridBoundColumn extends FluentDataGridColumn<FluentDataGridBoundColumn> {
	private DataGridBoundColumn column = null;

	/**
	 * Creates a builder backed by a new {@link DataGridBoundColumn}.
	 */
	public FluentDataGridBoundColumn() {
		column = new DataGridBoundColumn();
	}

	/**
	 * Creates a builder backed by the supplied {@link DataGridBoundColumn}.
	 *
	 * @param column
	 *            the metadata instance to mutate
	 */
	public FluentDataGridBoundColumn(DataGridBoundColumn column) {
		this.column = column;
	}

	/**
	 * Copies all supported bound-column state from runtime metadata.
	 *
	 * <p>
	 * Side effects: replaces this builder's input-widget reference when present.
	 *
	 * @param column
	 *            the source metadata to copy
	 * @return this builder
	 * @throws IllegalStateException
	 *             if an unsupported input widget type is encountered
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
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

	/**
	 * Sets the binding expression for this bound column.
	 *
	 * @param binding
	 *            the binding expression
	 * @return this builder
	 */
	public FluentDataGridBoundColumn binding(String binding) {
		column.setBinding(binding);
		return this;
	}

	/**
	 * Sets whether this bound column is editable inline.
	 *
	 * @param editable
	 *            {@code true} to allow inline editing
	 * @return this builder
	 */
	public FluentDataGridBoundColumn editable(boolean editable) {
		column.setEditable(editable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether HTML characters in this column's value are escaped.
	 *
	 * @param escape
	 *            {@code true} to escape HTML characters
	 * @return this builder
	 */
	public FluentDataGridBoundColumn escape(boolean escape) {
		column.setEscape(escape ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the sanitisation policy for this bound column's output.
	 *
	 * @param sanitise
	 *            the sanitisation policy
	 * @return this builder
	 */
	public FluentDataGridBoundColumn sanitise(Sanitisation sanitise) {
		column.setSanitise(sanitise);
		return this;
	}

	/**
	 * Sets the named formatter to apply to this bound column's value.
	 *
	 * @param formatterName
	 *            the formatter name
	 * @return this builder
	 */
	public FluentDataGridBoundColumn formatter(FormatterName formatterName) {
		column.setFormatterName(formatterName);
		return this;
	}

	/**
	 * Sets the custom formatter name for this bound column.
	 *
	 * @param customFormatterName
	 *            the custom formatter name
	 * @return this builder
	 */
	public FluentDataGridBoundColumn customFormatter(String customFormatterName) {
		column.setCustomFormatterName(customFormatterName);
		return this;
	}

	/**
	 * Sets a {@link org.skyve.impl.metadata.view.widget.bound.input.ContentImage} inline editor for this column.
	 *
	 * @param image
	 *            the content image widget builder
	 * @return this builder
	 */
	public FluentDataGridBoundColumn inputWidget(FluentContentImage image) {
		return inputWidgetReference(image);
	}

	/**
	 * Sets a {@link org.skyve.impl.metadata.view.widget.bound.input.ContentLink} inline editor for this column.
	 *
	 * @param link
	 *            the content link widget builder
	 * @return this builder
	 */
	public FluentDataGridBoundColumn inputWidget(FluentContentLink link) {
		return inputWidgetReference(link);
	}

	/**
	 * Sets a {@link org.skyve.impl.metadata.view.widget.bound.input.CheckBox} inline editor for this column.
	 *
	 * @param check
	 *            the check-box widget builder
	 * @return this builder
	 */
	public FluentDataGridBoundColumn inputWidget(FluentCheckBox check) {
		return inputWidgetReference(check);
	}

	/**
	 * Sets a {@link org.skyve.impl.metadata.view.widget.bound.input.ColourPicker} inline editor for this column.
	 *
	 * @param colour
	 *            the colour-picker widget builder
	 * @return this builder
	 */
	public FluentDataGridBoundColumn inputWidget(FluentColourPicker colour) {
		return inputWidgetReference(colour);
	}

	/**
	 * Sets a {@link org.skyve.impl.metadata.view.widget.bound.input.Combo} inline editor for this column.
	 *
	 * @param combo
	 *            the combo widget builder
	 * @return this builder
	 */
	public FluentDataGridBoundColumn inputWidget(FluentCombo combo) {
		return inputWidgetReference(combo);
	}

	/**
	 * Sets a {@link org.skyve.impl.metadata.view.widget.bound.input.Geometry} inline editor for this column.
	 *
	 * @param geometry
	 *            the geometry widget builder
	 * @return this builder
	 */
	public FluentDataGridBoundColumn inputWidget(FluentGeometry geometry) {
		return inputWidgetReference(geometry);
	}

	/**
	 * Sets an {@link org.skyve.impl.metadata.view.widget.bound.input.HTML} inline editor for this column.
	 *
	 * @param html
	 *            the HTML widget builder
	 * @return this builder
	 */
	public FluentDataGridBoundColumn inputWidget(FluentHTML html) {
		return inputWidgetReference(html);
	}

	/**
	 * Sets a {@link org.skyve.impl.metadata.view.widget.bound.input.LookupDescription} inline editor for this column.
	 *
	 * @param lookup
	 *            the lookup-description widget builder
	 * @return this builder
	 */
	public FluentDataGridBoundColumn inputWidget(FluentLookupDescription lookup) {
		return inputWidgetReference(lookup);
	}

	/**
	 * Sets a {@link org.skyve.impl.metadata.view.widget.bound.input.Password} inline editor for this column.
	 *
	 * @param password
	 *            the password widget builder
	 * @return this builder
	 */
	public FluentDataGridBoundColumn inputWidget(FluentPassword password) {
		return inputWidgetReference(password);
	}

	/**
	 * Sets a {@link org.skyve.impl.metadata.view.widget.bound.input.Radio} inline editor for this column.
	 *
	 * @param radio
	 *            the radio widget builder
	 * @return this builder
	 */
	public FluentDataGridBoundColumn inputWidget(FluentRadio radio) {
		return inputWidgetReference(radio);
	}

	/**
	 * Sets a {@link org.skyve.impl.metadata.view.widget.bound.input.RichText} inline editor for this column.
	 *
	 * @param richText
	 *            the rich-text widget builder
	 * @return this builder
	 */
	public FluentDataGridBoundColumn inputWidget(FluentRichText richText) {
		return inputWidgetReference(richText);
	}

	/**
	 * Sets a {@link org.skyve.impl.metadata.view.widget.bound.input.Slider} inline editor for this column.
	 *
	 * @param slider
	 *            the slider widget builder
	 * @return this builder
	 */
	public FluentDataGridBoundColumn inputWidget(FluentSlider slider) {
		return inputWidgetReference(slider);
	}

	/**
	 * Sets a {@link org.skyve.impl.metadata.view.widget.bound.input.Spinner} inline editor for this column.
	 *
	 * @param spinner
	 *            the spinner widget builder
	 * @return this builder
	 */
	public FluentDataGridBoundColumn inputWidget(FluentSpinner spinner) {
		return inputWidgetReference(spinner);
	}

	/**
	 * Sets a {@link org.skyve.impl.metadata.view.widget.bound.input.TextArea} inline editor for this column.
	 *
	 * @param textArea
	 *            the text-area widget builder
	 * @return this builder
	 */
	public FluentDataGridBoundColumn inputWidget(FluentTextArea textArea) {
		return inputWidgetReference(textArea);
	}

	/**
	 * Sets a {@link org.skyve.impl.metadata.view.widget.bound.input.TextField} inline editor for this column.
	 *
	 * @param textField
	 *            the text-field widget builder
	 * @return this builder
	 */
	public FluentDataGridBoundColumn inputWidget(FluentTextField textField) {
		return inputWidgetReference(textField);
	}

	/**
	 * Replaces the bound input-widget reference.
	 *
	 * @param widget
	 *            the fluent input widget to reference, or {@code null} to clear the reference
	 * @return this builder
	 */
	private FluentDataGridBoundColumn inputWidgetReference(FluentInputWidget<?> widget) {
		WidgetReference reference = null;
		if (widget != null) {
			reference = new WidgetReference();
			reference.setWidget(widget.get());
		}
		column.setInputWidget(reference);
		return this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped bound column metadata
	 */
	@Override
	public DataGridBoundColumn get() {
		return column;
	}
}
