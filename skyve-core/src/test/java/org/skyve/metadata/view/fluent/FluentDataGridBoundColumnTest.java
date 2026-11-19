package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.view.TextOutput.Sanitisation;

/**
 * Tests for {@link FluentDataGridBoundColumn} setters and {@code from()} round-trips.
 */
@SuppressWarnings("static-method")
class FluentDataGridBoundColumnTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentDataGridBoundColumn().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		DataGridBoundColumn col = new DataGridBoundColumn();
		assertThat(new FluentDataGridBoundColumn(col).get(), is(col));
	}

	// ---- base column setters (from FluentDataGridColumn) ----

	@Test
	void titleSetsValue() {
		assertThat(new FluentDataGridBoundColumn().title("Name").get().getTitle(), is("Name"));
	}

	@Test
	void alignmentSetsValue() {
		assertThat(new FluentDataGridBoundColumn().alignment(HorizontalAlignment.right).get().getAlignment(),
				is(HorizontalAlignment.right));
	}

	@Test
	void pixelWidthSetsValue() {
		assertThat(new FluentDataGridBoundColumn().pixelWidth(150).get().getPixelWidth(), is(Integer.valueOf(150)));
	}

	// ---- bound column setters ----

	@Test
	void bindingSetsValue() {
		assertThat(new FluentDataGridBoundColumn().binding("firstName").get().getBinding(), is("firstName"));
	}

	@Test
	void editableSetsTrue() {
		assertThat(new FluentDataGridBoundColumn().editable(true).get().getEditable(), is(Boolean.TRUE));
	}

	@Test
	void editableSetsFalse() {
		assertThat(new FluentDataGridBoundColumn().editable(false).get().getEditable(), is(Boolean.FALSE));
	}

	@Test
	void escapeSetsTrue() {
		assertThat(new FluentDataGridBoundColumn().escape(true).get().getEscape(), is(Boolean.TRUE));
	}

	@Test
	void escapeSetsFalse() {
		assertThat(new FluentDataGridBoundColumn().escape(false).get().getEscape(), is(Boolean.FALSE));
	}

	@Test
	void sanitiseSetsValue() {
		assertThat(new FluentDataGridBoundColumn().sanitise(Sanitisation.basic).get().getSanitise(),
				is(Sanitisation.basic));
	}

	@Test
	void formatterSetsValue() {
		assertThat(new FluentDataGridBoundColumn().formatter(FormatterName.DD_MM_YYYY).get().getFormatterName(),
				is(FormatterName.DD_MM_YYYY));
	}

	@Test
	void customFormatterSetsValue() {
		assertThat(new FluentDataGridBoundColumn().customFormatter("myFormatter").get().getCustomFormatterName(),
				is("myFormatter"));
	}

	// ---- inputWidget overloads ----

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void inputWidgetCheckBoxSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentCheckBox());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(CheckBox.class));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void inputWidgetColourPickerSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentColourPicker());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(ColourPicker.class));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void inputWidgetComboSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentCombo());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(Combo.class));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void inputWidgetGeometrySetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentGeometry());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(Geometry.class));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void inputWidgetHTMLSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentHTML());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(HTML.class));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void inputWidgetLookupDescriptionSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentLookupDescription());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(LookupDescription.class));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void inputWidgetPasswordSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentPassword());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(Password.class));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void inputWidgetRadioSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentRadio());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(Radio.class));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void inputWidgetRichTextSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentRichText());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(RichText.class));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void inputWidgetSliderSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentSlider());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(Slider.class));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void inputWidgetSpinnerSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentSpinner());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(Spinner.class));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void inputWidgetTextAreaSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentTextArea());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(TextArea.class));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void inputWidgetTextFieldSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentTextField());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(TextField.class));
	}

	@Test
	void inputWidgetNullClearsWidget() {
		@SuppressWarnings("null") // passes null on purpose
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn()
				.inputWidget(new FluentTextField())
				.inputWidget((FluentTextField) null);
		assertThat(col.get().getInputWidget(), is(nullValue()));
	}

	// ---- from() round-trips ----

	@Test
	void fromCopiesEditable() {
		DataGridBoundColumn source = new DataGridBoundColumn();
		source.setEditable(Boolean.TRUE);
		assertThat(new FluentDataGridBoundColumn().from(source).get().getEditable(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesEscape() {
		DataGridBoundColumn source = new DataGridBoundColumn();
		source.setEscape(Boolean.FALSE);
		assertThat(new FluentDataGridBoundColumn().from(source).get().getEscape(), is(Boolean.FALSE));
	}

	@Test
	void fromCopiesSanitise() {
		DataGridBoundColumn source = new DataGridBoundColumn();
		source.setSanitise(Sanitisation.relaxed);
		assertThat(new FluentDataGridBoundColumn().from(source).get().getSanitise(), is(Sanitisation.relaxed));
	}

	@Test
	void fromCopiesFormatter() {
		DataGridBoundColumn source = new DataGridBoundColumn();
		source.setFormatterName(FormatterName.DD_MM_YYYY);
		assertThat(new FluentDataGridBoundColumn().from(source).get().getFormatterName(),
				is(FormatterName.DD_MM_YYYY));
	}

	@Test
	void fromCopiesCustomFormatter() {
		DataGridBoundColumn source = new DataGridBoundColumn();
		source.setCustomFormatterName("myFmt");
		assertThat(new FluentDataGridBoundColumn().from(source).get().getCustomFormatterName(), is("myFmt"));
	}

	@Test
	void fromCopiesTitle() {
		DataGridBoundColumn source = new DataGridBoundColumn();
		source.setTitle("First Name");
		assertThat(new FluentDataGridBoundColumn().from(source).get().getTitle(), is("First Name"));
	}

	@Test
	void fromWithNullInputWidgetRoundTrips() {
		DataGridBoundColumn source = new DataGridBoundColumn();
		// no inputWidget set
		assertThat(new FluentDataGridBoundColumn().from(source).get().getInputWidget(), is(nullValue()));
	}

	private static DataGridBoundColumn withInputWidget(org.skyve.impl.metadata.view.widget.bound.input.InputWidget widget) {
		DataGridBoundColumn col = new DataGridBoundColumn();
		WidgetReference ref = new WidgetReference();
		ref.setWidget(widget);
		col.setInputWidget(ref);
		return col;
	}

	private static InputWidget roundTrippedInputWidget(InputWidget widget) {
		WidgetReference ref = new FluentDataGridBoundColumn().from(withInputWidget(widget)).get().getInputWidget();
		assertNotNull(ref);
		return ref.getWidget();
	}

	@Test
	void fromCheckBoxInputWidgetRoundTrips() {
		assertThat(roundTrippedInputWidget(new CheckBox()), instanceOf(CheckBox.class));
	}

	@Test
	void fromColourPickerInputWidgetRoundTrips() {
		assertThat(roundTrippedInputWidget(new ColourPicker()), instanceOf(ColourPicker.class));
	}

	@Test
	void fromComboInputWidgetRoundTrips() {
		assertThat(roundTrippedInputWidget(new Combo()), instanceOf(Combo.class));
	}

	@Test
	void fromGeometryInputWidgetRoundTrips() {
		assertThat(roundTrippedInputWidget(new Geometry()), instanceOf(Geometry.class));
	}

	@Test
	void fromHTMLInputWidgetRoundTrips() {
		assertThat(roundTrippedInputWidget(new HTML()), instanceOf(HTML.class));
	}

	@Test
	void fromLookupDescriptionInputWidgetRoundTrips() {
		assertThat(roundTrippedInputWidget(new LookupDescription()), instanceOf(LookupDescription.class));
	}

	@Test
	void fromPasswordInputWidgetRoundTrips() {
		assertThat(roundTrippedInputWidget(new Password()), instanceOf(Password.class));
	}

	@Test
	void fromRadioInputWidgetRoundTrips() {
		assertThat(roundTrippedInputWidget(new Radio()), instanceOf(Radio.class));
	}

	@Test
	void fromRichTextInputWidgetRoundTrips() {
		assertThat(roundTrippedInputWidget(new RichText()), instanceOf(RichText.class));
	}

	@Test
	void fromSliderInputWidgetRoundTrips() {
		assertThat(roundTrippedInputWidget(new Slider()), instanceOf(Slider.class));
	}

	@Test
	void fromSpinnerInputWidgetRoundTrips() {
		assertThat(roundTrippedInputWidget(new Spinner()), instanceOf(Spinner.class));
	}

	@Test
	void fromTextFieldInputWidgetRoundTrips() {
		assertThat(roundTrippedInputWidget(new TextField()), instanceOf(TextField.class));
	}

	@Test
	void fromTextAreaInputWidgetRoundTrips() {
		assertThat(roundTrippedInputWidget(new TextArea()), instanceOf(TextArea.class));
	}

	@Test
	void fromWithPixelWidthNonNullCopiesPixelWidth() {
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setPixelWidth(Integer.valueOf(120));
		FluentDataGridBoundColumn result = new FluentDataGridBoundColumn().from(col);
		assertThat(result.get().getPixelWidth(), is(Integer.valueOf(120)));
	}

	@Test
	void getSourceReturnsBinding() {
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("contact.name");
		assertThat(col.getSource(), is("contact.name"));
	}

	@Test
	void fromWithUnknownInputWidgetTypeThrowsIllegalState() {
		DataGridBoundColumn col = new DataGridBoundColumn();
		WidgetReference ref = new WidgetReference();
		ref.setWidget(new InputWidget() {
			private static final long serialVersionUID = 1L;
			// Deliberately unknown InputWidget subtype for exception-path coverage.
		});
		col.setInputWidget(ref);
		FluentDataGridBoundColumn fluentDataGridBoundColumn = new FluentDataGridBoundColumn();
		assertThrows(IllegalStateException.class, () -> fluentDataGridBoundColumn.from(col));
	}
}
