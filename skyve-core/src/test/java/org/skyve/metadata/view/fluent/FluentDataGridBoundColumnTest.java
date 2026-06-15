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
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
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

	@Test
	void inputWidgetContentImageSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentContentImage());
		WidgetReference ref = col.get().getInputWidget();
		assertThat(ref, is(notNullValue()));
		assertThat(ref.getWidget(), instanceOf(ContentImage.class));
	}

	@Test
	void inputWidgetContentLinkSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentContentLink());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(ContentLink.class));
	}

	@Test
	void inputWidgetCheckBoxSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentCheckBox());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(CheckBox.class));
	}

	@Test
	void inputWidgetColourPickerSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentColourPicker());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(ColourPicker.class));
	}

	@Test
	void inputWidgetComboSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentCombo());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(Combo.class));
	}

	@Test
	void inputWidgetGeometrySetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentGeometry());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(Geometry.class));
	}

	@Test
	void inputWidgetHTMLSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentHTML());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(HTML.class));
	}

	@Test
	void inputWidgetLookupDescriptionSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentLookupDescription());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(LookupDescription.class));
	}

	@Test
	void inputWidgetPasswordSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentPassword());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(Password.class));
	}

	@Test
	void inputWidgetRadioSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentRadio());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(Radio.class));
	}

	@Test
	void inputWidgetRichTextSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentRichText());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(RichText.class));
	}

	@Test
	void inputWidgetSliderSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentSlider());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(Slider.class));
	}

	@Test
	void inputWidgetSpinnerSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentSpinner());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(Spinner.class));
	}

	@Test
	void inputWidgetTextAreaSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentTextArea());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(TextArea.class));
	}

	@Test
	void inputWidgetTextFieldSetsWidget() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().inputWidget(new FluentTextField());
		assertThat(col.get().getInputWidget().getWidget(), instanceOf(TextField.class));
	}

	@Test
	void inputWidgetNullClearsWidget() {
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

	@Test
	void fromContentImageInputWidgetRoundTrips() {
		assertThat(new FluentDataGridBoundColumn().from(withInputWidget(new ContentImage()))
				.get().getInputWidget().getWidget(), instanceOf(ContentImage.class));
	}

	@Test
	void fromContentLinkInputWidgetRoundTrips() {
		assertThat(new FluentDataGridBoundColumn().from(withInputWidget(new ContentLink()))
				.get().getInputWidget().getWidget(), instanceOf(ContentLink.class));
	}

	@Test
	void fromCheckBoxInputWidgetRoundTrips() {
		assertThat(new FluentDataGridBoundColumn().from(withInputWidget(new CheckBox()))
				.get().getInputWidget().getWidget(), instanceOf(CheckBox.class));
	}

	@Test
	void fromColourPickerInputWidgetRoundTrips() {
		assertThat(new FluentDataGridBoundColumn().from(withInputWidget(new ColourPicker()))
				.get().getInputWidget().getWidget(), instanceOf(ColourPicker.class));
	}

	@Test
	void fromComboInputWidgetRoundTrips() {
		assertThat(new FluentDataGridBoundColumn().from(withInputWidget(new Combo()))
				.get().getInputWidget().getWidget(), instanceOf(Combo.class));
	}

	@Test
	void fromGeometryInputWidgetRoundTrips() {
		assertThat(new FluentDataGridBoundColumn().from(withInputWidget(new Geometry()))
				.get().getInputWidget().getWidget(), instanceOf(Geometry.class));
	}

	@Test
	void fromHTMLInputWidgetRoundTrips() {
		assertThat(new FluentDataGridBoundColumn().from(withInputWidget(new HTML()))
				.get().getInputWidget().getWidget(), instanceOf(HTML.class));
	}

	@Test
	void fromLookupDescriptionInputWidgetRoundTrips() {
		assertThat(new FluentDataGridBoundColumn().from(withInputWidget(new LookupDescription()))
				.get().getInputWidget().getWidget(), instanceOf(LookupDescription.class));
	}

	@Test
	void fromPasswordInputWidgetRoundTrips() {
		assertThat(new FluentDataGridBoundColumn().from(withInputWidget(new Password()))
				.get().getInputWidget().getWidget(), instanceOf(Password.class));
	}

	@Test
	void fromRadioInputWidgetRoundTrips() {
		assertThat(new FluentDataGridBoundColumn().from(withInputWidget(new Radio()))
				.get().getInputWidget().getWidget(), instanceOf(Radio.class));
	}

	@Test
	void fromRichTextInputWidgetRoundTrips() {
		assertThat(new FluentDataGridBoundColumn().from(withInputWidget(new RichText()))
				.get().getInputWidget().getWidget(), instanceOf(RichText.class));
	}

	@Test
	void fromSliderInputWidgetRoundTrips() {
		assertThat(new FluentDataGridBoundColumn().from(withInputWidget(new Slider()))
				.get().getInputWidget().getWidget(), instanceOf(Slider.class));
	}

	@Test
	void fromSpinnerInputWidgetRoundTrips() {
		assertThat(new FluentDataGridBoundColumn().from(withInputWidget(new Spinner()))
				.get().getInputWidget().getWidget(), instanceOf(Spinner.class));
	}

	@Test
	void fromTextFieldInputWidgetRoundTrips() {
		assertThat(new FluentDataGridBoundColumn().from(withInputWidget(new TextField()))
				.get().getInputWidget().getWidget(), instanceOf(TextField.class));
	}

	@Test
	void fromTextAreaInputWidgetRoundTrips() {
		assertThat(new FluentDataGridBoundColumn().from(withInputWidget(new TextArea()))
				.get().getInputWidget().getWidget(), instanceOf(TextArea.class));
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
			// Deliberately unknown InputWidget subtype for exception-path coverage.
		});
		col.setInputWidget(ref);
		FluentDataGridBoundColumn fluentDataGridBoundColumn = new FluentDataGridBoundColumn();
		assertThrows(IllegalStateException.class, () -> fluentDataGridBoundColumn.from(col));
	}
}
