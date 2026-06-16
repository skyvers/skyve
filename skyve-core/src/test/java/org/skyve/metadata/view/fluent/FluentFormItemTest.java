package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.MetaData;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.DefaultWidget;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;

/**
 * Tests for {@link FluentFormItem} setters and {@code from()} round-trips.
 */
@SuppressWarnings("static-method")
class FluentFormItemTest {

	// ---- constructor ----

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentFormItem().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		FormItem fi = new FormItem();
		assertThat(new FluentFormItem(fi).get(), is(fi));
	}

	// ---- property setters ----

	@Test
	void colspanSetsValue() {
		FluentFormItem fi = new FluentFormItem().colspan(3);
		assertThat(fi.get().getColspan(), is(Integer.valueOf(3)));
	}

	@Test
	void rowspanSetsValue() {
		FluentFormItem fi = new FluentFormItem().rowspan(2);
		assertThat(fi.get().getRowspan(), is(Integer.valueOf(2)));
	}

	@Test
	void horizontalAlignmentSetsValue() {
		FluentFormItem fi = new FluentFormItem().horizontalAlignment(HorizontalAlignment.centre);
		assertThat(fi.get().getHorizontalAlignment(), is(HorizontalAlignment.centre));
	}

	@Test
	void labelStringSetsValue() {
		FluentFormItem fi = new FluentFormItem().label("Name");
		assertThat(fi.get().getLabel(), is("Name"));
	}

	@Test
	void showLabelSetsTrue() {
		FluentFormItem fi = new FluentFormItem().showLabel(true);
		assertThat(fi.get().getShowLabel(), is(Boolean.TRUE));
	}

	@Test
	void showLabelSetsFalse() {
		FluentFormItem fi = new FluentFormItem().showLabel(false);
		assertThat(fi.get().getShowLabel(), is(Boolean.FALSE));
	}

	@Test
	void showHelpSetsTrue() {
		FluentFormItem fi = new FluentFormItem().showHelp(true);
		assertThat(fi.get().getShowHelp(), is(Boolean.TRUE));
	}

	@Test
	void helpSetsValue() {
		FluentFormItem fi = new FluentFormItem().help("Enter the name");
		assertThat(fi.get().getHelp(), is("Enter the name"));
	}

	@Test
	void requiredSetsTrue() {
		FluentFormItem fi = new FluentFormItem().required(true);
		assertThat(fi.get().getRequired(), is(Boolean.TRUE));
	}

	@Test
	void requiredSetsFalse() {
		FluentFormItem fi = new FluentFormItem().required(false);
		assertThat(fi.get().getRequired(), is(Boolean.FALSE));
	}

	// ---- widget setters ----

	@Test
	void defaultWidgetSetsWidget() {
		FluentFormItem fi = new FluentFormItem().defaultWidget(new FluentDefaultWidget());
		assertThat(fi.get().getWidget(), instanceOf(DefaultWidget.class));
	}

	@Test
	void contentSignatureSetsWidget() {
		FluentFormItem fi = new FluentFormItem().contentSignature(new FluentContentSignature());
		assertThat(fi.get().getWidget(), instanceOf(ContentSignature.class));
	}

	@Test
	void buttonSetsWidget() {
		FluentFormItem fi = new FluentFormItem().button(new FluentButton());
		assertThat(fi.get().getWidget(), instanceOf(Button.class));
	}

	@Test
	void zoomInSetsWidget() {
		FluentFormItem fi = new FluentFormItem().zoomIn(new FluentZoomIn());
		assertThat(fi.get().getWidget(), instanceOf(ZoomIn.class));
	}

	@Test
	void dialogButtonSetsWidget() {
		FluentFormItem fi = new FluentFormItem().dialogButton(new FluentDialogButton());
		assertThat(fi.get().getWidget(), instanceOf(DialogButton.class));
	}

	@Test
	void geometrySetsWidget() {
		FluentFormItem fi = new FluentFormItem().geometry(new FluentGeometry());
		assertThat(fi.get().getWidget(), instanceOf(Geometry.class));
	}

	@Test
	void geometryMapSetsWidget() {
		FluentFormItem fi = new FluentFormItem().geometryMap(new FluentGeometryMap());
		assertThat(fi.get().getWidget(), instanceOf(GeometryMap.class));
	}

	@Test
	void htmlSetsWidget() {
		FluentFormItem fi = new FluentFormItem().html(new FluentHTML());
		assertThat(fi.get().getWidget(), instanceOf(HTML.class));
	}

	@Test
	void labelFluentSetsWidget() {
		FluentFormItem fi = new FluentFormItem().label(new FluentLabel());
		assertThat(fi.get().getWidget(), instanceOf(Label.class));
	}

	@Test
	void blurbSetsWidget() {
		FluentFormItem fi = new FluentFormItem().blurb(new FluentBlurb());
		assertThat(fi.get().getWidget(), instanceOf(Blurb.class));
	}

	@Test
	void progressBarSetsWidget() {
		FluentFormItem fi = new FluentFormItem().progressBar(new FluentProgressBar());
		assertThat(fi.get().getWidget(), instanceOf(ProgressBar.class));
	}

	@Test
	void checkBoxSetsWidget() {
		FluentFormItem fi = new FluentFormItem().checkBox(new FluentCheckBox());
		assertThat(fi.get().getWidget(), instanceOf(CheckBox.class));
	}

	@Test
	void colourPickerSetsWidget() {
		FluentFormItem fi = new FluentFormItem().colourPicker(new FluentColourPicker());
		assertThat(fi.get().getWidget(), instanceOf(ColourPicker.class));
	}

	@Test
	void comboSetsWidget() {
		FluentFormItem fi = new FluentFormItem().combo(new FluentCombo());
		assertThat(fi.get().getWidget(), instanceOf(Combo.class));
	}

	@Test
	void radioSetsWidget() {
		FluentFormItem fi = new FluentFormItem().radio(new FluentRadio());
		assertThat(fi.get().getWidget(), instanceOf(Radio.class));
	}

	@Test
	void lookupDescriptionSetsWidget() {
		FluentFormItem fi = new FluentFormItem().lookupDescription(new FluentLookupDescription());
		assertThat(fi.get().getWidget(), instanceOf(LookupDescription.class));
	}

	@Test
	void passwordSetsWidget() {
		FluentFormItem fi = new FluentFormItem().password(new FluentPassword());
		assertThat(fi.get().getWidget(), instanceOf(Password.class));
	}

	@Test
	void richTextSetsWidget() {
		FluentFormItem fi = new FluentFormItem().richText(new FluentRichText());
		assertThat(fi.get().getWidget(), instanceOf(RichText.class));
	}

	@Test
	void sliderSetsWidget() {
		FluentFormItem fi = new FluentFormItem().slider(new FluentSlider());
		assertThat(fi.get().getWidget(), instanceOf(Slider.class));
	}

	@Test
	void spacerSetsWidget() {
		FluentFormItem fi = new FluentFormItem().spacer(new FluentSpacer());
		assertThat(fi.get().getWidget(), instanceOf(Spacer.class));
	}

	@Test
	void spinnerSetsWidget() {
		FluentFormItem fi = new FluentFormItem().spinner(new FluentSpinner());
		assertThat(fi.get().getWidget(), instanceOf(Spinner.class));
	}

	@Test
	void staticImageSetsWidget() {
		FluentFormItem fi = new FluentFormItem().staticImage(new FluentStaticImage());
		assertThat(fi.get().getWidget(), instanceOf(StaticImage.class));
	}

	@Test
	void linkSetsWidget() {
		FluentFormItem fi = new FluentFormItem().link(new FluentLink());
		assertThat(fi.get().getWidget(), instanceOf(Link.class));
	}

	@Test
	void textAreaSetsWidget() {
		FluentFormItem fi = new FluentFormItem().textArea(new FluentTextArea());
		assertThat(fi.get().getWidget(), instanceOf(TextArea.class));
	}

	@Test
	void textFieldSetsWidget() {
		FluentFormItem fi = new FluentFormItem().textField(new FluentTextField());
		assertThat(fi.get().getWidget(), instanceOf(TextField.class));
	}

	@Test
	void injectSetsWidget() {
		FluentFormItem fi = new FluentFormItem().inject(new FluentInject());
		assertThat(fi.get().getWidget(), instanceOf(Inject.class));
	}

	// ---- from() round-trips for each widget type ----

	@Test
	void fromDefaultWidgetRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new DefaultWidget());
		FluentFormItem fi = new FluentFormItem().from(source);
		assertThat(fi.get().getWidget(), instanceOf(DefaultWidget.class));
	}

	@Test
	void fromContentSignatureRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new ContentSignature());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(ContentSignature.class));
	}

	@Test
	void fromButtonRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new Button());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(Button.class));
	}

	@Test
	void fromZoomInRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new ZoomIn());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(ZoomIn.class));
	}

	@Test
	void fromDialogButtonRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new DialogButton());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(DialogButton.class));
	}

	@Test
	void fromGeometryRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new Geometry());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(Geometry.class));
	}

	@Test
	void fromGeometryMapRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new GeometryMap());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(GeometryMap.class));
	}

	@Test
	void fromHTMLRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new HTML());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(HTML.class));
	}

	@Test
	void fromLabelRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new Label());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(Label.class));
	}

	@Test
	void fromBlurbRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new Blurb());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(Blurb.class));
	}

	@Test
	void fromProgressBarRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new ProgressBar());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(ProgressBar.class));
	}

	@Test
	void fromCheckBoxRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new CheckBox());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(CheckBox.class));
	}

	@Test
	void fromColourPickerRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new ColourPicker());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(ColourPicker.class));
	}

	@Test
	void fromComboRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new Combo());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(Combo.class));
	}

	@Test
	void fromRadioRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new Radio());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(Radio.class));
	}

	@Test
	void fromLookupDescriptionRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new LookupDescription());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(LookupDescription.class));
	}

	@Test
	void fromPasswordRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new Password());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(Password.class));
	}

	@Test
	void fromRichTextRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new RichText());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(RichText.class));
	}

	@Test
	void fromSliderRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new Slider());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(Slider.class));
	}

	@Test
	void fromSpacerRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new Spacer());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(Spacer.class));
	}

	@Test
	void fromSpinnerRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new Spinner());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(Spinner.class));
	}

	@Test
	void fromStaticImageRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new StaticImage());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(StaticImage.class));
	}

	@Test
	void fromLinkRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new Link());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(Link.class));
	}

	@Test
	void fromTextAreaRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new TextArea());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(TextArea.class));
	}

	@Test
	void fromTextFieldRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new TextField());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(TextField.class));
	}

	@Test
	void fromInjectRoundTrips() {
		FormItem source = new FormItem();
		source.setWidget(new Inject());
		assertThat(new FluentFormItem().from(source).get().getWidget(), instanceOf(Inject.class));
	}

	@Test
	void fromCopiesColspanRowspanAndLabel() {
		FormItem source = new FormItem();
		source.setColspan(Integer.valueOf(2));
		source.setRowspan(Integer.valueOf(3));
		source.setLabel("Email");
		source.setShowLabel(Boolean.TRUE);
		source.setHelp("Enter email");
		source.setRequired(Boolean.TRUE);
		source.setWidget(new TextField());

		FluentFormItem fi = new FluentFormItem().from(source);
		assertThat(fi.get().getColspan(), is(Integer.valueOf(2)));
		assertThat(fi.get().getRowspan(), is(Integer.valueOf(3)));
		assertThat(fi.get().getLabel(), is("Email"));
		assertThat(fi.get().getShowLabel(), is(Boolean.TRUE));
		assertThat(fi.get().getHelp(), is("Enter email"));
		assertThat(fi.get().getRequired(), is(Boolean.TRUE));
	}

	@Test
	void fromFormItemWithUnknownWidgetTypeThrowsIllegalState() {
		FormItem source = new FormItem();
		source.setWidget(new MetaData() {
			// intentionally empty anonymous metadata implementation for invalid widget path
		});
		FluentFormItem fluentFormItem = new FluentFormItem();
		assertThrows(IllegalStateException.class, () -> fluentFormItem.from(source));
	}
}
