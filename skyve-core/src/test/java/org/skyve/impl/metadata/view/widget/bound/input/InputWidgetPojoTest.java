package org.skyve.impl.metadata.view.widget.bound.input;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.TextOutput.Sanitisation;

/**
 * Tests for input widget POJO getter/setter methods not covered by existing test files.
 */
@SuppressWarnings("static-method")
class InputWidgetPojoTest {

	// ---- InputWidget (via TextField) - visible/enabled conditions ----

	@Test
	void inputWidgetSetDisabledConditionNameRoundtrip() {
		TextField tf = new TextField();
		tf.setDisabledConditionName("isReadonly");
		assertThat(tf.getDisabledConditionName(), is("isReadonly"));
	}

	@Test
	void inputWidgetSetEnabledConditionNameNegatesCondition() {
		TextField tf = new TextField();
		tf.setEnabledConditionName("isEditable");
		assertThat(tf.getDisabledConditionName(), startsWith("not"));
	}

	@Test
	void inputWidgetSetInvisibleConditionNameRoundtrip() {
		TextField tf = new TextField();
		tf.setInvisibleConditionName("hiddenWhen");
		assertThat(tf.getInvisibleConditionName(), is("hiddenWhen"));
	}

	@Test
	void inputWidgetSetVisibleConditionNameNegatesCondition() {
		TextField tf = new TextField();
		tf.setVisibleConditionName("showWhen");
		assertThat(tf.getInvisibleConditionName(), startsWith("not"));
	}

	// ---- TextField ----

	@Test
	void textFieldShowsLabelByDefault() {
		assertTrue(new TextField().showsLabelByDefault());
	}

	@Test
	void textFieldSetPixelWidthRoundtrip() {
		TextField tf = new TextField();
		tf.setPixelWidth(Integer.valueOf(200));
		assertThat(tf.getPixelWidth(), is(Integer.valueOf(200)));
	}

	@Test
	void textFieldSetEditableRoundtrip() {
		TextField tf = new TextField();
		tf.setEditable(Boolean.TRUE);
		assertThat(tf.getEditable(), is(Boolean.TRUE));
	}

	@Test
	void textFieldSetKeyboardTypeRoundtrip() {
		TextField tf = new TextField();
		tf.setKeyboardType(KeyboardType.email);
		assertThat(tf.getKeyboardType(), is(KeyboardType.email));
	}

	@Test
	void textFieldSetCompleteRoundtrip() {
		TextField tf = new TextField();
		tf.setComplete(CompleteType.suggest);
		assertThat(tf.getComplete(), is(CompleteType.suggest));
	}

	@Test
	void textFieldGetPropertiesNotNull() {
		assertNotNull(new TextField().getProperties());
	}

	// ---- TextArea ----

	@Test
	void textAreaShowsLabelByDefault() {
		assertTrue(new TextArea().showsLabelByDefault());
	}

	@Test
	void textAreaSetPixelWidthRoundtrip() {
		TextArea ta = new TextArea();
		ta.setPixelWidth(Integer.valueOf(400));
		assertThat(ta.getPixelWidth(), is(Integer.valueOf(400)));
	}

	@Test
	void textAreaSetPixelHeightRoundtrip() {
		TextArea ta = new TextArea();
		ta.setPixelHeight(Integer.valueOf(150));
		assertThat(ta.getPixelHeight(), is(Integer.valueOf(150)));
	}

	@Test
	void textAreaSetWordWrapRoundtrip() {
		TextArea ta = new TextArea();
		ta.setWordWrap(Boolean.TRUE);
		assertThat(ta.getWordWrap(), is(Boolean.TRUE));
	}

	@Test
	void textAreaSetEditableRoundtrip() {
		TextArea ta = new TextArea();
		ta.setEditable(Boolean.FALSE);
		assertThat(ta.getEditable(), is(Boolean.FALSE));
	}

	@Test
	void textAreaSetKeyboardTypeRoundtrip() {
		TextArea ta = new TextArea();
		ta.setKeyboardType(KeyboardType.email);
		assertThat(ta.getKeyboardType(), is(KeyboardType.email));
	}

	@Test
	void textAreaSetMinPixelHeightRoundtrip() {
		TextArea ta = new TextArea();
		ta.setMinPixelHeight(Integer.valueOf(50));
		assertThat(ta.getMinPixelHeight(), is(Integer.valueOf(50)));
	}

	@Test
	void textAreaGetPropertiesNotNull() {
		assertNotNull(new TextArea().getProperties());
	}

	// ---- Password ----

	@Test
	void passwordShowsLabelByDefault() {
		assertTrue(new Password().showsLabelByDefault());
	}

	@Test
	void passwordSetPixelWidthRoundtrip() {
		Password pw = new Password();
		pw.setPixelWidth(Integer.valueOf(150));
		assertThat(pw.getPixelWidth(), is(Integer.valueOf(150)));
	}

	@Test
	void passwordGetPropertiesNotNull() {
		assertNotNull(new Password().getProperties());
	}

	// ---- Radio ----

	@Test
	void radioShowsLabelByDefault() {
		assertTrue(new Radio().showsLabelByDefault());
	}

	@Test
	void radioSetPixelWidthRoundtrip() {
		Radio radio = new Radio();
		radio.setPixelWidth(Integer.valueOf(120));
		assertThat(radio.getPixelWidth(), is(Integer.valueOf(120)));
	}

	@Test
	void radioSetVerticalRoundtrip() {
		Radio radio = new Radio();
		radio.setVertical(Boolean.TRUE);
		assertThat(radio.getVertical(), is(Boolean.TRUE));
	}

	@Test
	void radioGetPropertiesNotNull() {
		assertNotNull(new Radio().getProperties());
	}

	// ---- RichText ----

	@Test
	void richTextShowsLabelByDefault() {
		assertTrue(new RichText().showsLabelByDefault());
	}

	@Test
	void richTextGetPropertiesNotNull() {
		assertNotNull(new RichText().getProperties());
	}

	// ---- Slider ----

	@Test
	void sliderShowsLabelByDefault() {
		assertTrue(new Slider().showsLabelByDefault());
	}

	@Test
	void sliderSetMinRoundtrip() {
		Slider slider = new Slider();
		slider.setMin(Double.valueOf(0.0));
		assertThat(slider.getMin(), is(Double.valueOf(0.0)));
	}

	@Test
	void sliderSetMaxRoundtrip() {
		Slider slider = new Slider();
		slider.setMax(Double.valueOf(100.0));
		assertThat(slider.getMax(), is(Double.valueOf(100.0)));
	}

	@Test
	void sliderGetPropertiesNotNull() {
		assertNotNull(new Slider().getProperties());
	}

	// ---- HTML ----

	@Test
	void htmlShowsLabelByDefault() {
		assertTrue(new HTML().showsLabelByDefault());
	}

	@Test
	void htmlSetPixelWidthRoundtrip() {
		HTML html = new HTML();
		html.setPixelWidth(Integer.valueOf(500));
		assertThat(html.getPixelWidth(), is(Integer.valueOf(500)));
	}

	@Test
	void htmlSetPixelHeightRoundtrip() {
		HTML html = new HTML();
		html.setPixelHeight(Integer.valueOf(300));
		assertThat(html.getPixelHeight(), is(Integer.valueOf(300)));
	}

	@Test
	void htmlSetMentionMarkersRoundtrip() {
		HTML html = new HTML();
		html.setMentionMarkers("@,#");
		assertThat(html.getMentionMarkers(), is("@,#"));
	}

	@Test
	void htmlBlankMentionMarkersBecomesNull() {
		HTML html = new HTML();
		html.setMentionMarkers("  ");
		assertNull(html.getMentionMarkers());
	}

	@Test
	void htmlSetSanitiseRoundtrip() {
		HTML html = new HTML();
		html.setSanitise(Sanitisation.relaxed);
		assertThat(html.getSanitise(), is(Sanitisation.relaxed));
	}

	@Test
	void htmlGetPropertiesNotNull() {
		assertNotNull(new HTML().getProperties());
	}

	// ---- CheckMembership ----

	@Test
	void checkMembershipGetPropertiesNotNull() {
		assertNotNull(new CheckMembership().getProperties());
	}

	// ---- DefaultWidget ----

	@Test
	void defaultWidgetGetPropertiesNotNull() {
		assertNotNull(new DefaultWidget().getProperties());
	}

	// ---- Combo ----

	@Test
	void comboShowsLabelByDefault() {
		assertTrue(new Combo().showsLabelByDefault());
	}

	@Test
	void comboSetPixelWidthRoundtrip() {
		Combo combo = new Combo();
		combo.setPixelWidth(Integer.valueOf(200));
		assertThat(combo.getPixelWidth(), is(Integer.valueOf(200)));
	}

	@Test
	void comboGetPropertiesNotNull() {
		assertNotNull(new Combo().getProperties());
	}

	// ---- CheckBox ----

	@Test
	void checkBoxShowsLabelByDefault() {
		assertTrue(new CheckBox().showsLabelByDefault());
	}

	@Test
	void checkBoxSetPixelWidthRoundtrip() {
		CheckBox cb = new CheckBox();
		cb.setPixelWidth(Integer.valueOf(100));
		assertThat(cb.getPixelWidth(), is(Integer.valueOf(100)));
	}

	@Test
	void checkBoxSetPixelHeightRoundtrip() {
		CheckBox cb = new CheckBox();
		cb.setPixelHeight(Integer.valueOf(50));
		assertThat(cb.getPixelHeight(), is(Integer.valueOf(50)));
	}

	@Test
	void checkBoxSetTriStateRoundtrip() {
		CheckBox cb = new CheckBox();
		cb.setTriState(Boolean.TRUE);
		assertThat(cb.getTriState(), is(Boolean.TRUE));
	}

	@Test
	void checkBoxGetPropertiesNotNull() {
		assertNotNull(new CheckBox().getProperties());
	}

	// ---- ColourPicker ----

	@Test
	void colourPickerShowsLabelByDefault() {
		assertTrue(new ColourPicker().showsLabelByDefault());
	}

	@Test
	void colourPickerSetPixelWidthRoundtrip() {
		ColourPicker cp = new ColourPicker();
		cp.setPixelWidth(Integer.valueOf(120));
		assertThat(cp.getPixelWidth(), is(Integer.valueOf(120)));
	}

	@Test
	void colourPickerGetPropertiesNotNull() {
		assertNotNull(new ColourPicker().getProperties());
	}
}
