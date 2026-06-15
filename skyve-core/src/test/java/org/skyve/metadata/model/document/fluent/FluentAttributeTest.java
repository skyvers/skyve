package org.skyve.metadata.model.document.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.view.fluent.FluentCheckBox;
import org.skyve.metadata.view.fluent.FluentColourPicker;
import org.skyve.metadata.view.fluent.FluentCombo;
import org.skyve.metadata.view.fluent.FluentContentImage;
import org.skyve.metadata.view.fluent.FluentContentLink;
import org.skyve.metadata.view.fluent.FluentGeometry;
import org.skyve.metadata.view.fluent.FluentHTML;
import org.skyve.metadata.view.fluent.FluentLookupDescription;
import org.skyve.metadata.view.fluent.FluentPassword;
import org.skyve.metadata.view.fluent.FluentRadio;
import org.skyve.metadata.view.fluent.FluentRichText;
import org.skyve.metadata.view.fluent.FluentSlider;
import org.skyve.metadata.view.fluent.FluentSpinner;
import org.skyve.metadata.view.fluent.FluentTextArea;
import org.skyve.metadata.view.fluent.FluentTextField;

/**
 * Tests for {@link FluentAttribute} base class methods, exercised via
 * the concrete {@link FluentText} subclass.
 */
@SuppressWarnings({"static-method", "java:S4144"})
class FluentAttributeTest {

	@Test
	void auditedTrueSetsFlag() {
		FluentText f = new FluentText().audited(true);
		assertTrue(f.get().isAudited());
	}

	@Test
	void auditedFalseClearsFlag() {
		FluentText f = new FluentText().audited(false);
		assertFalse(f.get().isAudited());
	}

	@Test
	void deprecatedTrueSetsFlag() {
		FluentText f = new FluentText().deprecated(true);
		assertTrue(f.get().isDeprecated());
	}

	@Test
	void deprecatedFalseClearsFlag() {
		FluentText f = new FluentText().deprecated(false);
		assertFalse(f.get().isDeprecated());
	}

	@Test
	void descriptionSetsValue() {
		FluentText f = new FluentText().description("desc");
		assertEquals("desc", f.get().getDescription());
	}

	@Test
	void displayNameSetsValue() {
		FluentText f = new FluentText().displayName("Display");
		assertEquals("Display", f.get().getDisplayName());
	}

	@Test
	void documentationSetsValue() {
		FluentText f = new FluentText().documentation("docs");
		assertEquals("docs", f.get().getDocumentation());
	}

	@Test
	void nameSetsValue() {
		FluentText f = new FluentText().name("attrName");
		assertEquals("attrName", f.get().getName());
	}

	@Test
	void trackChangesTrueSetsFlag() {
		FluentText f = new FluentText().trackChanges(true);
		assertTrue(f.get().isTrackChanges());
	}

	@Test
	void trackChangesFalseClearsFlag() {
		FluentText f = new FluentText().trackChanges(false);
		assertFalse(f.get().isTrackChanges());
	}

	@Test
	void transientAttributeTrueSetsFlag() {
		FluentText f = new FluentText().transientAttribute(true);
		assertTrue(f.get().isTransient());
	}

	@Test
	void transientAttributeFalseClearsFlag() {
		FluentText f = new FluentText().transientAttribute(false);
		assertFalse(f.get().isTransient());
	}

	@Test
	void usageSetsValue() {
		FluentText f = new FluentText().usage(UsageType.view);
		assertEquals(UsageType.view, f.get().getUsage());
	}

	@Test
	void usageDomainSetsValue() {
		FluentText f = new FluentText().usage(UsageType.domain);
		assertEquals(UsageType.domain, f.get().getUsage());
	}

	@Test
	void sensitivitySetsValue() {
		FluentText f = new FluentText().sensitivity(Sensitivity.confidential);
		assertEquals(Sensitivity.confidential, f.get().getSensitivity());
	}

	@Test
	void sensitivitySecretSetsValue() {
		FluentText f = new FluentText().sensitivity(Sensitivity.secret);
		assertEquals(Sensitivity.secret, f.get().getSensitivity());
	}

	@Test
	void defaultWidgetContentImageSetsWidget() {
		FluentText f = new FluentText().name("x").defaultWidget(new FluentContentImage());
		assertThat(f.get(), is(notNullValue()));
	}

	@Test
	void defaultWidgetContentLinkSetsWidget() {
		FluentText f = new FluentText().name("x").defaultWidget(new FluentContentLink());
		assertThat(f.get(), is(notNullValue()));
	}

	@Test
	void defaultWidgetCheckBoxSetsWidget() {
		FluentText f = new FluentText().name("x").defaultWidget(new FluentCheckBox());
		assertThat(f.get(), is(notNullValue()));
	}

	@Test
	void defaultWidgetColourPickerSetsWidget() {
		FluentText f = new FluentText().name("x").defaultWidget(new FluentColourPicker());
		assertThat(f.get(), is(notNullValue()));
	}

	@Test
	void defaultWidgetComboSetsWidget() {
		FluentText f = new FluentText().name("x").defaultWidget(new FluentCombo());
		assertThat(f.get(), is(notNullValue()));
	}

	@Test
	void defaultWidgetGeometrySetsWidget() {
		FluentText f = new FluentText().name("x").defaultWidget(new FluentGeometry());
		assertThat(f.get(), is(notNullValue()));
	}

	@Test
	void defaultWidgetHTMLSetsWidget() {
		FluentText f = new FluentText().name("x").defaultWidget(new FluentHTML());
		assertThat(f.get(), is(notNullValue()));
	}

	@Test
	void defaultWidgetLookupDescriptionSetsWidget() {
		FluentText f = new FluentText().name("x").defaultWidget(new FluentLookupDescription());
		assertThat(f.get(), is(notNullValue()));
	}

	@Test
	void defaultWidgetPasswordSetsWidget() {
		FluentText f = new FluentText().name("x").defaultWidget(new FluentPassword());
		assertThat(f.get(), is(notNullValue()));
	}

	@Test
	void defaultWidgetRadioSetsWidget() {
		FluentText f = new FluentText().name("x").defaultWidget(new FluentRadio());
		assertThat(f.get(), is(notNullValue()));
	}

	@Test
	void defaultWidgetRichTextSetsWidget() {
		FluentText f = new FluentText().name("x").defaultWidget(new FluentRichText());
		assertThat(f.get(), is(notNullValue()));
	}

	@Test
	void defaultWidgetSliderSetsWidget() {
		FluentText f = new FluentText().name("x").defaultWidget(new FluentSlider());
		assertThat(f.get(), is(notNullValue()));
	}

	@Test
	void defaultWidgetSpinnerSetsWidget() {
		FluentText f = new FluentText().name("x").defaultWidget(new FluentSpinner());
		assertThat(f.get(), is(notNullValue()));
	}

	@Test
	void defaultWidgetTextAreaSetsWidget() {
		FluentText f = new FluentText().name("x").defaultWidget(new FluentTextArea());
		assertThat(f.get(), is(notNullValue()));
	}

	@Test
	void defaultWidgetTextFieldSetsWidget() {
		FluentText f = new FluentText().name("x").defaultWidget(new FluentTextField());
		assertThat(f.get(), is(notNullValue()));
	}

	@Test
	void fromCopiesBaseAttributes() {
		// Build a source text attribute with all base fields set
		FluentText source = new FluentText()
				.name("srcName")
				.displayName("Display Name")
				.description("desc")
				.documentation("docs")
				.audited(true)
				.deprecated(true)
				.trackChanges(true)
				.transientAttribute(true)
				.usage(UsageType.both)
				.sensitivity(Sensitivity.restricted);

		// Copy via a new FluentText.from()
		FluentText copy = new FluentText();
		copy.from(source.get());

		assertEquals("srcName", copy.get().getName());
		assertEquals("Display Name", copy.get().getDisplayName());
		assertEquals("desc", copy.get().getDescription());
		assertEquals("docs", copy.get().getDocumentation());
		assertTrue(copy.get().isAudited());
		assertTrue(copy.get().isDeprecated());
		assertTrue(copy.get().isTrackChanges());
		assertTrue(copy.get().isTransient());
		assertEquals(UsageType.both, copy.get().getUsage());
		assertEquals(Sensitivity.restricted, copy.get().getSensitivity());
	}

	@Test
	void fromCopiesDefaultWidgetWhenPresent() {
		FluentText source = new FluentText().name("x").defaultWidget(new FluentTextField());
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertNotNull(copy.get());
	}

	@Test
	void fromCopiesDefaultWidgetColourPicker() {
		FluentText source = new FluentText().name("x").defaultWidget(new FluentColourPicker());
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertNotNull(copy.get());
	}

	@Test
	void fromCopiesDefaultWidgetCombo() {
		FluentText source = new FluentText().name("x").defaultWidget(new FluentCombo());
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertNotNull(copy.get());
	}

	@Test
	void fromCopiesDefaultWidgetPassword() {
		FluentText source = new FluentText().name("x").defaultWidget(new FluentPassword());
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertNotNull(copy.get());
	}

	@Test
	void fromCopiesDefaultWidgetRadio() {
		FluentText source = new FluentText().name("x").defaultWidget(new FluentRadio());
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertNotNull(copy.get());
	}

	@Test
	void fromCopiesDefaultWidgetRichText() {
		FluentText source = new FluentText().name("x").defaultWidget(new FluentRichText());
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertNotNull(copy.get());
	}

	@Test
	void fromCopiesDefaultWidgetSlider() {
		FluentText source = new FluentText().name("x").defaultWidget(new FluentSlider());
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertNotNull(copy.get());
	}

	@Test
	void fromCopiesDefaultWidgetSpinner() {
		FluentText source = new FluentText().name("x").defaultWidget(new FluentSpinner());
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertNotNull(copy.get());
	}

	@Test
	void fromCopiesDefaultWidgetTextArea() {
		FluentText source = new FluentText().name("x").defaultWidget(new FluentTextArea());
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertNotNull(copy.get());
	}

	@Test
	void fromCopiesDefaultWidgetTextField() {
		FluentText source = new FluentText().name("x").defaultWidget(new FluentTextField());
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertNotNull(copy.get());
	}

	@Test
	void fromCopiesDefaultWidgetContentImage() {
		FluentText source = new FluentText().name("x").defaultWidget(new FluentContentImage());
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertNotNull(copy.get());
	}

	@Test
	void fromCopiesDefaultWidgetContentLink() {
		FluentText source = new FluentText().name("x").defaultWidget(new FluentContentLink());
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertNotNull(copy.get());
	}

	@Test
	void fromCopiesDefaultWidgetCheckBox() {
		FluentText source = new FluentText().name("x").defaultWidget(new FluentCheckBox());
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertNotNull(copy.get());
	}

	@Test
	void fromCopiesDefaultWidgetGeometry() {
		FluentText source = new FluentText().name("x").defaultWidget(new FluentGeometry());
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertNotNull(copy.get());
	}

	@Test
	void fromCopiesDefaultWidgetHTML() {
		FluentText source = new FluentText().name("x").defaultWidget(new FluentHTML());
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertNotNull(copy.get());
	}

	@Test
	void fromCopiesDefaultWidgetLookupDescription() {
		FluentText source = new FluentText().name("x").defaultWidget(new FluentLookupDescription());
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertNotNull(copy.get());
	}

	@Test
	void fromWithUnknownDefaultWidgetThrowsIllegalState() {
		Text text = new Text();
		WidgetReference ref = new WidgetReference();
		ref.setWidget(new InputWidget() {
			// intentionally empty unknown widget for error-path coverage
		});
		text.setDefaultWidgetReference(ref);
		FluentText fluentText = new FluentText();
		assertThrows(IllegalStateException.class, () -> fluentText.from(text));
	}
}
