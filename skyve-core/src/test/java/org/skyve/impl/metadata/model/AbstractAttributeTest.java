package org.skyve.impl.metadata.model;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.InverseOne;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentDisplay;
import org.skyve.impl.metadata.view.widget.bound.input.ContentUpload;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.Attribute.UsageType;

/**
 * Tests AbstractAttribute via the concrete Text subclass (which inherits from Field -> ConvertibleField -> AbstractAttribute).
 */
class AbstractAttributeTest {

	// ---- displayName ----

	@Test
	@SuppressWarnings("static-method")
	void setDisplayNameAndGet() {
		Text attr = new Text();
		attr.setDisplayName("My Field");
		assertThat(attr.getDisplayName(), is("My Field"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDisplayNameBlankBecomesNull() {
		Text attr = new Text();
		attr.setDisplayName("  ");
		assertNull(attr.getDisplayName());
	}

	// ---- description ----

	@Test
	@SuppressWarnings("static-method")
	void setDescriptionAndGet() {
		Text attr = new Text();
		attr.setDescription("Field description");
		assertThat(attr.getDescription(), is("Field description"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDescriptionBlankBecomesNull() {
		Text attr = new Text();
		attr.setDescription("  ");
		assertNull(attr.getDescription());
	}

	// ---- usage ----

	@Test
	@SuppressWarnings("static-method")
	void setUsageAndGet() {
		Text attr = new Text();
		attr.setUsage(UsageType.view);
		assertThat(attr.getUsage(), is(UsageType.view));
	}

	// ---- sensitivity ----

	@Test
	@SuppressWarnings("static-method")
	void setSensitivityAndGet() {
		Text attr = new Text();
		attr.setSensitivity(Sensitivity.personal);
		assertThat(attr.getSensitivity(), is(Sensitivity.personal));
	}

	// ---- deprecated ----

	@Test
	@SuppressWarnings("static-method")
	void isDeprecatedDefaultFalse() {
		Text attr = new Text();
		assertFalse(attr.isDeprecated());
	}

	@Test
	@SuppressWarnings("static-method")
	void setDeprecatedTrue() {
		Text attr = new Text();
		attr.setDeprecated(true);
		assertTrue(attr.isDeprecated());
	}

	@Test
	@SuppressWarnings("static-method")
	void setDeprecatedBoolTrue() {
		Text attr = new Text();
		attr.setDeprecatedBool(Boolean.TRUE);
		assertTrue(attr.isDeprecated());
		assertThat(attr.getDeprecatedBool(), is(Boolean.TRUE));
	}

	// ---- trackChanges ----

	@Test
	@SuppressWarnings("static-method")
	void isTrackChangesDefaultTrueForTextType() {
		Text attr = new Text();
		assertTrue(attr.isTrackChanges());
	}

	@Test
	@SuppressWarnings("static-method")
	void setTrackChangesFalse() {
		Text attr = new Text();
		attr.setTrackChanges(false);
		assertFalse(attr.isTrackChanges());
	}

	@Test
	@SuppressWarnings("static-method")
	void setTrackChangesBoolNull() {
		Text attr = new Text();
		attr.setTrackChangesBool(null);
		assertNull(attr.getTrackChangesBool());
		// when trackChanges is null and type is text, defaults to true
		assertTrue(attr.isTrackChanges());
	}

	// ---- audited ----

	@Test
	@SuppressWarnings("static-method")
	void isAuditedDefaultTrue() {
		Text attr = new Text();
		assertTrue(attr.isAudited());
	}

	@Test
	@SuppressWarnings("static-method")
	void setAuditedFalse() {
		Text attr = new Text();
		attr.setAudited(false);
		assertFalse(attr.isAudited());
	}

	@Test
	@SuppressWarnings("static-method")
	void setAuditedBoolFalse() {
		Text attr = new Text();
		attr.setAuditedBool(Boolean.FALSE);
		assertFalse(attr.isAudited());
		assertThat(attr.getAuditedBool(), is(Boolean.FALSE));
	}

	// ---- transient ----

	@Test
	@SuppressWarnings("static-method")
	void isTransientDefaultFalse() {
		Text attr = new Text();
		assertFalse(attr.isTransient());
	}

	@Test
	@SuppressWarnings("static-method")
	void setTransientTrue() {
		Text attr = new Text();
		attr.setTransient(true);
		assertTrue(attr.isTransient());
	}

	@Test
	@SuppressWarnings("static-method")
	void getTransientBoolReflectsState() {
		Text attr = new Text();
		attr.setTransient(true);
		assertThat(attr.getTransientBool(), is(Boolean.TRUE));
	}

	@Test
	@SuppressWarnings("static-method")
	void setTransientBoolTrue() {
		Text attr = new Text();
		attr.setTransientBool(Boolean.TRUE);
		assertTrue(attr.isTransient());
	}

	// ---- documentation ----

	@Test
	@SuppressWarnings("static-method")
	void setDocumentationAndGet() {
		Text attr = new Text();
		attr.setDocumentation("Some docs");
		assertThat(attr.getDocumentation(), is("Some docs"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDocumentationBlankBecomesNull() {
		Text attr = new Text();
		attr.setDocumentation("  ");
		assertNull(attr.getDocumentation());
	}

	// ---- properties map ----

	@Test
	@SuppressWarnings("static-method")
	void propertiesMapInitiallyEmpty() {
		Text attr = new Text();
		assertNotNull(attr.getProperties());
		assertEquals(0, attr.getProperties().size());
	}

	@Test
	@SuppressWarnings("static-method")
	void propertiesMapCanAddEntries() {
		Text attr = new Text();
		attr.getProperties().put("key", "value");
		assertThat(attr.getProperties().get("key"), is("value"));
	}

	// ---- getDefaultInputWidget for text type ----

	@Test
	@SuppressWarnings("static-method")
	void defaultInputWidgetForTextIsTextField() {
		Text attr = new Text();
		attr.setName("myField");
		assertThat(attr.getDefaultInputWidget().getClass().getSimpleName(), is("TextField"));
	}

	// ---- getDefaultInputWidget for bool type ----

	@Test
	@SuppressWarnings("static-method")
	void defaultInputWidgetForBoolIsCheckBox() {
		Text attr = new Text();
		attr.setName("myBool");
		attr.setAttributeType(AttributeType.bool);
		assertThat(attr.getDefaultInputWidget().getClass().getSimpleName(), is("CheckBox"));
	}

	// ---- setDefaultInputWidget ----

	@Test
	@SuppressWarnings("static-method")
	void setDefaultInputWidgetOverridesComputed() {
		Text attr = new Text();
		attr.setName("myField");
		CheckBox cb = new CheckBox();
		attr.setDefaultInputWidget(cb);
		assertThat(attr.getDefaultInputWidget().getClass().getSimpleName(), is("CheckBox"));
	}

	// ---- getRequiredMessage returns null ----

	@Test
	@SuppressWarnings("static-method")
	void getRequiredMessageNullByDefault() {
		Text attr = new Text();
		assertNull(attr.getRequiredMessage());
	}

	// ---- suppress unused import warnings ----
	// These ensure the import of Combo, LookupDescription, TextField are referenced
	@Test
	@SuppressWarnings("static-method")
	void defaultInputWidgetForEnumerationIsCombo() {
		Text attr = new Text();
		attr.setName("myEnum");
		attr.setAttributeType(AttributeType.enumeration);
		assertTrue(attr.getDefaultInputWidget() instanceof Combo);
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultInputWidgetForAssociationIsLookupDescription() {
		Text attr = new Text();
		attr.setName("myAssoc");
		attr.setAttributeType(AttributeType.association);
		assertTrue(attr.getDefaultInputWidget() instanceof LookupDescription);
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultInputWidgetForTextIsTextFieldInstance() {
		Text attr = new Text();
		attr.setName("myText");
		// Text has AttributeType.text set in its constructor
		assertTrue(attr.getDefaultInputWidget() instanceof TextField);
	}

	// ---- defaultWidgetReference ----

	@Test
	@SuppressWarnings("static-method")
	void setDefaultWidgetReferenceAndGet() {
		Text attr = new Text();
		attr.setName("myField");
		WidgetReference ref = new WidgetReference();
		// WidgetReference needs a widget and the attribute needs a name so binding can be set
		TextField tf = new TextField();
		ref.setWidget(tf);
		attr.setDefaultWidgetReference(ref);
		assertThat(attr.getDefaultWidgetReference(), is(ref));
	}

	@Test
	@SuppressWarnings("static-method")
	void setNullWidgetReferenceIsNoOp() {
		Text attr = new Text();
		attr.setDefaultWidgetReference(null);
		assertNull(attr.getDefaultWidgetReference());
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultWidgetReferenceNullByDefault() {
		Text attr = new Text();
		assertNull(attr.getDefaultWidgetReference());
	}

	// ---- getLocalisedDisplayName ----

	@Test
	@SuppressWarnings("static-method")
	void getLocalisedDisplayNameNullWhenDisplayNameNull() {
		Text attr = new Text();
		// getDisplayName() is null → i18n(null) returns null
		assertNull(attr.getLocalisedDisplayName());
	}

	@Test
	@SuppressWarnings("static-method")
	void getLocalisedDisplayNameNonNullWhenSet() {
		Text attr = new Text();
		attr.setDisplayName("My Field");
		assertNotNull(attr.getLocalisedDisplayName());
	}

	// ---- getLocalisedDescription ----

	@Test
	@SuppressWarnings("static-method")
	void getLocalisedDescriptionNullWhenDescriptionNull() {
		Text attr = new Text();
		assertNull(attr.getLocalisedDescription());
	}

	// ---- getRequiredMessage returns null for non-Field types ----

	@Test
	@SuppressWarnings("static-method")
	void getRequiredMessageNullForInverseType() {
		InverseOne inv = new InverseOne();
		// InverseOne does not override getRequiredMessage() — base AbstractAttribute returns null
		assertNull(inv.getRequiredMessage());
	}

	// ---- getDefaultInputWidget for remaining AttributeType branches ----

	@Test
	@SuppressWarnings("static-method")
	void defaultInputWidgetForMemoIsTextArea() {
		Text attr = new Text();
		attr.setName("myMemo");
		attr.setAttributeType(AttributeType.memo);
		assertTrue(attr.getDefaultInputWidget() instanceof TextArea);
		assertEquals("myMemo", attr.getDefaultInputWidget().getBinding());
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultInputWidgetForMarkupIsRichText() {
		Text attr = new Text();
		attr.setName("myMarkup");
		attr.setAttributeType(AttributeType.markup);
		assertTrue(attr.getDefaultInputWidget() instanceof RichText);
		assertEquals("myMarkup", attr.getDefaultInputWidget().getBinding());
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultInputWidgetForContentIsContentUploadAuto() {
		Text attr = new Text();
		attr.setName("myContent");
		attr.setAttributeType(AttributeType.content);
		assertTrue(attr.getDefaultInputWidget() instanceof ContentUpload);
		assertEquals(ContentDisplay.auto, ((ContentUpload) attr.getDefaultInputWidget()).getResolvedDisplay());
		assertEquals("myContent", attr.getDefaultInputWidget().getBinding());
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultInputWidgetForImageIsContentUploadImage() {
		Text attr = new Text();
		attr.setName("myImage");
		attr.setAttributeType(AttributeType.image);
		assertTrue(attr.getDefaultInputWidget() instanceof ContentUpload);
		assertEquals(ContentDisplay.image, ((ContentUpload) attr.getDefaultInputWidget()).getResolvedDisplay());
		assertEquals("myImage", attr.getDefaultInputWidget().getBinding());
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultInputWidgetForGeometryIsGeometry() {
		Text attr = new Text();
		attr.setName("myGeom");
		attr.setAttributeType(AttributeType.geometry);
		assertTrue(attr.getDefaultInputWidget() instanceof Geometry);
		assertEquals("myGeom", attr.getDefaultInputWidget().getBinding());
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultInputWidgetForColourIsColourPicker() {
		Text attr = new Text();
		attr.setName("myColour");
		attr.setAttributeType(AttributeType.colour);
		assertTrue(attr.getDefaultInputWidget() instanceof ColourPicker);
		assertEquals("myColour", attr.getDefaultInputWidget().getBinding());
	}
}
