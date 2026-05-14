package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.component.Component;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;

/**
 * Tests for {@link FluentComponent}, {@link FluentProgressBar},
 * and {@link FluentSpinner}: constructors, setters, and {@code from()}.
 */
@SuppressWarnings("static-method")
class FluentComponentProgressBarSpinnerTest {

	// --- FluentComponent ---

	@Test
	void componentDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentComponent().get());
	}

	@Test
	void componentWrappingConstructorPreservesInstance() {
		Component c = new Component();
		FluentComponent fc = new FluentComponent(c);
		assertEquals(c, fc.get());
	}

	@Test
	void componentModuleNameSetsValue() {
		FluentComponent fc = new FluentComponent().moduleName("admin");
		assertEquals("admin", fc.get().getModuleName());
	}

	@Test
	void componentDocumentNameSetsValue() {
		FluentComponent fc = new FluentComponent().documentName("Contact");
		assertEquals("Contact", fc.get().getDocumentName());
	}

	@Test
	void componentNameSetsValue() {
		FluentComponent fc = new FluentComponent().name("myComponent");
		assertEquals("myComponent", fc.get().getName());
	}

	@Test
	void componentWidgetIdSetsValue() {
		FluentComponent fc = new FluentComponent().widgetId("wid1");
		assertEquals("wid1", fc.get().getWidgetId());
	}

	@Test
	void componentInvisibleConditionNameSetsValue() {
		FluentComponent fc = new FluentComponent().invisibleConditionName("hide");
		assertEquals("hide", fc.get().getInvisibleConditionName());
	}

	@Test
	void componentFromCopiesAllFields() {
		Component source = new Component();
		source.setModuleName("admin");
		source.setDocumentName("User");
		source.setName("cmpUser");
		source.setWidgetId("w99");
		source.setInvisibleConditionName("noShow");

		FluentComponent copy = new FluentComponent().from(source);

		assertEquals("admin", copy.get().getModuleName());
		assertEquals("User", copy.get().getDocumentName());
		assertEquals("cmpUser", copy.get().getName());
		assertEquals("w99", copy.get().getWidgetId());
		assertEquals("noShow", copy.get().getInvisibleConditionName());
	}

	// --- FluentProgressBar ---

	@Test
	void progressBarDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentProgressBar().get());
	}

	@Test
	void progressBarWrappingConstructorPreservesInstance() {
		ProgressBar pb = new ProgressBar();
		FluentProgressBar fpb = new FluentProgressBar(pb);
		assertEquals(pb, fpb.get());
	}

	@Test
	void progressBarInvisibleConditionNameSetsValue() {
		FluentProgressBar fpb = new FluentProgressBar().invisibleConditionName("hide");
		assertEquals("hide", fpb.get().getInvisibleConditionName());
	}

	@Test
	void progressBarPixelWidthSetsValue() {
		FluentProgressBar fpb = new FluentProgressBar().pixelWidth(200);
		assertEquals(Integer.valueOf(200), fpb.get().getPixelWidth());
	}

	@Test
	void progressBarPixelHeightSetsValue() {
		FluentProgressBar fpb = new FluentProgressBar().pixelHeight(40);
		assertEquals(Integer.valueOf(40), fpb.get().getPixelHeight());
	}

	@Test
	void progressBarMinPixelWidthSetsValue() {
		FluentProgressBar fpb = new FluentProgressBar().minPixelWidth(100);
		assertEquals(Integer.valueOf(100), fpb.get().getMinPixelWidth());
	}

	@Test
	void progressBarMaxPixelWidthSetsValue() {
		FluentProgressBar fpb = new FluentProgressBar().maxPixelWidth(500);
		assertEquals(Integer.valueOf(500), fpb.get().getMaxPixelWidth());
	}

	@Test
	void progressBarFromCopiesInvisibleConditionName() {
		ProgressBar source = new ProgressBar();
		source.setInvisibleConditionName("gone");

		FluentProgressBar copy = new FluentProgressBar().from(source);
		assertEquals("gone", copy.get().getInvisibleConditionName());
	}

	// --- FluentSpinner ---

	@Test
	void spinnerDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentSpinner().get());
	}

	@Test
	void spinnerWrappingConstructorPreservesInstance() {
		Spinner s = new Spinner();
		FluentSpinner fs = new FluentSpinner(s);
		assertEquals(s, fs.get());
	}

	@Test
	void spinnerMinSetsValue() {
		FluentSpinner fs = new FluentSpinner().min(1.0);
		assertEquals(Double.valueOf(1.0), fs.get().getMin());
	}

	@Test
	void spinnerMaxSetsValue() {
		FluentSpinner fs = new FluentSpinner().max(100.0);
		assertEquals(Double.valueOf(100.0), fs.get().getMax());
	}

	@Test
	void spinnerStepSetsValue() {
		FluentSpinner fs = new FluentSpinner().step(0.5);
		assertEquals(Double.valueOf(0.5), fs.get().getStep());
	}

	@Test
	void spinnerFromCopiesMinMaxStep() {
		Spinner source = new Spinner();
		source.setMin(Double.valueOf(0.0));
		source.setMax(Double.valueOf(50.0));
		source.setStep(Double.valueOf(2.5));

		FluentSpinner copy = new FluentSpinner().from(source);

		assertEquals(Double.valueOf(0.0), copy.get().getMin());
		assertEquals(Double.valueOf(50.0), copy.get().getMax());
		assertEquals(Double.valueOf(2.5), copy.get().getStep());
	}
}
