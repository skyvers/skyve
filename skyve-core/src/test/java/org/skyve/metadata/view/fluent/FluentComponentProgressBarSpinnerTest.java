package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;

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
	void componentInvisibleConditionSetsValue() {
		FluentComponent fc = new FluentComponent().invisibleCondition("hide");
		assertEquals("hide", fc.get().getInvisibleCondition());
	}

	@Test
	void componentAddActionAddsAnActionMapping() {
		FluentComponent fc = new FluentComponent().addAction("Check", "Verify").addAction("New", "Create");
		assertEquals(2, fc.get().getActions().size());
		assertEquals("Check", fc.get().getActions().get(0).getFromComponent());
		assertEquals("Verify", fc.get().getActions().get(0).getMappedTo());
		assertEquals("Create", fc.get().getActions().get(1).getMappedTo());
		assertEquals(0, fc.get().getConditions().size());
	}

	@Test
	void componentAddConditionAddsAConditionMapping() {
		FluentComponent fc = new FluentComponent().addCondition("editable", "canEdit").addCondition("{el:bean.flag}", "{el:bean.other}");
		assertEquals(2, fc.get().getConditions().size());
		assertEquals("editable", fc.get().getConditions().get(0).getFromComponent());
		assertEquals("canEdit", fc.get().getConditions().get(0).getMappedTo());
		assertEquals("{el:bean.flag}", fc.get().getConditions().get(1).getFromComponent());
		assertEquals("{el:bean.other}", fc.get().getConditions().get(1).getMappedTo());
		assertEquals(0, fc.get().getActions().size());
	}

	@Test
	void componentClearActionsAndConditionsRemoveOnlyTheirOwnMappings() {
		FluentComponent fc = new FluentComponent().addAction("Check", "Verify").addCondition("editable", "canEdit");
		fc.clearActions();
		assertEquals(0, fc.get().getActions().size());
		assertEquals(1, fc.get().getConditions().size());
		fc.addAction("Check", "Verify").clearConditions();
		assertEquals(1, fc.get().getActions().size());
		assertEquals(0, fc.get().getConditions().size());
	}

	@Test
	void componentFromCopiesActionAndConditionMappings() {
		Component source = new FluentComponent().name("cmp")
													.addAction("Check", "Verify")
													.addCondition("editable", "canEdit")
													.addCondition("{el:bean.flag}", "locked")
													.get();

		Component copy = new FluentComponent().from(source).get();

		assertEquals(1, copy.getActions().size());
		assertEquals("Check", copy.getActions().get(0).getFromComponent());
		assertEquals("Verify", copy.getActions().get(0).getMappedTo());
		assertEquals(2, copy.getConditions().size());
		assertEquals("{el:bean.flag}", copy.getConditions().get(1).getFromComponent());
		assertEquals("locked", copy.getConditions().get(1).getMappedTo());
		// the mappings are copies, not the source's own
		assertNotSame(source.getActions().get(0), copy.getActions().get(0));
		assertNotSame(source.getConditions().get(0), copy.getConditions().get(0));
	}

	@Test
	void componentFromCopiesAllFields() {
		Component source = new Component();
		source.setModuleName("admin");
		source.setDocumentName("User");
		source.setName("cmpUser");
		source.setWidgetId("w99");
		source.setXmlInvisibleCondition("noShow");

		FluentComponent copy = new FluentComponent().from(source);

		assertEquals("admin", copy.get().getModuleName());
		assertEquals("User", copy.get().getDocumentName());
		assertEquals("cmpUser", copy.get().getName());
		assertEquals("w99", copy.get().getWidgetId());
		assertEquals("noShow", copy.get().getInvisibleCondition());
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
	void progressBarInvisibleConditionSetsValue() {
		FluentProgressBar fpb = new FluentProgressBar().invisibleCondition("hide");
		assertEquals("hide", fpb.get().getInvisibleCondition());
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
	void progressBarFromCopiesInvisibleCondition() {
		ProgressBar source = new ProgressBar();
		source.setXmlInvisibleCondition("gone");

		FluentProgressBar copy = new FluentProgressBar().from(source);
		assertEquals("gone", copy.get().getInvisibleCondition());
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
