package org.skyve.impl.sail.execution;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;

@SuppressWarnings("static-method")
class TestDataEnterViewVisitorTest {
	private static final class ConditionAwareBean extends DynamicBean {
		private static final long serialVersionUID = 1L;
		private final boolean conditionResult;

		private ConditionAwareBean(String moduleName, String documentName, Map<String, Object> properties, boolean conditionResult) {
			super(moduleName, documentName, new HashMap<>(properties));
			this.conditionResult = conditionResult;
		}

		@Override
		public boolean evaluateCondition(String conditionName) {
			return conditionResult;
		}
	}

	@Test
	void visitTabAddsSelectionStepWhenVisibleAndEnabled() {
		TestDataEnterViewVisitor visitor = newVisitor(newBean(Map.of()));
		Tab tab = new Tab();
		tab.setTitle("Details");

		visitor.visitTab(tab, true, true);

		Step step = visitor.getScalarSteps().get(0);
		TabSelect select = assertInstanceOf(TabSelect.class, step);
		assertEquals("Details", select.getTabPath());
	}

	@Test
	void visitCheckBoxAddsBooleanDataEnterStep() {
		TestDataEnterViewVisitor visitor = newVisitor(newBean(Map.of("active", Boolean.TRUE)));
		CheckBox checkBox = new CheckBox();
		checkBox.setBinding("active");

		visitor.visitCheckBox(checkBox, true, true);

		Step step = visitor.getScalarSteps().get(0);
		DataEnter enter = assertInstanceOf(DataEnter.class, step);
		assertEquals("active", enter.getBinding());
		assertEquals("true", enter.getValue());
	}

	@Test
	void visitCheckBoxSkipsNullValuesAndHiddenOrDisabledParents() {
		Map<String, Object> properties = new HashMap<>();
		properties.put("active", null);
		TestDataEnterViewVisitor visitor = newVisitor(newBean(properties));
		CheckBox checkBox = new CheckBox();
		checkBox.setBinding("active");

		visitor.visitCheckBox(checkBox, true, true);
		visitor.visitCheckBox(checkBox, false, true);
		visitor.visitCheckBox(checkBox, true, false);

		assertTrue(visitor.getScalarSteps().isEmpty());
	}

	@Test
	void visitCheckBoxHonoursInvisibleAndDisabledConditions() {
		TestDataEnterViewVisitor visitor = newVisitor(new ConditionAwareBean("testModule",
																				"testDocument",
																				Map.of("active", Boolean.TRUE),
																				true));
		CheckBox checkBox = new CheckBox();
		checkBox.setBinding("active");
		checkBox.setInvisibleConditionName("hideActive");
		checkBox.setDisabledConditionName("disableActive");

		visitor.visitCheckBox(checkBox, true, true);

		assertTrue(visitor.getScalarSteps().isEmpty());
	}

	@Test
	void dataGridScopeSuppressesScalarStepsUntilExited() {
		TestDataEnterViewVisitor visitor = newVisitor(newBean(Map.of("active", Boolean.FALSE)));
		CheckBox checkBox = new CheckBox();
		checkBox.setBinding("active");
		DataGrid grid = new DataGrid();

		visitor.visitDataGrid(grid, true, true);
		visitor.visitCheckBox(checkBox, true, true);
		visitor.visitedDataGrid(grid, true, true);
		visitor.visitCheckBox(checkBox, true, true);

		assertEquals(1, visitor.getScalarSteps().size());
		DataEnter enter = assertInstanceOf(DataEnter.class, visitor.getScalarSteps().get(0));
		assertEquals("false", enter.getValue());
	}

	private static DynamicBean newBean(Map<String, Object> properties) {
		return new DynamicBean("testModule", "testDocument", new HashMap<>(properties));
	}

	private static TestDataEnterViewVisitor newVisitor(DynamicBean bean) {
		CustomerImpl customer = new CustomerImpl();
		ModuleImpl module = new ModuleImpl();
		module.setName("testModule");
		DocumentImpl document = new DocumentImpl();
		document.setName("testDocument");
		document.setOwningModuleName("testModule");
		ViewImpl view = new ViewImpl();

		return new TestDataEnterViewVisitor(customer, module, document, view, "desktop", bean);
	}
}
