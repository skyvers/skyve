package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
import org.skyve.impl.metadata.repository.view.actions.CustomAction;
import org.skyve.impl.metadata.view.component.Component;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;

@SuppressWarnings({"static-method", "java:S112"})
class FluentInputWidgetTest {

	// ---- FluentCheckBox -----------------------------------------------------

	@Test
	void fluentCheckBoxDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentCheckBox().get());
	}

	@Test
	void fluentCheckBoxTriStateTrueSetsTrue() {
		assertEquals(Boolean.TRUE, new FluentCheckBox().triState(true).get().getTriState());
	}

	@Test
	void fluentCheckBoxTriStateFalseSetsFalse() {
		assertNotEquals(Boolean.TRUE, new FluentCheckBox().triState(false).get().getTriState());
	}

	@Test
	void fluentCheckBoxPixelWidthSetsValue() {
		assertEquals(40, new FluentCheckBox().pixelWidth(40).get().getPixelWidth().intValue());
	}

	@Test
	void fluentCheckBoxPixelHeightSetsValue() {
		assertEquals(40, new FluentCheckBox().pixelHeight(40).get().getPixelHeight().intValue());
	}

	// ---- from() keeps the authored sense of a condition pair ----------------

	@Test
	void fluentTextFieldFromKeepsAuthoredVisibleAndEnabledConditions() {
		TextField source = new TextField();
		source.setXmlVisibleCondition("shown");
		source.setXmlEnabledCondition("{el:bean.editable}");

		TextField copy = new FluentTextField().from(source).get();

		assertEquals("shown", copy.getXmlVisibleCondition());
		assertEquals("notShown", copy.getInvisibleCondition());
		assertEquals("{el:bean.editable}", copy.getXmlEnabledCondition());
		assertEquals("not{el:bean.editable}", copy.getDisabledCondition());
	}

	@Test
	void fluentTextFieldFromKeepsAuthoredInvisibleAndDisabledConditions() {
		TextField source = new TextField();
		source.setXmlInvisibleCondition("hidden");
		source.setXmlDisabledCondition("locked");

		TextField copy = new FluentTextField().from(source).get();

		assertEquals("hidden", copy.getInvisibleCondition());
		assertNull(copy.getXmlVisibleCondition());
		assertEquals("locked", copy.getDisabledCondition());
		assertNull(copy.getXmlEnabledCondition());
	}

	@Test
	void fluentDataGridFromKeepsAuthoredEnableConditions() {
		DataGrid source = new DataGrid();
		source.setXmlEnableAddCondition("canAdd");
		source.setXmlDisableRemoveCondition("noRemove");

		DataGrid copy = new FluentDataGrid().from(source).get();

		assertEquals("canAdd", copy.getXmlEnableAddCondition());
		assertEquals("notCanAdd", copy.getDisableAddCondition());
		assertEquals("noRemove", copy.getDisableRemoveCondition());
		assertNull(copy.getXmlEnableRemoveCondition());
	}

	/**
	 * Every fluent builder whose {@code from()} copies condition pairs, with a fresh source to copy from.
	 */
	private static Stream<Object[]> fluentBuildersWithConditionPairs() {
		return Stream.of(new Object[] {new FluentBlurb(), new Blurb()},
							new Object[] {new FluentChart(), new Chart()},
							new Object[] {new FluentComponent(), new Component()},
							new Object[] {new FluentCustomAction(), new CustomAction()},
							new Object[] {new FluentDataGrid(), new DataGrid()},
							new Object[] {new FluentDataRepeater(), new DataRepeater()},
							new Object[] {new FluentDialogButton(), new DialogButton()},
							new Object[] {new FluentForm(), new Form()},
							new Object[] {new FluentHBox(), new HBox()},
							new Object[] {new FluentLabel(), new Label()},
							new Object[] {new FluentLink(), new Link()},
							new Object[] {new FluentListGrid(), new ListGrid()},
							new Object[] {new FluentListRepeater(), new ListRepeater()},
							new Object[] {new FluentLookupDescription(), new LookupDescription()},
							new Object[] {new FluentMapDisplay(), new MapDisplay()},
							new Object[] {new FluentProgressBar(), new ProgressBar()},
							new Object[] {new FluentSetDisabledEventAction(), new SetDisabledEventAction()},
							new Object[] {new FluentSetInvisibleEventAction(), new SetInvisibleEventAction()},
							new Object[] {new FluentSidebar(), new Sidebar()},
							new Object[] {new FluentSpacer(), new Spacer()},
							new Object[] {new FluentStaticImage(), new StaticImage()},
							new Object[] {new FluentTab(), new Tab()},
							new Object[] {new FluentTabPane(), new TabPane()},
							new Object[] {new FluentTextField(), new TextField()},
							new Object[] {new FluentVBox(), new VBox()},
							new Object[] {new FluentZoomIn(), new ZoomIn()});
	}

	@TestFactory
	Stream<DynamicTest> fromKeepsEveryConditionAuthoredInThePositiveSense() {
		return fluentBuildersWithConditionPairs().map(pair -> dynamicTest(pair[0].getClass().getSimpleName(),
																			() -> assertFromKeepsEveryCondition(pair[0], pair[1], "setXml(Visible|Enabled|Enable[A-Z][a-z]+)Condition")));
	}

	@TestFactory
	Stream<DynamicTest> fromKeepsEveryConditionAuthoredInTheNegativeSense() {
		return fluentBuildersWithConditionPairs().map(pair -> dynamicTest(pair[0].getClass().getSimpleName(),
																			() -> assertFromKeepsEveryCondition(pair[0], pair[1], "setXml(Invisible|Disabled|Disable[A-Z][a-z]+)Condition")));
	}

	private static void assertFromKeepsEveryCondition(Object fluent, Object source, String setterPattern) throws Exception {
		List<String> properties = setEveryCondition(source, setterPattern);

		Object copy = copyThroughFrom(fluent, source);

		for (String property : properties) {
			assertEquals("{el:bean." + property + "}",
							copy.getClass().getMethod("get" + property).invoke(copy),
							fluent.getClass().getSimpleName() + " " + property);
		}
	}

	/**
	 * Sets a distinct inline condition through every public setter matching the pattern.
	 *
	 * @return the property names set, each with its {@code Condition} suffix
	 */
	@SuppressWarnings("java:S9142") // regex compilation cost is irrelevant in a test
	private static List<String> setEveryCondition(Object source, String setterPattern) throws Exception {
		List<String> result = new ArrayList<>();
		for (Method method : source.getClass().getMethods()) {
			String methodName = method.getName();
			if (methodName.matches(setterPattern)) {
				String property = methodName.substring(3);
				method.invoke(source, "{el:bean." + property + "}");
				result.add(property);
			}
		}
		assertFalse(result.isEmpty(), source.getClass() + " has no condition matching " + setterPattern);
		return result;
	}

	/**
	 * Copies the source into the fluent builder through the builder's own instance {@code from()} and returns the built metadata.
	 */
	private static Object copyThroughFrom(Object fluent, Object source) throws ReflectiveOperationException {
		for (Method method : fluent.getClass().getDeclaredMethods()) {
			if ("from".equals(method.getName()) &&
					(! Modifier.isStatic(method.getModifiers())) &&
					(! method.isBridge()) &&
					(method.getParameterCount() == 1) &&
					method.getParameterTypes()[0].equals(source.getClass())) {
				method.invoke(fluent, source);
				return fluent.getClass().getMethod("get").invoke(fluent);
			}
		}
		throw new IllegalStateException(fluent.getClass() + " has no from(" + source.getClass().getSimpleName() + ")");
	}

	// ---- FluentCombo --------------------------------------------------------

	@Test
	void fluentComboDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentCombo().get());
	}

	@Test
	void fluentComboPixelWidthSetsValue() {
		assertEquals(300, new FluentCombo().pixelWidth(300).get().getPixelWidth().intValue());
	}

	// ---- FluentDataGrid -----------------------------------------------------

	@Test
	void fluentDataGridDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentDataGrid().get());
	}

	@Test
	void fluentDataGridDisabledConditionSetsValue() {
		assertEquals("notReady", new FluentDataGrid().disabledCondition("notReady").get().getDisabledCondition());
	}

	@Test
	void fluentDataGridDisableAddConditionSetsValue() {
		assertEquals("noAdd", new FluentDataGrid().disableAddCondition("noAdd").get().getDisableAddCondition());
	}

	@Test
	void fluentDataGridShowAddTrueSetsTrue() {
		assertEquals(Boolean.TRUE, new FluentDataGrid().showAdd(true).get().getShowAdd());
	}

	@Test
	void fluentDataGridShowRemoveFalseSetsFalse() {
		assertNotEquals(Boolean.TRUE, new FluentDataGrid().showRemove(false).get().getShowRemove());
	}

	@Test
	void fluentDataGridSelectedIdBindingSetsValue() {
		assertEquals("{id}", new FluentDataGrid().selectedIdBinding("{id}").get().getSelectedIdBinding());
	}

	@Test
	void fluentDataGridInlineTrueSetsTrue() {
		assertEquals(Boolean.TRUE, new FluentDataGrid().inline(true).get().getInline());
	}

	@Test
	void fluentDataGridEditableTrueSetsTrue() {
		assertEquals(Boolean.TRUE, new FluentDataGrid().editable(true).get().getEditable());
	}

	@Test
	void fluentDataGridWordWrapTrueSetsTrue() {
		assertEquals(Boolean.TRUE, new FluentDataGrid().wordWrap(true).get().getWordWrap());
	}

	// ---- FluentListGrid -----------------------------------------------------

	@Test
	void fluentListGridDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentListGrid().get());
	}

	@Test
	void fluentListGridTitleSetsValue() {
		assertEquals("Users", new FluentListGrid().title("Users").get().getTitle());
	}

	@Test
	void fluentListGridQueryNameSetsValue() {
		assertEquals("qUsers", new FluentListGrid().queryName("qUsers").get().getQueryName());
	}

	@Test
	void fluentListGridModelNameSetsValue() {
		assertEquals("UserModel", new FluentListGrid().modelName("UserModel").get().getModelName());
	}

	@Test
	void fluentListGridShowAddTrueSetsTrue() {
		assertEquals(Boolean.TRUE, new FluentListGrid().showAdd(true).get().getShowAdd());
	}

	@Test
	void fluentListGridShowExportTrueSetsTrue() {
		assertEquals(Boolean.TRUE, new FluentListGrid().showExport(true).get().getShowExport());
	}

	@Test
	void fluentListGridDisabledConditionSetsValue() {
		assertEquals("locked", new FluentListGrid().disabledCondition("locked").get().getDisabledCondition());
	}

	// ---- FluentBlurb --------------------------------------------------------

	@Test
	void fluentBlurbDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentBlurb().get());
	}

	// ---- FluentHTML ---------------------------------------------------------

	@Test
	void fluentHTMLDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentHTML().get());
	}

	// ---- FluentSidebar -------------------------------------------------------

	@Test
	void fluentSidebarDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentSidebar().get());
	}
}
