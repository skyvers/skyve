package org.skyve.impl.metadata.view;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.impl.metadata.repository.view.actions.SaveAction;
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
import org.skyve.impl.metadata.view.widget.DynamicImage;
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
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;

/**
 * Every setter of a view condition normalises what it is given with {@code UtilImpl.processStringValue()},
 * so a condition is trimmed and a blank one is no condition at all, however the metadata was made.
 */
@SuppressWarnings("static-method")
class ConditionSetterNormalisationTest {
	/**
	 * A concrete class for everything that declares or inherits a condition setter.
	 * {@code ViewImpl} is not here: its one condition is only ever set from {@code ViewMetaData}, which has normalised it.
	 */
	private static final Class<?>[] CLASSES = {
		ViewMetaData.class, ActionImpl.class, SaveAction.class,
		VBox.class, HBox.class, Sidebar.class, Form.class, Tab.class, TabPane.class, Component.class,
		Spacer.class, Blurb.class, Label.class, Link.class, StaticImage.class, DynamicImage.class, Chart.class, MapDisplay.class,
		DialogButton.class, ZoomIn.class, ProgressBar.class,
		TextField.class, LookupDescription.class,
		DataGrid.class, DataRepeater.class, ListGrid.class, ListRepeater.class, TreeGrid.class,
		SetDisabledEventAction.class, SetInvisibleEventAction.class
	};

	private static List<Method> conditionSetters(Class<?> type) {
		List<Method> result = new ArrayList<>();
		for (Method method : type.getMethods()) {
			String name = method.getName();
			if (name.startsWith("set") && name.endsWith("Condition") &&
					(method.getParameterCount() == 1) && String.class.equals(method.getParameterTypes()[0])) {
				result.add(method);
			}
		}
		return result;
	}

	@TestFactory
	Stream<DynamicTest> everyConditionSetterTrimsAndTreatsBlankAsNoCondition() {
		List<DynamicTest> tests = new ArrayList<>();
		for (Class<?> type : CLASSES) {
			for (Method setter : conditionSetters(type)) {
				tests.add(dynamicTest(type.getSimpleName() + '.' + setter.getName(), () -> {
					Object metadata = type.getDeclaredConstructor().newInstance();
					Method getter = type.getMethod("get" + setter.getName().substring(3));
					for (String condition : new String[] {"name", "  name  ", "\tname\n", " {el:bean.flag} "}) {
						setter.invoke(metadata, condition);
						assertEquals(condition.trim(), getter.invoke(metadata), '[' + condition + ']');
					}
					for (String blank : new String[] {"", " ", " \t\n", null}) {
						setter.invoke(metadata, "name");
						setter.invoke(metadata, blank);
						assertNull(getter.invoke(metadata), '[' + blank + ']');
					}
				}));
			}
		}
		return tests.stream();
	}

	@Test
	void theRuntimeViewKeepsItsRefreshConditionAsItWasConverted() {
		ViewImpl view = new ViewImpl();
		view.setRefreshIfCondition(" asGiven ");
		assertEquals(" asGiven ", view.getRefreshIfCondition());

		// ... because the XML-facing metadata it is converted from has normalised it
		ViewMetaData metaData = new ViewMetaData();
		metaData.setName("edit");
		metaData.setTitle("Test");
		metaData.setRefreshTimeInSeconds(Integer.valueOf(10));
		metaData.setRefreshIfCondition("  canRefresh\t");
		assertEquals("canRefresh", metaData.convert("test").getRefreshIfCondition());
	}

	@Test
	void everyClassHasConditionSetters() {
		int setters = 0;
		for (Class<?> type : CLASSES) {
			int count = conditionSetters(type).size();
			assertTrue(count > 0, type.getName());
			setters += count;
		}
		// 2 for each of invisible and disabled on most classes, 8 more on the grids and the lookup
		assertTrue(setters > 100, String.valueOf(setters));
	}
}
