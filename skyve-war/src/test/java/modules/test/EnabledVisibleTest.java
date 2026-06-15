package modules.test;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DisableableCRUDGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.util.Binder;

public class EnabledVisibleTest {
	private static final String CONDITION = "test";
	private static final String NEGATED_CONDITION = "notTest";
	private static final String TRUE = Boolean.TRUE.toString();
	private static final String FALSE = Boolean.FALSE.toString();

	@Test
	@SuppressWarnings("static-method")
	public void testBlurb() {
		testVisible(new Blurb());
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testDialogButton() {
		DialogButton button = new DialogButton();
		testVisible(button);
		testEnabled(button);
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testDynamicImage() {
		testVisible(new DynamicImage());
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testForm() {
		Form form = new Form();
		testVisible(form);
		testEnabled(form);
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testHBox() {
		testVisible(new HBox());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testInputWidget() {
		TextField text = new TextField();
		testVisible(text);
		testEnabled(text);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testLookupDescription() {
		LookupDescription lookup = new LookupDescription();
		testVisible(lookup);
		testEnabled(lookup);
		testCondition(lookup, "disableEditConditionName", "enableEditConditionName");
		testCondition(lookup, "disableAddConditionName", "enableAddConditionName");
		testCondition(lookup, "disableClearConditionName", "enableClearConditionName");
		testCondition(lookup, "disablePickConditionName", "enablePickConditionName");
	}

	@Test
	@SuppressWarnings("static-method")
	public void testLabel() {
		testVisible(new Label());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testLink() {
		testVisible(new Link());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testListGrid() {
		ListGrid list = new ListGrid();
		testVisible(list);
		testEnabled(list);
		testDisableableCRUDGrid(list);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testMapDisplay() {
		testVisible(new MapDisplay());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testProgressBar() {
		testVisible(new ProgressBar());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSetInvisibleEventAction() {
		testVisible(new SetInvisibleEventAction());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSetDisabledEventAction() {
		testEnabled(new SetDisabledEventAction());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testStaticImage() {
		testVisible(new StaticImage());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testTab() {
		Tab tab = new Tab();
		testVisible(tab);
		testEnabled(tab);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testTabPane() {
		TabPane pane = new TabPane();
		testVisible(pane);
		testEnabled(pane);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testDataGrid() {
		DataGrid grid = new DataGrid();
		testVisible(grid);
		testEnabled(grid);
		testDisableableCRUDGrid(grid);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testVBox() {
		testVisible(new VBox());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testActionImpl() {
		ActionImpl action = new ActionImpl();
		testVisible(action);
		testEnabled(action);
	}
	
	private static void testVisible(Invisible metaData) {
		testCondition(metaData, "invisibleConditionName", "visibleConditionName");
	}

	private static void testEnabled(Disableable metaData) {
		testCondition(metaData, "disabledConditionName", "enabledConditionName");
	}
	
	private static void testDisableableCRUDGrid(DisableableCRUDGrid metaData) {
		testCondition(metaData, "disableAddConditionName", "enableAddConditionName");
		testCondition(metaData, "disableZoomConditionName", "enableZoomConditionName");
		testCondition(metaData, "disableEditConditionName", "enableEditConditionName");
		testCondition(metaData, "disableRemoveConditionName", "enableRemoveConditionName");
	}
	
	private static void testCondition(Object metaData,
										String conditionName, 
										String negatedConditionName) {
		Binder.set(metaData, conditionName, CONDITION);
		Assert.assertEquals(CONDITION, Binder.get(metaData, conditionName));

		Binder.set(metaData, negatedConditionName, CONDITION);
		Assert.assertEquals(NEGATED_CONDITION, Binder.get(metaData, conditionName));
		Binder.set(metaData, negatedConditionName, NEGATED_CONDITION);
		Assert.assertEquals(CONDITION, Binder.get(metaData, conditionName));
		Binder.set(metaData, negatedConditionName, TRUE);
		Assert.assertEquals(FALSE, Binder.get(metaData, conditionName));
		Binder.set(metaData, negatedConditionName, FALSE);
		Assert.assertEquals(TRUE, Binder.get(metaData, conditionName));
	}
}
