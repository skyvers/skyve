package modules.test;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.util.Binder;
import org.skyve.wildcat.metadata.view.ActionImpl;
import org.skyve.wildcat.metadata.view.container.HBox;
import org.skyve.wildcat.metadata.view.container.Tab;
import org.skyve.wildcat.metadata.view.container.TabPane;
import org.skyve.wildcat.metadata.view.container.VBox;
import org.skyve.wildcat.metadata.view.container.form.Form;
import org.skyve.wildcat.metadata.view.event.SetDisabledEventAction;
import org.skyve.wildcat.metadata.view.event.SetInvisibleEventAction;
import org.skyve.wildcat.metadata.view.widget.Blurb;
import org.skyve.wildcat.metadata.view.widget.DialogButton;
import org.skyve.wildcat.metadata.view.widget.DynamicImage;
import org.skyve.wildcat.metadata.view.widget.GeoLocator;
import org.skyve.wildcat.metadata.view.widget.Link;
import org.skyve.wildcat.metadata.view.widget.MapDisplay;
import org.skyve.wildcat.metadata.view.widget.StaticImage;
import org.skyve.wildcat.metadata.view.widget.bound.Label;
import org.skyve.wildcat.metadata.view.widget.bound.ProgressBar;
import org.skyve.wildcat.metadata.view.widget.bound.input.Lookup;
import org.skyve.wildcat.metadata.view.widget.bound.input.TextField;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.DisableableCRUDGrid;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.ListGrid;

public class EnabledVisibleTest {
	private static final String CONDITION = "test";
	private static final String NEGATED_CONDITION = "notTest";
	private static final String TRUE = Boolean.TRUE.toString();
	private static final String FALSE = Boolean.FALSE.toString();

	@Test
	@SuppressWarnings("static-method")
	public void testBlurb() throws Exception {
		testVisible(new Blurb());
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testDialogButton() throws Exception {
		DialogButton button = new DialogButton();
		testVisible(button);
		testEnabled(button);
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testDynamicImage() throws Exception {
		testVisible(new DynamicImage());
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testForm() throws Exception {
		Form form = new Form();
		testVisible(form);
		testEnabled(form);
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testGeoLocator() throws Exception {
		GeoLocator loc = new GeoLocator();
		testVisible(loc);
		testEnabled(loc);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testHBox() throws Exception {
		testVisible(new HBox());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testInputWidget() throws Exception {
		TextField text = new TextField();
		testVisible(text);
		testEnabled(text);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testLookup() throws Exception {
		Lookup lookup = new Lookup();
		testVisible(lookup);
		testEnabled(lookup);
		testCondition(lookup, "disableEditConditionName", "enableEditConditionName");
		testCondition(lookup, "disableAddConditionName", "enableAddConditionName");
		testCondition(lookup, "disableClearConditionName", "enableClearConditionName");
		testCondition(lookup, "disablePickConditionName", "enablePickConditionName");
	}

	@Test
	@SuppressWarnings("static-method")
	public void testLabel() throws Exception {
		testVisible(new Label());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testLink() throws Exception {
		testVisible(new Link());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testListGrid() throws Exception {
		ListGrid list = new ListGrid();
		testVisible(list);
		testEnabled(list);
		testDisableableCRUDGrid(list);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testMapDisplay() throws Exception {
		testVisible(new MapDisplay());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testProgressBar() throws Exception {
		testVisible(new ProgressBar());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSetInvisibleEventAction() throws Exception {
		testVisible(new SetInvisibleEventAction());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSetDisabledEventAction() throws Exception {
		testEnabled(new SetDisabledEventAction());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testStaticImage() throws Exception {
		testVisible(new StaticImage());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testTab() throws Exception {
		Tab tab = new Tab();
		testVisible(tab);
		testEnabled(tab);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testTabPane() throws Exception {
		TabPane pane = new TabPane();
		testVisible(pane);
		testEnabled(pane);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testDataGrid() throws Exception {
		DataGrid grid = new DataGrid();
		testVisible(grid);
		testEnabled(grid);
		testDisableableCRUDGrid(grid);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testVBox() throws Exception {
		testVisible(new VBox());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testActionImpl() throws Exception {
		ActionImpl action = new ActionImpl();
		testVisible(action);
		testEnabled(action);
	}
	
	private static void testVisible(Invisible metaData)
	throws Exception {
		testCondition(metaData, "invisibleConditionName", "visibleConditionName");
	}

	private static void testEnabled(Disableable metaData)
	throws Exception {
		testCondition(metaData, "disabledConditionName", "enabledConditionName");
	}
	
	private static void testDisableableCRUDGrid(DisableableCRUDGrid metaData)
	throws Exception {
		testCondition(metaData, "disableAddConditionName", "enableAddConditionName");
		testCondition(metaData, "disableZoomConditionName", "enableZoomConditionName");
		testCondition(metaData, "disableEditConditionName", "enableEditConditionName");
		testCondition(metaData, "disableRemoveConditionName", "enableRemoveConditionName");
	}
	
	private static void testCondition(Object metaData,
										String conditionName, 
										String negatedConditionName)
	throws Exception {
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
