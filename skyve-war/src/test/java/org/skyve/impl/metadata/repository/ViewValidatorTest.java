package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.repository.view.actions.SaveAction;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.View.ViewType;

import modules.test.AbstractSkyveTest;

class ViewValidatorTest extends AbstractSkyveTest {

	private static final String UXUI = "external";

	private ViewValidator newValidator(ViewImpl view) {
		ProvidedRepository repo = (ProvidedRepository) CORE.getRepository();
		return new ViewValidator(view, repo, (CustomerImpl) c, (DocumentImpl) aapd, UXUI);
	}

	private static ViewImpl editView() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");
		return view;
	}

	private static Form formWith(String... bindings) {
		Form form = new Form();
		for (String binding : bindings) {
			form.getColumns().add(new FormColumn());
			FormRow row = new FormRow();
			form.getRows().add(row);
			FormItem item = new FormItem();
			TextField tf = new TextField();
			tf.setBinding(binding);
			item.setWidget(tf);
			row.getItems().add(item);
		}
		return form;
	}

	@Test
	void validateViewWithTextField() {
		ViewImpl view = editView();
		view.getContained().add(formWith("text"));
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithTextArea() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextArea ta = new TextArea();
		ta.setBinding("memo");
		item.setWidget(ta);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithCheckBox() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		CheckBox cb = new CheckBox();
		cb.setBinding("booleanFlag");
		item.setWidget(cb);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithCombo() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Combo combo = new Combo();
		combo.setBinding("enum3");
		item.setWidget(combo);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithRadio() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Radio radio = new Radio();
		radio.setBinding("enum3");
		item.setWidget(radio);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithColourPicker() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		ColourPicker cp = new ColourPicker();
		cp.setBinding("colour");
		item.setWidget(cp);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithHTML() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		HTML html = new HTML();
		html.setBinding("markup");
		item.setWidget(html);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithPassword() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Password pw = new Password();
		pw.setBinding("text");
		item.setWidget(pw);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithRichText() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		RichText rt = new RichText();
		rt.setBinding("markup");
		item.setWidget(rt);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithSlider() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Slider slider = new Slider();
		slider.setBinding("decimal2");
		item.setWidget(slider);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithSpinner() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Spinner spinner = new Spinner();
		spinner.setBinding("normalInteger");
		item.setWidget(spinner);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithZoomIn() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		ZoomIn zi = new ZoomIn();
		zi.setBinding("aggregatedAssociation");
		item.setWidget(zi);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithLabel() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Label label = new Label();
		label.setBinding("text");
		item.setWidget(label);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithProgressBar() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		ProgressBar pb = new ProgressBar();
		pb.setBinding("decimal2");
		item.setWidget(pb);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithGeometry() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Geometry geo = new Geometry();
		geo.setBinding("geometry");
		item.setWidget(geo);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithGeometryMap() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		GeometryMap gm = new GeometryMap();
		gm.setBinding("geometry");
		item.setWidget(gm);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithContentLink() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		ContentLink cl = new ContentLink();
		item.setWidget(cl);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithBlurb() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Blurb blurb = new Blurb();
		blurb.setMarkup("Hello World");
		item.setWidget(blurb);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithSpacer() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Spacer spacer = new Spacer();
		spacer.setPixelWidth(Integer.valueOf(10));
		item.setWidget(spacer);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithButton() {
		ViewImpl view = editView();

		SaveAction saveAction = new SaveAction();
		saveAction.setName("MyAction");
		view.putAction(saveAction.toMetaDataAction());

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Button button = new Button();
		button.setActionName("MyAction");
		item.setWidget(button);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithDataGrid() {
		ViewImpl view = editView();

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("text");
		col.setTitle("Text");
		grid.getColumns().add(col);
		view.getContained().add(grid);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithListGrid() {
		ViewImpl view = editView();

		ListGrid grid = new ListGrid();
		grid.setQueryName("qExpressionQuery");
		view.getContained().add(grid);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithListRepeater() {
		ViewImpl view = editView();

		ListRepeater repeater = new ListRepeater();
		repeater.setQueryName("qExpressionQuery");
		view.getContained().add(repeater);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithHBox() {
		ViewImpl view = editView();

		HBox hbox = new HBox();
		hbox.getContained().add(formWith("text"));
		view.getContained().add(hbox);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithVBox() {
		ViewImpl view = editView();

		VBox vbox = new VBox();
		vbox.getContained().add(formWith("text"));
		view.getContained().add(vbox);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithTabPane() {
		ViewImpl view = editView();

		TabPane tabPane = new TabPane();
		Tab tab = new Tab();
		tab.setTitle("Home");
		tab.getContained().add(formWith("text"));
		tabPane.getTabs().add(tab);
		view.getContained().add(tabPane);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithConditionOnWidget() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		tf.setDisabledConditionName("condition");
		tf.setInvisibleConditionName("notCondition");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithRerenderEventAction() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		RerenderEventAction rerender = new RerenderEventAction();
		tf.getChangedActions().add(rerender);
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithServerSideActionEventAction() {
		ViewImpl view = editView();

		SaveAction saveAction = new SaveAction();
		saveAction.setName("OnChangeAction");
		view.putAction(saveAction.toMetaDataAction());

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		ServerSideActionEventAction serverAction = new ServerSideActionEventAction();
		serverAction.setActionName("OnChangeAction");
		tf.getChangedActions().add(serverAction);
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithToggleDisabledEventAction() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		ToggleDisabledEventAction toggleDisabled = new ToggleDisabledEventAction();
		toggleDisabled.setBinding("booleanFlag");
		tf.getChangedActions().add(toggleDisabled);
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithToggleVisibilityEventAction() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		ToggleVisibilityEventAction toggleVisibility = new ToggleVisibilityEventAction();
		toggleVisibility.setBinding("booleanFlag");
		tf.getChangedActions().add(toggleVisibility);
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithSetDisabledEventAction() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		SetDisabledEventAction setDisabled = new SetDisabledEventAction();
		setDisabled.setBinding("booleanFlag");
		setDisabled.setDisabledConditionName("condition");
		tf.getChangedActions().add(setDisabled);
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithStaticImage() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		StaticImage image = new StaticImage();
		image.setRelativeFile("test.png");
		item.setWidget(image);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithTextFieldHavingInvalidBindingThrowsException() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("nonExistentBinding");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithTextFieldHavingInvalidConditionThrowsException() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		tf.setDisabledConditionName("nonExistentCondition");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}
}
