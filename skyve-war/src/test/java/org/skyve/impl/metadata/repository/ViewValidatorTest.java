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
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.reference.ExternalReference;
import org.skyve.impl.metadata.view.reference.ImplicitActionReference;
import org.skyve.impl.metadata.view.reference.EditViewReference;
import org.skyve.impl.metadata.view.reference.QueryListViewReference;
import org.skyve.metadata.controller.ImplicitActionName;
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
	void validateViewWithCheckMembershipHavingInvalidBindingThrowsException() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		CheckMembership cm = new CheckMembership();
		cm.setBinding("text");
		item.setWidget(cm);
		row.getItems().add(item);
		view.getContained().add(form);
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithListMembershipHavingInvalidBindingThrowsException() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		ListMembership lm = new ListMembership();
		lm.setBinding("text");
		item.setWidget(lm);
		row.getItems().add(item);
		view.getContained().add(form);
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithDataRepeater() {
		ViewImpl view = editView();
		DataRepeater repeater = new DataRepeater();
		repeater.setBinding("aggregatedCollection");
		view.getContained().add(repeater);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithDataRepeaterHavingInvalidBindingThrowsException() {
		ViewImpl view = editView();
		DataRepeater repeater = new DataRepeater();
		repeater.setBinding("nonExistentBinding");
		view.getContained().add(repeater);
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithDataGridContainerColumn() {
		ViewImpl view = editView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridContainerColumn col = new DataGridContainerColumn();
		grid.getColumns().add(col);
		view.getContained().add(grid);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithDialogButton() {
		ViewImpl view = editView();
		DialogButton btn = new DialogButton();
		btn.setDialogName("someDialog");
		view.getContained().add(btn);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithTreeGrid() {
		ViewImpl view = editView();
		TreeGrid tg = new TreeGrid();
		tg.setQueryName("qAssociations");
		view.getContained().add(tg);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithChartHavingNoModelThrowsException() {
		ViewImpl view = editView();
		Chart chart = new Chart();
		view.getContained().add(chart);
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithLinkHavingExternalReference() {
		ViewImpl view = editView();
		Link link = new Link();
		link.setValue("Google");
		ExternalReference ref = new ExternalReference();
		ref.setHref("https://example.com");
		link.setReference(ref);
		view.getContained().add(link);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithLinkHavingImplicitActionReference() {
		ViewImpl view = editView();
		Link link = new Link();
		link.setValue("OK");
		ImplicitActionReference ref = new ImplicitActionReference();
		ref.setImplicitActionName(ImplicitActionName.OK);
		link.setReference(ref);
		view.getContained().add(link);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithLinkHavingEditViewReferenceToValidDocument() {
		ViewImpl view = editView();
		Link link = new Link();
		link.setValue("Admin");
		EditViewReference ref = new EditViewReference();
		ref.setModuleName("test");
		ref.setDocumentName("AllAttributesPersistent");
		link.setReference(ref);
		view.getContained().add(link);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithLinkHavingEditViewReferenceToInvalidDocumentThrowsException() {
		ViewImpl view = editView();
		Link link = new Link();
		link.setValue("Bad");
		EditViewReference ref = new EditViewReference();
		ref.setModuleName("test");
		ref.setDocumentName("NonExistentDocument");
		link.setReference(ref);
		view.getContained().add(link);
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithLinkHavingQueryListViewReferenceToValidQuery() {
		ViewImpl view = editView();
		Link link = new Link();
		link.setValue("Query");
		QueryListViewReference ref = new QueryListViewReference();
		ref.setQueryName("qAssociations");
		link.setReference(ref);
		view.getContained().add(link);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithLinkHavingQueryListViewReferenceToInvalidQueryThrowsException() {
		ViewImpl view = editView();
		Link link = new Link();
		link.setValue("Bad");
		QueryListViewReference ref = new QueryListViewReference();
		ref.setQueryName("nonExistentQuery");
		link.setReference(ref);
		view.getContained().add(link);
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithDynamicImage() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		org.skyve.impl.metadata.view.widget.DynamicImage dynImage = new org.skyve.impl.metadata.view.widget.DynamicImage();
		dynImage.setName("testImage");
		item.setWidget(dynImage);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithContentImageHavingMissingBindingThrowsException() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		org.skyve.impl.metadata.view.widget.bound.input.ContentImage ci = new org.skyve.impl.metadata.view.widget.bound.input.ContentImage();
		// binding is required for ContentImage
		item.setWidget(ci);
		row.getItems().add(item);
		view.getContained().add(form);
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithContentSignatureHavingMissingBindingThrowsException() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		org.skyve.impl.metadata.view.widget.bound.input.ContentSignature cs = new org.skyve.impl.metadata.view.widget.bound.input.ContentSignature();
		// binding is required for ContentSignature
		item.setWidget(cs);
		row.getItems().add(item);
		view.getContained().add(form);
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithContentSignatureHavingInvalidBackgroundColourThrowsException() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		org.skyve.impl.metadata.view.widget.bound.input.ContentSignature cs = new org.skyve.impl.metadata.view.widget.bound.input.ContentSignature();
		cs.setRgbHexBackgroundColour("BADCOLOUR");
		item.setWidget(cs);
		row.getItems().add(item);
		view.getContained().add(form);
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithContentSignatureHavingInvalidForegroundColourThrowsException() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		org.skyve.impl.metadata.view.widget.bound.input.ContentSignature cs = new org.skyve.impl.metadata.view.widget.bound.input.ContentSignature();
		cs.setRgbHexForegroundColour("BADCOLOUR");
		item.setWidget(cs);
		row.getItems().add(item);
		view.getContained().add(form);
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithDataGridBoundColumn() {
		ViewImpl view = editView();
		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("text");
		grid.getColumns().add(col);
		view.getContained().add(grid);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithMap() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.view.widget.MapDisplay map = new org.skyve.impl.metadata.view.widget.MapDisplay();
		// no model name - null model is allowed
		view.getContained().add(map);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithSidebarHavingPixelWidth() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.view.container.Sidebar sidebar = new org.skyve.impl.metadata.view.container.Sidebar();
		sidebar.setPixelWidth(Integer.valueOf(200));
		view.setSidebar(sidebar);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithSidebarHavingNoWidthThrowsException() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.view.container.Sidebar sidebar = new org.skyve.impl.metadata.view.container.Sidebar();
		view.setSidebar(sidebar);
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithLookupDescription() {
		ViewImpl view = editView();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		org.skyve.impl.metadata.view.widget.bound.input.LookupDescription lookup = new org.skyve.impl.metadata.view.widget.bound.input.LookupDescription();
		lookup.setBinding("aggregatedAssociation");
		lookup.setDescriptionBinding(org.skyve.domain.Bean.BIZ_KEY);
		item.setWidget(lookup);
		row.getItems().add(item);
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithSaveAction() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.repository.view.actions.SaveAction action = new org.skyve.impl.metadata.repository.view.actions.SaveAction();
		action.setName("MySave");
		view.putAction(action.toMetaDataAction());
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithRemoveAction() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.repository.view.actions.RemoveAction action = new org.skyve.impl.metadata.repository.view.actions.RemoveAction();
		action.setName("MyRemove");
		view.putAction(action.toMetaDataAction());
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithZoomOutAction() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.repository.view.actions.ZoomOutAction action = new org.skyve.impl.metadata.repository.view.actions.ZoomOutAction();
		action.setName("MyZoomOut");
		view.putAction(action.toMetaDataAction());
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithAddAction() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.repository.view.actions.AddAction action = new org.skyve.impl.metadata.repository.view.actions.AddAction();
		action.setName("MyAdd");
		view.putAction(action.toMetaDataAction());
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithCancelAction() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.repository.view.actions.CancelAction action = new org.skyve.impl.metadata.repository.view.actions.CancelAction();
		action.setName("MyCancel");
		view.putAction(action.toMetaDataAction());
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithDeleteAction() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.repository.view.actions.DeleteAction action = new org.skyve.impl.metadata.repository.view.actions.DeleteAction();
		action.setName("MyDelete");
		view.putAction(action.toMetaDataAction());
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithOKAction() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.repository.view.actions.OKAction action = new org.skyve.impl.metadata.repository.view.actions.OKAction();
		action.setName("MyOK");
		view.putAction(action.toMetaDataAction());
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithNewAction() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.repository.view.actions.NewAction action = new org.skyve.impl.metadata.repository.view.actions.NewAction();
		action.setName("MyNew");
		view.putAction(action.toMetaDataAction());
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithNavigateAction() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.view.ActionImpl action = new org.skyve.impl.metadata.view.ActionImpl();
		action.setImplicitName(org.skyve.metadata.controller.ImplicitActionName.Navigate);
		action.setName("MyNavigate");
		view.putAction(action);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithBizExportActionHavingInvalidClassThrowsException() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.repository.view.actions.BizExportAction action = new org.skyve.impl.metadata.repository.view.actions.BizExportAction();
		action.setClassName("NonExistentClass");
		view.putAction(action.toMetaDataAction());
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithBizImportActionHavingInvalidClassThrowsException() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.repository.view.actions.BizImportAction action = new org.skyve.impl.metadata.repository.view.actions.BizImportAction();
		action.setClassName("NonExistentClass");
		view.putAction(action.toMetaDataAction());
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithDownloadActionHavingInvalidClassThrowsException() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.repository.view.actions.DownloadAction action = new org.skyve.impl.metadata.repository.view.actions.DownloadAction();
		action.setClassName("NonExistentClass");
		view.putAction(action.toMetaDataAction());
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithUploadActionHavingInvalidClassThrowsException() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.repository.view.actions.UploadAction action = new org.skyve.impl.metadata.repository.view.actions.UploadAction();
		action.setClassName("NonExistentClass");
		view.putAction(action.toMetaDataAction());
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithCustomActionHavingInvalidClassThrowsException() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.repository.view.actions.CustomAction action = new org.skyve.impl.metadata.repository.view.actions.CustomAction();
		action.setClassName("NonExistentClass");
		view.putAction(action.toMetaDataAction());
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithActionHavingIconStyleClassShowIconDoesNotThrow() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.repository.view.actions.SaveAction action = new org.skyve.impl.metadata.repository.view.actions.SaveAction();
		action.setName("MySave");
		action.setShow(org.skyve.metadata.view.Action.ActionShow.icon);
		action.setIconStyleClass("fa fa-save");
		view.putAction(action.toMetaDataAction());
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithActionHavingIconShowButNoIconThrowsException() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.repository.view.actions.SaveAction action = new org.skyve.impl.metadata.repository.view.actions.SaveAction();
		action.setName("MySave");
		action.setShow(org.skyve.metadata.view.Action.ActionShow.icon);
		// No icon class or file name → exception
		view.putAction(action.toMetaDataAction());
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithFormContainingColumns() {
		ViewImpl view = editView();
		Form form = formWith("text");
		view.getContained().add(form);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithFormHavingInvalidPixelWidthThrowsException() {
		ViewImpl view = editView();
		FormColumn col = new FormColumn();
		col.setPixelWidth(-10);
		Form form = new Form();
		form.getColumns().add(col);
		FormRow row = new FormRow();
		form.getRows().add(row);
		view.getContained().add(form);
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithTabContainingTitle() {
		ViewImpl view = editView();
		TabPane tabPane = new TabPane();
		Tab tab = new Tab();
		tab.setTitle("My Tab");
		tabPane.getTabs().add(tab);
		view.getContained().add(tabPane);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithInjectWidget() {
		ViewImpl view = editView();
		org.skyve.impl.metadata.view.Inject inject = new org.skyve.impl.metadata.view.Inject();
		view.getContained().add(inject);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithHBoxHavingPixelWidth() {
		ViewImpl view = editView();
		HBox hbox = new HBox();
		hbox.setPixelWidth(200);
		TextField tf = new TextField();
		tf.setBinding("text");
		hbox.getContained().add(tf);
		view.getContained().add(hbox);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithVBoxHavingPixelWidth() {
		ViewImpl view = editView();
		VBox vbox = new VBox();
		vbox.setPixelWidth(300);
		TextField tf = new TextField();
		tf.setBinding("text");
		vbox.getContained().add(tf);
		view.getContained().add(vbox);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

	@Test
	void validateViewWithHBoxHavingInvalidPixelWidthThrowsException() {
		ViewImpl view = editView();
		HBox hbox = new HBox();
		hbox.setPixelWidth(-1);
		view.getContained().add(hbox);
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithHBoxHavingInvalidResponsiveWidthThrowsException() {
		ViewImpl view = editView();
		HBox hbox = new HBox();
		hbox.setResponsiveWidth(15); // > 12
		view.getContained().add(hbox);
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithHBoxHavingZeroResponsiveWidthThrowsException() {
		ViewImpl view = editView();
		HBox hbox = new HBox();
		hbox.setResponsiveWidth(0); // <= 0
		view.getContained().add(hbox);
		assertThrows(MetaDataException.class, () -> newValidator(view).visit());
	}

	@Test
	void validateViewWithHBoxHavingValidResponsiveWidth() {
		ViewImpl view = editView();
		HBox hbox = new HBox();
		hbox.setResponsiveWidth(6); // valid: 1-12
		view.getContained().add(hbox);
		assertDoesNotThrow(() -> newValidator(view).visit());
	}

}

