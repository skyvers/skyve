package org.skyve.wildcat.tools.test.wail.generate;

import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.wildcat.metadata.customer.CustomerImpl;
import org.skyve.wildcat.metadata.model.document.DocumentImpl;
import org.skyve.wildcat.metadata.module.ModuleImpl;
import org.skyve.wildcat.metadata.view.Action;
import org.skyve.wildcat.metadata.view.Inject;
import org.skyve.wildcat.metadata.view.ViewImpl;
import org.skyve.wildcat.metadata.view.ViewVisitor;
import org.skyve.wildcat.metadata.view.container.HBox;
import org.skyve.wildcat.metadata.view.container.Tab;
import org.skyve.wildcat.metadata.view.container.TabPane;
import org.skyve.wildcat.metadata.view.container.VBox;
import org.skyve.wildcat.metadata.view.container.form.Form;
import org.skyve.wildcat.metadata.view.container.form.FormColumn;
import org.skyve.wildcat.metadata.view.container.form.FormItem;
import org.skyve.wildcat.metadata.view.container.form.FormRow;
import org.skyve.wildcat.metadata.view.event.Changeable;
import org.skyve.wildcat.metadata.view.event.Focusable;
import org.skyve.wildcat.metadata.view.event.RerenderEventAction;
import org.skyve.wildcat.metadata.view.event.ServerSideActionEventAction;
import org.skyve.wildcat.metadata.view.event.SetDisabledEventAction;
import org.skyve.wildcat.metadata.view.event.SetInvisibleEventAction;
import org.skyve.wildcat.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.wildcat.metadata.view.widget.Blurb;
import org.skyve.wildcat.metadata.view.widget.Button;
import org.skyve.wildcat.metadata.view.widget.DialogButton;
import org.skyve.wildcat.metadata.view.widget.DynamicImage;
import org.skyve.wildcat.metadata.view.widget.GeoLocator;
import org.skyve.wildcat.metadata.view.widget.Link;
import org.skyve.wildcat.metadata.view.widget.MapDisplay;
import org.skyve.wildcat.metadata.view.widget.Spacer;
import org.skyve.wildcat.metadata.view.widget.StaticImage;
import org.skyve.wildcat.metadata.view.widget.bound.Label;
import org.skyve.wildcat.metadata.view.widget.bound.ProgressBar;
import org.skyve.wildcat.metadata.view.widget.bound.input.CheckBox;
import org.skyve.wildcat.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.wildcat.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.wildcat.metadata.view.widget.bound.input.Combo;
import org.skyve.wildcat.metadata.view.widget.bound.input.Comparison;
import org.skyve.wildcat.metadata.view.widget.bound.input.ContentImage;
import org.skyve.wildcat.metadata.view.widget.bound.input.ContentLink;
import org.skyve.wildcat.metadata.view.widget.bound.input.Geometry;
import org.skyve.wildcat.metadata.view.widget.bound.input.HTML;
import org.skyve.wildcat.metadata.view.widget.bound.input.ListMembership;
import org.skyve.wildcat.metadata.view.widget.bound.input.Lookup;
import org.skyve.wildcat.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.wildcat.metadata.view.widget.bound.input.Password;
import org.skyve.wildcat.metadata.view.widget.bound.input.Radio;
import org.skyve.wildcat.metadata.view.widget.bound.input.RichText;
import org.skyve.wildcat.metadata.view.widget.bound.input.Slider;
import org.skyve.wildcat.metadata.view.widget.bound.input.Spinner;
import org.skyve.wildcat.metadata.view.widget.bound.input.TextArea;
import org.skyve.wildcat.metadata.view.widget.bound.input.TextField;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.PickList;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.PickListColumn;
import org.skyve.wildcat.tools.test.wail.language.TestCase;
import org.skyve.wildcat.tools.test.wail.language.TestSuite;
import org.skyve.wildcat.tools.test.wail.language.step.Call;

class GenerateViewVisitor extends ViewVisitor {
	private TestSuite testSuite;
	
	protected GenerateViewVisitor(CustomerImpl customer,
									ModuleImpl module,
									DocumentImpl document,
									ViewImpl view,
									TestSuite testSuite) {
		super(customer, module, document, view);
		this.testSuite = testSuite;
	}

	@Override
	public void visitView() throws MetaDataException {
		TestCase foo = new TestCase();
		foo.setIdentifier(module.getName() + '.' + document.getName());
		Call call = new Call();
		call.setIdentifier(foo.getIdentifier());
		foo.getSteps().add(call);
		testSuite.getCases().add(foo);
	}

	@Override
	public void visitedView() throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitTabPane(TabPane tabPane, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedTabPane(TabPane tabPane, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitTab(Tab tab, boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedTab(Tab tab, boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitVBox(VBox vbox, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedVBox(VBox vbox, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitHBox(HBox hbox, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedHBox(HBox hbox, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitForm(Form form, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedForm(Form form, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitFormColumn(FormColumn column, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitFormRow(FormRow row, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitFormItem(FormItem item, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedFormItem(FormItem item, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedFormRow(FormRow row, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitButton(Button button, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitGeoLocator(GeoLocator locator, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitGeometry(Geometry geometry, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitMap(MapDisplay map, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitDialogButton(DialogButton button, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitDynamicImage(DynamicImage image, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitSpacer(Spacer spacer) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitStaticImage(StaticImage image, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitLink(Link link, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitBlurb(Blurb blurb, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitLabel(Label label, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitProgressBar(ProgressBar progressBar,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitListGrid(ListGrid grid, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedListGrid(ListGrid grid, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitDataGrid(DataGrid grid, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedDataGrid(DataGrid grid, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitDataGridBoundColumn(DataGridBoundColumn column,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedDataGridBoundColumn(DataGridBoundColumn column,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitDataGridContainerColumn(DataGridContainerColumn column,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedDataGridContainerColumn(DataGridContainerColumn column,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitPickList(PickList list, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedPickList(PickList list, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitPickListColumn(PickListColumn column,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitCheckBox(CheckBox checkBox, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedCheckBox(CheckBox checkBox, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitCheckMembership(CheckMembership membership,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedCheckMembership(CheckMembership membership,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitColourPicker(ColourPicker colour, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedColourPicker(ColourPicker colour, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitCombo(Combo combo, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedCombo(Combo combo, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitContentImage(ContentImage image, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitContentLink(ContentLink link, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitHTML(HTML html, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitListMembership(ListMembership membership,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedListMembership(ListMembership membership,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitComparison(Comparison comparison, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitLookupDescription(LookupDescription lookup,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedLookupDescription(LookupDescription lookup,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitLookup(Lookup lookup, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedLookup(Lookup lookup, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitPassword(Password password, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedPassword(Password password, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitRadio(Radio radio, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedRadio(Radio radio, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitRichText(RichText richText, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedRichText(RichText richText, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitSlider(Slider slider, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedSlider(Slider slider, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitSpinner(Spinner spinner, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedSpinner(Spinner spinner, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitTextArea(TextArea text, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedTextArea(TextArea text, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitTextField(TextField text, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedTextField(TextField text, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitInject(Inject inject, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitOnChangedEventHandler(Changeable changeable,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedOnChangedEventHandler(Changeable changeable,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitOnFocusEventHandler(Focusable blurable,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedOnFocusEventHandler(Focusable blurable,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitOnBlurEventHandler(Focusable blurable,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedOnBlurEventHandler(Focusable blurable,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitOnEditedEventHandler(Lookup lookup, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedOnEditedEventHandler(Lookup lookup,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitOnAddedEventHandler(Lookup lookup, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedOnAddedEventHandler(Lookup lookup,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitOnPickedEventHandler(Lookup lookup, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedOnPickedEventHandler(Lookup lookup,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitOnClearedEventHandler(Lookup lookup,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitedOnClearedEventHandler(Lookup lookup,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitRerenderEventAction(RerenderEventAction rerender,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitServerSideActionEventAction(
			ServerSideActionEventAction server, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitSetDisabledEventAction(SetDisabledEventAction setDisabled,
			boolean parentVisible, boolean parentEnabled)
			throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitSetInvisibleEventAction(
			SetInvisibleEventAction setInvisible, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitToggleDisabledEventAction(
			ToggleDisabledEventAction toggleDisabled, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitAction(Action action) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitAddAction(Action action) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitRemoveAction(Action action) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitZoomOutAction(Action action) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitNavigateAction(Action action) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitOKAction(Action action) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitSaveAction(Action action) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitCancelAction(Action action) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitDeleteAction(Action action) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitReportAction(Action action) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitBizExportAction(Action action) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitBizImportAction(Action action) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitUploadAction(Action action) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitNewAction(Action action) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitEditAction(Action action) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitParameter(Parameter parameter, boolean parentVisible,
			boolean parentEnabled) throws MetaDataException {
		// TODO Auto-generated method stub
		
	}

}
