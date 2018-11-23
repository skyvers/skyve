package org.skyve.impl.generate.pwa.react;

import java.util.Map;

import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.ViewVisitor;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.event.Addable;
import org.skyve.impl.metadata.view.event.Changeable;
import org.skyve.impl.metadata.view.event.Editable;
import org.skyve.impl.metadata.view.event.EventSource;
import org.skyve.impl.metadata.view.event.Focusable;
import org.skyve.impl.metadata.view.event.Removable;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.Selectable;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.GeoLocator;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.Lookup;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;

public class PrimeReactViewVisitor extends ViewVisitor {
	private static final String PRIME_REACT_VIEW_FILE = "../../PrimeReactView.js";
	static final String STARTING_INDENT = "\t\t\t";
	
	// Map of component to file - ie {View} to ../../View.js
	private Map<String, String> imports;
	private StringBuilder jsx;
	private StringBuilder indent = new StringBuilder(STARTING_INDENT);
	
	protected PrimeReactViewVisitor(Customer customer,
									Module module,
									Document document,
									View view,
									Map<String, String> imports,
									StringBuilder jsx,
									boolean extraIndent) {
		super((CustomerImpl) customer, (ModuleImpl) module, (DocumentImpl) document, (ViewImpl) view);
		this.imports = imports;
		this.jsx = jsx;
		if (extraIndent) {
			indent();
		}
	}

	private StringBuilder indent() {
		return indent.append('\t');
	}
	
	private StringBuilder outdent() {
		indent.setLength(indent.length() - 1);
		return indent;
	}
	
	@Override
	public void visitView() {
		imports.put("{VBox}", PRIME_REACT_VIEW_FILE);
		jsx.append(indent).append("<VBox>\n");
		indent();

		imports.put("{Toolbar}", "primereact/toolbar");
		jsx.append(indent).append("<Toolbar>\n");
		imports.put("{Button}", "primereact/button");
		jsx.append(indent()).append("<Button label=\"Cancel\" onClick={(e) => this.props.history.goBack()} />\n");
		jsx.append(outdent()).append("</Toolbar>\n");
	}

	@Override
	public void visitedView() {
		jsx.append(outdent()).append("</VBox>\n");
	}

	@Override
	public void visitTabPane(TabPane tabPane, boolean parentVisible, boolean parentEnabled) {
		imports.put("{TabView, TabPanel}", "primereact/tabview");
		jsx.append(indent).append("<TabView>\n");
		indent();
	}

	@Override
	public void visitedTabPane(TabPane tabPane, boolean parentVisible, boolean parentEnabled) {
		jsx.append(outdent()).append("</TabView>\n");
	}

	@Override
	public void visitTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<TabPanel header=\"").append(tab.getTitle()).append("\">\n");
		indent();
	}

	@Override
	public void visitedTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		jsx.append(outdent()).append("</TabPanel>\n");
	}

	@Override
	public void visitVBox(VBox vbox, boolean parentVisible, boolean parentEnabled) {
		imports.put("{VBox}", PRIME_REACT_VIEW_FILE);
		jsx.append(indent).append("<VBox>\n");
		indent();
	}

	@Override
	public void visitedVBox(VBox vbox, boolean parentVisible, boolean parentEnabled) {
		jsx.append(outdent()).append("</VBox>\n");
	}

	@Override
	public void visitHBox(HBox hbox, boolean parentVisible, boolean parentEnabled) {
		imports.put("{HBox}", PRIME_REACT_VIEW_FILE);
		jsx.append(indent).append("<HBox>\n");
		indent();
	}

	@Override
	public void visitedHBox(HBox hbox, boolean parentVisible, boolean parentEnabled) {
		jsx.append(outdent()).append("</HBox>\n");
	}

	@Override
	public void visitForm(Form form, boolean parentVisible, boolean parentEnabled) {
		imports.put("{Form}", PRIME_REACT_VIEW_FILE);
		jsx.append(indent).append("<Form>\n");
		indent();
	}

	@Override
	public void visitedForm(Form form, boolean parentVisible, boolean parentEnabled) {
		jsx.append(outdent()).append("</Form>\n");
	}

	@Override
	public void visitFormColumn(FormColumn column, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void visitFormRow(FormRow row, boolean parentVisible, boolean parentEnabled) {
		imports.put("{HBox}", PRIME_REACT_VIEW_FILE);
		jsx.append(indent).append("<HBox>\n");
		indent();
	}

	@Override
	public void visitFormItem(FormItem item, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitedFormItem(FormItem item, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitedFormRow(FormRow row, boolean parentVisible, boolean parentEnabled) {
		jsx.append(outdent()).append("</HBox>\n");
	}

	@Override
	public void visitButton(Button button, boolean parentVisible, boolean parentEnabled) {
		imports.put("{Button}", "primereact/button");
		jsx.append(indent).append("<Button label=\"").append(button.getActionName()).append("\" />\n");
	}

	@Override
	public void visitGeoLocator(GeoLocator locator, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>GeoLocator</span>\n");
	}

	@Override
	public void visitGeometry(Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>Geometry</span>\n");
	}

	@Override
	public void visitMap(MapDisplay map, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>Map</span>\n");
	}

	@Override
	public void visitDialogButton(DialogButton button, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>DialogButton</span>\n");
	}

	@Override
	public void visitDynamicImage(DynamicImage image, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>DynamicImage</span>\n");
	}

	@Override
	public void visitSpacer(Spacer spacer) {
		// nothing to see here
	}

	@Override
	public void visitStaticImage(StaticImage image, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>StaticImage</span>\n");
	}

	@Override
	public void visitLink(Link link, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>Link</span>\n");
	}

	@Override
	public void visitBlurb(Blurb blurb, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>Blurb</span>\n");
	}

	@Override
	public void visitLabel(Label label, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>Label</span>\n");
	}

	@Override
	public void visitProgressBar(ProgressBar progressBar, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>ProgressBar</span>\n");
	}

	@Override
	public void visitListGrid(ListGrid grid, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>ListGrid</span>\n");
	}

	@Override
	public void visitedListGrid(ListGrid grid, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitListRepeater(ListRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>ListRepeater</span>\n");
	}

	@Override
	public void visitedListRepeater(ListRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitTreeGrid(TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>TreeGrid</span>\n");
	}

	@Override
	public void visitedTreeGrid(TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>DataGrid</span>\n");
	}

	@Override
	public void visitedDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>DataRepeater</span>\n");
	}

	@Override
	public void visitedDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitDataGridBoundColumn(DataGridBoundColumn column, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>DataGridBoundColumn</span>\n");
	}

	@Override
	public void visitedDataGridBoundColumn(DataGridBoundColumn column, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitDataGridContainerColumn(DataGridContainerColumn column, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>DataGridContainerColumn</span>\n");
	}

	@Override
	public void visitedDataGridContainerColumn(DataGridContainerColumn column, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitCheckBox(CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>CheckBox</span>\n");
	}

	@Override
	public void visitedCheckBox(CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitCheckMembership(CheckMembership membership, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>CheckMembership</span>\n");
	}

	@Override
	public void visitedCheckMembership(CheckMembership membership, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitColourPicker(ColourPicker colour, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>ColourPicker</span>\n");
	}

	@Override
	public void visitedColourPicker(ColourPicker colour, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitCombo(Combo combo, boolean parentVisible, boolean parentEnabled) {
		imports.put("{Dropdown}", "primereact/dropdown");
		jsx.append(indent).append("<Dropdown />\n");
	}

	@Override
	public void visitedCombo(Combo combo, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitContentImage(ContentImage image, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>ContentImage</span>\n");
	}

	@Override
	public void visitContentLink(ContentLink link, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>ContentLink</span>\n");
	}

	@Override
	public void visitHTML(HTML html, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>HTML</span>\n");
	}

	@Override
	public void visitListMembership(ListMembership membership, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>ListMembership</span>\n");
	}

	@Override
	public void visitedListMembership(ListMembership membership, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitComparison(Comparison comparison, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>Comparison</span>\n");
	}

	@Override
	public void visitLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>LookupDescription</span>\n");
	}

	@Override
	public void visitedLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitLookup(Lookup lookup, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>LookupDescription</span>\n");
	}

	@Override
	public void visitedLookup(Lookup lookup, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitPassword(Password password, boolean parentVisible, boolean parentEnabled) {
		imports.put("{Password}", "primereact/password");
		jsx.append(indent).append("<Password />\n");
	}

	@Override
	public void visitedPassword(Password password, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitRadio(Radio radio, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>Radio</span>\n");
	}

	@Override
	public void visitedRadio(Radio radio, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitRichText(RichText richText, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>RichText</span>\n");
	}

	@Override
	public void visitedRichText(RichText richText, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitSlider(Slider slider, boolean parentVisible, boolean parentEnabled) {
		imports.put("{Slider}", "primereact/slider");
		jsx.append(indent).append("<Slider />\n");
	}

	@Override
	public void visitedSlider(Slider slider, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitSpinner(Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		imports.put("{Spinner}", "primereact/spinner");
		jsx.append(indent).append("<Spinner />\n");
	}

	@Override
	public void visitedSpinner(Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitTextArea(TextArea text, boolean parentVisible, boolean parentEnabled) {
		imports.put("{InputTextarea}", "primereact/inputtextarea");
		jsx.append(indent).append("<InputTextarea />\n");
	}

	@Override
	public void visitedTextArea(TextArea text, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
		imports.put("{InputText}", "primereact/inputtext");
		jsx.append(indent).append("<InputText />\n");
	}

	@Override
	public void visitedTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitInject(Inject inject, boolean parentVisible, boolean parentEnabled) {
		jsx.append(indent).append("<span>RichText</span>\n");
	}

	@Override
	public void visitOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitedOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitedOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitedOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitedOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitedOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitedOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitedOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitOnPickedEventHandler(Lookup lookup, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitedOnPickedEventHandler(Lookup lookup, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitOnClearedEventHandler(Lookup lookup, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitedOnClearedEventHandler(Lookup lookup, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitRerenderEventAction(RerenderEventAction rerender, EventSource source, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitServerSideActionEventAction(ServerSideActionEventAction server, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitSetDisabledEventAction(SetDisabledEventAction setDisabled, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitSetInvisibleEventAction(SetInvisibleEventAction setInvisible, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitToggleDisabledEventAction(ToggleDisabledEventAction toggleDisabled, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitToggleVisibilityEventAction(ToggleVisibilityEventAction toggleVisibility, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitCustomAction(ActionImpl action) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitAddAction(ActionImpl action) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitRemoveAction(ActionImpl action) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitZoomOutAction(ActionImpl action) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitNavigateAction(ActionImpl action) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitOKAction(ActionImpl action) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitSaveAction(ActionImpl action) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitCancelAction(ActionImpl action) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitDeleteAction(ActionImpl action) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitReportAction(ActionImpl action) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitBizExportAction(ActionImpl action) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitBizImportAction(ActionImpl action) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitDownloadAction(ActionImpl action) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitUploadAction(ActionImpl action) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitNewAction(ActionImpl action) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitEditAction(ActionImpl action) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitParameter(Parameter parameter, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitFilterParameter(FilterParameter parameter, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}
}
