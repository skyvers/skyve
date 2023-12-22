package org.skyve.impl.report.freemarker;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.ViewVisitor;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
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
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
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
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;

public class FreemarkerCsvReportViewVisitor extends ViewVisitor {

	private StringBuilder csv = new StringBuilder();
	private StringBuilder header = new StringBuilder();
	private StringBuilder data = new StringBuilder();
	private boolean showLabel = true;
	private boolean ignoreSection = false;
	private String childBinding = null;

	public String getCsv() {
		return csv.toString();
	}

	protected FreemarkerCsvReportViewVisitor(CustomerImpl customer, ModuleImpl module, DocumentImpl document, ViewImpl view) {
		super(customer, module, document, view, null);
		csv.append("\"${title}\"").append("${'\n'}");
	}

	@Override
	public void visitView() {
		// TODO Not yet implemented

	}

	@Override
	public void visitedView() {
		// TODO Not yet implemented

	}

	@Override
	public void visitTabPane(TabPane tabPane, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitedTabPane(TabPane tabPane, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitedTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitVBox(VBox vbox, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented
	}

	@Override
	public void visitedVBox(VBox vbox, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented
	}

	@Override
	public void visitHBox(HBox hbox, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitedHBox(HBox hbox, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitForm(Form form, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitedForm(Form form, boolean parentVisible, boolean parentEnabled) {
		if (header.length() > 0) {
			csv.append(header).append("${'\n'}");
			header = new StringBuilder();
		}
		if (data.length() > 0) {
			csv.append(data).append("${'\n'}").append("${'\n'}");
			data = new StringBuilder();
		}
	}

	@Override
	public void visitFormColumn(FormColumn column, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitFormRow(FormRow row, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitFormItem(FormItem item, boolean parentVisible, boolean parentEnabled) {
		if (!ignoreSection) {
			if (item.getShowLabel() != null) {
				showLabel = Boolean.TRUE.equals(item.getShowLabel());
			}
		}
	}

	@Override
	public void visitedFormItem(FormItem item, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitedFormRow(FormRow row, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitButton(Button button, boolean parentVisible, boolean parentEnabled) {
		// Ignore

	}

	@Override
	public void visitZoomIn(ZoomIn zoomIn, boolean parentVisible, boolean parentEnabled) {
		// Ignore

	}

	@Override
	public void visitGeometry(Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitedGeometry(Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitGeometryMap(GeometryMap geometry, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitedGeometryMap(GeometryMap geometry, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitMap(MapDisplay map, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitChart(Chart chart, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitDialogButton(DialogButton button, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitDynamicImage(DynamicImage image, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitSpacer(Spacer spacer) {
		// Ignore
	}

	@Override
	public void visitStaticImage(StaticImage image, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitLink(Link link, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitBlurb(Blurb blurb, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented
	}

	@Override
	public void visitLabel(Label label, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented
	}

	@Override
	public void visitProgressBar(ProgressBar progressBar, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitListGrid(ListGrid grid, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitedListGrid(ListGrid grid, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitListRepeater(ListRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitedListRepeater(ListRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitTreeGrid(TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitedTreeGrid(TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		this.childBinding = grid.getBinding();
	}

	@Override
	public void visitedDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		this.childBinding = null;
	}

	@Override
	public void visitDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitedDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitDataGridBoundColumn(DataGridBoundColumn column, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitedDataGridBoundColumn(DataGridBoundColumn column, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitDataGridContainerColumn(DataGridContainerColumn column, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitedDataGridContainerColumn(DataGridContainerColumn column, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitCheckBox(CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		handleDefaultInputWidget(checkBox);
	}

	@Override
	public void visitedCheckBox(CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitCheckMembership(CheckMembership membership, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitedCheckMembership(CheckMembership membership, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitColourPicker(ColourPicker colour, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitedColourPicker(ColourPicker colour, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitCombo(Combo combo, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented
	}

	@Override
	public void visitedCombo(Combo combo, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitContentImage(ContentImage image, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitContentLink(ContentLink link, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitContentSignature(ContentSignature signature, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitHTML(HTML html, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitListMembership(ListMembership membership, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitedListMembership(ListMembership membership, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitComparison(Comparison comparison, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		handleDefaultInputWidget(lookup);
	}

	@Override
	public void visitedLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitPassword(Password password, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitedPassword(Password password, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitRadio(Radio radio, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitedRadio(Radio radio, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitRichText(RichText richText, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitedRichText(RichText richText, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitSlider(Slider slider, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented

	}

	@Override
	public void visitedSlider(Slider slider, boolean parentVisible, boolean parentEnabled) {
		// TODO Not yet implemented
	}

	@Override
	public void visitSpinner(Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		handleDefaultInputWidget(spinner);
	}

	@Override
	public void visitedSpinner(Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitTextArea(TextArea text, boolean parentVisible, boolean parentEnabled) {
		handleDefaultInputWidget(text);
	}

	@Override
	public void visitedTextArea(TextArea text, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
		handleDefaultInputWidget(text);
	}

	@Override
	public void visitedTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitInject(Inject inject, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitedOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitedOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitedOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitedOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitedOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitedOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitedOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitOnPickedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// Ignore
	}

	@Override
	public void visitedOnPickedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// Ignore

	}

	@Override
	public void visitOnClearedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// Ignore

	}

	@Override
	public void visitedOnClearedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// Ignore

	}

	@Override
	public void visitRerenderEventAction(RerenderEventAction rerender, EventSource source, boolean parentVisible, boolean parentEnabled) {
		// Ignore

	}

	@Override
	public void visitServerSideActionEventAction(ServerSideActionEventAction server, boolean parentVisible, boolean parentEnabled) {
		// Ignore

	}

	@Override
	public void visitSetDisabledEventAction(SetDisabledEventAction setDisabled, boolean parentVisible, boolean parentEnabled) {
		// Ignore

	}

	@Override
	public void visitSetInvisibleEventAction(SetInvisibleEventAction setInvisible, boolean parentVisible, boolean parentEnabled) {
		// Ignore

	}

	@Override
	public void visitToggleDisabledEventAction(ToggleDisabledEventAction toggleDisabled, boolean parentVisible, boolean parentEnabled) {
		// Ignore

	}

	@Override
	public void visitToggleVisibilityEventAction(ToggleVisibilityEventAction toggleVisibility, boolean parentVisible, boolean parentEnabled) {
		// Ignore

	}

	@Override
	public void visitCustomAction(ActionImpl action) {
		// Ignore

	}

	@Override
	public void visitAddAction(ActionImpl action) {
		// Ignore

	}

	@Override
	public void visitRemoveAction(ActionImpl action) {
		// Ignore

	}

	@Override
	public void visitZoomOutAction(ActionImpl action) {
		// Ignore

	}

	@Override
	public void visitNavigateAction(ActionImpl action) {
		// Ignore

	}

	@Override
	public void visitOKAction(ActionImpl action) {
		// Ignore

	}

	@Override
	public void visitSaveAction(ActionImpl action) {
		// Ignore

	}

	@Override
	public void visitCancelAction(ActionImpl action) {
		// Ignore

	}

	@Override
	public void visitDeleteAction(ActionImpl action) {
		// Ignore

	}

	@Override
	public void visitReportAction(ActionImpl action) {
		// Ignore

	}

	@Override
	public void visitBizExportAction(ActionImpl action) {
		// Ignore

	}

	@Override
	public void visitBizImportAction(ActionImpl action) {
		// Ignore

	}

	@Override
	public void visitDownloadAction(ActionImpl action) {
		// Ignore

	}

	@Override
	public void visitUploadAction(ActionImpl action) {
		// Ignore

	}

	@Override
	public void visitNewAction(ActionImpl action) {
		// Ignore

	}

	@Override
	public void visitEditAction(ActionImpl action) {
		// Ignore

	}

	@Override
	public void visitParameter(Parameter parameter, boolean parentVisible, boolean parentEnabled) {
		// Ignore

	}

	@Override
	public void visitFilterParameter(FilterParameter parameter, boolean parentVisible, boolean parentEnabled) {
		// Ignore

	}

	/**
	 * Handle default input widgets
	 * 
	 * @param w
	 */
	private void handleDefaultInputWidget(InputWidget w) {
		if (!this.ignoreSection) {
			// find the attribute
			String binding = w.getBinding();
			if (this.childBinding != null) {
				binding = Binder.createCompoundBinding(this.childBinding, w.getBinding());
			}
			TargetMetaData tm = Binder.getMetaDataForBinding(CORE.getCustomer(), this.module, this.document, binding);
			Attribute a = tm.getAttribute();
			if (a == null) {
				throw new ValidationException(
						new Message(
								String.format("The view could not be processed because the binding %s was not found in document %s", w.getBinding(), this.document.getName())));
			}
			if (this.showLabel) {
				this.header.append(String.format("%s", a.getLocalisedDisplayName())).append(",");
			}
			this.data.append(String.format("<@format bean=bean binding=\"%s\"/>", w.getBinding())).append(",");
		}
	}

	@Override
	public void visitSidebar(Sidebar sidebar, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub

	}

	@Override
	public void visitedSidebar(Sidebar sidebar, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub

	}
}
