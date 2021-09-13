package org.skyve.impl.metadata.view.component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.Identifiable;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.InjectBinding;
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
import org.skyve.impl.metadata.view.event.EventAction;
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
import org.skyve.impl.metadata.view.widget.FilterParameterImpl;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.DefaultWidget;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
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
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.metadata.view.widget.bound.Parameter;

/**
 * ViewVisitor to prefix all bindings and map actions and conditions
 * defined in the component to the encapsulating view.
 */
public class ComponentViewVisitor extends ViewVisitor {
	private String bindingPrefix;
	private Map<String, String> names;
	private String widgetId;
	
	public ComponentViewVisitor(CustomerImpl customer, 
									ModuleImpl module, 
									DocumentImpl document, 
									ViewImpl view, 
									String bindingPrefix,
									List<ComponentNameMap> names,
									String widgetId) {
		super(customer, module, document, view);
		this.bindingPrefix = bindingPrefix;
		this.names = new TreeMap<>();
		for (ComponentNameMap name : names) {
			this.names.put(name.getFromComponent(), name.getMappedTo());
		}
		this.widgetId = widgetId;
	}
	
	private Identifiable identifiable;
	
	public List<MetaData> getContained() {
		if (identifiable == null) {
			if (widgetId != null) {
				throw new MetaDataException(String.format("Component definition with widgetId %s is not a valid widgetId.", widgetId));
			}
			return view.getContained();
		}
		
		List<MetaData> result = new ArrayList<>(1);
		result.add(identifiable);
		return result;
	}
	
	@Override
	public void visitView() {
		// nothing to do here
	}

	@Override
	public void visitedView() {
		// nothing to do here
	}

	@Override
	public void visitTabPane(TabPane tabPane, boolean parentVisible, boolean parentEnabled) {
		tabPane.setSelectedTabIndexBinding(prefixBinding(tabPane.getSelectedTabIndexBinding()));
		disable(tabPane);
		invisible(tabPane);

		// capture the targeted widget, if applicable
		if ((widgetId != null) && (widgetId.equals(tabPane.getWidgetId()))) {
			identifiable = tabPane;
		}
	}

	@Override
	public void visitedTabPane(TabPane tabPane, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		disable(tab);
		invisible(tab);
	}

	@Override
	public void visitedTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitVBox(VBox vbox, boolean parentVisible, boolean parentEnabled) {
		invisible(vbox);

		// capture the targeted widget, if applicable
		if ((widgetId != null) && (widgetId.equals(vbox.getWidgetId()))) {
			identifiable = vbox;
		}
	}

	@Override
	public void visitedVBox(VBox vbox, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitHBox(HBox hbox, boolean parentVisible, boolean parentEnabled) {
		invisible(hbox);

		// capture the targeted widget, if applicable
		if ((widgetId != null) && (widgetId.equals(hbox.getWidgetId()))) {
			identifiable = hbox;
		}
	}

	@Override
	public void visitedHBox(HBox hbox, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitForm(Form form, boolean parentVisible, boolean parentEnabled) {
		disable(form);
		invisible(form);

		// capture the targeted widget, if applicable
		if ((widgetId != null) && (widgetId.equals(form.getWidgetId()))) {
			identifiable = form;
		}
	}

	@Override
	public void visitedForm(Form form, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitFormColumn(FormColumn column, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitFormRow(FormRow row, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitFormItem(FormItem item, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedFormItem(FormItem item, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedFormRow(FormRow row, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitButton(Button button, boolean parentVisible, boolean parentEnabled) {
		button.setActionName(translate(button.getActionName()));
	}

	@Override
	public void visitGeometry(Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		bound(geometry);
		disable(geometry);
		invisible(geometry);
	}

	@Override
	public void visitedGeometry(Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitGeometryMap(GeometryMap geometry, boolean parentVisible, boolean parentEnabled) {
		bound(geometry);
		disable(geometry);
		invisible(geometry);
	}

	@Override
	public void visitedGeometryMap(GeometryMap geometry, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}
	
	@Override
	public void visitMap(MapDisplay map, boolean parentVisible, boolean parentEnabled) {
		invisible(map);
	}

	@Override
	public void visitChart(Chart chart, boolean parentVisible, boolean parentEnabled) {
		invisible(chart);
	}

	@Override
	public void visitDialogButton(DialogButton button, boolean parentVisible, boolean parentEnabled) {
		disable(button);
		invisible(button);
	}

	@Override
	public void visitDynamicImage(DynamicImage image, boolean parentVisible, boolean parentEnabled) {
		invisible(image);
	}

	@Override
	public void visitSpacer(Spacer spacer) {
		invisible(spacer);
	}

	@Override
	public void visitStaticImage(StaticImage image, boolean parentVisible, boolean parentEnabled) {
		invisible(image);
	}

	@Override
	public void visitLink(Link link, boolean parentVisible, boolean parentEnabled) {
		invisible(link);
		link.setValue(prefixExpression(link.getLocalisedValue()));
	}

	@Override
	public void visitBlurb(Blurb blurb, boolean parentVisible, boolean parentEnabled) {
		invisible(blurb);
		blurb.setMarkup(prefixExpression(blurb.getLocalisedMarkup()));
	}

	@Override
	public void visitLabel(Label label, boolean parentVisible, boolean parentEnabled) {
		bound(label);
		invisible(label);
		label.setFor(prefixBinding(label.getFor()));
		label.setValue(prefixExpression(label.getLocalisedValue()));
	}

	@Override
	public void visitProgressBar(ProgressBar progressBar, boolean parentVisible, boolean parentEnabled) {
		bound(progressBar);
		invisible(progressBar);
	}

	@Override
	public void visitListGrid(ListGrid grid, boolean parentVisible, boolean parentEnabled) {
		disable(grid);
		invisible(grid);
		grid.setDisableAddConditionName(translate(grid.getDisableAddConditionName()));
		grid.setDisableEditConditionName(translate(grid.getDisableEditConditionName()));
		grid.setDisableRemoveConditionName(translate(grid.getDisableRemoveConditionName()));
		grid.setDisableZoomConditionName(translate(grid.getDisableZoomConditionName()));
		grid.setPostRefreshConditionName(translate(grid.getPostRefreshConditionName()));
		grid.setSelectedIdBinding(prefixBinding(grid.getSelectedIdBinding()));
	}

	@Override
	public void visitedListGrid(ListGrid grid, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitTreeGrid(TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
		visitListGrid(grid, parentVisible, parentEnabled);
		grid.setRootIdBinding(prefixBinding(grid.getRootIdBinding()));
	}

	@Override
	public void visitedTreeGrid(TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitListRepeater(ListRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		invisible(repeater);
		repeater.setPostRefreshConditionName(translate(repeater.getPostRefreshConditionName()));
	}

	@Override
	public void visitedListRepeater(ListRepeater grid, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		bound(grid);
		disable(grid);
		invisible(grid);
		grid.setDisableAddConditionName(translate(grid.getDisableAddConditionName()));
		grid.setDisableEditConditionName(translate(grid.getDisableEditConditionName()));
		grid.setDisableRemoveConditionName(translate(grid.getDisableRemoveConditionName()));
		grid.setDisableZoomConditionName(translate(grid.getDisableZoomConditionName()));
		grid.setSelectedIdBinding(prefixBinding(grid.getSelectedIdBinding()));

		// capture the targeted widget, if applicable
		if ((widgetId != null) && (widgetId.equals(grid.getWidgetId()))) {
			identifiable = grid;
		}
	}

	@Override
	public void visitedDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		bound(repeater);
		invisible(repeater);

		// capture the targeted widget, if applicable
		if ((widgetId != null) && (widgetId.equals(repeater.getWidgetId()))) {
			identifiable = repeater;
		}
	}

	@Override
	public void visitedDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitDataGridBoundColumn(DataGridBoundColumn column, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedDataGridBoundColumn(DataGridBoundColumn column, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitDataGridContainerColumn(DataGridContainerColumn column, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedDataGridContainerColumn(DataGridContainerColumn column, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitCheckBox(CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		bound(checkBox);
		disable(checkBox);
		invisible(checkBox);
	}

	@Override
	public void visitedCheckBox(CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitCheckMembership(CheckMembership membership, boolean parentVisible, boolean parentEnabled) {
		bound(membership);
		disable(membership);
		invisible(membership);
	}

	@Override
	public void visitedCheckMembership(CheckMembership membership, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitColourPicker(ColourPicker colour, boolean parentVisible, boolean parentEnabled) {
		bound(colour);
		disable(colour);
		invisible(colour);
	}

	@Override
	public void visitedColourPicker(ColourPicker colour, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitCombo(Combo combo, boolean parentVisible, boolean parentEnabled) {
		bound(combo);
		disable(combo);
		invisible(combo);
	}

	@Override
	public void visitedCombo(Combo combo, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitContentImage(ContentImage image, boolean parentVisible, boolean parentEnabled) {
		bound(image);
		disable(image);
		invisible(image);
	}

	@Override
	public void visitContentLink(ContentLink link, boolean parentVisible, boolean parentEnabled) {
		bound(link);
		disable(link);
		invisible(link);
		link.setValue(prefixExpression(link.getLocalisedValue()));
	}

	@Override
	public void visitContentSignature(ContentSignature signature, boolean parentVisible, boolean parentEnabled) {
		bound(signature);
		disable(signature);
		invisible(signature);
	}

	@Override
	public void visitHTML(HTML html, boolean parentVisible, boolean parentEnabled) {
		bound(html);
		disable(html);
		invisible(html);
	}

	@Override
	public void visitListMembership(ListMembership membership, boolean parentVisible, boolean parentEnabled) {
		bound(membership);
		disable(membership);
		invisible(membership);
	}

	@Override
	public void visitedListMembership(ListMembership membership, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitComparison(Comparison comparison, boolean parentVisible, boolean parentEnabled) {
		bound(comparison);
		disable(comparison);
		invisible(comparison);
	}

	@Override
	public void visitLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		bound(lookup);
		disable(lookup);
		invisible(lookup);
		lookup.setDisableAddConditionName(translate(lookup.getDisableAddConditionName()));
		lookup.setDisableClearConditionName(translate(lookup.getDisableClearConditionName()));
		lookup.setDisableEditConditionName(translate(lookup.getDisableEditConditionName()));
		lookup.setDisablePickConditionName(translate(lookup.getDisablePickConditionName()));
	}

	@Override
	public void visitedLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitPassword(Password password, boolean parentVisible, boolean parentEnabled) {
		bound(password);
		disable(password);
		invisible(password);
	}

	@Override
	public void visitedPassword(Password password, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitRadio(Radio radio, boolean parentVisible, boolean parentEnabled) {
		bound(radio);
		disable(radio);
		invisible(radio);
	}

	@Override
	public void visitedRadio(Radio radio, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitRichText(RichText richText, boolean parentVisible, boolean parentEnabled) {
		bound(richText);
		disable(richText);
		invisible(richText);
	}

	@Override
	public void visitedRichText(RichText richText, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitSlider(Slider slider, boolean parentVisible, boolean parentEnabled) {
		bound(slider);
		disable(slider);
		invisible(slider);
	}

	@Override
	public void visitedSlider(Slider slider, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitSpinner(Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		bound(spinner);
		disable(spinner);
		invisible(spinner);
	}

	@Override
	public void visitedSpinner(Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitTextArea(TextArea text, boolean parentVisible, boolean parentEnabled) {
		bound(text);
		disable(text);
		invisible(text);
	}

	@Override
	public void visitedTextArea(TextArea text, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
		bound(text);
		disable(text);
		invisible(text);
	}

	@Override
	public void visitedTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitInject(Inject inject, boolean parentVisible, boolean parentEnabled) {
		for (InjectBinding binding : inject.getBindings()) {
			bound(binding);
		}
	}

	@Override
	public void visitOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		event(changeable.getChangedActions());
	}

	@Override
	public void visitedOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		event(blurable.getFocusActions());
	}

	@Override
	public void visitedOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		event(blurable.getBlurActions());
	}

	@Override
	public void visitedOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
		event(addable.getAddedActions());
	}

	@Override
	public void visitedOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
		event(editable.getEditedActions());
	}

	@Override
	public void visitedOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
		event(removable.getRemovedActions());
	}

	@Override
	public void visitedOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
		event(selectable.getSelectedActions());
	}

	@Override
	public void visitedOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnPickedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		event(lookup.getPickedActions());
	}

	@Override
	public void visitedOnPickedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnClearedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		event(lookup.getClearedActions());
	}

	@Override
	public void visitedOnClearedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitRerenderEventAction(RerenderEventAction rerender, EventSource source, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitServerSideActionEventAction(ServerSideActionEventAction server, boolean parentVisible, boolean parentEnabled) {
		server.setActionName(translate(server.getActionName()));
	}

	@Override
	public void visitSetDisabledEventAction(SetDisabledEventAction setDisabled, boolean parentVisible, boolean parentEnabled) {
		bound(setDisabled);
		disable(setDisabled);
	}

	@Override
	public void visitSetInvisibleEventAction(SetInvisibleEventAction setInvisible, boolean parentVisible, boolean parentEnabled) {
		bound(setInvisible);
		invisible(setInvisible);
	}

	@Override
	public void visitToggleDisabledEventAction(ToggleDisabledEventAction toggleDisabled, boolean parentVisible, boolean parentEnabled) {
		bound(toggleDisabled);
	}

	@Override
	public void visitToggleVisibilityEventAction(ToggleVisibilityEventAction toggleVisibility, boolean parentVisible, boolean parentEnabled) {
		bound(toggleVisibility);
	}

	@Override
	public void visitCustomAction(ActionImpl action) {
		// nothing to do here
	}

	@Override
	public void visitAddAction(ActionImpl action) {
		// nothing to do here
	}

	@Override
	public void visitRemoveAction(ActionImpl action) {
		// nothing to do here
	}

	@Override
	public void visitZoomOutAction(ActionImpl action) {
		// nothing to do here
	}

	@Override
	public void visitNavigateAction(ActionImpl action) {
		// nothing to do here
	}

	@Override
	public void visitOKAction(ActionImpl action) {
		// nothing to do here
	}

	@Override
	public void visitSaveAction(ActionImpl action) {
		// nothing to do here
	}

	@Override
	public void visitCancelAction(ActionImpl action) {
		// nothing to do here
	}

	@Override
	public void visitDeleteAction(ActionImpl action) {
		// nothing to do here
	}

	@Override
	public void visitReportAction(ActionImpl action) {
		// nothing to do here
	}

	@Override
	public void visitBizExportAction(ActionImpl action) {
		// nothing to do here
	}

	@Override
	public void visitBizImportAction(ActionImpl action) {
		// nothing to do here
	}

	@Override
	public void visitDownloadAction(ActionImpl action) {
		// nothing to do here
	}

	@Override
	public void visitUploadAction(ActionImpl action) {
		// nothing to do here
	}

	@Override
	public void visitNewAction(ActionImpl action) {
		// nothing to do here
	}

	@Override
	public void visitEditAction(ActionImpl action) {
		// nothing to do here
	}

	@Override
	public void visitParameter(Parameter parameter, boolean parentVisible, boolean parentEnabled) {
		if (parameter instanceof ParameterImpl) {
			ParameterImpl p = (ParameterImpl) parameter;
			p.setValueBinding(prefixBinding(parameter.getValueBinding()));
			p.setValue(prefixExpression(parameter.getValue()));
		}
	}

	@Override
	public void visitFilterParameter(FilterParameter parameter, boolean parentVisible, boolean parentEnabled) {
		if (parameter instanceof FilterParameterImpl) {
			FilterParameterImpl p = (FilterParameterImpl) parameter;
			p.setValueBinding(prefixBinding(parameter.getValueBinding()));
			p.setValue(prefixExpression(parameter.getValue()));
		}
	}

	/**
	 * This is overridden to not visit the default widget as per the document attribute specification,
	 * but instead just changes the binding used.
	 */
	@Override
	public void visitDefaultWidget(DefaultWidget widget, boolean parentVisible, boolean parentEnabled) {
		bound(widget);
	}
	
	private void bound(Bound bound) {
		bound.setBinding(prefixBinding(bound.getBinding()));
	}

	private void disable(Disableable disableable) {
		disableable.setDisabledConditionName(translate(disableable.getDisabledConditionName()));
	}
	
	private void invisible(Invisible invisible) {
		invisible.setInvisibleConditionName(translate(invisible.getInvisibleConditionName()));
	}

	private void event(List<EventAction> actions) {
		for (EventAction action : actions) {
			if (action instanceof ServerSideActionEventAction) {
				ServerSideActionEventAction server = (ServerSideActionEventAction) action;
				server.setActionName(translate(server.getActionName()));
			}
		}
	}
	
	private String prefixExpression(String expression) {
		if (bindingPrefix == null) {
			return expression;
		}
		return BindUtil.prefixMessageBindings(expression, bindingPrefix);
	}

	private String translate(String name) {
		if (name == null) {
			return null;
		}
		String result = names.get(name);
		if (result == null) {
			// if we have a not condition, lookup the negation of it
			// and use the negation of the mapping.
			if (name.startsWith("not")) {
				String condish = BindUtil.negateCondition(name);
				result = names.get(condish);
				if (result == null) { // not a mapped negated condition - not found
					result = name;
				}
				else { // negate the mapped negated condition
					result = BindUtil.negateCondition(result);
				}
			}
			else { // not found
				result = name;
			}
		}
		return result;
	}
	
	private String prefixBinding(String binding) {
		if (binding == null) {
			return null;
		}
		if (bindingPrefix == null) {
			return binding;
		}
		return String.format("%s.%s", bindingPrefix, binding);
	}
}
