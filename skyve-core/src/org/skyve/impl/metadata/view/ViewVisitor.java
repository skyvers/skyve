package org.skyve.impl.metadata.view;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
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
import org.skyve.impl.metadata.view.widget.bound.input.DefaultWidget;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
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
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.PickList;
import org.skyve.impl.metadata.view.widget.bound.tabular.PickListColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.util.Binder.TargetMetaData;

public abstract class ViewVisitor extends ActionVisitor {
	protected CustomerImpl customer;
	protected ModuleImpl module;
	protected DocumentImpl document;
	protected ViewImpl view;
	
	protected ViewVisitor(CustomerImpl customer, ModuleImpl module, DocumentImpl document, ViewImpl view) {
		this.customer = customer;
		this.module = module;
		this.document = document;
		this.view = view;
	}

	public final void visit() {
		visitContainer(view, true, true);
	}
	
	public abstract void visitView();
	public abstract void visitedView();

	public abstract void visitTabPane(TabPane tabPane,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitedTabPane(TabPane tabPane,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitTab(Tab tab,
									boolean parentVisible,
									boolean parentEnabled);
	public abstract void visitedTab(Tab tab,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitVBox(VBox vbox,
									boolean parentVisible,
									boolean parentEnabled);
	public abstract void visitedVBox(VBox vbox,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitHBox(HBox hbox,
									boolean parentVisible,
									boolean parentEnabled);
	public abstract void visitedHBox(HBox hbox,
										boolean parentVisible,
										boolean parentEnabled);

	// form
	public abstract void visitForm(Form form, 
									boolean parentVisible,
									boolean parentEnabled);
	public abstract void visitedForm(Form form,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitFormColumn(FormColumn column,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitFormRow(FormRow row,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitFormItem(FormItem item,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitedFormItem(FormItem item,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitedFormRow(FormRow row,
											boolean parentVisible,
											boolean parentEnabled);

	// widgets
	public abstract void visitButton(Button button,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitGeoLocator(GeoLocator locator,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitGeometry(Geometry geometry,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitMap(MapDisplay map,
									boolean parentVisible,
									boolean parentEnabled);
	public abstract void visitDialogButton(DialogButton button,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitDynamicImage(DynamicImage image,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitSpacer(Spacer spacer);
	public abstract void visitStaticImage(StaticImage image,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitLink(Link link,
									boolean parentVisible,
									boolean parentEnabled);

	public abstract void visitBlurb(Blurb blurb,
										boolean parentVisible,
										boolean parentEnabled);
	
	// bound widgets
	public abstract void visitLabel(Label label,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitProgressBar(ProgressBar progressBar,
											boolean parentVisible,
											boolean parentEnabled);

	// tabular widgets
	public abstract void visitListGrid(ListGrid grid,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitedListGrid(ListGrid grid,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitTreeGrid(TreeGrid grid,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitedTreeGrid(TreeGrid grid,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitDataGrid(DataGrid grid,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitedDataGrid(DataGrid grid,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitDataGridBoundColumn(DataGridBoundColumn column,
													boolean parentVisible,
													boolean parentEnabled);
	public abstract void visitedDataGridBoundColumn(DataGridBoundColumn column,
														boolean parentVisible,
														boolean parentEnabled);
	public abstract void visitDataGridContainerColumn(DataGridContainerColumn column,
														boolean parentVisible,
														boolean parentEnabled);
	public abstract void visitedDataGridContainerColumn(DataGridContainerColumn column,
															boolean parentVisible,
															boolean parentEnabled);
	public abstract void visitPickList(PickList list,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitedPickList(PickList list,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitPickListColumn(PickListColumn column,
												boolean parentVisible,
												boolean parentEnabled);

	// input widgets
	public abstract void visitCheckBox(CheckBox checkBox,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitedCheckBox(CheckBox checkBox,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitCheckMembership(CheckMembership membership,
												boolean parentVisible,
												boolean parentEnabled);
	public abstract void visitedCheckMembership(CheckMembership membership,
													boolean parentVisible,
													boolean parentEnabled);
	public abstract void visitColourPicker(ColourPicker colour,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitedColourPicker(ColourPicker colour,
												boolean parentVisible,
												boolean parentEnabled);
	public abstract void visitCombo(Combo combo,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitedCombo(Combo combo,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitContentImage(ContentImage image,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitContentLink(ContentLink link,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitHTML(HTML html,
									boolean parentVisible,
									boolean parentEnabled);
	public abstract void visitListMembership(ListMembership membership,
												boolean parentVisible,
												boolean parentEnabled);
	public abstract void visitedListMembership(ListMembership membership,
												boolean parentVisible,
												boolean parentEnabled);
	public abstract void visitComparison(Comparison comparison,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitLookupDescription(LookupDescription lookup,
													boolean parentVisible,
													boolean parentEnabled);
	public abstract void visitedLookupDescription(LookupDescription lookup,
													boolean parentVisible,
													boolean parentEnabled);
	public abstract void visitLookup(Lookup lookup,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitedLookup(Lookup lookup,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitPassword(Password password,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitedPassword(Password password,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitRadio(Radio radio,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitedRadio(Radio radio,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitRichText(RichText richText,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitedRichText(RichText richText,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitSlider(Slider slider,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitedSlider(Slider slider,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitSpinner(Spinner spinner,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitedSpinner(Spinner spinner,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitTextArea(TextArea text,
										boolean parentVisible,
										boolean parentEnabled);
	public abstract void visitedTextArea(TextArea text,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitTextField(TextField text,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitedTextField(TextField text,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitInject(Inject inject,
										boolean parentVisible,
										boolean parentEnabled);

	public abstract void visitOnChangedEventHandler(Changeable changeable,
														boolean parentVisible,
														boolean parentEnabled);
	public abstract void visitedOnChangedEventHandler(Changeable changeable,
														boolean parentVisible,
														boolean parentEnabled);
	public abstract void visitOnFocusEventHandler(Focusable blurable,
													boolean parentVisible,
													boolean parentEnabled);
	public abstract void visitedOnFocusEventHandler(Focusable blurable,
														boolean parentVisible,
														boolean parentEnabled);
	public abstract void visitOnBlurEventHandler(Focusable blurable,
													boolean parentVisible,
													boolean parentEnabled);
	public abstract void visitedOnBlurEventHandler(Focusable blurable,
													boolean parentVisible,
													boolean parentEnabled);
	public abstract void visitOnAddedEventHandler(Addable addable,
													boolean parentVisible,
													boolean parentEnabled);
	public abstract void visitedOnAddedEventHandler(Addable addable,
														boolean parentVisible,
														boolean parentEnabled);
	public abstract void visitOnEditedEventHandler(Editable editable,
													boolean parentVisible,
													boolean parentEnabled);
	public abstract void visitedOnEditedEventHandler(Editable editable,
														boolean parentVisible,
														boolean parentEnabled);
	public abstract void visitOnRemovedEventHandler(Removable removable,
													boolean parentVisible,
													boolean parentEnabled);
	public abstract void visitedOnRemovedEventHandler(Removable removable,
														boolean parentVisible,
														boolean parentEnabled);
	public abstract void visitOnSelectedEventHandler(Selectable selectable,
														boolean parentVisible,
														boolean parentEnabled);
	public abstract void visitedOnSelectedEventHandler(Selectable selectable,
														boolean parentVisible,
														boolean parentEnabled);
	public abstract void visitOnPickedEventHandler(Lookup lookup,
													boolean parentVisible,
													boolean parentEnabled);
	public abstract void visitedOnPickedEventHandler(Lookup lookup,
														boolean parentVisible,
														boolean parentEnabled);
	public abstract void visitOnClearedEventHandler(Lookup lookup,
													boolean parentVisible,
													boolean parentEnabled);
	public abstract void visitedOnClearedEventHandler(Lookup lookup,
														boolean parentVisible,
														boolean parentEnabled);
	public abstract void visitRerenderEventAction(RerenderEventAction rerender,
													EventSource source,
													boolean parentVisible,
													boolean parentEnabled);
	public abstract void visitServerSideActionEventAction(ServerSideActionEventAction server,
															boolean parentVisible,
															boolean parentEnabled);
	public abstract void visitSetDisabledEventAction(SetDisabledEventAction setDisabled,
														boolean parentVisible,
														boolean parentEnabled);
	public abstract void visitSetInvisibleEventAction(SetInvisibleEventAction setInvisible,
														boolean parentVisible,
														boolean parentEnabled);
	public abstract void visitToggleDisabledEventAction(ToggleDisabledEventAction toggleDisabled,
															boolean parentVisible,
															boolean parentEnabled);
	public abstract void visitToggleVisibilityEventAction(ToggleVisibilityEventAction toggleVisibility,
															boolean parentVisible,
															boolean parentEnabled);

	/**
	 * Determines if an <code>Invisible</code> widget is visible or not.
	 * @param invisible	The widget to test.
	 * @return	if the widget is visible or not
	 */
	@SuppressWarnings("static-method")
	protected boolean visible(Invisible invisible) {
		return true;
	}
	
	/**
	 * Determines if an <code>Disableable</code> widget is visible or not.
	 * @param disableable	The widget to test.
	 * @return	if the widget is enabled or not
	 */
	@SuppressWarnings("static-method")
	protected boolean enabled(Disableable disableable) {
		return true;
	}

	private void visitWidget(MetaData widget, 
								boolean parentVisible,
								boolean parentEnabled) {
		// containers
		if (widget instanceof Container) {
			visitContainer((Container) widget, parentVisible, parentEnabled);
		}
		else if (widget instanceof Form) {
			Form form = (Form) widget;
			visitForm(form, parentVisible, parentEnabled);
			boolean formVisible = parentVisible && visible(form);
			boolean formEnabled = parentEnabled && enabled(form);
			for (FormColumn column : form.getColumns()) {
				visitFormColumn(column, formVisible, formEnabled);
			}

			for (FormRow row : form.getRows()) {
				visitFormRow(row, formVisible, formEnabled);
				for (FormItem item : row.getItems()) {
					visitFormItem(item, formVisible, formEnabled);
					MetaData itemWidget = item.getWidget();
					if (itemWidget instanceof DefaultWidget) {
						// determine the widget to use
						String binding = ((Bound) itemWidget).getBinding();
						TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
						Attribute attribute = target.getAttribute();
						if (attribute != null) {
							Bound defaultWidget = attribute.getDefaultInputWidget();
							String definedBinding = defaultWidget.getBinding();
							try {
								// Temporarily set the binding in the default widget binding
								defaultWidget.setBinding(binding);
								visitWidget(defaultWidget, parentVisible, parentEnabled);
							}
							finally {
								defaultWidget.setBinding(definedBinding);
							}
						}
					}
					else {
						visitWidget(itemWidget, formVisible, formEnabled);
					}
					visitedFormItem(item, formVisible, formEnabled);
				}
				visitedFormRow(row, formVisible, formEnabled);
			}

			visitedForm(form, parentVisible, parentEnabled);
		}
		// widgets
		else if (widget instanceof TabPane) {
			TabPane tabPane = (TabPane) widget;
			visitTabPane(tabPane, parentVisible, parentEnabled);
			boolean tabPaneVisible = parentVisible && visible(tabPane);
			boolean tabPaneEnabled = parentEnabled && enabled(tabPane);
			for (Tab tab : tabPane.getTabs()) {
				visitContainer(tab, tabPaneVisible, tabPaneEnabled);
			}
			visitedTabPane(tabPane, parentVisible, parentEnabled);
		}
		else if (widget instanceof Button) {
			Button button = (Button) widget;
			visitButton(button, parentVisible, parentEnabled);
		}
		else if (widget instanceof GeoLocator) {
			GeoLocator locator = (GeoLocator) widget;
			visitGeoLocator(locator, parentVisible, parentEnabled);
		}
		else if (widget instanceof Geometry) {
			Geometry geometry = (Geometry) widget;
			visitGeometry(geometry, parentVisible, parentEnabled);
		}
		else if (widget instanceof MapDisplay) {
			MapDisplay map = (MapDisplay) widget;
			visitMap(map, parentVisible, parentEnabled);
		}
		else if (widget instanceof DialogButton) {
			DialogButton button = (DialogButton) widget;
			visitDialogButton(button, parentVisible, parentEnabled);
			visitParameterizable(button, parentVisible, parentEnabled);
		}
		else if (widget instanceof DynamicImage) {
			DynamicImage image = (DynamicImage) widget;
			visitDynamicImage(image, parentVisible, parentEnabled);
			visitParameterizable(image, parentVisible, parentEnabled);
		}
		else if (widget instanceof Spacer) {
			visitSpacer((Spacer) widget);
		}
		else if (widget instanceof StaticImage) {
			StaticImage image = (StaticImage) widget;
			visitStaticImage(image, parentVisible, parentEnabled);
		}
		else if (widget instanceof Link) {
			Link link = (Link) widget;
			visitLink(link, parentVisible, parentEnabled);
		}
		else if (widget instanceof Blurb) {
			Blurb blurb = (Blurb) widget;
			visitBlurb(blurb, parentVisible, parentEnabled);
		}
		// bound
		else if (widget instanceof Label) {
			Label label = (Label) widget;
			visitLabel(label, parentVisible, parentEnabled);
		}
		else if (widget instanceof ProgressBar) {
			ProgressBar bar = (ProgressBar) widget;
			visitProgressBar(bar, parentVisible, parentEnabled);
		}
		// tabular
		else if (widget instanceof TreeGrid) {
			TreeGrid grid = (TreeGrid) widget;
			visitTreeGrid(grid, parentVisible, parentEnabled);
			visitFilterable(grid, parentVisible, parentEnabled);
			visitEditableActions(grid, parentVisible, parentEnabled);
			visitRemovableActions(grid, parentVisible, parentEnabled);
			visitSelectableActions(grid, parentVisible, parentEnabled);
			visitedTreeGrid(grid, parentVisible, parentEnabled);
		}
		else if (widget instanceof ListGrid) {
			ListGrid grid = (ListGrid) widget;
			visitListGrid(grid, parentVisible, parentEnabled);
			visitFilterable(grid, parentVisible, parentEnabled);
			visitEditableActions(grid, parentVisible, parentEnabled);
			visitRemovableActions(grid, parentVisible, parentEnabled);
			visitSelectableActions(grid, parentVisible, parentEnabled);
			visitedListGrid(grid, parentVisible, parentEnabled);
		}
		else if (widget instanceof DataGrid) {
			DataGrid grid = (DataGrid) widget;
			String gridBindingPrefix = grid.getBinding();
			if (gridBindingPrefix == null) {
				gridBindingPrefix = "";
			}
			else {
				gridBindingPrefix += '.';
			}
			visitDataGrid(grid, parentVisible, parentEnabled);
			boolean gridVisible = parentVisible && visible(grid);
			// Disregard grid.getEditable() as it could be that there are links that 
			// change the grid data client-side
			boolean gridEnabled = parentEnabled && enabled(grid);

			for (DataGridColumn column : grid.getColumns()) {
				if (column instanceof DataGridBoundColumn) {
					DataGridBoundColumn boundColumn = (DataGridBoundColumn) column;
					visitDataGridBoundColumn(boundColumn, gridVisible, gridEnabled);
	
					boolean gridColumnEnabled = gridEnabled && (! Boolean.FALSE.equals(boundColumn.getEditable())); // can be true or null
	
					InputWidget inputWidget = null;
					String columnBinding = boundColumn.getBinding();
					WidgetReference widgetRef = boundColumn.getInputWidget();
					if (widgetRef != null) {
						inputWidget = widgetRef.getWidget();
					}
					else {
						// determine the widget to use
						String fullyQualifiedColumnBinding = columnBinding;
						if (fullyQualifiedColumnBinding == null) {
							fullyQualifiedColumnBinding = grid.getBinding();
						}
						else {
							fullyQualifiedColumnBinding = gridBindingPrefix + fullyQualifiedColumnBinding;
						}
		
						if (fullyQualifiedColumnBinding.endsWith(Bean.BIZ_KEY)) {
							inputWidget = DocumentImpl.getBizKeyAttribute().getDefaultInputWidget();
						}
						else if (fullyQualifiedColumnBinding.endsWith(Bean.ORDINAL_NAME)) {
							inputWidget = DocumentImpl.getBizOrdinalAttribute().getDefaultInputWidget();
						}
						else {
							TargetMetaData target = BindUtil.getMetaDataForBinding(customer, 
																					module, 
																					document, 
																					fullyQualifiedColumnBinding);
							Attribute attribute = target.getAttribute();
							if (attribute != null) {
								inputWidget = attribute.getDefaultInputWidget();
							}
						}
					}
					
					if (inputWidget == null) {
						throw new MetaDataException("Could not determine the input widget to use from grid column " + grid.getBinding() + '.' + columnBinding);
					}
	
					String definedBinding = inputWidget.getBinding();
					try {
						// Temporarily set the binding to the datagrid column binding
						inputWidget.setBinding(columnBinding);
						visitWidget(inputWidget, gridVisible, gridColumnEnabled);
						visitedDataGridBoundColumn(boundColumn, gridVisible, gridEnabled);
					}
					finally {
						inputWidget.setBinding(definedBinding);
					}
				}
				else {
					DataGridContainerColumn containerColumn = (DataGridContainerColumn) column;
					visitDataGridContainerColumn(containerColumn, parentVisible, parentEnabled);
					
					for (MetaData containedWidget : containerColumn.getWidgets()) {
						visitWidget(containedWidget, parentVisible, parentEnabled);
					}
					
					visitedDataGridContainerColumn(containerColumn, parentVisible, parentEnabled);
				}
			}
			visitAddableActions(grid, parentVisible, parentEnabled);
			visitEditableActions(grid, parentVisible, parentEnabled);
			visitRemovableActions(grid, parentVisible, parentEnabled);
			visitSelectableActions(grid, parentVisible, parentEnabled);
			visitedDataGrid(grid, parentVisible, parentEnabled);
		}
		else if (widget instanceof PickList) {
			PickList list = (PickList) widget;
			visitPickList(list, parentVisible, parentEnabled);
			boolean listVisible = parentVisible && visible(list);
			for (PickListColumn column : list.getColumns()) {
				visitPickListColumn(column, listVisible, parentEnabled);
			}
			visitedPickList(list, parentVisible, parentEnabled);
		}
		// input
		else if (widget instanceof CheckBox) {
			CheckBox box = (CheckBox) widget;
			visitCheckBox(box, parentVisible, parentEnabled);
			visitFocusable(box, parentVisible, parentEnabled);
			visitChangeable(box, parentVisible, parentEnabled);
			visitedCheckBox(box, parentVisible, parentEnabled);
		}
		else if (widget instanceof CheckMembership) {
			CheckMembership membership = (CheckMembership) widget;
			visitCheckMembership(membership, parentVisible, parentEnabled);
			visitFocusable(membership, parentVisible, parentEnabled);
			visitChangeable(membership, parentVisible, parentEnabled);
			visitedCheckMembership(membership, parentVisible, parentEnabled);
		}
		else if (widget instanceof ColourPicker) {
			ColourPicker colour = (ColourPicker) widget;
			visitColourPicker(colour, parentVisible, parentEnabled);
			visitFocusable(colour, parentVisible, parentEnabled);
			visitChangeable(colour, parentVisible, parentEnabled);
			visitedColourPicker(colour, parentVisible, parentEnabled);
		}
		else if (widget instanceof Combo) {
			Combo combo = (Combo) widget;
			visitCombo(combo, parentVisible, parentEnabled);
			visitFocusable(combo, parentVisible, parentEnabled);
			visitChangeable(combo, parentVisible, parentEnabled);
			visitedCombo(combo, parentVisible, parentEnabled);
		}
		else if (widget instanceof ContentImage) {
			ContentImage image = (ContentImage) widget;
			visitContentImage(image, parentVisible, parentEnabled);
		}
		else if (widget instanceof ContentLink) {
			ContentLink link = (ContentLink) widget;
			visitContentLink(link, parentVisible, parentEnabled);
			visitParameterizable(link, parentVisible, parentEnabled);
		}
		else if (widget instanceof HTML) {
			HTML html = (HTML) widget;
			visitHTML(html, parentVisible, parentEnabled);
		}
		else if (widget instanceof ListMembership) {
			ListMembership membership = (ListMembership) widget;
			visitListMembership(membership, parentVisible, parentEnabled);
			visitChangeable(membership, parentVisible, parentEnabled);
			visitedListMembership(membership, parentVisible, parentEnabled);
		}
		else if (widget instanceof Comparison) {
			Comparison comparison = (Comparison) widget;
			visitComparison(comparison, parentVisible, parentEnabled);
		}
		// subclass of Lookup, so test for it first
		else if (widget instanceof LookupDescription) {
			LookupDescription lookup = (LookupDescription) widget;
			visitLookupDescription(lookup, parentVisible, parentEnabled);
			visitLookupActions(lookup, parentVisible, parentEnabled);
			visitFilterable(lookup, parentVisible, parentEnabled);
			visitedLookupDescription(lookup, parentVisible, parentEnabled);
		}
		else if (widget instanceof Lookup) {
			Lookup lookup = (Lookup) widget;
			visitLookup(lookup, parentVisible, parentEnabled);
			visitLookupActions(lookup, parentVisible, parentEnabled);
			visitFilterable(lookup, parentVisible, parentEnabled);
			visitedLookup(lookup, parentVisible, parentEnabled);
		}
		else if (widget instanceof Password) {
			Password password = (Password) widget;
			visitPassword(password, parentVisible, parentEnabled);
			visitFocusable(password, parentVisible, parentEnabled);
			visitChangeable(password, parentVisible, parentEnabled);
			visitedPassword(password, parentVisible, parentEnabled);
		}
		else if (widget instanceof Radio) {
			Radio radio = (Radio) widget;
			visitRadio(radio, parentVisible, parentEnabled);
			visitFocusable(radio, parentVisible, parentEnabled);
			visitChangeable(radio, parentVisible, parentEnabled);
			visitedRadio(radio, parentVisible, parentEnabled);
		}
		else if (widget instanceof RichText) {
			RichText text = (RichText) widget;
			visitRichText(text, parentVisible, parentEnabled);
			visitFocusable(text, parentVisible, parentEnabled);
			visitChangeable(text, parentVisible, parentEnabled);
			visitedRichText(text, parentVisible, parentEnabled);
		}
		else if (widget instanceof Slider) {
			Slider slider = (Slider) widget;
			visitSlider(slider, parentVisible, parentEnabled);
			visitFocusable(slider, parentVisible, parentEnabled);
			visitChangeable(slider, parentVisible, parentEnabled);
			visitedSlider(slider, parentVisible, parentEnabled);
		}
		else if (widget instanceof Spinner) {
			Spinner spinner = (Spinner) widget;
			visitSpinner(spinner, parentVisible, parentEnabled);
			visitFocusable(spinner, parentVisible, parentEnabled);
			visitChangeable(spinner, parentVisible, parentEnabled);
			visitedSpinner(spinner, parentVisible, parentEnabled);
		}
		else if (widget instanceof TextArea) {
			TextArea text = (TextArea) widget;
			visitTextArea(text, parentVisible, parentEnabled);
			visitFocusable(text, parentVisible, parentEnabled);
			visitChangeable(text, parentVisible, parentEnabled);
			visitedTextArea(text, parentVisible, parentEnabled);
		}
		else if (widget instanceof TextField) {
			TextField text = (TextField) widget;
			visitTextField(text, parentVisible, parentEnabled);
			visitFocusable(text, parentVisible, parentEnabled);
			visitChangeable(text, parentVisible, parentEnabled);
			visitedTextField(text, parentVisible, parentEnabled);
		}
		else if (widget instanceof Inject) {
			Inject inject = (Inject) widget;
			visitInject(inject, parentVisible, parentEnabled);
		}
		else {
			throw new MetaDataException("Widget " + widget + " not catered for.");
		}
	}

	private void visitContainer(Container container, 
									boolean parentVisible,
									boolean parentEnabled) {
		if (container == view) {
			visitView();
			for (MetaData widget : container.getContained()) {
				visitWidget(widget, parentVisible, parentEnabled);
			}
			visitActions(view);
			visitedView();
		}
		else if (container instanceof Tab) {
			Tab tab = (Tab) container;
			visitTab(tab, parentVisible, parentEnabled);
			boolean tabVisible = parentVisible && visible(tab);
			boolean tabEnabled = parentEnabled && enabled(tab);
			for (MetaData widget : container.getContained()) {
				visitWidget(widget, tabVisible, tabEnabled);
			}
			visitedTab(tab, parentVisible, parentEnabled);
		}
		else if (container instanceof VBox) {
			VBox vbox = (VBox) container;
			visitVBox(vbox, parentVisible, parentEnabled);
			boolean vboxVisible = parentVisible && visible(vbox);
			for (MetaData widget : container.getContained()) {
				visitWidget(widget, vboxVisible, parentEnabled);
			}
			visitedVBox(vbox, parentVisible, parentEnabled);
		}
		else if (container instanceof HBox) {
			HBox hbox = (HBox) container;
			visitHBox(hbox, parentVisible, parentEnabled);
			boolean hboxVisible = parentVisible && visible(hbox);
			for (MetaData widget : container.getContained()) {
				visitWidget(widget, hboxVisible, parentEnabled);
			}
			visitedHBox(hbox, parentVisible, parentEnabled);
		}
		else {
			throw new MetaDataException("Container " + container + " not catered for.");
		}
	}
	
	protected void visitChangeable(Changeable changeable,
									boolean parentVisible,
									boolean parentEnabled) {
		List<EventAction> actions = changeable.getChangedActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnChangedEventHandler(changeable, parentVisible, parentEnabled);
			visitActions(changeable, actions, parentVisible, parentEnabled);
			visitedOnChangedEventHandler(changeable, parentVisible, parentEnabled);
		}
	}

	protected void visitFocusable(Focusable focusable,
									boolean parentVisible,
									boolean parentEnabled) {
		List<EventAction> actions = focusable.getFocusActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnFocusEventHandler(focusable, parentVisible, parentEnabled);
			visitActions(focusable, actions, parentVisible, parentEnabled);
			visitedOnFocusEventHandler(focusable, parentVisible, parentEnabled);
		}
		actions = focusable.getBlurActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnBlurEventHandler(focusable, parentVisible, parentEnabled);
			visitActions(focusable, actions, parentVisible, parentEnabled);
			visitedOnBlurEventHandler(focusable, parentVisible, parentEnabled);
		}
	}
	
	private void visitLookupActions(Lookup lookup,
										boolean parentVisible,
										boolean parentEnabled) {
		visitAddableActions(lookup, parentVisible, parentEnabled);
		visitEditableActions(lookup, parentVisible, parentEnabled);

		List<EventAction> actions = lookup.getPickedActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnPickedEventHandler(lookup, parentVisible, parentEnabled);
			visitActions(lookup, actions, parentVisible, parentEnabled);
			visitedOnPickedEventHandler(lookup, parentVisible, parentEnabled);
		}
		actions = lookup.getClearedActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnClearedEventHandler(lookup, parentVisible, parentEnabled);
			visitActions(lookup, actions, parentVisible, parentEnabled);
			visitedOnClearedEventHandler(lookup, parentVisible, parentEnabled);
		}
	}

	private void visitAddableActions(Addable addable,
										boolean parentVisible,
										boolean parentEnabled) {
		List<EventAction> actions = addable.getAddedActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnAddedEventHandler(addable, parentVisible, parentEnabled);
			visitActions(addable, actions, parentVisible, parentEnabled);
			visitedOnAddedEventHandler(addable, parentVisible, parentEnabled);
		}
	}

	private void visitEditableActions(Editable editable,
										boolean parentVisible,
										boolean parentEnabled) {
		List<EventAction> actions = editable.getEditedActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnEditedEventHandler(editable, parentVisible, parentEnabled);
			visitActions(editable, actions, parentVisible, parentEnabled);
			visitedOnEditedEventHandler(editable, parentVisible, parentEnabled);
		}
	}

	private void visitRemovableActions(Removable removable,
										boolean parentVisible,
										boolean parentEnabled) {
		List<EventAction> actions = removable.getRemovedActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnRemovedEventHandler(removable, parentVisible, parentEnabled);
			visitActions(removable, actions, parentVisible, parentEnabled);
			visitedOnRemovedEventHandler(removable, parentVisible, parentEnabled);
		}
	}

	private void visitSelectableActions(Selectable selectable,
											boolean parentVisible,
											boolean parentEnabled) {
		List<EventAction> actions = selectable.getSelectedActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnSelectedEventHandler(selectable, parentVisible, parentEnabled);
			visitActions(selectable, actions, parentVisible, parentEnabled);
			visitedOnSelectedEventHandler(selectable, parentVisible, parentEnabled);
		}
	}

	private void visitActions(EventSource source, 
								List<EventAction> actions, 
								boolean parentVisible, 
								boolean parentEnabled) {
		if (actions != null) {
			for (EventAction action : actions) {
				if (action instanceof RerenderEventAction) {
					visitRerenderEventAction((RerenderEventAction) action, source, parentVisible, parentEnabled);
				}
				else if (action instanceof ServerSideActionEventAction) {
					visitServerSideActionEventAction((ServerSideActionEventAction) action, parentVisible, parentEnabled);
				}
				else if (action instanceof SetDisabledEventAction) {
					visitSetDisabledEventAction((SetDisabledEventAction) action, parentVisible, parentEnabled);
				}
				else if (action instanceof SetInvisibleEventAction) {
					visitSetInvisibleEventAction((SetInvisibleEventAction) action, parentVisible, parentEnabled);
				}
				else if (action instanceof ToggleDisabledEventAction) {
					visitToggleDisabledEventAction((ToggleDisabledEventAction) action, parentVisible, parentEnabled);
				}
				else if (action instanceof ToggleVisibilityEventAction) {
					visitToggleVisibilityEventAction((ToggleVisibilityEventAction) action, parentVisible, parentEnabled);
				}
				else {
					throw new MetaDataException(action + " is not catered for in ViewVisitor.visitChangeable()");
				}
			}
		}
	}
}
