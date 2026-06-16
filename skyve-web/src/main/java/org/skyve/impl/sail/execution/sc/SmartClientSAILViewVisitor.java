package org.skyve.impl.sail.execution.sc;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.component.Component;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.DefaultWidget;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.impl.sail.execution.sc.Locator.InputType;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.Util;

/**
 * Responsible for traversing a SmartClient (SC) view definition and
 * generating locators for all supported user interactions.
 * <p>
 * It processes containers, forms, widgets, and actions within the view,
 * using contextual metadata to build locators for SAIL automation.
 * 
 * @author simeonsolomou
 */
public class SmartClientSAILViewVisitor {

	private CustomerImpl customer;
	private ModuleImpl module;
	private DocumentImpl document;
	private ViewImpl view;
	private String currentUxUi;

	private SmartClientAutomationContext automationContext;

	private String windowPrefix;

	int containerIndex = 0;
	int formIndex = 0;

	private boolean visitingForm = false;

	/**
	 * Creates a SmartClient view visitor that records interaction locators into the supplied context.
	 *
	 * @param user the current user
	 * @param module the module metadata being visited
	 * @param document the document metadata being visited
	 * @param view the view metadata being traversed
	 * @param uxui the active UX/UI name
	 * @param automationContext the automation context receiving generated locators
	 * @param windowPrefix the SmartClient window locator prefix
	 */
	public SmartClientSAILViewVisitor(
			User user,
			Module module,
			Document document,
			View view,
			String uxui,
			SmartClientAutomationContext automationContext,
			String windowPrefix) {
		this.customer = (CustomerImpl) user.getCustomer();
		this.module = (ModuleImpl) module;
		this.document = (DocumentImpl) document;
		this.view = (ViewImpl) view;
		this.currentUxUi = uxui;
		this.automationContext = automationContext;
		this.windowPrefix = windowPrefix;
	}

	/**
	 * Traverses the configured view and records supported automation locators in the current context.
	 */
	public final void visit() {
		visitContainer(view);
	}

	/**
	 * Visits a supported container type and dispatches traversal to contained metadata.
	 *
	 * @param container the container metadata to traverse
	 */
	private void visitContainer(Container container) {
		if (container == view) {
			for (MetaData widget : container.getContained()) {
				visitWidget(widget);
			}

			visitActions();
		}
		else if (container instanceof Tab tab) {
			visitTab(tab);

			for (MetaData widget : container.getContained()) {
				visitWidget(widget);
			}
		}
		else if (container instanceof VBox) {
			for (MetaData widget : container.getContained()) {
				visitWidget(widget);
			}
		}
		else if (container instanceof HBox) {
			for (MetaData widget : container.getContained()) {
				visitWidget(widget);
			}
		}
		else {
			throw new MetaDataException(String.format("Container %s not catered for", container));
		}
	}

	/**
	 * Enters form traversal mode and resets the form-item index for locator generation.
	 */
	private final void visitForm() {
		formIndex = 0;

		visitingForm = true;
	}

	/**
	 * Leaves form traversal mode and advances the container index.
	 */
	private final void visitedForm() {
		containerIndex++;

		visitingForm = false;
	}

	/**
	 * Dispatches a metadata widget to the SmartClient locator visitor that matches its concrete widget type.
	 *
	 * @param widget the metadata widget to visit
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private void visitWidget(MetaData widget) {
		if (widget instanceof Container container) {
			visitContainer(container);
		}
		else if (widget instanceof Form form) {
			visitForm();

			for (FormRow row : form.getRows()) {
				for (FormItem item : row.getItems()) {
					MetaData itemWidget = item.getWidget();
					if (itemWidget instanceof DefaultWidget defaultWidget) {
						visitDefaultWidget(defaultWidget);
					}
					else {
						visitWidget(itemWidget);
					}

					formIndex++;
				}
			}

			visitedForm();
		}
		else if (widget instanceof TabPane tabPane) {
			for (Tab tab : tabPane.getTabs()) {
				visitContainer(tab);
			}
		}
		else if (widget instanceof Button button) {
			visitButton(button);
		}
		else if (widget instanceof ZoomIn zoomIn) {
			visitZoomIn(zoomIn);
		}
		else if (widget instanceof Geometry geometry) {
			visitGeometry(geometry);
		}
		else if (widget instanceof MapDisplay map) {
			visitMap(map);
		}
		else if (widget instanceof Link link) {
			visitLink(link);
		}
		else if (widget instanceof TreeGrid grid) {
			visitTreeGrid(grid);
		}
		else if (widget instanceof ListGrid grid) {
			visitListGrid(grid);
			visitedListGrid();
		}
		else if (widget instanceof ListRepeater) {
			visitedListRepeater();
		}
		else if (widget instanceof DataGrid grid) {
			String gridBindingPrefix = grid.getBinding();
			if (gridBindingPrefix == null) {
				gridBindingPrefix = "";
			}
			else {
				gridBindingPrefix += '.';
			}

			visitDataGrid(grid);
			visitedDataGrid();
		}
		else if (widget instanceof CheckBox box) {
			visitCheckBox(box);
		}
		else if (widget instanceof ColourPicker colour) {
			visitColourPicker(colour);
		}
		else if (widget instanceof Combo combo) {
			visitCombo(combo);
		}
		else if (widget instanceof ContentSignature signature) {
			visitContentSignature(signature);
		}
		else if (widget instanceof HTML html) {
			visitHTML(html);
		}
		else if (widget instanceof LookupDescription lookup) {
			visitLookupDescription(lookup);
		}
		else if (widget instanceof Password password) {
			visitPassword(password);
		}
		else if (widget instanceof Radio radio) {
			visitRadio(radio);
		}
		else if (widget instanceof RichText text) {
			visitRichText(text);
		}
		else if (widget instanceof Slider slider) {
			visitSlider(slider);
		}
		else if (widget instanceof Spinner spinner) {
			visitSpinner(spinner);
		}
		else if (widget instanceof TextArea text) {
			visitTextArea(text);
		}
		else if (widget instanceof TextField text) {
			visitTextField(text);
		}
		else if (widget instanceof Component component) {
			visitComponent(component);
		}
	}

	/**
	 * Resolves a default widget to its concrete input widget and visits the resolved widget.
	 *
	 * @param widget the default widget metadata
	 */
	private void visitDefaultWidget(DefaultWidget widget) {
		String binding = widget.getBinding();

		TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);

		Attribute attribute = target.getAttribute();
		if (attribute != null) {
			Bound defaultWidget = attribute.getDefaultInputWidget();
			String definedBinding = defaultWidget.getBinding();

			try {
				defaultWidget.setBinding(binding);
				visitWidget(defaultWidget);
			} finally {
				defaultWidget.setBinding(definedBinding);
			}
		}
	}

	/**
	 * Registers a locator for a form button action.
	 *
	 * @param button the button metadata
	 */
	private final void visitButton(Button button) {
		String actionName = button.getActionName();

		automationContext.put(actionName,
				new Locator(String.format(
						"//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[index=%d||Class=CanvasItem]/canvas/member[Class=BizButton||classIndex=0]",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex),
						Integer.valueOf(formIndex))));
	}

	/**
	 * Registers a locator for a zoom-in widget.
	 *
	 * @param zoomIn the zoom-in metadata
	 */
	private final void visitZoomIn(ZoomIn zoomIn) {
		String binding = zoomIn.getBinding();
		String identifier = String.format("%s.zoomIn", binding.replaceAll("\\.", "_"));

		if (visitingForm) {
			automationContext.put(identifier,
					new Locator(String.format(
							"//:DynamicForm[ID=\"%s_%s_edit_%d\"]//IButton[name=\"%s\"]",
							module.getName(),
							document.getName(),
							Integer.valueOf(containerIndex),
							identifier)));
		} else {
			automationContext.put(binding, new Locator(String.format("%s//IButton[name=\"%s\"]", windowPrefix, identifier)));
		}
	}

	/**
	 * Placeholder for map widget locator generation.
	 *
	 * @param map the map metadata
	 */
	@SuppressWarnings("java:S1172")
	private final void visitMap(MapDisplay map) {
		// TODO
	}

	/**
	 * Registers a locator for a geometry widget.
	 *
	 * @param geometry the geometry metadata
	 */
	private final void visitGeometry(Geometry geometry) {
		String binding = geometry.getBinding();

		automationContext.put(binding,
				new Locator(String.format(
						"//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[index=%d||Class=GeometryItem]/canvas/member[Class=DynamicForm||classIndex=0]/item[Class=TextItem||name=value]/element",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex),
						Integer.valueOf(formIndex)), InputType.TEXT));
	}

	/**
	 * Placeholder for tree-grid locator generation.
	 *
	 * @param grid the tree-grid metadata
	 */
	@SuppressWarnings("java:S1172")
	private final void visitTreeGrid(TreeGrid grid) {
		// TODO
	}

	/**
	 * Placeholder for link locator generation.
	 *
	 * @param link the link metadata
	 */
	@SuppressWarnings("java:S1172")
	private final void visitLink(Link link) {
		// TODO
	}

	/**
	 * Registers list-grid action and selection locators.
	 *
	 * @param grid the list-grid metadata
	 */
	private final void visitListGrid(ListGrid grid) {
		String key = NavigateList.listGridIdentifier(
				automationContext,
				module.getName(),
				grid.getQueryName(),
				document.getName(),
				grid.getModelName());

		visitListGridNew(key);
		visitListGridZoom(key);
		visitListGridSelect(key);
	}

	/**
	 * Marks completion of list-grid traversal by advancing the container index.
	 */
	private final void visitedListGrid() {
		containerIndex++;
	}

	/**
	 * Marks completion of list-repeater traversal by advancing the container index.
	 */
	private final void visitedListRepeater() {
		containerIndex++;
	}

	/**
	 * Registers the list-grid new-action locator.
	 *
	 * @param key the list-grid automation key
	 */
	private final void visitListGridNew(String key) {
		automationContext.put(String.format("%s.new", key),
				new Locator(String.format(
						"//:VLayout[ID=\"%s_%s_edit_%d\"]/member[Class=Toolstrip||classIndex=0]/member[Class=ToolstripButton||name=new]/icon",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex))));
	}

	/**
	 * Registers the list-grid zoom-action locator.
	 *
	 * @param key the list-grid automation key
	 */
	private final void visitListGridZoom(String key) {
		automationContext.put(String.format("%s.zoom", key),
				new Locator(String.format(
						"//:VLayout[ID=\"%s_%s_edit_%d\"]/member[Class=Toolstrip||classIndex=0]/member[Class=ToolstripButton||name=zoom]/icon",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex))));
	}

	/**
	 * Registers the list-grid row-selection locator.
	 *
	 * @param key the list-grid automation key
	 */
	private final void visitListGridSelect(String key) {
		automationContext.put(String.format("%s.select", key),
				new Locator(String.format(
						"//BizListGrid[ID=\"%s_%s_edit_%d\"]/member[Class=ListGrid||classIndex=0]/body/row[%%d]/col[%%d]",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex))));
	}

	/**
	 * Registers the standard SmartClient data-grid toolbar and row-selection locators for the given binding.
	 *
	 * @param grid the data-grid metadata to visit
	 */
	private final void visitDataGrid(DataGrid grid) {
		String binding = grid.getBinding();

		visitDataGridNew(binding);
		visitDataGridZoom(binding);
		visitDataGridEdit(binding);
		visitDataGridRemove(binding);
		visitDataGridSelect(binding);
	}

	/**
	 * Marks completion of data-grid traversal by advancing the container index.
	 */
	private final void visitedDataGrid() {
		containerIndex++;
	}

	/**
	 * Registers the data-grid new-action locator.
	 *
	 * @param binding the data-grid binding
	 */
	private final void visitDataGridNew(String binding) {
		automationContext.put(String.format("%s.new", binding),
				new Locator(String.format(
						"//:VLayout[ID=\"%s_%s_edit_%d\"]/member[Class=Toolstrip]/member[Class=ToolstripButton||name=new]/icon",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex))));
	}

	/**
	 * Registers the data-grid zoom-action locator.
	 *
	 * @param binding the data-grid binding
	 */
	private final void visitDataGridZoom(String binding) {
		automationContext.put(String.format("%s.zoom", binding),
				new Locator(String.format(
						"//:VLayout[ID=\"%s_%s_edit_%d\"]/member[Class=Toolstrip]/member[Class=ToolstripButton||name=zoom]/icon",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex))));
	}

	/**
	 * Registers the data-grid edit-action locator.
	 *
	 * @param binding the data-grid binding
	 */
	private final void visitDataGridEdit(String binding) {
		automationContext.put(String.format("%s.edit", binding),
				new Locator(String.format(
						"//:VLayout[ID=\"%s_%s_edit_%d\"]/member[Class=Toolstrip]/member[Class=ToolstripButton||name=edit]/icon",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex))));
	}

	/**
	 * Registers the data-grid remove-action locator.
	 *
	 * @param binding the data-grid binding
	 */
	private final void visitDataGridRemove(String binding) {
		automationContext.put(String.format("%s.remove", binding),
				new Locator(String.format(
						"//:VLayout[ID=\"%s_%s_edit_%d\"]/member[Class=Toolstrip]/member[Class=ToolstripButton||name=deleteRemoveSelected]/icon",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex))));
	}

	/**
	 * Registers the data-grid row-selection locator.
	 *
	 * @param binding the data-grid binding
	 */
	private final void visitDataGridSelect(String binding) {
		automationContext.put(String.format("%s.select", binding),
				new Locator(String.format(
						"//BizDataGrid[ID=\"%s_%s_edit_%d\"]/member[Class=ListGrid||classIndex=0]/body/row[%%d]/col[%%d]",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex))));
	}

	/**
	 * Registers a locator for a checkbox widget.
	 *
	 * @param checkBox the checkbox metadata
	 */
	private final void visitCheckBox(CheckBox checkBox) {
		String binding = checkBox.getBinding();

		automationContext.put(binding,
				new Locator(String.format("//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[index=%d||Class=CheckboxItem]",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex),
						Integer.valueOf(formIndex)), InputType.CHECKBOX));
	}

	/**
	 * Registers a locator for a colour-picker widget.
	 *
	 * @param colour the colour-picker metadata
	 */
	private final void visitColourPicker(ColourPicker colour) {
		String binding = colour.getBinding();

		automationContext.put(binding,
				new Locator(String.format("//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[index=%d||Class=ColorItem]",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex),
						Integer.valueOf(formIndex)), InputType.TEXT));
	}

	/**
	 * Registers a locator for a combo widget.
	 *
	 * @param combo the combo metadata
	 */
	private final void visitCombo(Combo combo) {
		String binding = combo.getBinding();

		automationContext.put(binding,
				new Locator(String.format("//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[index=%d||Class=SelectItem]",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex),
						Integer.valueOf(formIndex)), InputType.COMBO));
	}

	/**
	 * Placeholder for content-signature locator generation.
	 *
	 * @param contentSignature the content-signature metadata
	 */
	@SuppressWarnings("java:S1172")
	private final void visitContentSignature(ContentSignature contentSignature) {
		// TODO
	}

	/**
	 * Placeholder for HTML widget locator generation.
	 *
	 * @param html the HTML metadata
	 */
	@SuppressWarnings("java:S1172")
	private final void visitHTML(HTML html) {
		// TODO
	}

	/**
	 * Registers a locator for a lookup-description widget.
	 *
	 * @param lookup the lookup-description metadata
	 */
	private final void visitLookupDescription(LookupDescription lookup) {
		String binding = lookup.getBinding();

		automationContext.put(binding,
				new Locator(String.format(
						"//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[index=%d||Class=BizLookupDescriptionItem]/canvas",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex),
						Integer.valueOf(formIndex)), InputType.LOOKUP_DESCRIPTION));
	}

	/**
	 * Registers a locator for a password widget.
	 *
	 * @param password the password metadata
	 */
	private final void visitPassword(Password password) {
		String binding = password.getBinding();

		automationContext.put(binding,
				new Locator(String.format("//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[index=%d||Class=PasswordItem]/element",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex),
						Integer.valueOf(formIndex)), InputType.TEXT));
	}

	/**
	 * Registers a locator for a radio widget.
	 *
	 * @param radio the radio metadata
	 */
	private final void visitRadio(Radio radio) {
		String binding = radio.getBinding();

		automationContext.put(binding,
				new Locator(String.format(
						"//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[index=%d||Class=RadioGroupItem]",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex),
						Integer.valueOf(formIndex)), InputType.RADIO));
	}

	/**
	 * Registers a locator for a rich-text widget.
	 *
	 * @param text the rich-text metadata
	 */
	private final void visitRichText(RichText text) {
		String binding = text.getBinding();

		automationContext.put(binding,
				new Locator(String.format(
						"//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[index=%d||Class=RichTextItem]/canvas/editArea",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex),
						Integer.valueOf(formIndex)), InputType.TEXT));
	}

	/**
	 * Placeholder for slider widget locator generation.
	 *
	 * @param slider the slider metadata
	 */
	@SuppressWarnings("java:S1172")
	private final void visitSlider(Slider slider) {
		// TODO
	}

	/**
	 * Registers a locator for a spinner widget.
	 *
	 * @param spinner the spinner metadata
	 */
	private final void visitSpinner(Spinner spinner) {
		String binding = spinner.getBinding();

		automationContext.put(binding,
				new Locator(String.format(
						"//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[index=%d||Class=SpinnerItem]/element",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex),
						Integer.valueOf(formIndex)), InputType.TEXT));
	}

	/**
	 * Registers a locator for a text-area widget.
	 *
	 * @param text the text-area metadata
	 */
	private final void visitTextArea(TextArea text) {
		String binding = text.getBinding();

		automationContext.put(binding,
				new Locator(String.format(
						"//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[index=%d||Class=TextItem]/element",
						module.getName(),
						document.getName(),
						Integer.valueOf(containerIndex),
						Integer.valueOf(formIndex)), InputType.TEXT));
	}

	/**
	 * Registers a text-field locator, selecting the correct sub-input for date/time-oriented attribute types.
	 *
	 * @param text the text-field metadata to visit
	 */
	private final void visitTextField(TextField text) {
		String binding = text.getBinding();

		Attribute attribute = BindUtil.getMetaDataForBinding(customer, module, document, binding).getAttribute();
		if (attribute != null) {
			AttributeType attributeType = attribute.getAttributeType();
			switch (attributeType) {
				case date:
				case dateTime:
				case timestamp:
					automationContext.put(binding,
							new Locator(String.format(
									"//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[index=%d]/item[name=dateTextField]/element",
									module.getName(),
									document.getName(),
									Integer.valueOf(containerIndex),
											Integer.valueOf(formIndex)),
									InputType.TEXT));
					break;
				case time:
					automationContext.put(binding,
							new Locator(String.format(
									"//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[index=%d]/item[name=timeTextField]/element",
									module.getName(),
									document.getName(),
									Integer.valueOf(containerIndex),
											Integer.valueOf(formIndex)),
									InputType.TEXT));
	
					break;
				default:
					automationContext.put(binding,
							new Locator(String.format(
									"//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[index=%d||Class=TextItem]/element",
									module.getName(),
									document.getName(),
									Integer.valueOf(containerIndex),
									Integer.valueOf(formIndex)), InputType.TEXT));
					break;
			}
		}
	}

	/**
	 * Visits a reusable component fragment by traversing its contained metadata.
	 *
	 * @param component the component metadata defining the fragment
	 */
	private void visitComponent(Component component) {
		for (MetaData widget : component.getFragment(customer, currentUxUi).getContained()) {
			visitWidget(widget);
		}
	}

	/**
	 * Visits all view actions and registers corresponding locators.
	 */
	private final void visitActions() {
		String name = view.getName();

		for (Action action : view.getActions()) {
			visit(name, (ActionImpl) action);
		}
	}

	/**
	 * Dispatches an action to implicit-action expansion or custom-action handling.
	 *
	 * @param viewName the current view name
	 * @param action the action metadata definition
	 */
	private void visit(String viewName, ActionImpl action) {
		ImplicitActionName implicitName = action.getImplicitName();
		if (implicitName != null) {
			visit(viewName, implicitName, action);
		} else {
			visitCustomAction(action);
		}
	}

	/**
	 * Expands implicit actions into the SmartClient locator set appropriate for the current view type.
	 *
	 * @param viewName the current view name
	 * @param implicitName the implicit action being expanded
	 * @param action the action metadata definition
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private void visit(String viewName, ImplicitActionName implicitName, ActionImpl action) {
		if (ImplicitActionName.DEFAULTS.equals(implicitName)) {
			if (ViewType.list.toString().equals(viewName)) {
				visit(viewName, ImplicitActionName.New, action);
			} else {
				for (ImplicitActionName value : ImplicitActionName.values()) {
					if (ImplicitActionName.DEFAULTS != value
							&& ImplicitActionName.New != value
							&& ImplicitActionName.Report != value
							&& ImplicitActionName.BizExport != value
							&& ImplicitActionName.BizImport != value
							&& ImplicitActionName.Download != value
							&& ImplicitActionName.Upload != value
							&& ImplicitActionName.Navigate != value
							&& ImplicitActionName.Print != value) {
						visit(viewName, value, action);
					}
				}
			}
		} else if (ImplicitActionName.Remove.equals(implicitName)) {
			visitRemoveAction();
		} else if (ImplicitActionName.ZoomOut.equals(implicitName)) {
			visitZoomOutAction();
		} else if (ImplicitActionName.OK.equals(implicitName)) {
			visitOKAction();
		} else if (ImplicitActionName.Save.equals(implicitName)) {
			visitSaveAction();
		} else if (ImplicitActionName.Cancel.equals(implicitName)) {
			visitCancelAction();
		} else if (ImplicitActionName.Delete.equals(implicitName)) {
			visitDeleteAction();
		} else if (ImplicitActionName.Add.equals(implicitName)) {
			visitAddAction();
		} else if (ImplicitActionName.Edit.equals(implicitName)) {
			visitEditAction();
		}
	}

	/**
	 * Registers the implicit remove-action locator.
	 */
	private final void visitRemoveAction() {
		automationContext.put(ImplicitActionName.Remove.toString(),
				new Locator(String.format("%s//IButton[actionName=\"Remove\"]", windowPrefix)));
	}

	/**
	 * Registers the implicit zoom-out action locator.
	 */
	private final void visitZoomOutAction() {
		automationContext.put(ImplicitActionName.ZoomOut.toString(),
				new Locator(String.format("%s//IButton[actionName=\"ZoomOut\"]", windowPrefix)));
	}

	/**
	 * Registers the implicit OK action locator.
	 */
	private final void visitOKAction() {
		automationContext.put(ImplicitActionName.OK.toString(),
				new Locator(String.format("%s//IButton[actionName=\"OK\"]", windowPrefix)));
	}

	/**
	 * Registers the implicit save action locator.
	 */
	private final void visitSaveAction() {
		automationContext.put(ImplicitActionName.Save.toString(),
				new Locator(String.format("%s//IButton[actionName=\"Save\"]", windowPrefix)));
	}

	/**
	 * Registers the implicit cancel action locator.
	 */
	private final void visitCancelAction() {
		automationContext.put(ImplicitActionName.Cancel.toString(),
				new Locator(String.format("%s//IButton[actionName=\"Cancel\"]", windowPrefix)));
	}

	/**
	 * Registers the implicit delete action locator.
	 */
	private final void visitDeleteAction() {
		automationContext.put(ImplicitActionName.Delete.toString(),
				new Locator(String.format("%s//IButton[actionName=\"Delete\"]", windowPrefix)));
	}

	/**
	 * Registers the implicit add action locator.
	 */
	private final void visitAddAction() {
		automationContext.put(ImplicitActionName.Add.toString(),
				new Locator(String.format("%s//IButton[actionName=\"Add\"]", windowPrefix)));
	}

	/**
	 * Registers the implicit edit action locator.
	 */
	private final void visitEditAction() {
		automationContext.put(ImplicitActionName.Edit.toString(),
				new Locator(String.format("%s//IButton[actionName=\"Edit\"]", windowPrefix)));
	}

	/**
	 * Registers a custom action locator by action name.
	 *
	 * @param action the custom action metadata definition
	 */
	private final void visitCustomAction(ActionImpl action) {
		String actionName = action.getName();

		automationContext.put(actionName, new Locator(String.format("%s//IButton[actionName=\"%s\"]", windowPrefix, actionName)));
	}

	/**
	 * Registers a tab-selection locator by localized tab title.
	 *
	 * @param tab the tab metadata definition
	 */
	private final void visitTab(Tab tab) {
		String path = tab.getTitle();

		// Check if this tab path represents an internationalised value
		String internationalisedPath = Util.i18n(path);

		automationContext.put(String.format("%s Tab", internationalisedPath == null ? path : internationalisedPath),
				new Locator(String.format("%s//SimpleTabButton[title=\"%s\"]", windowPrefix, internationalisedPath == null ? path : internationalisedPath)));
	}
}
