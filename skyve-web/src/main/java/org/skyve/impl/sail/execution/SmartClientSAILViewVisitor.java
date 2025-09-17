package org.skyve.impl.sail.execution;

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
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.DefaultWidget;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
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
import org.skyve.impl.sail.execution.Locator.InputType;
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

public class SmartClientSAILViewVisitor {

	private CustomerImpl customer;
	private ModuleImpl module;
	private DocumentImpl document;
	private ViewImpl view;
	private String currentUxUi;

	private SmartClientAutomationContext automationContext;

	private String windowPrefix;

	Integer containerIndex;

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

	public final void visit() {
		visitContainer(view);
	}

	private void visitContainer(Container container) {
		if (container == view) {
			for (MetaData widget : container.getContained()) {
				visitWidget(widget);
			}

			visitActions();
		} else if (container instanceof Tab) {
			Tab tab = (Tab) container;
			visitTab(tab);

			for (MetaData widget : container.getContained()) {
				visitWidget(widget);
			}
		} else if (container instanceof VBox) {
			for (MetaData widget : container.getContained()) {
				visitWidget(widget);
			}
		} else if (container instanceof HBox) {
			for (MetaData widget : container.getContained()) {
				visitWidget(widget);
			}
		} else {
			throw new MetaDataException(String.format("Container %s not catered for", container));
		}
	}

	private final void visitForm() {
		incrementContainerIndex();
	}

	private void visitWidget(MetaData widget) {
		if (widget instanceof Container) {
			visitContainer((Container) widget);
		} else if (widget instanceof Form) {
			Form form = (Form) widget;

			visitForm();

			for (FormRow row : form.getRows()) {
				for (FormItem item : row.getItems()) {
					MetaData itemWidget = item.getWidget();
					if (itemWidget instanceof DefaultWidget) {
						visitDefaultWidget((DefaultWidget) itemWidget);
					} else {
						visitWidget(itemWidget);
					}
				}
			}
		} else if (widget instanceof TabPane) {
			TabPane tabPane = (TabPane) widget;
			for (Tab tab : tabPane.getTabs()) {
				visitContainer(tab);
			}
		} else if (widget instanceof Button) {
			Button button = (Button) widget;
			visitButton(button);
		} else if (widget instanceof ZoomIn) {
			ZoomIn zoomIn = (ZoomIn) widget;
			visitZoomIn(zoomIn);
		} else if (widget instanceof Geometry) {
			Geometry geometry = (Geometry) widget;

			visitGeometry(geometry);
		} else if (widget instanceof TreeGrid) {
			TreeGrid grid = (TreeGrid) widget;

			visitTreeGrid(grid);
		} else if (widget instanceof ListGrid) {
			ListGrid grid = (ListGrid) widget;

			visitListGrid(grid);
		} else if (widget instanceof ListRepeater) {
			visitListRepeater();
		} else if (widget instanceof DataGrid) {
			DataGrid grid = (DataGrid) widget;

			String gridBindingPrefix = grid.getBinding();
			if (gridBindingPrefix == null) {
				gridBindingPrefix = "";
			} else {
				gridBindingPrefix += '.';
			}

			visitDataGrid(grid);
		} else if (widget instanceof CheckBox) {
			CheckBox box = (CheckBox) widget;

			visitCheckBox(box);
		} else if (widget instanceof ColourPicker) {
			ColourPicker colour = (ColourPicker) widget;

			visitColourPicker(colour);
		} else if (widget instanceof Combo) {
			Combo combo = (Combo) widget;

			visitCombo(combo);
		} else if (widget instanceof ContentImage) {
			ContentImage image = (ContentImage) widget;

			visitContentImage(image);
		} else if (widget instanceof ContentLink) {
			ContentLink link = (ContentLink) widget;

			visitContentLink(link);
		} else if (widget instanceof ContentSignature) {
			ContentSignature signature = (ContentSignature) widget;

			visitContentSignature(signature);
		} else if (widget instanceof LookupDescription) {
			LookupDescription lookup = (LookupDescription) widget;

			visitLookupDescription(lookup);
		} else if (widget instanceof Password) {
			Password password = (Password) widget;

			visitPassword(password);
		} else if (widget instanceof Radio) {
			Radio radio = (Radio) widget;

			visitRadio(radio);
		} else if (widget instanceof RichText) {
			RichText text = (RichText) widget;

			visitRichText(text);
		} else if (widget instanceof Slider) {
			Slider slider = (Slider) widget;

			visitSlider(slider);
		} else if (widget instanceof Spinner) {
			Spinner spinner = (Spinner) widget;

			visitSpinner(spinner);
		} else if (widget instanceof TextArea) {
			TextArea text = (TextArea) widget;

			visitTextArea(text);
		} else if (widget instanceof TextField) {
			TextField text = (TextField) widget;

			visitTextField(text);
		} else if (widget instanceof Component) {
			Component component = (Component) widget;

			visitComponent(component);
		}
	}

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

	private final void visitButton(Button button) {
		String actionName = button.getActionName();

		automationContext.put(actionName, new Locator(String.format("%s//IButton[actionName=\"%s\"]", windowPrefix, actionName)));
	}

	private final void visitZoomIn(ZoomIn zoomIn) {
		// TODO
	}

	private final void visitGeometry(Geometry geometry) {
		String binding = geometry.getBinding();

		automationContext.put(binding,
				new Locator(String.format(
						"//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[Class=GeometryItem||name=%s]/canvas/member[Class=DynamicForm||classIndex=0]/item[Class=TextItem||name=value]/element",
						module.getName(),
						document.getName(),
						containerIndex,
						binding.replaceAll("\\.", "_")), InputType.TEXT));
	}

	private final void visitTreeGrid(TreeGrid grid) {
		// TODO
	}

	private final void visitListGrid(ListGrid grid) {
		incrementContainerIndex();

		String key = NavigateList.listGridIdentifier(automationContext,
				module.getName(),
				grid.getQueryName(),
				document.getName(),
				grid.getModelName());

		visitListGridNew(key);
		visitListGridZoom(key);
		visitListGridSelect(key);
	}

	private final void visitListRepeater() {
		incrementContainerIndex();
	}

	private final void visitListGridNew(String key) {
		automationContext.put(String.format("%s.new", key),
				new Locator(String.format(
						"//:VLayout[ID=\"%s_%s_edit_%d\"]/member[Class=Toolstrip||classIndex=0]/member[Class=ToolstripButton||name=new]/icon",
						module.getName(),
						document.getName(),
						containerIndex)));
	}

	private final void visitListGridZoom(String key) {
		automationContext.put(String.format("%s.zoom", key),
				new Locator(String.format(
						"//:VLayout[ID=\"%s_%s_edit_%d\"]/member[Class=Toolstrip||classIndex=0]/member[Class=ToolstripButton||name=zoom]/icon",
						module.getName(),
						document.getName(),
						containerIndex)));
	}

	private final void visitListGridSelect(String key) {
		automationContext.put(String.format("%s.select", key),
				new Locator(String.format(
						"//BizListGrid[ID=\"%s_%s_edit_%d\"]/member[Class=ListGrid||classIndex=0]/body/row[%%d]/col[%%d]",
						module.getName(),
						document.getName(),
						containerIndex)));
	}

	private final void visitDataGrid(DataGrid grid) {
		incrementContainerIndex();

		String binding = grid.getBinding();

		visitDataGridNew(binding);
		visitDataGridZoom(binding);
		visitDataGridEdit(binding);
		visitDataGridRemove(binding);
		visitDataGridSelect(binding);
	}

	private final void visitDataGridNew(String binding) {
		automationContext.put(String.format("%s.new", binding),
				new Locator(String.format(
						"//:VLayout[ID=\"%s_%s_edit_%d\"]/member[Class=Toolstrip]/member[Class=ToolstripButton||name=new]/icon",
						module.getName(),
						document.getName(),
						containerIndex)));
	}

	private final void visitDataGridZoom(String binding) {
		automationContext.put(String.format("%s.zoom", binding),
				new Locator(String.format(
						"//:VLayout[ID=\"%s_%s_edit_%d\"]/member[Class=Toolstrip]/member[Class=ToolstripButton||name=zoom]/icon",
						module.getName(),
						document.getName(),
						containerIndex)));
	}

	private final void visitDataGridEdit(String binding) {
		automationContext.put(String.format("%s.edit", binding),
				new Locator(String.format(
						"//:VLayout[ID=\"%s_%s_edit_%d\"]/member[Class=Toolstrip]/member[Class=ToolstripButton||name=edit]/icon",
						module.getName(),
						document.getName(),
						containerIndex)));
	}

	private final void visitDataGridRemove(String binding) {
		automationContext.put(String.format("%s.remove", binding),
				new Locator(String.format(
						"//:VLayout[ID=\"%s_%s_edit_%d\"]/member[Class=Toolstrip]/member[Class=ToolstripButton||name=deleteRemoveSelected]/icon",
						module.getName(),
						document.getName(),
						containerIndex)));
	}

	private final void visitDataGridSelect(String binding) {
		automationContext.put(String.format("%s.select", binding),
				new Locator(String.format(
						"//BizDataGrid[ID=\"%s_%s_edit_%d\"]/member[Class=ListGrid||classIndex=0]/body/row[%%d]/col[%%d]",
						module.getName(),
						document.getName(),
						containerIndex)));
	}

	private final void visitCheckBox(CheckBox checkBox) {
		String binding = checkBox.getBinding();

		automationContext.put(binding,
				new Locator(String.format("//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[Class=CheckboxItem||name=%s]",
						module.getName(),
						document.getName(),
						containerIndex,
						binding.replaceAll("\\.", "_")), InputType.CHECKBOX));
	}

	private final void visitColourPicker(ColourPicker colour) {
		String binding = colour.getBinding();

		automationContext.put(binding,
				new Locator(String.format("//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[Class=ColorItem||name=%s]",
						module.getName(),
						document.getName(),
						containerIndex,
						binding.replaceAll("\\.", "_")), InputType.TEXT));
	}

	private final void visitCombo(Combo combo) {
		String binding = combo.getBinding();

		automationContext.put(binding,
				new Locator(String.format("//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[Class=SelectItem||name=%s]",
						module.getName(),
						document.getName(),
						containerIndex,
						binding.replaceAll("\\.", "_")), InputType.COMBO));
	}

	private final void visitContentImage(ContentImage contentImage) {
		// TODO
	}

	private final void visitContentLink(ContentLink contentLink) {
		// TODO
	}

	private final void visitContentSignature(ContentSignature contentSignature) {
		// TODO
	}

	private final void visitLookupDescription(LookupDescription lookup) {
		String binding = lookup.getBinding();

		automationContext.put(binding,
				new Locator(String.format(
						"//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[Class=BizLookupDescriptionItem||name=%s]/canvas",
						module.getName(),
						document.getName(),
						containerIndex,
						binding.replaceAll("\\.", "_")), InputType.LOOKUP_DESCRIPTION));
	}

	private final void visitPassword(Password password) {
		String binding = password.getBinding();

		automationContext.put(binding,
				new Locator(String.format("//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[Class=PasswordItem||name=%s]/element",
						module.getName(),
						document.getName(),
						containerIndex,
						binding.replaceAll("\\.", "_")), InputType.TEXT));
	}

	private final void visitRadio(Radio radio) {
		String binding = radio.getBinding();

		automationContext.put(binding,
				new Locator(String.format(
						"//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[Class=RadioGroupItem||name=%s]",
						module.getName(),
						document.getName(),
						containerIndex,
						binding.replaceAll("\\.", "_")), InputType.RADIO));
	}

	private final void visitRichText(RichText text) {
		String binding = text.getBinding();

		automationContext.put(binding,
				new Locator(String.format(
						"//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[Class=RichTextItem||name=%s]/canvas/editArea",
						module.getName(),
						document.getName(),
						containerIndex,
						binding.replaceAll("\\.", "_")), InputType.TEXT));
	}

	private final void visitSlider(Slider slider) {
		// TODO
	}

	private final void visitSpinner(Spinner spinner) {
		String binding = spinner.getBinding();

		automationContext.put(binding,
				new Locator(String.format("//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[Class=SpinnerItem||name=%s]/element",
						module.getName(),
						document.getName(),
						containerIndex,
						binding.replaceAll("\\.", "_")), InputType.TEXT));
	}

	private final void visitTextArea(TextArea text) {
		String binding = text.getBinding();

		automationContext.put(binding,
				new Locator(String.format("//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[Class=TextItem||name=%s]/element",
						module.getName(),
						document.getName(),
						containerIndex,
						binding.replaceAll("\\.", "_")), InputType.TEXT));
	}

	private final void visitTextField(TextField text) {
		String binding = text.getBinding();

		AttributeType attributeType = BindUtil.getMetaDataForBinding(customer, module, document, binding)
				.getAttribute()
				.getAttributeType();

		switch (attributeType) {
			case date:
			case dateTime:
			case timestamp:
				automationContext.put(binding,
						new Locator(String.format("//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[name=%s]/item[name=dateTextField]/element",
								module.getName(),
								document.getName(),
								containerIndex,
								binding.replaceAll("\\.", "_")), InputType.TEXT));
				break;
			case time:
				automationContext.put(binding,
						new Locator(String.format("//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[name=%s]/item[name=timeTextField]/element",
								module.getName(),
								document.getName(),
								containerIndex,
								binding.replaceAll("\\.", "_")), InputType.TEXT));

				break;
			default:
				automationContext.put(binding,
						new Locator(String.format("//DynamicForm[ID=\"%s_%s_edit_%d\"]/item[Class=TextItem||name=%s]/element",
								module.getName(),
								document.getName(),
								containerIndex,
								binding.replaceAll("\\.", "_")), InputType.TEXT));
				break;
		}
	}

	private void visitComponent(Component component) {
		for (MetaData widget : component.getFragment(customer, currentUxUi).getContained()) {
			visitWidget(widget);
		}
	}

	private final void visitActions() {
		String name = view.getName();

		for (Action action : view.getActions()) {
			visit(name, (ActionImpl) action);
		}
	}

	private void visit(String viewName, ActionImpl action) {
		ImplicitActionName implicitName = action.getImplicitName();
		if (implicitName != null) {
			visit(viewName, implicitName, action);
		} else {
			visitCustomAction(action);
		}
	}

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
		} else {
			throw new IllegalArgumentException(String.format("%s is not supported by ActionVisitor", implicitName));
		}
	}

	private final void visitRemoveAction() {
		automationContext.put(ImplicitActionName.Remove.toString(),
				new Locator(String.format("%s//IButton[actionName=\"Remove\"]", windowPrefix)));
	}

	private final void visitZoomOutAction() {
		automationContext.put(ImplicitActionName.ZoomOut.toString(),
				new Locator(String.format("%s//IButton[actionName=\"ZoomOut\"]", windowPrefix)));
	}

	private final void visitOKAction() {
		automationContext.put(ImplicitActionName.OK.toString(),
				new Locator(String.format("%s//IButton[actionName=\"OK\"]", windowPrefix)));
	}

	private final void visitSaveAction() {
		automationContext.put(ImplicitActionName.Save.toString(),
				new Locator(String.format("%s//IButton[actionName=\"Save\"]", windowPrefix)));
	}

	private final void visitCancelAction() {
		automationContext.put(ImplicitActionName.Cancel.toString(),
				new Locator(String.format("%s//IButton[actionName=\"Cancel\"]", windowPrefix)));
	}

	private final void visitDeleteAction() {
		automationContext.put(ImplicitActionName.Delete.toString(),
				new Locator(String.format("%s//IButton[actionName=\"Delete\"]", windowPrefix)));
	}

	private final void visitCustomAction(ActionImpl action) {
		String actionName = action.getName();

		automationContext.put(actionName, new Locator(String.format("%s//IButton[actionName=\"%s\"]", windowPrefix, actionName)));
	}

	private final void visitTab(Tab tab) {
		String path = tab.getTitle();

		// Check if this tab path represents an internationalised value
		String internationalisedPath = Util.i18n(path);

		automationContext.put(String.format("%s Tab", internationalisedPath == null ? path : internationalisedPath),
				new Locator(String.format("%s//SimpleTabButton[title=\"%s\"]", windowPrefix, internationalisedPath == null ? path : internationalisedPath)));
	}

	@SuppressWarnings("boxing")
	private void incrementContainerIndex() {
		containerIndex = containerIndex == null ? 0 : containerIndex + 1;
	}
}