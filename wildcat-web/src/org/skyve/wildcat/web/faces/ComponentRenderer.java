package org.skyve.wildcat.web.faces;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.el.MethodExpression;
import javax.el.ValueExpression;
import javax.faces.component.UICommand;
import javax.faces.component.UIComponent;
import javax.faces.component.UIComponentBase;
import javax.faces.component.UIOutput;
import javax.faces.component.UIParameter;
import javax.faces.component.UISelectItems;
import javax.faces.component.behavior.ClientBehavior;
import javax.faces.component.html.HtmlForm;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlInputTextarea;
import javax.faces.component.html.HtmlOutputLabel;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.component.html.HtmlPanelGroup;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

import org.primefaces.behavior.ajax.AjaxBehavior;
import org.primefaces.component.accordionpanel.AccordionPanel;
import org.primefaces.component.autocomplete.AutoComplete;
import org.primefaces.component.calendar.Calendar;
import org.primefaces.component.celleditor.CellEditor;
import org.primefaces.component.colorpicker.ColorPicker;
import org.primefaces.component.column.Column;
import org.primefaces.component.commandbutton.CommandButton;
import org.primefaces.component.commandlink.CommandLink;
import org.primefaces.component.datalist.DataList;
import org.primefaces.component.datatable.DataTable;
import org.primefaces.component.editor.Editor;
import org.primefaces.component.fieldset.Fieldset;
import org.primefaces.component.fileupload.FileUpload;
import org.primefaces.component.graphicimage.GraphicImage;
import org.primefaces.component.inputmask.InputMask;
import org.primefaces.component.inputtext.InputText;
import org.primefaces.component.inputtextarea.InputTextarea;
import org.primefaces.component.message.Message;
import org.primefaces.component.outputpanel.OutputPanel;
import org.primefaces.component.panel.Panel;
import org.primefaces.component.panelgrid.PanelGrid;
import org.primefaces.component.password.Password;
import org.primefaces.component.progressbar.ProgressBar;
import org.primefaces.component.row.Row;
import org.primefaces.component.selectbooleancheckbox.SelectBooleanCheckbox;
import org.primefaces.component.selectmanycheckbox.SelectManyCheckbox;
import org.primefaces.component.selectonemenu.SelectOneMenu;
import org.primefaces.component.selectoneradio.SelectOneRadio;
import org.primefaces.component.spacer.Spacer;
import org.primefaces.component.spinner.Spinner;
import org.primefaces.component.tabview.Tab;
import org.primefaces.component.tabview.TabView;
import org.primefaces.component.toolbar.Toolbar;
import org.primefaces.extensions.component.tristatecheckbox.TriStateCheckbox;
import org.primefaces.mobile.component.field.Field;

// TODO can't see boolean properties as value expressions - ie disabled/rendered never found if expression is "true" or "false"
public class ComponentRenderer {
	private static final char INDENT = '\t';
	StringBuilder out = new StringBuilder(1024);
	String indentation = "";
	
	public ComponentRenderer(UIComponent root) {
		renderComponent((UIComponentBase) root);
	}
	
	@Override
	public String toString() {
		return out.toString();
	}

	private void renderComponent(UIComponentBase component) {
		String tagName = null;
		Map<String, Object> attributes = new LinkedHashMap<>();
		Set<String> excludedAttributeNames = new LinkedHashSet<>();
		Set<String> tagAttributeNames = new LinkedHashSet<>();
		
		boolean hasNoChildTags = true;
		
		if (component instanceof AccordionPanel) {
			tagName = "p:accordionPanel";
			
			putValue(attributes, "style", ((AccordionPanel) component).getStyle());
		}
		else if (component instanceof AutoComplete) {
			tagName = "p:autoComplete";
			
			AutoComplete complete = (AutoComplete) component;
			putValue(attributes, "var", complete.getVar());
			putValueExpression(attributes, "itemLabel", component);
			putValueExpression(attributes, "itemValue", component);
			putMethodExpression(attributes, "completeMethod", complete.getCompleteMethod());
			putValue(attributes, "forceSelection", Boolean.valueOf(complete.isForceSelection()));
			putValue(attributes, "dropdown", Boolean.valueOf(complete.isDropdown()));
			putValueExpression(attributes, "scrollHeight", component);
			putValue(attributes, "style", complete.getStyle());

			
	    	tagAttributeNames.add("module");
	    	tagAttributeNames.add("query");
	    	tagAttributeNames.add("display");
		}
		else if (component instanceof Calendar) {
			tagName = "p:calendar";
			
			Calendar calendar = (Calendar) component;
			putValue(attributes, "mode", calendar.getMode());
			putValue(attributes, "showOn", calendar.getShowOn());
			putValue(attributes, "navigator", Boolean.valueOf(calendar.isNavigator()));
			putValue(attributes, "showButtonPanel", Boolean.valueOf(calendar.isShowButtonPanel()));
			putValue(attributes, "pattern", calendar.getPattern());
			putValue(attributes, "mask", calendar.getMask());
			putValue(attributes, "style", calendar.getStyle());

		}
		else if (component instanceof CellEditor) {
			tagName = "p:cellEditor";
		}
		else if (component instanceof ColorPicker) {
			tagName = "p:colorPicker";
			
			putValue(attributes, "var", ((ColorPicker) component).getStyle());
		}
		else if (component instanceof Column) {
			tagName = "p:column";

			putValueExpression(attributes, "sortBy", component);
			putValue(attributes, "style", ((Column) component).getStyle());
		}
		else if (component instanceof CommandButton) {
			tagName = "p:commandButton";
			
			CommandButton button = (CommandButton) component;
			putValue(attributes, "immediate", Boolean.valueOf(button.isImmediate()));
			putValue(attributes, "ajax", Boolean.valueOf(button.isAjax()));
			putValue(attributes, "process", button.getProcess());
			putValue(attributes, "title", button.getTitle());
			putValue(attributes, "update", button.getUpdate());
			if (button.isDisabled()) {
				putValue(attributes, "disabled", "true");
			}
			putValue(attributes, "style", button.getStyle());
		}
		else if (component instanceof CommandLink) {
			tagName = "p:commandLink";

			CommandLink link = (CommandLink) component;
			putValue(attributes, "immediate", Boolean.valueOf(link.isImmediate()));
			putValue(attributes, "ajax", Boolean.valueOf(link.isAjax()));
			putValue(attributes, "process", link.getProcess());
			putValue(attributes, "title", link.getTitle());
			putValue(attributes, "update", link.getUpdate());
			if (link.isDisabled()) {
				putValue(attributes, "disabled", "true");
			}
			putValue(attributes, "style", link.getStyle());
		}
		else if (component instanceof DataList) {
			tagName = "p:dataList";
			
			DataList list = (DataList) component;
			putValue(attributes, "var", list.getVar());
			putValue(attributes, "style", list.getStyle());
		}
		else if (component instanceof DataTable) {
			tagName = "p:dataTable";

			DataTable table = (DataTable) component;
			putValue(attributes, "var", table.getVar());
			putValue(attributes, "widgetVar", table.getWidgetVar());
			putValue(attributes, "selectionMode", table.getSelectionMode());
			putValueExpression(attributes, "rowKey", component);
			putValue(attributes, "style", table.getStyle());
		}
		else if (component instanceof Editor) {
			tagName = "p:editor";
			
			putValue(attributes, "style", ((Editor) component).getStyle());
		}
		else if (component instanceof Field) {
			tagName = "pm:field";
		}
		else if (component instanceof Fieldset) {
			tagName = "p:fieldset";
			
			Fieldset fs = (Fieldset) component;
			putValue(attributes, "legend", fs.getLegend());
			putValue(attributes, "style", fs.getStyle());
		}
		else if (component instanceof FileUpload) {
			tagName = "p:fileUpload";
			
			putValue(attributes, "style", ((FileUpload) component).getStyle());
		}
		else if (component instanceof GraphicImage) {
			tagName = "p:graphicImage";
			
			GraphicImage image = (GraphicImage) component;
			putValue(attributes, "url", image.getUrl());
			putValue(attributes, "style", image.getStyle());
		}
		else if (component instanceof HtmlForm) {
			tagName = "h:form";
			
			putValue(attributes, "style", ((HtmlForm) component).getStyle());
		}
		else if (component instanceof HtmlOutputLabel) {
			tagName = "h:outputLabel";
			
			HtmlOutputLabel label = (HtmlOutputLabel) component;
			putValue(attributes, "for", label.getFor());
			putValue(attributes, "style", label.getStyle());
		}
		else if (component instanceof HtmlOutputLink) {
			tagName = "h:outputLink";
			
			HtmlOutputLink link = (HtmlOutputLink) component;
			putValue(attributes, "target", link.getTarget());
			putValue(attributes, "style", link.getStyle());
		}
		else if (component instanceof HtmlOutputText) {
			tagName = "h:outputText";
			
			HtmlOutputText text = (HtmlOutputText) component;
			putValue(attributes, "escape", Boolean.valueOf(text.isEscape()));
			putValue(attributes, "style", text.getStyle());
		}
		else if (component instanceof HtmlPanelGroup) {
			tagName = "h:panelGroup";
			
			putValue(attributes, "style", ((HtmlPanelGroup) component).getStyle());

			excludedAttributeNames.add("type");
			excludedAttributeNames.add("managedBean");
		}
		else if (component instanceof InputMask) {
			tagName = "p:inputMask";
			
			InputMask mask = (InputMask) component;
			putValue(attributes, "target", mask.getMask());
			putValue(attributes, "style", mask.getStyle());
		}
		else if (component instanceof InputText) {
			tagName = "p:inputText";
			
			putValue(attributes, "style", ((InputText) component).getStyle());
		}
		else if (component instanceof InputTextarea) {
			tagName = "p:inputTextarea";
			
			putValue(attributes, "style", ((InputTextarea) component).getStyle());
		}
		else if (component instanceof Message) {
			tagName = "p:message";
			
			Message message = (Message) component;
			putValue(attributes, "for", message.getFor());
			putValue(attributes, "showDetail", Boolean.valueOf(message.isShowDetail()));
			putValue(attributes, "showSummary", Boolean.valueOf(message.isShowSummary()));
			putValue(attributes, "display", message.getDisplay());
			putValue(attributes, "style", message.getStyle());
		}
		else if (component instanceof OutputPanel) {
			tagName = "p:outputPanel";
			
			putValue(attributes, "style", ((OutputPanel) component).getStyle());
		}
		else if (component instanceof Panel) {
			tagName = "p:panel";
			
			Panel panel = (Panel) component;
			putValue(attributes, "header", panel.getHeader());
			putValue(attributes, "style", panel.getStyle());
		}
		else if (component instanceof PanelGrid) {
			tagName = "p:panelGrid";
			
			PanelGrid grid = (PanelGrid) component;
			putValue(attributes, "columns", Integer.valueOf(grid.getColumns()));
			putValue(attributes, "style", grid.getStyle());
		}
		else if (component instanceof Password) {
			tagName = "p:password";
			
			putValue(attributes, "style", ((Password) component).getStyle());
		}
		else if (component instanceof ProgressBar) {
			tagName = "p:progressBar";
			
			putValue(attributes, "style", ((ProgressBar) component).getStyle());
		}
		else if (component instanceof Row) {
			tagName = "p:row";
		}
		else if (component instanceof SelectBooleanCheckbox) {
			tagName = "p:selectBooleanCheckbox";
			
			SelectBooleanCheckbox check = (SelectBooleanCheckbox) component;
			putValue(attributes, "itemLabel", check.getItemLabel());
			putValue(attributes, "style", check.getStyle());
		}
		else if (component instanceof SelectManyCheckbox) {
			tagName = "p:selectManyCheckbox";
			
			putValue(attributes, "style", ((SelectManyCheckbox) component).getStyle());
		}
		else if (component instanceof SelectOneMenu) {
			tagName = "p:selectOneMenu";
			
			putValue(attributes, "style", ((SelectOneMenu) component).getStyle());
		}
		else if (component instanceof SelectOneRadio) {
			tagName = "p:selectManyRadio";

			putValue(attributes, "style", ((SelectOneRadio) component).getStyle());
		}
		else if (component instanceof Spacer) {
			tagName = "p:spacer";
			
			putValue(attributes, "style", ((Spacer) component).getStyle());
		}
		else if (component instanceof Spinner) {
			tagName = "p:spinner";
			
			putValue(attributes, "style", ((Spinner) component).getStyle());
		}
		else if (component instanceof Tab) {
			tagName = "p:tab";
			
			putValue(attributes, "title", ((Tab) component).getTitle());
		}
		else if (component instanceof TabView) {
			tagName = "p:tabView";
			
			putValue(attributes, "style", ((TabView) component).getStyle());
		}
		else if (component instanceof Toolbar) {
			tagName = "p:toolbar";
			
			putValue(attributes, "style", ((Toolbar) component).getStyle());
		}
		else if (component instanceof TriStateCheckbox) {
			tagName = "pe:triStateCheckbox";
			
			putValue(attributes, "style", ((TriStateCheckbox) component).getStyle());
		}
		else if (component instanceof UIOutput) {
			// do nothing - the value is what we want - notice there is no tagName assigned
		}
		else if (component instanceof UIParameter) {
			tagName = "f:parameter";
			
			UIParameter param = (UIParameter) component;
			putValue(attributes, "name", param.getName());
			putValueExpression(attributes, "value", component);
		}
		else if (component instanceof UISelectItems) {
			tagName = "f:selectItems";
			
			putValueExpression(attributes, "value", component);
		}
		else {
			tagName = component.getClass().getName();
		}
		
		if (tagName != null) {
			out.append(indentation).append('<').append(tagName);
			out.append(" id=\"").append(component.getId()).append('"');
//			String clientId = component.getClientId();
//			if (clientId != null) {
//				out.append(" clientId=\"").append(clientId).append('"');
//			}
		}
		
		if (component instanceof HtmlInputText) {
			HtmlInputText text = (HtmlInputText) component;
			if (text.isDisabled()) {
				putValue(attributes, "disabled", "true");
			}
			if (text.isReadonly()) {
				putValue(attributes, "readonly", "true");
			}
			putValue(attributes, "title", text.getTitle());
		}
		else if (component instanceof HtmlInputTextarea) {
			HtmlInputTextarea text = (HtmlInputTextarea) component;
			if (text.isDisabled()) {
				putValue(attributes, "disabled", "true");
			}
			if (text.isReadonly()) {
				putValue(attributes, "readonly", "true");
			}
			putValue(attributes, "title", text.getTitle());
		}
		else if (component instanceof UICommand) {
			UICommand command = (UICommand) component;
			
			putValue(attributes, "value", command.getValue());
			putMethodExpression(attributes, "action", command.getActionExpression());
		}
		else if (component instanceof UIOutput) {
			UIOutput output = (UIOutput) component;

			putValue(attributes, "value", output.getValue());
			putValueExpression(attributes, "value", component);

			if (tagName == null) { // no tag
				out.append(indentation).append(attributes.get("value")).append('\n');
				return;
			}

			Converter converter = output.getConverter();
			if (converter != null) {
				String converterName = converter.getClass().getSimpleName();
				if (converterName.endsWith("Converter")) {
					converterName = converterName.substring(0, converterName.length() - 9);
				}
				putValue(attributes, "converter", converterName);
			}
		}
		
		putValueExpression(attributes, "title", component);
		putValueExpression(attributes, "disabled", component);
		putValueExpression(attributes, "rendered", component);
		putValueExpression(attributes, "style", component);

		// Add specific attributes detected above
		for (String attributeName : attributes.keySet()) {
			out.append(' ').append(attributeName).append("=\"").append(attributes.get(attributeName)).append('"');
		}

		// Add general attributes
		attributes = component.getAttributes();
		for (String attributeName : attributes.keySet()) {
			if ((! attributeName.startsWith("com.sun")) && (! excludedAttributeNames.contains(attributeName))) {
				out.append(' ').append(attributeName).append("=\"").append(attributes.get(attributeName)).append('"');
			}
		}

		// Add passthrough attributes
		attributes = component.getPassThroughAttributes();
		for (String attributeName : attributes.keySet()) {
			out.append(" pt:").append(attributeName).append("=\"").append(attributes.get(attributeName)).append('"');
		}

		indentation += INDENT;
		Map<String, UIComponent> facets = component.getFacets();
		for (String facetName : facets.keySet()) {
			if (hasNoChildTags) {
				out.append(">\n");
				hasNoChildTags = false;
			}
			UIComponentBase facet = (UIComponentBase) facets.get(facetName);
			renderFacet(facetName, facet);
		}
		
		Map<String, List<ClientBehavior>> behaviours = component.getClientBehaviors();
		for (String eventName : behaviours.keySet()) {
			if (hasNoChildTags) {
				out.append(">\n");
				hasNoChildTags = false;
			}
			for (ClientBehavior behaviour : behaviours.get(eventName)) {
				renderAjaxBehaviour(eventName, (AjaxBehavior) behaviour);
			}
		}
		
		attributes = component.getAttributes();
		for (String tagAttributeName : tagAttributeNames) {
			if (hasNoChildTags) {
				out.append(">\n");
				hasNoChildTags = false;
			}
			Object attributeValue = attributes.get(tagAttributeName);
			renderAttribute(tagAttributeName, attributeValue);
		}
		
		for (UIComponent child : component.getChildren()) {
			if (hasNoChildTags) {
				out.append(">\n");
				hasNoChildTags = false;
			}
			renderComponent((UIComponentBase) child);
		}
		
		if (hasNoChildTags) {
			out.append(" />\n");
		}
		indentation = indentation.substring(1);
		if (! hasNoChildTags) {
			out.append(indentation).append("</").append(tagName).append(">\n");
		}
	}
	
	private static void putValueExpression(Map<String, Object> attributes, String name, UIComponentBase component) {
		ValueExpression ve = component.getValueExpression(name);
		if (ve != null) {
			Object value = ve.getExpressionString();
			if (value == null) {
				value = ve.getValue(FacesContext.getCurrentInstance().getELContext());
			}
			attributes.put(name, value);
		}
	}
	
	private static void putMethodExpression(Map<String, Object> attributes, String name, MethodExpression me) {
		if (me != null) {
			attributes.put(name, me.getExpressionString());
		}
	}

	private static void putValue(Map<String, Object> attributes, String name, Object value) {
		if ((value != null) && (! "".equals(value))) {
			attributes.put(name, value);
		}
	}
	
	private void renderFacet(String facetName, UIComponentBase facet) {
		out.append(indentation).append("<f:facet name=\"").append(facetName).append("\">\n");
		indentation += INDENT;
		renderComponent(facet);
		indentation = indentation.substring(1);
		out.append(indentation).append("</f:facet/>\n");
	}
	
	private void renderAttribute(String name, Object value) {
		out.append(indentation).append("<f:attribute name=\"").append(name).append("\" value=\"").append(value).append("\" />\n");
	}
	
	private void renderAjaxBehaviour(String eventName, AjaxBehavior behaviour) {
		out.append(indentation).append("<p:ajax event=\"").append(eventName).append('"');
		MethodExpression listener = behaviour.getListener();
		if (listener != null) {
			out.append(" listener=\"").append(listener.getExpressionString()).append('"');
		}

		String value = behaviour.getProcess();
		if (value != null) {
			out.append(" process=\"").append(value).append('"');
		}
		value = behaviour.getUpdate();
		if (value != null) {
			out.append(" update=\"").append(value).append('"');
		}
		out.append(" />\n");
	}
}
