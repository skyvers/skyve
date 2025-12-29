package org.skyve.impl.web.faces.pipeline.component;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.primefaces.behavior.ajax.AjaxBehavior;
import org.primefaces.behavior.confirm.ConfirmBehavior;
import org.primefaces.component.accordionpanel.AccordionPanel;
import org.primefaces.component.autocomplete.AutoComplete;
import org.primefaces.component.button.Button;
import org.primefaces.component.celleditor.CellEditor;
import org.primefaces.component.colorpicker.ColorPicker;
import org.primefaces.component.column.Column;
import org.primefaces.component.commandbutton.CommandButton;
import org.primefaces.component.commandlink.CommandLink;
import org.primefaces.component.datalist.DataList;
import org.primefaces.component.datatable.DataTable;
import org.primefaces.component.datepicker.DatePicker;
import org.primefaces.component.fieldset.Fieldset;
import org.primefaces.component.fileupload.FileUpload;
import org.primefaces.component.graphicimage.GraphicImage;
import org.primefaces.component.inputmask.InputMask;
import org.primefaces.component.inputtext.InputText;
import org.primefaces.component.inputtextarea.InputTextarea;
import org.primefaces.component.message.Message;
import org.primefaces.component.outputlabel.OutputLabel;
import org.primefaces.component.outputpanel.OutputPanel;
import org.primefaces.component.overlaypanel.OverlayPanel;
import org.primefaces.component.panel.Panel;
import org.primefaces.component.panelgrid.PanelGrid;
import org.primefaces.component.password.Password;
import org.primefaces.component.picklist.PickList;
import org.primefaces.component.progressbar.ProgressBar;
import org.primefaces.component.row.Row;
import org.primefaces.component.selectbooleancheckbox.SelectBooleanCheckbox;
import org.primefaces.component.selectmanycheckbox.SelectManyCheckbox;
import org.primefaces.component.selectonemenu.SelectOneMenu;
import org.primefaces.component.selectoneradio.SelectOneRadio;
import org.primefaces.component.signature.Signature;
import org.primefaces.component.spacer.Spacer;
import org.primefaces.component.spinner.Spinner;
import org.primefaces.component.tabview.Tab;
import org.primefaces.component.tabview.TabView;
import org.primefaces.component.texteditor.TextEditor;
import org.primefaces.component.toolbar.Toolbar;
import org.primefaces.component.tristatecheckbox.TriStateCheckbox;

import jakarta.el.MethodExpression;
import jakarta.el.ValueExpression;
import jakarta.faces.component.UICommand;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIComponentBase;
import jakarta.faces.component.UIInput;
import jakarta.faces.component.UIOutput;
import jakarta.faces.component.UIParameter;
import jakarta.faces.component.UISelectItems;
import jakarta.faces.component.behavior.ClientBehavior;
import jakarta.faces.component.html.HtmlForm;
import jakarta.faces.component.html.HtmlInputText;
import jakarta.faces.component.html.HtmlInputTextarea;
import jakarta.faces.component.html.HtmlOutputLabel;
import jakarta.faces.component.html.HtmlOutputLink;
import jakarta.faces.component.html.HtmlOutputText;
import jakarta.faces.component.html.HtmlPanelGrid;
import jakarta.faces.component.html.HtmlPanelGroup;
import jakarta.faces.convert.Converter;

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
		
		if (component instanceof AccordionPanel accordionPanel) {
			tagName = "p:accordionPanel";
			
			putValue(attributes, "style", accordionPanel.getStyle());
			putValue(attributes, "styleClass", accordionPanel.getStyleClass());
		}
		else if (component instanceof AutoComplete complete) {
			tagName = "p:autoComplete";
			
			putValue(attributes, "var", complete.getVar());
			putValueExpression(attributes, "itemLabel", component);
			putValueExpression(attributes, "itemValue", component);
			putMethodExpression(attributes, "completeMethod", complete.getCompleteMethod());
			putValue(attributes, "forceSelection", Boolean.valueOf(complete.isForceSelection()));
			putValue(attributes, "dropdown", Boolean.valueOf(complete.isDropdown()));
			putValue(attributes, "scrollHeight", Integer.valueOf(complete.getScrollHeight()));
			putValue(attributes, "style", complete.getStyle());
			putValue(attributes, "styleClass", complete.getStyleClass());

			
	    	tagAttributeNames.add("module");
	    	tagAttributeNames.add("query");
	    	tagAttributeNames.add("display");
		}
		else if (component instanceof Button button) {
			tagName = "p:button";
			
			putValue(attributes, "href", button.getHref());
			putValue(attributes, "title", button.getTitle());
			if (button.isDisabled()) {
				putValue(attributes, "disabled", "true");
			}
			putValue(attributes, "style", button.getStyle());
			putValue(attributes, "styleClass", button.getStyleClass());
		}
		else if (component instanceof DatePicker picker) {
			tagName = "p:datePicker";
			
			putValue(attributes, "showIcon", Boolean.valueOf(picker.isShowIcon()));
			putValue(attributes, "showOnFocus", Boolean.valueOf(picker.isShowOnFocus()));
			putValue(attributes, "showButtonBar", Boolean.valueOf(picker.isShowButtonBar()));
			putValue(attributes, "pattern", picker.getPattern());
			putValue(attributes, "monthNavigator", Boolean.valueOf(picker.isMonthNavigator()));
			putValue(attributes, "yearNavigator", Boolean.valueOf(picker.isYearNavigator()));
			putValue(attributes, "mask", picker.getMask());
			putValue(attributes, "showTime", Boolean.valueOf(picker.isShowTime()));
			putValue(attributes, "hourFormat", picker.getHourFormat());
			putValue(attributes, "showSeconds", Boolean.valueOf(picker.isShowSeconds()));
			putValue(attributes, "style", picker.getStyle());
			putValue(attributes, "styleClass", picker.getStyleClass());
		}
		else if (component instanceof CellEditor) {
			tagName = "p:cellEditor";
		}
		else if (component instanceof ColorPicker picker) {
			tagName = "p:colorPicker";
			
			putValue(attributes, "var", picker.getStyle());
		}
		else if (component instanceof Column column) {
			tagName = "p:column";

			putValue(attributes, "headerText", column.getHeaderText());
			putValueExpression(attributes, "sortBy", component);
			putValue(attributes, "style", column.getStyle());
			putValue(attributes, "styleClass", column.getStyleClass());
			int priority = column.getResponsivePriority();
			if ((priority > 0) && (priority <= 6)) {
				putValue(attributes, "priority", Integer.toString(priority));
			}
			putValue(attributes, "field", column.getField());
			if (! column.isSortable()) {
				putValue(attributes, "sortable", Boolean.FALSE);
			}
		}
		else if (component instanceof CommandButton button) {
			tagName = "p:commandButton";
			
			putValue(attributes, "immediate", Boolean.valueOf(button.isImmediate()));
			putValue(attributes, "ajax", Boolean.valueOf(button.isAjax()));
			putValue(attributes, "process", button.getProcess());
			putValue(attributes, "title", button.getTitle());
			putValue(attributes, "update", button.getUpdate());
			if (button.isDisabled()) {
				putValue(attributes, "disabled", "true");
			}
			if (button.isDisableOnAjax()) {
				putValue(attributes, "disableOnAjax", "true");
			}
			putValue(attributes, "style", button.getStyle());
			putValue(attributes, "styleClass", button.getStyleClass());
		}
		else if (component instanceof CommandLink link) {
			tagName = "p:commandLink";

			putValue(attributes, "immediate", Boolean.valueOf(link.isImmediate()));
			putValue(attributes, "ajax", Boolean.valueOf(link.isAjax()));
			putValue(attributes, "process", link.getProcess());
			putValue(attributes, "title", link.getTitle());
			putValue(attributes, "update", link.getUpdate());
			if (link.isDisabled()) {
				putValue(attributes, "disabled", "true");
			}
			putValue(attributes, "style", link.getStyle());
			putValue(attributes, "styleClass", link.getStyleClass());
		}
		else if (component instanceof DataList list) {
			tagName = "p:dataList";
			
			putValue(attributes, "var", list.getVar());
			putValue(attributes, "style", list.getStyle());
			putValue(attributes, "styleClass", list.getStyleClass());
		}
		else if (component instanceof DataTable table) {
			tagName = "p:dataTable";

			putValueExpression(attributes, "value", component);
			putValue(attributes, "var", table.getVar());
			if (table.isPaginator()) {
				putValue(attributes, "paginator", Boolean.TRUE);
				putValue(attributes, "rowsPerPageTemplate", table.getRowsPerPageTemplate());
				putValue(attributes, "rows", Integer.valueOf(table.getRows()));
				putValue(attributes, "paginatorAlwaysVisible", Boolean.valueOf(table.isPaginatorAlwaysVisible()));
			}
			if (table.isLazy()) {
				putValue(attributes, "lazy", Boolean.TRUE);
			}
			putValue(attributes, "emptyMessage", table.getEmptyMessage());
			if (table.isStickyHeader()) {
				putValue(attributes, "stickyHeader", Boolean.TRUE);
			}
			if ("multiple".equals(table.getSortMode())) {
				putValue(attributes, "sortMode", "multiple");
			}
			putValue(attributes, "widgetVar", table.getWidgetVar());
			putValue(attributes, "selectionMode", table.getSelectionMode());
			putValueExpression(attributes, "rowKey", component);
			putValue(attributes, "style", table.getStyle());
			putValue(attributes, "styleClass", table.getStyleClass());
		}
		else if (component instanceof TextEditor editor) {
			tagName = "p:editor";
			
			putValue(attributes, "style", editor.getStyle());
			putValue(attributes, "styleClass", editor.getStyleClass());
		}
		else if (component instanceof Fieldset field) {
			tagName = "p:fieldset";
			
			putValue(attributes, "legend", field.getLegend());
			putValue(attributes, "style", field.getStyle());
			putValue(attributes, "styleClass", field.getStyleClass());
		}
		else if (component instanceof FileUpload upload) {
			tagName = "p:fileUpload";
			
			putValue(attributes, "style", upload.getStyle());
			putValue(attributes, "styleClass", upload.getStyleClass());
			putValue(attributes, "onStart", upload.getOnstart());
			putValue(attributes, "update", upload.getUpdate());
			putValue(attributes, "fileLimit", Integer.valueOf(upload.getFileLimit()));
			putValue(attributes, "fileLimitMessage", upload.getFileLimitMessage());
			putMethodExpression(attributes, "fileUploadListener", upload.getListener());
		}
		else if (component instanceof GraphicImage image) {
			tagName = "p:graphicImage";
			
			// URL is populated from the value if present, so check the value first
			putValue(attributes, "value", image.getValue());
			putValueExpression(attributes, "value", component);
			if (! attributes.containsKey("value")) {
				putValue(attributes, "url", image.getUrl());
			}
			putValue(attributes, "style", image.getStyle());
			putValue(attributes, "styleClass", image.getStyleClass());
		}
		else if (component instanceof HtmlForm form) {
			tagName = "h:form";
			
			putValue(attributes, "style", form.getStyle());
			putValue(attributes, "styleClass", form.getStyleClass());
		}
		else if (component instanceof HtmlOutputLabel label) {
			if (component instanceof OutputLabel) {
				tagName = "p:outputLabel";
			}
			else {
				tagName = "h:outputLabel";
			}
			
			putValue(attributes, "for", label.getFor());
			putValue(attributes, "style", label.getStyle());
			putValue(attributes, "styleClass", label.getStyleClass());
		}
		else if (component instanceof HtmlOutputLink link) {
			tagName = "h:outputLink";
			
			putValue(attributes, "target", link.getTarget());
			putValue(attributes, "style", link.getStyle());
			putValue(attributes, "styleClass", link.getStyleClass());
		}
		else if (component instanceof HtmlOutputText text) {
			tagName = "h:outputText";
			
			putValue(attributes, "escape", Boolean.valueOf(text.isEscape()));
			putValue(attributes, "style", text.getStyle());
			putValue(attributes, "styleClass", text.getStyleClass());
		}
		else if (component instanceof HtmlPanelGroup panel) {
			tagName = "h:panelGroup";
			
			putValue(attributes, "layout", panel.getLayout());
			putValue(attributes, "style", panel.getStyle());
			putValue(attributes, "styleClass", panel.getStyleClass());

			excludedAttributeNames.add("type");
			excludedAttributeNames.add("managedBean");
		}
		else if (component instanceof InputMask mask) {
			tagName = "p:inputMask";
			
			putValue(attributes, "mask", mask.getMask());
			putValue(attributes, "style", mask.getStyle());
			putValue(attributes, "styleClass", mask.getStyleClass());
		}
		else if (component instanceof InputText text) {
			tagName = "p:inputText";
			
			putValue(attributes, "style", text.getStyle());
			putValue(attributes, "styleClass", text.getStyleClass());
		}
		else if (component instanceof InputTextarea text) {
			tagName = "p:inputTextarea";
			
			putValue(attributes, "style", text.getStyle());
			putValue(attributes, "styleClass", text.getStyleClass());
		}
		else if (component instanceof Message message) {
			tagName = "p:message";
			
			putValue(attributes, "for", message.getFor());
			putValue(attributes, "showDetail", Boolean.valueOf(message.isShowDetail()));
			putValue(attributes, "showSummary", Boolean.valueOf(message.isShowSummary()));
			putValue(attributes, "display", message.getDisplay());
			putValue(attributes, "style", message.getStyle());
			putValue(attributes, "styleClass", message.getStyleClass());
		}
		else if (component instanceof OutputPanel panel) {
			tagName = "p:outputPanel";
			
			putValue(attributes, "style", panel.getStyle());
			putValue(attributes, "styleClass", panel.getStyleClass());
		}
		else if (component instanceof Panel panel) {
			tagName = "p:panel";
			
			putValue(attributes, "header", panel.getHeader());
			putValue(attributes, "style", panel.getStyle());
			putValue(attributes, "styleClass", panel.getStyleClass());
		}
		else if (component instanceof HtmlPanelGrid grid) {
			tagName = "h:panelGrid";

			putValue(attributes, "columns", Integer.valueOf(grid.getColumns()));
			putValue(attributes, "style", grid.getStyle());
			putValue(attributes, "styleClass", grid.getStyleClass());
		}
		else if (component instanceof PanelGrid grid) {
			tagName = "p:panelGrid";
			
			putValue(attributes, "columns", Integer.valueOf(grid.getColumns()));
			putValue(attributes, "style", grid.getStyle());
			putValue(attributes, "styleClass", grid.getStyleClass());
		}
		else if (component instanceof OverlayPanel overlay) {
			tagName = "p:overlayPanel";
			
			putValue(attributes, "widgetVar", overlay.getWidgetVar());
			putValue(attributes, "for", overlay.getFor());
			putValue(attributes, "dynamic", String.valueOf(overlay.isDynamic()));
			putValue(attributes, "showCloseIcon", String.valueOf(overlay.isShowCloseIcon()));
			putValue(attributes, "modal", String.valueOf(overlay.isModal()));
			putValue(attributes, "style", overlay.getStyle());
			putValue(attributes, "onHide", overlay.getOnHide());
			putValueExpression(attributes, "onShow", component);
		}
		else if (component instanceof Password password) {
			tagName = "p:password";
			
			putValue(attributes, "style", password.getStyle());
			putValue(attributes, "styleClass", password.getStyleClass());
		}
		else if (component instanceof PickList pickList) {
			tagName = "p:pickList";
			
			putValue(attributes, "var", pickList.getVar());
			putValue(attributes, "showSourceControls", Boolean.valueOf(pickList.isShowSourceControls()));
			putValue(attributes, "showTargetControls", Boolean.valueOf(pickList.isShowTargetControls()));
			putValue(attributes, "showSourceFilter", Boolean.valueOf(pickList.isShowSourceFilter()));
			putValue(attributes, "showTargetFilter", Boolean.valueOf(pickList.isShowTargetFilter()));
			putValue(attributes, "responsive", Boolean.valueOf(pickList.isResponsive()));
			putValueExpression(attributes, "itemValue", component);
			putValueExpression(attributes, "itemLabel", component);
			putValue(attributes, "style", pickList.getStyle());
			putValue(attributes, "styleClass", pickList.getStyleClass());
		}
		else if (component instanceof ProgressBar progress) {
			tagName = "p:progressBar";
			
			putValue(attributes, "style", progress.getStyle());
			putValue(attributes, "styleClass", progress.getStyleClass());
		}
		else if (component instanceof Row) {
			tagName = "p:row";
		}
		else if (component instanceof SelectBooleanCheckbox check) {
			tagName = "p:selectBooleanCheckbox";
			
			putValue(attributes, "itemLabel", check.getItemLabel());
			putValue(attributes, "style", check.getStyle());
			putValue(attributes, "styleClass", check.getStyleClass());
		}
		else if (component instanceof SelectManyCheckbox checks) {
			tagName = "p:selectManyCheckbox";
			
			putValue(attributes, "style", checks.getStyle());
			putValue(attributes, "styleClass", checks.getStyleClass());
		}
		else if (component instanceof SelectOneMenu pick) {
			tagName = "p:selectOneMenu";
			
			putValue(attributes, "style", pick.getStyle());
			putValue(attributes, "styleClass", pick.getStyleClass());
		}
		else if (component instanceof SelectOneRadio radio) {
			tagName = "p:selectOneRadio";

			putValue(attributes, "style", radio.getStyle());
			putValue(attributes, "styleClass", radio.getStyleClass());
		}
		else if (component instanceof Signature signature) {
			tagName = "p:signature";
			
			putValue(attributes, "style", signature.getStyle());
			putValue(attributes, "styleClass", signature.getStyleClass());
			putValue(attributes, "guideline", Boolean.valueOf(signature.isGuideline()));
		}
		else if (component instanceof Spacer spacer) {
			tagName = "p:spacer";
			
			putValue(attributes, "style", spacer.getStyle());
			putValue(attributes, "styleClass", spacer.getStyleClass());
		}
		else if (component instanceof Spinner spinner) {
			tagName = "p:spinner";
			
			putValue(attributes, "style", spinner.getStyle());
			putValue(attributes, "styleClass", spinner.getStyleClass());
		}
		else if (component instanceof Tab tab) {
			tagName = "p:tab";
			
			putValue(attributes, "title", tab.getTitle());
			putValue(attributes, "titleStyleClass", tab.getTitleStyleClass());
		}
		else if (component instanceof TabView tabs) {
			tagName = "p:tabView";
			
			putValue(attributes, "style", tabs.getStyle());
			putValue(attributes, "styleClass", tabs.getStyleClass());
		}
		else if (component instanceof Toolbar tools) {
			tagName = "p:toolbar";
			
			putValue(attributes, "style", tools.getStyle());
			putValue(attributes, "styleClass", tools.getStyleClass());
		}
		else if (component instanceof TriStateCheckbox check) {
			tagName = "p:triStateCheckbox";
			
			putValue(attributes, "style", check.getStyle());
			putValue(attributes, "styleClass", check.getStyleClass());
		}
		else if (component instanceof UIOutput) {
			// do nothing - the value is what we want - notice there is no tagName assigned
		}
		else if (component instanceof UIParameter param) {
			tagName = "f:parameter";
			
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
		}
		
		if (component instanceof UICommand command) {
			putValue(attributes, "value", command.getValue());
			putMethodExpression(attributes, "action", command.getActionExpression());
		}
		else if (component instanceof UIOutput output) {
			putValueExpression(attributes, "value", component);
			if (! attributes.containsKey("value")) {
				putValue(attributes, "value", output.getValue());
			}
			
			if (tagName == null) { // no tag
				out.append(indentation).append(attributes.get("value")).append('\n');
				return;
			}

			Converter<?> converter = output.getConverter();
			if (converter != null) {
				String converterName = converter.getClass().getSimpleName();
				if (converterName.endsWith("Converter")) {
					converterName = converterName.substring(0, converterName.length() - 9);
				}
				putValue(attributes, "converter", converterName);
			}
			
			if (component instanceof UIInput input) {
				if (input instanceof HtmlInputText text) {
					if (text.isDisabled()) {
						putValue(attributes, "disabled", "true");
					}
					if (text.isReadonly()) {
						putValue(attributes, "readonly", "true");
					}
					putValue(attributes, "title", text.getTitle());
				}
				else if (input instanceof HtmlInputTextarea text) {
					if (text.isDisabled()) {
						putValue(attributes, "disabled", "true");
					}
					if (text.isReadonly()) {
						putValue(attributes, "readonly", "true");
					}
					putValue(attributes, "title", text.getTitle());
				}

				putValue(attributes, "requiredMessage", input.getRequiredMessage());
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
			if ((! attributeName.startsWith("com.sun")) && 
					(! excludedAttributeNames.contains(attributeName)) &&
					(! tagAttributeNames.contains(attributeName))) {
				out.append(' ').append(attributeName).append("=\"").append(attributes.get(attributeName)).append('"');
			}
		}

		// Add passthrough attributes
		attributes = component.getPassThroughAttributes();
		for (String attributeName : attributes.keySet()) {
			if ((! excludedAttributeNames.contains(attributeName)) &&
					(! tagAttributeNames.contains(attributeName))) {
				out.append(" pt:").append(attributeName).append("=\"").append(attributes.get(attributeName)).append('"');
			}
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
				if (behaviour instanceof AjaxBehavior ajax) {
					renderAjaxBehaviour(eventName, ajax);
				}
				else if (behaviour instanceof ConfirmBehavior confirm) {
					renderConfirmBehaviour(confirm);
				}
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
			// NB We cannot evaluate the value here as it could cause NPEs in the domain objects (think conditions).
			// No ve.getValue() call

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
		out.append(indentation).append("</f:facet>\n");
	}
	
	private void renderAttribute(String name, Object value) {
		out.append(indentation).append("<f:attribute name=\"").append(name).append("\" value=\"").append(value).append("\" />\n");
	}
	
	private void renderAjaxBehaviour(String eventName, AjaxBehavior behaviour) {
		out.append(indentation).append("<p:ajax event=\"").append(eventName).append("\" listener=\"<cant obtain AjaxBehaviourListener(s) from primefaces>\"");

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

	private void renderConfirmBehaviour(ConfirmBehavior behaviour) {
		out.append(indentation).append("<p:confirm message=\"").append(behaviour.getMessage()).append("\" />\n");
	}
}
