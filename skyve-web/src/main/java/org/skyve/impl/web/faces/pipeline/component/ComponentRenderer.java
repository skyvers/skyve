package org.skyve.impl.web.faces.pipeline.component;

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
import javax.faces.component.UIInput;
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
import javax.faces.component.html.HtmlPanelGrid;
import javax.faces.component.html.HtmlPanelGroup;
import javax.faces.convert.Converter;

import org.primefaces.behavior.ajax.AjaxBehavior;
import org.primefaces.behavior.confirm.ConfirmBehavior;
import org.primefaces.component.accordionpanel.AccordionPanel;
import org.primefaces.component.autocomplete.AutoComplete;
import org.primefaces.component.button.Button;
import org.primefaces.component.calendar.Calendar;
import org.primefaces.component.celleditor.CellEditor;
import org.primefaces.component.colorpicker.ColorPicker;
import org.primefaces.component.column.Column;
import org.primefaces.component.commandbutton.CommandButton;
import org.primefaces.component.commandlink.CommandLink;
import org.primefaces.component.datalist.DataList;
import org.primefaces.component.datatable.DataTable;
import org.primefaces.component.texteditor.TextEditor;
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
import org.primefaces.component.toolbar.Toolbar;
import org.primefaces.component.tristatecheckbox.TriStateCheckbox;

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
			
			AccordionPanel accordionPanel = (AccordionPanel) component;
			putValue(attributes, "style", accordionPanel.getStyle());
			putValue(attributes, "styleClass", accordionPanel.getStyleClass());
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
			putValue(attributes, "scrollHeight", Integer.valueOf(complete.getScrollHeight()));
			putValue(attributes, "style", complete.getStyle());
			putValue(attributes, "styleClass", complete.getStyleClass());

			
	    	tagAttributeNames.add("module");
	    	tagAttributeNames.add("query");
	    	tagAttributeNames.add("display");
		}
		else if (component instanceof Button) {
			tagName = "p:button";
			
			Button button = (Button) component;
			putValue(attributes, "href", button.getHref());
			putValue(attributes, "title", button.getTitle());
			if (button.isDisabled()) {
				putValue(attributes, "disabled", "true");
			}
			putValue(attributes, "style", button.getStyle());
			putValue(attributes, "styleClass", button.getStyleClass());
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
			putValue(attributes, "styleClass", calendar.getStyleClass());
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

			Column column = (Column) component;
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
			putValue(attributes, "styleClass", button.getStyleClass());
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
			putValue(attributes, "styleClass", link.getStyleClass());
		}
		else if (component instanceof DataList) {
			tagName = "p:dataList";
			
			DataList list = (DataList) component;
			putValue(attributes, "var", list.getVar());
			putValue(attributes, "style", list.getStyle());
			putValue(attributes, "styleClass", list.getStyleClass());
		}
		else if (component instanceof DataTable) {
			tagName = "p:dataTable";

			DataTable table = (DataTable) component;
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
		else if (component instanceof TextEditor) {
			tagName = "p:editor";
			
			TextEditor editor = (TextEditor) component;
			putValue(attributes, "style", editor.getStyle());
			putValue(attributes, "styleClass", editor.getStyleClass());
		}
		else if (component instanceof Fieldset) {
			tagName = "p:fieldset";
			
			Fieldset fs = (Fieldset) component;
			putValue(attributes, "legend", fs.getLegend());
			putValue(attributes, "style", fs.getStyle());
			putValue(attributes, "styleClass", fs.getStyleClass());
		}
		else if (component instanceof FileUpload) {
			tagName = "p:fileUpload";
			
			FileUpload fu = (FileUpload) component;
			putValue(attributes, "style", fu.getStyle());
			putValue(attributes, "styleClass", fu.getStyleClass());
			putValue(attributes, "onStart", fu.getOnstart());
			putValue(attributes, "update", fu.getUpdate());
			putValue(attributes, "fileLimit", Integer.valueOf(fu.getFileLimit()));
			putValue(attributes, "fileLimitMessage", fu.getFileLimitMessage());
			putMethodExpression(attributes, "fileUploadListener", fu.getListener());
		}
		else if (component instanceof GraphicImage) {
			tagName = "p:graphicImage";
			
			GraphicImage image = (GraphicImage) component;
			// URL is populated from the value if present, so check the value first
			putValue(attributes, "value", image.getValue());
			putValueExpression(attributes, "value", component);
			if (! attributes.containsKey("value")) {
				putValue(attributes, "url", image.getUrl());
			}
			putValue(attributes, "style", image.getStyle());
			putValue(attributes, "styleClass", image.getStyleClass());
		}
		else if (component instanceof HtmlForm) {
			tagName = "h:form";
			
			HtmlForm form = (HtmlForm) component;
			putValue(attributes, "style", form.getStyle());
			putValue(attributes, "styleClass", form.getStyleClass());
		}
		else if (component instanceof HtmlOutputLabel) {
			if (component instanceof OutputLabel) {
				tagName = "p:outputLabel";
			}
			else {
				tagName = "h:outputLabel";
			}
			
			HtmlOutputLabel label = (HtmlOutputLabel) component;
			putValue(attributes, "for", label.getFor());
			putValue(attributes, "style", label.getStyle());
			putValue(attributes, "styleClass", label.getStyleClass());
		}
		else if (component instanceof HtmlOutputLink) {
			tagName = "h:outputLink";
			
			HtmlOutputLink link = (HtmlOutputLink) component;
			putValue(attributes, "target", link.getTarget());
			putValue(attributes, "style", link.getStyle());
			putValue(attributes, "styleClass", link.getStyleClass());
		}
		else if (component instanceof HtmlOutputText) {
			tagName = "h:outputText";
			
			HtmlOutputText text = (HtmlOutputText) component;
			putValue(attributes, "escape", Boolean.valueOf(text.isEscape()));
			putValue(attributes, "style", text.getStyle());
			putValue(attributes, "styleClass", text.getStyleClass());
		}
		else if (component instanceof HtmlPanelGroup) {
			tagName = "h:panelGroup";
			
			HtmlPanelGroup panel = (HtmlPanelGroup) component;
			putValue(attributes, "layout", panel.getLayout());
			putValue(attributes, "style", panel.getStyle());
			putValue(attributes, "styleClass", panel.getStyleClass());

			excludedAttributeNames.add("type");
			excludedAttributeNames.add("managedBean");
		}
		else if (component instanceof InputMask) {
			tagName = "p:inputMask";
			
			InputMask mask = (InputMask) component;
			putValue(attributes, "mask", mask.getMask());
			putValue(attributes, "style", mask.getStyle());
			putValue(attributes, "styleClass", mask.getStyleClass());
		}
		else if (component instanceof InputText) {
			tagName = "p:inputText";
			
			InputText text = (InputText) component;
			putValue(attributes, "style", text.getStyle());
			putValue(attributes, "styleClass", text.getStyleClass());
		}
		else if (component instanceof InputTextarea) {
			tagName = "p:inputTextarea";
			
			InputTextarea text = (InputTextarea) component;
			putValue(attributes, "style", text.getStyle());
			putValue(attributes, "styleClass", text.getStyleClass());
		}
		else if (component instanceof Message) {
			tagName = "p:message";
			
			Message message = (Message) component;
			putValue(attributes, "for", message.getFor());
			putValue(attributes, "showDetail", Boolean.valueOf(message.isShowDetail()));
			putValue(attributes, "showSummary", Boolean.valueOf(message.isShowSummary()));
			putValue(attributes, "display", message.getDisplay());
			putValue(attributes, "style", message.getStyle());
			putValue(attributes, "styleClass", message.getStyleClass());
		}
		else if (component instanceof OutputPanel) {
			tagName = "p:outputPanel";
			
			OutputPanel panel = (OutputPanel) component;
			putValue(attributes, "style", panel.getStyle());
			putValue(attributes, "styleClass", panel.getStyleClass());
		}
		else if (component instanceof Panel) {
			tagName = "p:panel";
			
			Panel panel = (Panel) component;
			putValue(attributes, "header", panel.getHeader());
			putValue(attributes, "style", panel.getStyle());
			putValue(attributes, "styleClass", panel.getStyleClass());
		}
		else if (component instanceof HtmlPanelGrid) {
			if (component instanceof PanelGrid) {
				tagName = "p:panelGrid";
			}
			else {
				tagName = "h:panelGrid";
			}
			
			HtmlPanelGrid grid = (HtmlPanelGrid) component;
			putValue(attributes, "columns", Integer.valueOf(grid.getColumns()));
			putValue(attributes, "style", grid.getStyle());
			putValue(attributes, "styleClass", grid.getStyleClass());
		}
		else if (component instanceof OverlayPanel) {
			tagName = "p:overlayPanel";
			
			OverlayPanel overlay = (OverlayPanel) component;
			putValue(attributes, "widgetVar", overlay.getWidgetVar());
			putValue(attributes, "for", overlay.getFor());
			putValue(attributes, "dynamic", String.valueOf(overlay.isDynamic()));
			putValue(attributes, "showCloseIcon", String.valueOf(overlay.isShowCloseIcon()));
			putValue(attributes, "modal", String.valueOf(overlay.isModal()));
			putValue(attributes, "style", overlay.getStyle());
			putValue(attributes, "onHide", overlay.getOnHide());
			putValueExpression(attributes, "onShow", component);
		}
		else if (component instanceof Password) {
			tagName = "p:password";
			
			Password password = (Password) component;
			putValue(attributes, "style", password.getStyle());
			putValue(attributes, "styleClass", password.getStyleClass());
		}
		else if (component instanceof ProgressBar) {
			tagName = "p:progressBar";
			
			ProgressBar progress = (ProgressBar) component;
			putValue(attributes, "style", progress.getStyle());
			putValue(attributes, "styleClass", progress.getStyleClass());
		}
		else if (component instanceof Row) {
			tagName = "p:row";
		}
		else if (component instanceof SelectBooleanCheckbox) {
			tagName = "p:selectBooleanCheckbox";
			
			SelectBooleanCheckbox check = (SelectBooleanCheckbox) component;
			putValue(attributes, "itemLabel", check.getItemLabel());
			putValue(attributes, "style", check.getStyle());
			putValue(attributes, "styleClass", check.getStyleClass());
		}
		else if (component instanceof SelectManyCheckbox) {
			tagName = "p:selectManyCheckbox";
			
			SelectManyCheckbox checks = (SelectManyCheckbox) component;
			putValue(attributes, "style", checks.getStyle());
			putValue(attributes, "styleClass", checks.getStyleClass());
		}
		else if (component instanceof SelectOneMenu) {
			tagName = "p:selectOneMenu";
			
			SelectOneMenu pick = (SelectOneMenu) component;
			putValue(attributes, "style", pick.getStyle());
			putValue(attributes, "styleClass", pick.getStyleClass());
		}
		else if (component instanceof SelectOneRadio) {
			tagName = "p:selectOneRadio";

			SelectOneRadio radio = (SelectOneRadio) component;
			putValue(attributes, "style", radio.getStyle());
			putValue(attributes, "styleClass", radio.getStyleClass());
		}
		else if (component instanceof Signature) {
			tagName = "p:signature";
			
			Signature signature = (Signature) component;
			putValue(attributes, "style", signature.getStyle());
			putValue(attributes, "styleClass", signature.getStyleClass());
			putValue(attributes, "guideline", Boolean.valueOf(signature.isGuideline()));
		}
		else if (component instanceof Spacer) {
			tagName = "p:spacer";
			
			Spacer spacer = (Spacer) component;
			putValue(attributes, "style", spacer.getStyle());
			putValue(attributes, "styleClass", spacer.getStyleClass());
		}
		else if (component instanceof Spinner) {
			tagName = "p:spinner";
			
			Spinner spinner = (Spinner) component;
			putValue(attributes, "style", spinner.getStyle());
			putValue(attributes, "styleClass", spinner.getStyleClass());
		}
		else if (component instanceof Tab) {
			tagName = "p:tab";
			
			Tab tab = (Tab) component;
			putValue(attributes, "title", tab.getTitle());
			putValue(attributes, "titleStyleClass", tab.getTitleStyleClass());
		}
		else if (component instanceof TabView) {
			tagName = "p:tabView";
			
			TabView tabs = (TabView) component;
			putValue(attributes, "style", tabs.getStyle());
			putValue(attributes, "styleClass", tabs.getStyleClass());
		}
		else if (component instanceof Toolbar) {
			tagName = "p:toolbar";
			
			Toolbar tools = (Toolbar) component;
			putValue(attributes, "style", tools.getStyle());
			putValue(attributes, "styleClass", tools.getStyleClass());
		}
		else if (component instanceof TriStateCheckbox) {
			tagName = "p:triStateCheckbox";
			
			TriStateCheckbox check = (TriStateCheckbox) component;
			putValue(attributes, "style", check.getStyle());
			putValue(attributes, "styleClass", check.getStyleClass());
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
		
		if (component instanceof UICommand) {
			UICommand command = (UICommand) component;
			
			putValue(attributes, "value", command.getValue());
			putMethodExpression(attributes, "action", command.getActionExpression());
		}
		else if (component instanceof UIOutput) {
			UIOutput output = (UIOutput) component;

			putValueExpression(attributes, "value", component);
			if (! attributes.containsKey("value")) {
				putValue(attributes, "value", output.getValue());
			}
			
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
			
			if (component instanceof UIInput) {
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

				putValue(attributes, "requiredMessage", ((UIInput) component).getRequiredMessage());
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
				if (behaviour instanceof AjaxBehavior) {
					renderAjaxBehaviour(eventName, (AjaxBehavior) behaviour);
				}
				else if (behaviour instanceof ConfirmBehavior) {
					renderConfirmBehaviour((ConfirmBehavior) behaviour);
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
// We cannot evaluate the value here as it could cause NPEs in the domain objects (think conditions).
/*
			if (value == null) {
				value = ve.getValue(FacesContext.getCurrentInstance().getELContext());
			}
*/
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

	private void renderConfirmBehaviour(ConfirmBehavior behaviour) {
		out.append(indentation).append("<p:confirm message=\"").append(behaviour.getMessage()).append("\" />\n");
	}
}
