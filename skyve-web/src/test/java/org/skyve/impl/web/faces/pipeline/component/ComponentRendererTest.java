package org.skyve.impl.web.faces.pipeline.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.owasp.encoder.Encode;
import org.primefaces.behavior.ajax.AjaxBehavior;
import org.primefaces.behavior.confirm.ConfirmBehavior;
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
import org.primefaces.component.panel.Panel;
import org.primefaces.component.panelgrid.PanelGrid;
import org.primefaces.component.overlaypanel.OverlayPanel;
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
import org.primefaces.component.toolbar.Toolbar;
import org.primefaces.component.tristatecheckbox.TriStateCheckbox;

import jakarta.el.MethodExpression;
import jakarta.el.ValueExpression;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIOutput;
import jakarta.faces.component.UIParameter;
import jakarta.faces.component.UISelectItems;
import jakarta.faces.component.behavior.ClientBehavior;
import jakarta.faces.component.html.HtmlOutputLink;
import jakarta.faces.component.html.HtmlOutputText;
import jakarta.faces.component.html.HtmlPanelGroup;
import jakarta.faces.context.FacesContext;
import jakarta.faces.convert.Converter;

@SuppressWarnings({"static-method", "boxing"})
class ComponentRendererTest {
	private static final String RAW_MARKUP_TEXT = "<b>Skyve \"Prime\"</b>";

	@Test
	void escapableTextDefaultsToEscapingUnlessExplicitlyFalse() {
		assertTrue(EscapableText.of(RAW_MARKUP_TEXT, true).shouldEscape());
		assertTrue(EscapableText.of(RAW_MARKUP_TEXT, true).shouldEscape());
		assertFalse(EscapableText.of(RAW_MARKUP_TEXT, false).shouldEscape());
	}

	@Test
	void escapableComponentSupportCreatesOutputTextWithRawValueAndEscapeDecision() {
		Application application = mock(Application.class);
		HtmlOutputText outputText = new HtmlOutputText();
		when(application.createComponent(HtmlOutputText.COMPONENT_TYPE)).thenReturn(outputText);

		HtmlOutputText result = EscapableComponentSupport.outputText(application, EscapableText.of(RAW_MARKUP_TEXT, false));

		assertSame(outputText, result);
		assertEquals(RAW_MARKUP_TEXT, result.getValue());
		assertFalse(result.isEscape());
	}

	@Test
	void escapableComponentSupportAddsFacetOnlyWhenTextIsPresent() {
		Application application = mock(Application.class);
		Panel panel = new Panel();
		HtmlOutputText outputText = new HtmlOutputText();
		when(application.createComponent(HtmlOutputText.COMPONENT_TYPE)).thenReturn(outputText);

		HtmlOutputText result = EscapableComponentSupport.putOutputTextFacet(application, panel, "header", EscapableText.of(RAW_MARKUP_TEXT, true));

		assertSame(outputText, result);
		assertSame(outputText, panel.getFacet("header"));
		assertEquals(RAW_MARKUP_TEXT, outputText.getValue());
		assertTrue(outputText.isEscape());

		assertNull(EscapableComponentSupport.putOutputTextFacet(application, panel, "footer", EscapableText.of(null, false)));
		assertNull(panel.getFacet("footer"));
	}

	@Test
	void escapableComponentSupportCreatesOutputLabelWithRawValueAndEscapeDecision() {
		Application application = mock(Application.class);
		OutputLabel label = new OutputLabel();
		when(application.createComponent(OutputLabel.COMPONENT_TYPE)).thenReturn(label);

		OutputLabel result = EscapableComponentSupport.outputLabel(application, EscapableText.of(RAW_MARKUP_TEXT, false), "input1");

		assertSame(label, result);
		assertEquals(RAW_MARKUP_TEXT, result.getValue());
		assertFalse(result.isEscape());
		assertEquals("input1", result.getFor());
	}

	@Test
	void escapableComponentSupportAddsConfirmBehaviourWithRawMessageAndEscapeDecision() {
		Application application = mock(Application.class);
		ConfirmBehavior confirm = new ConfirmBehavior();
		CommandButton button = new CommandButton();
		when(application.createBehavior(ConfirmBehavior.BEHAVIOR_ID)).thenReturn(confirm);

		EscapableComponentSupport.addConfirmBehavior(application, button, EscapableText.of(RAW_MARKUP_TEXT, false));

		Map<String, List<ClientBehavior>> behaviours = button.getClientBehaviors();
		assertSame(confirm, behaviours.get("click").get(0));
		assertEquals(RAW_MARKUP_TEXT, confirm.getMessage());
		assertFalse(confirm.isEscape());
	}

	@Test
	void rendersUiOutputValueWithoutTagWhenComponentTypeIsGenericUiOutput() {
		UIOutput output = mock(UIOutput.class);
		stubCommonComponentState(output);
		when(output.getValue()).thenReturn("rawValue");

		String rendered = new ComponentRenderer(output).toString();

		assertEquals("rawValue\n", rendered);
	}

	@Test
	void rendersSelfClosingTagWhenComponentHasNoFacetsBehavioursOrChildren() {
		HtmlOutputLink link = mock(HtmlOutputLink.class);
		stubCommonComponentState(link);
		when(link.getId()).thenReturn("link1");
		when(link.getTarget()).thenReturn("_blank");

		String rendered = new ComponentRenderer(link).toString();

		assertTrue(rendered.startsWith("<h:outputLink id=\"link1\""));
		assertTrue(rendered.contains(" target=\"_blank\""));
		assertTrue(rendered.endsWith(" />\n"));
	}

	@Test
	void rendersFacetAndChildrenForContainerComponents() {
		HtmlPanelGroup panel = mock(HtmlPanelGroup.class);
		Map<String, Object> panelAttributes = new HashMap<>();
		Map<String, Object> panelPassThroughAttributes = new HashMap<>();
		Map<String, jakarta.faces.component.UIComponent> panelFacets = new HashMap<>();
		List<jakarta.faces.component.UIComponent> panelChildren = new ArrayList<>();
		when(panel.getId()).thenReturn("panel1");
		when(panel.getLayout()).thenReturn("block");
		when(panel.getAttributes()).thenReturn(panelAttributes);
		when(panel.getPassThroughAttributes()).thenReturn(panelPassThroughAttributes);
		when(panel.getFacets()).thenReturn(panelFacets);
		when(panel.getChildren()).thenReturn(panelChildren);
		when(panel.getClientBehaviors()).thenReturn(new HashMap<>());
		when(panel.getValueExpression(anyString())).thenReturn(null);
		panelAttributes.put("data-test", "root");
		panelPassThroughAttributes.put("aria-label", "container");

		HtmlOutputText facet = mock(HtmlOutputText.class);
		stubCommonComponentState(facet);
		when(facet.getId()).thenReturn("facet1");
		when(facet.getValue()).thenReturn("Facet Text");
		panelFacets.put("header", facet);

		HtmlOutputText child = mock(HtmlOutputText.class);
		stubCommonComponentState(child);
		when(child.getId()).thenReturn("child1");
		when(child.getValue()).thenReturn("Child Text");
		panelChildren.add(child);

		String rendered = new ComponentRenderer(panel).toString();

		assertTrue(rendered.contains("<h:panelGroup id=\"panel1\" layout=\"block\" data-test=\"root\" pt:aria-label=\"container\">"));
		assertTrue(rendered.contains("<f:facet name=\"header\">"));
		assertTrue(rendered.contains("<h:outputText id=\"facet1\""));
		assertTrue(rendered.contains("<h:outputText id=\"child1\""));
		assertTrue(rendered.contains("</f:facet>"));
		assertTrue(rendered.contains("</h:panelGroup>"));
	}

	@Test
	void rendersConverterNameWithoutConverterSuffixForOutputComponents() {
		HtmlOutputText text = mock(HtmlOutputText.class);
		stubCommonComponentState(text);
		when(text.getId()).thenReturn("text1");
		when(text.getValue()).thenReturn("hello");
		when(text.getConverter()).thenReturn(new NameOnlyConverter());

		String rendered = new ComponentRenderer(text).toString();

		assertTrue(rendered.contains(" converter=\"NameOnly\""));
	}

	@Test
	void rendersOutputTextValueEscapeFlagAndSyntaxEscapedAttributeValue() {
		HtmlOutputText text = mock(HtmlOutputText.class);
		stubCommonComponentState(text);
		when(text.getId()).thenReturn("textEscaping");
		when(text.getValue()).thenReturn(RAW_MARKUP_TEXT);
		when(text.isEscape()).thenReturn(false);

		String rendered = new ComponentRenderer(text).toString();

		assertTrue(rendered.contains("<h:outputText id=\"textEscaping\""));
		assertTrue(rendered.contains(" escape=\"false\""));
		assertTrue(rendered.contains(" value=\"" + Encode.forHtmlAttribute(RAW_MARKUP_TEXT) + "\""));
	}

	@Test
	void rendersAjaxAndConfirmBehavioursWithProcessAndUpdate() {
		CommandButton button = mock(CommandButton.class);
		stubCommonComponentState(button);
		when(button.getId()).thenReturn("btn1");
		when(button.getValue()).thenReturn("Save");
		when(button.isEscape()).thenReturn(true);

		AjaxBehavior ajax = new AjaxBehavior();
		ajax.setProcess("@this");
		ajax.setUpdate("@form");
		ConfirmBehavior confirm = new ConfirmBehavior();
		confirm.setMessage("Are you sure?");
		Map<String, List<jakarta.faces.component.behavior.ClientBehavior>> behaviours = new HashMap<>();
		behaviours.put("click", List.of(ajax, confirm));
		when(button.getClientBehaviors()).thenReturn(behaviours);

		String rendered = new ComponentRenderer(button).toString();

		assertTrue(rendered.contains("<p:ajax event=\"click\""));
		assertTrue(rendered.contains(" process=\"@this\""));
		assertTrue(rendered.contains(" update=\"@form\""));
		assertTrue(rendered.contains("<p:confirm message=\"Are you sure?\" />"));
		assertFalse(rendered.contains(" escape=\"false\""));
		assertTrue(rendered.contains("</p:commandButton>"));
		assertFalse(rendered.contains("<cant obtain AjaxBehaviourListener(s) from primefaces></p:ajax>"));
	}

	@Test
	void rendersConfirmBehaviourEscapeDecisionAndSyntaxEscapedMessage() {
		CommandButton button = mock(CommandButton.class);
		stubCommonComponentState(button);
		when(button.getId()).thenReturn("btnConfirm");
		when(button.getValue()).thenReturn("Delete");

		ConfirmBehavior confirm = new ConfirmBehavior();
		confirm.setMessage(RAW_MARKUP_TEXT);
		confirm.setEscape(false);
		Map<String, List<ClientBehavior>> behaviours = new HashMap<>();
		behaviours.put("click", List.of(confirm));
		when(button.getClientBehaviors()).thenReturn(behaviours);

		String rendered = new ComponentRenderer(button).toString();

		assertTrue(rendered.contains("<p:confirm message=\"" + Encode.forHtmlAttribute(RAW_MARKUP_TEXT) + "\" escape=\"false\" />"));
	}

	@Test
	void rendersAutoCompleteTagAttributesAsFacetAttributes() {
		AutoComplete complete = mock(AutoComplete.class);
		stubCommonComponentState(complete);
		when(complete.getId()).thenReturn("ac1");
		when(complete.getVar()).thenReturn("item");
		when(complete.getAttributes()).thenReturn(new HashMap<>(Map.of("module", "admin", "query", "qUser", "display", "name")));

		String rendered = new ComponentRenderer(complete).toString();

		assertTrue(rendered.contains("<p:autoComplete id=\"ac1\""));
		assertTrue(rendered.contains(" var=\"item\""));
		assertTrue(rendered.contains("<f:attribute name=\"module\" value=\"admin\" />"));
		assertTrue(rendered.contains("<f:attribute name=\"query\" value=\"qUser\" />"));
		assertTrue(rendered.contains("<f:attribute name=\"display\" value=\"name\" />"));
		assertTrue(rendered.contains("</p:autoComplete>"));
	}

	@Test
	void rendersInputTextInputSpecificAttributes() {
		InputText input = mock(InputText.class);
		stubCommonComponentState(input);
		when(input.getId()).thenReturn("input1");
		when(input.getValue()).thenReturn("abc");
		when(input.isDisabled()).thenReturn(true);
		when(input.isReadonly()).thenReturn(true);
		when(input.getTitle()).thenReturn("Title");
		when(input.getRequiredMessage()).thenReturn("Required");

		String rendered = new ComponentRenderer(input).toString();

		assertTrue(rendered.contains("<p:inputText id=\"input1\""));
		assertTrue(rendered.contains(" value=\"abc\""));
		assertTrue(rendered.contains(" disabled=\"true\""));
		assertTrue(rendered.contains(" readonly=\"true\""));
		assertTrue(rendered.contains(" title=\"Title\""));
		assertTrue(rendered.contains(" requiredMessage=\"Required\""));
	}

	@Test
	void rendersUiParameterAndUiSelectItemsAsFaceletTags() {
		UIParameter parameter = mock(UIParameter.class);
		stubCommonComponentState(parameter);
		when(parameter.getId()).thenReturn("param1");
		when(parameter.getName()).thenReturn("myParam");
		String parameterRendered = new ComponentRenderer(parameter).toString();

		UISelectItems selectItems = mock(UISelectItems.class);
		stubCommonComponentState(selectItems);
		when(selectItems.getId()).thenReturn("items1");
		String selectItemsRendered = new ComponentRenderer(selectItems).toString();

		assertTrue(parameterRendered.contains("<f:parameter id=\"param1\" name=\"myParam\""));
		assertTrue(parameterRendered.endsWith(" />\n"));
		assertTrue(selectItemsRendered.contains("<f:selectItems id=\"items1\""));
		assertTrue(selectItemsRendered.endsWith(" />\n"));
	}

	@Test
	void rendersDataTableConditionalAttributesWhenEnabled() {
		DataTable table = mock(DataTable.class);
		stubCommonComponentState(table);
		when(table.getId()).thenReturn("table1");
		when(table.getVar()).thenReturn("row");
		when(table.isPaginator()).thenReturn(true);
		when(table.getRowsPerPageTemplate()).thenReturn("10,20,50");
		when(table.getRows()).thenReturn(20);
		when(table.isPaginatorAlwaysVisible()).thenReturn(false);
		when(table.isLazy()).thenReturn(true);
		when(table.getEmptyMessage()).thenReturn("No records");
		when(table.isStickyHeader()).thenReturn(true);
		when(table.getSortMode()).thenReturn("multiple");
		when(table.getWidgetVar()).thenReturn("tableWidget");
		when(table.getSelectionMode()).thenReturn("single");

		String rendered = new ComponentRenderer(table).toString();

		assertTrue(rendered.contains("<p:dataTable id=\"table1\""));
		assertTrue(rendered.contains(" paginator=\"true\""));
		assertTrue(rendered.contains(" rowsPerPageTemplate=\"10,20,50\""));
		assertTrue(rendered.contains(" rows=\"20\""));
		assertTrue(rendered.contains(" paginatorAlwaysVisible=\"false\""));
		assertTrue(rendered.contains(" lazy=\"true\""));
		assertTrue(rendered.contains(" stickyHeader=\"true\""));
		assertTrue(rendered.contains(" sortMode=\"multiple\""));
	}

	@Test
	void omitsDataTableConditionalAttributesWhenDisabled() {
		DataTable table = mock(DataTable.class);
		stubCommonComponentState(table);
		when(table.getId()).thenReturn("table2");
		when(table.getVar()).thenReturn("row");
		when(table.isPaginator()).thenReturn(false);
		when(table.isLazy()).thenReturn(false);
		when(table.isStickyHeader()).thenReturn(false);
		when(table.getSortMode()).thenReturn("single");

		String rendered = new ComponentRenderer(table).toString();

		assertTrue(rendered.contains("<p:dataTable id=\"table2\""));
		assertFalse(rendered.contains(" paginator=\"true\""));
		assertFalse(rendered.contains(" lazy=\"true\""));
		assertFalse(rendered.contains(" stickyHeader=\"true\""));
		assertFalse(rendered.contains(" sortMode=\"multiple\""));
	}

	@Test
	void rendersColumnPriorityAndSortableFalseWhenConfigured() {
		Column column = mock(Column.class);
		stubCommonComponentState(column);
		when(column.getId()).thenReturn("col1");
		when(column.getHeaderText()).thenReturn("Name");
		when(column.getResponsivePriority()).thenReturn(3);
		when(column.getField()).thenReturn("name");
		when(column.isSortable()).thenReturn(false);

		String rendered = new ComponentRenderer(column).toString();

		assertTrue(rendered.contains("<p:column id=\"col1\""));
		assertTrue(rendered.contains(" headerText=\"Name\""));
		assertTrue(rendered.contains(" priority=\"3\""));
		assertTrue(rendered.contains(" field=\"name\""));
		assertTrue(rendered.contains(" sortable=\"false\""));
	}

	@Test
	void rendersGraphicImageUrlWhenNoValueOrValueExpressionPresent() {
		GraphicImage image = mock(GraphicImage.class);
		stubCommonComponentState(image);
		when(image.getId()).thenReturn("img1");
		when(image.getValue()).thenReturn(null);
		when(image.getUrl()).thenReturn("/content/image.png");

		String rendered = new ComponentRenderer(image).toString();

		assertTrue(rendered.contains("<p:graphicImage id=\"img1\""));
		assertTrue(rendered.contains(" url=\"/content/image.png\""));
		assertFalse(rendered.contains(" value=\""));
	}

	@Test
	void rendersGraphicImageValueAndOmitsUrlWhenValuePresent() {
		GraphicImage image = mock(GraphicImage.class);
		stubCommonComponentState(image);
		when(image.getId()).thenReturn("img2");
		when(image.getValue()).thenReturn("#{bean.image}");
		when(image.getUrl()).thenReturn("/content/fallback.png");

		String rendered = new ComponentRenderer(image).toString();

		assertTrue(rendered.contains("<p:graphicImage id=\"img2\""));
		assertTrue(rendered.contains(" value=\"#{bean.image}\""));
		assertFalse(rendered.contains(" url=\"/content/fallback.png\""));
	}

	@Test
	void rendersGraphicImageValueExpressionAndOmitsUrl() {
		GraphicImage image = mock(GraphicImage.class);
		stubCommonComponentState(image);
		when(image.getId()).thenReturn("img3");
		when(image.getValue()).thenReturn(null);
		when(image.getUrl()).thenReturn("/content/fallback.png");

		ValueExpression valueExpression = mock(ValueExpression.class);
		when(valueExpression.getExpressionString()).thenReturn("#{bean.content}");
		when(image.getValueExpression("value")).thenReturn(valueExpression);

		String rendered = new ComponentRenderer(image).toString();

		assertTrue(rendered.contains("<p:graphicImage id=\"img3\""));
		assertTrue(rendered.contains(" value=\"#{bean.content}\""));
		assertFalse(rendered.contains(" url=\"/content/fallback.png\""));
	}

	@Test
	void rendersUiCommandActionExpressionAttributeWhenPresent() {
		CommandButton button = mock(CommandButton.class);
		stubCommonComponentState(button);
		when(button.getId()).thenReturn("btnAction");
		when(button.getValue()).thenReturn("Submit");

		MethodExpression actionExpression = mock(MethodExpression.class);
		when(actionExpression.getExpressionString()).thenReturn("#{bean.submit}");
		when(button.getActionExpression()).thenReturn(actionExpression);

		String rendered = new ComponentRenderer(button).toString();

		assertTrue(rendered.contains(" action=\"#{bean.submit}\""));
	}

	@Test
	void rendersCommandButtonDisableOnAjaxWhenEnabled() {
		CommandButton button = mock(CommandButton.class);
		stubCommonComponentState(button);
		when(button.getId()).thenReturn("btnDisableAjax");
		when(button.getValue()).thenReturn("Save");
		when(button.isDisableOnAjax()).thenReturn(true);

		String rendered = new ComponentRenderer(button).toString();

		assertTrue(rendered.contains("<p:commandButton id=\"btnDisableAjax\""));
		assertTrue(rendered.contains(" disableOnAjax=\"true\""));
	}

	@Test
	void rendersCommandLinkDisabledAttributeWhenDisabled() {
		CommandLink link = mock(CommandLink.class);
		stubCommonComponentState(link);
		when(link.getId()).thenReturn("lnk1");
		when(link.getValue()).thenReturn("Go");
		when(link.isDisabled()).thenReturn(true);

		String rendered = new ComponentRenderer(link).toString();

		assertTrue(rendered.contains("<p:commandLink id=\"lnk1\""));
		assertTrue(rendered.contains(" value=\"Go\""));
		assertTrue(rendered.contains(" disabled=\"true\""));
	}

	@Test
	void omitsCommandLinkDisabledAttributeWhenEnabled() {
		CommandLink link = mock(CommandLink.class);
		stubCommonComponentState(link);
		when(link.getId()).thenReturn("lnk2");
		when(link.getValue()).thenReturn("Go");
		when(link.isDisabled()).thenReturn(false);

		String rendered = new ComponentRenderer(link).toString();

		assertTrue(rendered.contains("<p:commandLink id=\"lnk2\""));
		assertFalse(rendered.contains(" disabled=\"true\""));
	}

	@Test
	void rendersInputTextareaSpecificAttributes() {
		InputTextarea input = mock(InputTextarea.class);
		stubCommonComponentState(input);
		when(input.getId()).thenReturn("inputArea1");
		when(input.getValue()).thenReturn("multiline");
		when(input.isDisabled()).thenReturn(true);
		when(input.isReadonly()).thenReturn(true);
		when(input.getTitle()).thenReturn("AreaTitle");
		when(input.getRequiredMessage()).thenReturn("AreaRequired");

		String rendered = new ComponentRenderer(input).toString();

		assertTrue(rendered.contains("<p:inputTextarea id=\"inputArea1\""));
		assertTrue(rendered.contains(" value=\"multiline\""));
		assertTrue(rendered.contains(" disabled=\"true\""));
		assertTrue(rendered.contains(" readonly=\"true\""));
		assertTrue(rendered.contains(" title=\"AreaTitle\""));
		assertTrue(rendered.contains(" requiredMessage=\"AreaRequired\""));
	}

	@Test
	void rendersOverlayPanelOnShowFromValueExpression() {
		OverlayPanel overlay = mock(OverlayPanel.class);
		stubCommonComponentState(overlay);
		when(overlay.getId()).thenReturn("overlay1");
		when(overlay.getWidgetVar()).thenReturn("ov");
		when(overlay.getFor()).thenReturn("target");
		when(overlay.isDynamic()).thenReturn(true);
		when(overlay.isShowCloseIcon()).thenReturn(true);
		when(overlay.isModal()).thenReturn(false);

		ValueExpression onShow = mock(ValueExpression.class);
		when(onShow.getExpressionString()).thenReturn("#{bean.show}");
		when(overlay.getValueExpression("onShow")).thenReturn(onShow);

		String rendered = new ComponentRenderer(overlay).toString();

		assertTrue(rendered.contains("<p:overlayPanel id=\"overlay1\""));
		assertTrue(rendered.contains(" onShow=\"#{bean.show}\""));
		assertTrue(rendered.contains(" dynamic=\"true\""));
		assertTrue(rendered.contains(" showCloseIcon=\"true\""));
	}

	@Test
	void omitsColumnPriorityWhenOutsideSupportedRange() {
		Column column = mock(Column.class);
		stubCommonComponentState(column);
		when(column.getId()).thenReturn("col2");
		when(column.getHeaderText()).thenReturn("Status");
		when(column.getResponsivePriority()).thenReturn(8);
		when(column.isSortable()).thenReturn(true);

		String rendered = new ComponentRenderer(column).toString();

		assertTrue(rendered.contains("<p:column id=\"col2\""));
		assertFalse(rendered.contains(" priority=\""));
		assertFalse(rendered.contains(" sortable=\"false\""));
	}

	@Test
	void excludesManagedBeanTypeAndComSunAttributesForPanelGroup() {
		HtmlPanelGroup panel = mock(HtmlPanelGroup.class);
		stubCommonComponentState(panel);
		when(panel.getId()).thenReturn("panel2");
		when(panel.getLayout()).thenReturn("block");
		Map<String, Object> attrs = new HashMap<>();
		attrs.put("managedBean", "shouldNotRender");
		attrs.put("type", "shouldNotRender");
		attrs.put("com.sun.faces.some", "ignored");
		attrs.put("data-test", "ok");
		when(panel.getAttributes()).thenReturn(attrs);

		String rendered = new ComponentRenderer(panel).toString();

		assertTrue(rendered.contains("<h:panelGroup id=\"panel2\""));
		assertTrue(rendered.contains(" data-test=\"ok\""));
		assertFalse(rendered.contains("managedBean=\"shouldNotRender\""));
		assertFalse(rendered.contains("type=\"shouldNotRender\""));
		assertFalse(rendered.contains("com.sun.faces.some=\"ignored\""));
	}

	@Test
	void rendersButtonTag() {
		Button button = mock(Button.class);
		stubCommonComponentState(button);
		when(button.getId()).thenReturn("btn1");
		String rendered = new ComponentRenderer(button).toString();
		assertTrue(rendered.contains("<p:button id=\"btn1\""), rendered);
	}

	@Test
	void rendersDatePickerTag() {
		DatePicker picker = mock(DatePicker.class);
		stubCommonComponentState(picker);
		when(picker.getId()).thenReturn("dp1");
		String rendered = new ComponentRenderer(picker).toString();
		assertTrue(rendered.contains("<p:datePicker id=\"dp1\""), rendered);
	}

	@Test
	void rendersCellEditorTag() {
		CellEditor ce = mock(CellEditor.class);
		stubCommonComponentState(ce);
		when(ce.getId()).thenReturn("ce1");
		String rendered = new ComponentRenderer(ce).toString();
		assertTrue(rendered.contains("<p:cellEditor id=\"ce1\""), rendered);
	}

	@Test
	void rendersColorPickerTag() {
		ColorPicker cp = mock(ColorPicker.class);
		stubCommonComponentState(cp);
		when(cp.getId()).thenReturn("color1");
		String rendered = new ComponentRenderer(cp).toString();
		assertTrue(rendered.contains("<p:colorPicker id=\"color1\""), rendered);
	}

	@Test
	void rendersDataListTag() {
		DataList list = mock(DataList.class);
		stubCommonComponentState(list);
		when(list.getId()).thenReturn("dl1");
		String rendered = new ComponentRenderer(list).toString();
		assertTrue(rendered.contains("<p:dataList id=\"dl1\""), rendered);
	}

	@Test
	void rendersTextEditorTag() {
		TextEditor editor = mock(TextEditor.class);
		stubCommonComponentState(editor);
		when(editor.getId()).thenReturn("editor1");
		String rendered = new ComponentRenderer(editor).toString();
		assertTrue(rendered.contains("<p:editor id=\"editor1\""), rendered);
	}

	@Test
	void rendersFieldsetTag() {
		Fieldset fs = mock(Fieldset.class);
		stubCommonComponentState(fs);
		when(fs.getId()).thenReturn("fs1");
		String rendered = new ComponentRenderer(fs).toString();
		assertTrue(rendered.contains("<p:fieldset id=\"fs1\""), rendered);
	}

	@Test
	void rendersFileUploadTag() {
		FileUpload upload = mock(FileUpload.class);
		stubCommonComponentState(upload);
		when(upload.getId()).thenReturn("fu1");
		String rendered = new ComponentRenderer(upload).toString();
		assertTrue(rendered.contains("<p:fileUpload id=\"fu1\""), rendered);
	}

	@Test
	void rendersInputMaskTag() {
		InputMask mask = mock(InputMask.class);
		stubCommonComponentState(mask);
		when(mask.getId()).thenReturn("mask1");
		String rendered = new ComponentRenderer(mask).toString();
		assertTrue(rendered.contains("<p:inputMask id=\"mask1\""), rendered);
	}

	@Test
	void rendersMessageTag() {
		Message msg = mock(Message.class);
		stubCommonComponentState(msg);
		when(msg.getId()).thenReturn("msg1");
		String rendered = new ComponentRenderer(msg).toString();
		assertTrue(rendered.contains("<p:message id=\"msg1\""), rendered);
	}

	@Test
	void rendersOutputPanelTag() {
		OutputPanel op = mock(OutputPanel.class);
		stubCommonComponentState(op);
		when(op.getId()).thenReturn("op1");
		String rendered = new ComponentRenderer(op).toString();
		assertTrue(rendered.contains("<p:outputPanel id=\"op1\""), rendered);
	}

	@Test
	void rendersPanelTag() {
		Panel panel = mock(Panel.class);
		stubCommonComponentState(panel);
		when(panel.getId()).thenReturn("panel1");
		String rendered = new ComponentRenderer(panel).toString();
		assertTrue(rendered.contains("<p:panel id=\"panel1\""), rendered);
	}

	@Test
	void rendersPanelGridTag() {
		PanelGrid grid = mock(PanelGrid.class);
		stubCommonComponentState(grid);
		when(grid.getId()).thenReturn("pg1");
		String rendered = new ComponentRenderer(grid).toString();
		assertTrue(rendered.contains("<p:panelGrid id=\"pg1\""), rendered);
	}

	@Test
	void rendersPasswordTag() {
		Password pw = mock(Password.class);
		stubCommonComponentState(pw);
		when(pw.getId()).thenReturn("pw1");
		String rendered = new ComponentRenderer(pw).toString();
		assertTrue(rendered.contains("<p:password id=\"pw1\""), rendered);
	}

	@Test
	void rendersPickListTag() {
		PickList pl = mock(PickList.class);
		stubCommonComponentState(pl);
		when(pl.getId()).thenReturn("pl1");
		String rendered = new ComponentRenderer(pl).toString();
		assertTrue(rendered.contains("<p:pickList id=\"pl1\""), rendered);
	}

	@Test
	void rendersProgressBarTag() {
		ProgressBar pb = mock(ProgressBar.class);
		stubCommonComponentState(pb);
		when(pb.getId()).thenReturn("pb1");
		String rendered = new ComponentRenderer(pb).toString();
		assertTrue(rendered.contains("<p:progressBar id=\"pb1\""), rendered);
	}

	@Test
	void rendersRowTag() {
		Row row = mock(Row.class);
		stubCommonComponentState(row);
		when(row.getId()).thenReturn("row1");
		String rendered = new ComponentRenderer(row).toString();
		assertTrue(rendered.contains("<p:row id=\"row1\""), rendered);
	}

	@Test
	void rendersSelectBooleanCheckboxTag() {
		SelectBooleanCheckbox check = mock(SelectBooleanCheckbox.class);
		stubCommonComponentState(check);
		when(check.getId()).thenReturn("sbc1");
		String rendered = new ComponentRenderer(check).toString();
		assertTrue(rendered.contains("<p:selectBooleanCheckbox id=\"sbc1\""), rendered);
	}

	@Test
	void rendersSelectManyCheckboxTag() {
		SelectManyCheckbox checks = mock(SelectManyCheckbox.class);
		stubCommonComponentState(checks);
		when(checks.getId()).thenReturn("smc1");
		String rendered = new ComponentRenderer(checks).toString();
		assertTrue(rendered.contains("<p:selectManyCheckbox id=\"smc1\""), rendered);
	}

	@Test
	void rendersSelectOneMenuTag() {
		SelectOneMenu pick = mock(SelectOneMenu.class);
		stubCommonComponentState(pick);
		when(pick.getId()).thenReturn("som1");
		String rendered = new ComponentRenderer(pick).toString();
		assertTrue(rendered.contains("<p:selectOneMenu id=\"som1\""), rendered);
	}

	@Test
	void rendersSelectOneRadioTag() {
		SelectOneRadio radio = mock(SelectOneRadio.class);
		stubCommonComponentState(radio);
		when(radio.getId()).thenReturn("sor1");
		String rendered = new ComponentRenderer(radio).toString();
		assertTrue(rendered.contains("<p:selectOneRadio id=\"sor1\""), rendered);
	}

	@Test
	void rendersSignatureTag() {
		Signature sig = mock(Signature.class);
		stubCommonComponentState(sig);
		when(sig.getId()).thenReturn("sig1");
		String rendered = new ComponentRenderer(sig).toString();
		assertTrue(rendered.contains("<p:signature id=\"sig1\""), rendered);
	}

	@Test
	void rendersSpacerTag() {
		Spacer spacer = mock(Spacer.class);
		stubCommonComponentState(spacer);
		when(spacer.getId()).thenReturn("spacer1");
		String rendered = new ComponentRenderer(spacer).toString();
		assertTrue(rendered.contains("<p:spacer id=\"spacer1\""), rendered);
	}

	@Test
	void rendersSpinnerTag() {
		Spinner spinner = mock(Spinner.class);
		stubCommonComponentState(spinner);
		when(spinner.getId()).thenReturn("spin1");
		String rendered = new ComponentRenderer(spinner).toString();
		assertTrue(rendered.contains("<p:spinner id=\"spin1\""), rendered);
	}

	@Test
	void rendersTabTag() {
		Tab tab = mock(Tab.class);
		stubCommonComponentState(tab);
		when(tab.getId()).thenReturn("tab1");
		String rendered = new ComponentRenderer(tab).toString();
		assertTrue(rendered.contains("<p:tab id=\"tab1\""), rendered);
	}

	@Test
	void rendersTabViewTag() {
		TabView tabs = mock(TabView.class);
		stubCommonComponentState(tabs);
		when(tabs.getId()).thenReturn("tv1");
		String rendered = new ComponentRenderer(tabs).toString();
		assertTrue(rendered.contains("<p:tabView id=\"tv1\""), rendered);
	}

	@Test
	void rendersToolbarTag() {
		Toolbar toolbar = mock(Toolbar.class);
		stubCommonComponentState(toolbar);
		when(toolbar.getId()).thenReturn("tb1");
		String rendered = new ComponentRenderer(toolbar).toString();
		assertTrue(rendered.contains("<p:toolbar id=\"tb1\""), rendered);
	}

	@Test
	void rendersTriStateCheckboxTag() {
		TriStateCheckbox check = mock(TriStateCheckbox.class);
		stubCommonComponentState(check);
		when(check.getId()).thenReturn("tsc1");
		String rendered = new ComponentRenderer(check).toString();
		assertTrue(rendered.contains("<p:triStateCheckbox id=\"tsc1\""), rendered);
	}

	@Test
	void rendersOutputLabelTag() {
		OutputLabel label = mock(OutputLabel.class);
		stubCommonComponentState(label);
		when(label.getId()).thenReturn("ol1");
		when(label.getFor()).thenReturn("input1");
		when(label.getValue()).thenReturn(RAW_MARKUP_TEXT);
		when(label.isEscape()).thenReturn(false);
		String rendered = new ComponentRenderer(label).toString();
		assertTrue(rendered.contains("<p:outputLabel id=\"ol1\""), rendered);
		assertTrue(rendered.contains(" for=\"input1\""), rendered);
		assertTrue(rendered.contains(" escape=\"false\""), rendered);
		assertTrue(rendered.contains(" value=\"" + Encode.forHtmlAttribute(RAW_MARKUP_TEXT) + "\""), rendered);
	}

	@Test
	void rendersUISelectItemsTag() {
		UISelectItems items = mock(UISelectItems.class);
		stubCommonComponentState(items);
		when(items.getId()).thenReturn("si1");
		String rendered = new ComponentRenderer(items).toString();
		assertTrue(rendered.contains("<f:selectItems id=\"si1\""), rendered);
	}

	private static void stubCommonComponentState(jakarta.faces.component.UIComponentBase component) {
		when(component.getAttributes()).thenReturn(new HashMap<>());
		when(component.getPassThroughAttributes()).thenReturn(new HashMap<>());
		when(component.getFacets()).thenReturn(new HashMap<>());
		when(component.getChildren()).thenReturn(new ArrayList<>());
		when(component.getClientBehaviors()).thenReturn(new HashMap<>());
		when(component.getValueExpression(anyString())).thenReturn(null);
	}

	private static final class NameOnlyConverter implements Converter<Object> {
		@Override
		public Object getAsObject(FacesContext context, jakarta.faces.component.UIComponent component, String value) {
			return value;
		}

		@Override
		public String getAsString(FacesContext context, jakarta.faces.component.UIComponent component, Object value) {
			return String.valueOf(value);
		}
	}
}
