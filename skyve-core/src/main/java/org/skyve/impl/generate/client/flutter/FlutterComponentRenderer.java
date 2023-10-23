package org.skyve.impl.generate.client.flutter;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.generate.client.ComponentRenderer;
import org.skyve.impl.generate.client.RenderedComponent;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;

public class FlutterComponentRenderer extends ComponentRenderer {
    
    /**
     * Template for a SkyveButton; params are actionType, actionName & label
     */
    private final String btnTemplate = "SkyveButton( actionType: '%s', actionName: '%s', label: '%s'),";
    
	public static final String BORDER_IMPORT = "widgets/skyve_border";
	public static final String BUTTON_IMPORT = "widgets/skyve_button";
	public static final String CHECKBOX_IMPORT = "widgets/skyve_checkbox";
	public static final String COLOURPICKER_IMPORT = "widgets/skyve_colourpicker";
	public static final String COMBO_IMPORT = "widgets/skyve_combo";
	public static final String CONTENTIMAGE_IMPORT = "widgets/skyve_contentimage";
	private static final String CONTENTLINK_IMPORT = "widgets/skyve_contentlink";
	public static final String DATA_GRID_IMPORT = "widgets/skyve_datagrid";
	public static final String LABEL_IMPORT = "widgets/skyve_label";
	public static final String SPACER_IMPORT = "widgets/skyve_spacer";
	public static final String TAB_PANE_IMPORT = "widgets/skyve_tabpane";
	public static final String TAB_IMPORT = "widgets/skyve_tab";
	public static final String TEXTFIELD_IMPORT = "widgets/skyve_textfield";
	public static final String TOOLBAR_IMPORT = "widgets/skyve_toolbar";
	private static final String CONTENTSIGNATURE_IMPORT = "widgets/skyve_contentsignature";
	private static final String HTML_IMPORT = "widgets/skyve_html";
	private static final String LOOKUP_IMPORT = "widgets/skyve_lookupdescription";
	private static final String PASSWORD_IMPORT = "widgets/skyve_password";
	private static final String RADIO_IMPORT = "widgets/skyve_radio";
	private static final String RICHTEXT_IMPORT = "widgets/skyve_richtext";
	private static final String SPINNER_IMPORT = "widgets/skyve_spinner";
	private static final String TEXTAREA_IMPORT = "widgets/skyve_textarea";
	private static final String ACTIONLINK_IMPORT = "widgets/skyve_actionlink";
	private static final String REPORT_IMPORT = "widgets/skyve_report";
	private static final String DOWNLOAD_IMPORT = "widgets/skyve_download";
	private static final String UPLOAD_IMPORT = "widgets/skyve_upload";
	private static final String STATICIMAGE_IMPORT = "widgets/skyve_staticimage";
	private static final String DYNAMICIMAGE_IMPORT = "widgets/skyve_dynamicimage";
	private static final String BLURB_IMPORT = "widgets/skyve_blurb";

	private Set<String> imports;
	private String startingIndent;

	public FlutterComponentRenderer(Set<String> imports, String startingIndent) {
		this.imports = imports;
		this.startingIndent = startingIndent;
	}
	
	@Override
	public RenderedComponent view(RenderedComponent component, String invisibleConditionName) {
		imports.add(FlutterLayoutRenderer.VBOX_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),").setIndent(startingIndent);
		StringBuilder output = result.getOutput();
		output.append("Column(children: [");
		return result;
	}

	@Override
	public List<RenderedComponent> toolbars(List<RenderedComponent> components, String widgetId) {
		imports.add(TOOLBAR_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),");
		StringBuilder output = result.getOutput();
		output.append("SkyveToolbar(children: [");
		return Collections.singletonList(result);
	}

	@Override
	public RenderedComponent tabPane(RenderedComponent component, TabPane tabPane) {
		imports.add(TAB_PANE_IMPORT);
		imports.add(TAB_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),");
		StringBuilder output = result.getOutput();
		output.append("SkyveTabPane(children: [");
		return result;
	}

	@Override
	public RenderedComponent tab(RenderedComponent component, String title, Tab tab) {
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),");
		StringBuilder output = result.getOutput();
		output.append("SkyveTab(title: '").append(title).append("', children: [");
		return result;
	}

	@Override
	public RenderedComponent border(RenderedComponent component,
										String title,
										String invisibileConditionName,
										Integer pixelWidth) {
		imports.add(BORDER_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveBorder(");
		if (title != null) {
			output.append("title: '").append(title).append("', ");
		}
		output.append("child: ");
		result.setAfter("),");
		return result;
	}

	@Override
	public RenderedComponent label(RenderedComponent component, String value) {
		imports.add(LABEL_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("const SkyveLabel('").append(value).append("'),");
		return result;
	}

	@Override
	public RenderedComponent spacer(RenderedComponent component, Spacer spacer) {
		imports.add(SPACER_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("const SkyveSpacer(),");
		return result;
	}

	@Override
	public RenderedComponent actionButton(RenderedComponent component,
											String dataWidgetBinding,
											String dataWidgetVar,
											Button button,
											Action action) {
        imports.add(BUTTON_IMPORT);
        RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);

        String btnText = createButton(deriveActionType(action.getImplicitName()), action.getName(),
                action.getLocalisedDisplayName());
        result.getOutput()
              .append(btnText);

        return result;
    }
	
    private String deriveActionType(ImplicitActionName implicitName) {

        return Optional.ofNullable(implicitName)
                       .map(ImplicitActionName::name)
                       .orElse(" ");
    }

	@Override
	public RenderedComponent reportButton(RenderedComponent component, Button button, Action action) {
		imports.add(BUTTON_IMPORT);
        RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
        
        String btnText = createButton("P", action.getName(), action.getLocalisedDisplayName());
        result.getOutput().append(btnText);

        return result;
    }

	@Override
	public RenderedComponent downloadButton(RenderedComponent component,
												Button button,
												Action action,
												String moduleName,
												String documentName) {
        imports.add(BUTTON_IMPORT);
        RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);

        String btnText = createButton("L", action.getName(), action.getLocalisedDisplayName());
        result.getOutput()
              .append(btnText);

        return result;
    }

	@Override
	public RenderedComponent staticImage(RenderedComponent component, String fileUrl, StaticImage image) {
		imports.add(STATICIMAGE_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveStaticImage(label: 'StaticImage'),");
		return result;
	}

	@Override
	public RenderedComponent dynamicImage(RenderedComponent component,
											DynamicImage image,
											String moduleName,
											String documentName) {
		imports.add(DYNAMICIMAGE_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveDynamicImage(label: 'DynamicImage'),");
		return result;
	}

	@Override
	public RenderedComponent blurb(RenderedComponent component,
									String dataWidgetVar,
									String value,
									String binding,
									Blurb blurb) {
		imports.add(BLURB_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveBlurb(label: 'Blurb'),");
		return result;
	}

	@Override
	public RenderedComponent label(RenderedComponent component,
									String dataWidgetVar,
									String value,
									String binding,
									Label label) {
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		if (value != null) {
			output.append("const SkyveLabel('").append(value).append("'),");
		}
		else {
			// TODO handle expressions coming through as bindings
			output.append("SkyveLabel('${_bean[\"").append(BindUtil.sanitiseBinding(binding)).append("\"]}'),");	
		}
		return result;
	}

	@Override
	public RenderedComponent dataGrid(RenderedComponent component,
										String dataWidgetVar,
										boolean ordered,
										String title,
										DataGrid grid) {
		imports.add(DATA_GRID_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("SkyveDataGrid(rows: _bean['").append(BindUtil.sanitiseBinding(grid.getBinding()));
		output.append("'], children: [");
		return result;
	}

	@Override
	public RenderedComponent dataRepeater(RenderedComponent component,
											String dataWidgetVar,
											String title,
											DataRepeater repeater) {
		imports.add(DATA_GRID_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("SkyveDataGrid(rows: _bean['").append(BindUtil.sanitiseBinding(repeater.getBinding()));
		output.append("'], children: [");
		return result;
	}

	@Override
	public RenderedComponent addDataGridBoundColumn(RenderedComponent component,
														RenderedComponent current,
														AbstractDataWidget widget,
														DataGridBoundColumn column,
														String dataWidgetVar,
														String columnTitle,
														String columnBinding,
														StringBuilder gridColumnExpression) {
/*
		imports.add("{Column}");
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setIndent("");
		StringBuilder output = result.getOutput();
		output.append("<Column field=\"").append(BindUtil.sanitiseBinding(columnBinding));
		output.append("\" header=\"").append(columnTitle).append("\" />");
		current.addChild(result);
		return result;
*/
		return current;
	}

	@Override
	public RenderedComponent addedDataGridBoundColumn(RenderedComponent component, RenderedComponent current) {
//		return current.getParent();
		return current;
	}

	@Override
	public RenderedComponent addDataGridContainerColumn(RenderedComponent component,
															RenderedComponent current,
															AbstractDataWidget widget,
															String columnTitle,
															DataGridContainerColumn column) {
		return current;
	}

	@Override
	public RenderedComponent addedDataGridContainerColumn(RenderedComponent component, RenderedComponent current) {
		return current;
	}

	@Override
	public RenderedComponent addDataGridActionColumn(RenderedComponent component,
														RenderedComponent current,
														DataGrid grid,
														String dataWidgetVar,
														String gridColumnExpression,
														String singluarDocumentAlias,
														boolean inline) {
		return current;
	}

	@Override
	public RenderedComponent listGrid(RenderedComponent component,
										String modelDocumentName,
										String modelName,
										ListModel<Bean> model,
										String title,
										ListGrid listGrid,
										boolean aggregateQuery) {
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("const Text('ListGrid'),");
		return result;
	}

	@Override
	public RenderedComponent listRepeater(RenderedComponent component,
											String modelDocumentName,
											String modelName,
											ListModel<Bean> model,
											List<FilterParameter> filterParameters,
											List<Parameter> parameters,
											String title,
											boolean showColumnHeaders,
											boolean showGrid) {
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("const Text('ListRepeater'),");
		return result;
	}

	@Override
	public RenderedComponent listMembership(RenderedComponent component, ListMembership membership) {
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("const Text('ListMembership'),");
		return result;
	}

	@Override
	public RenderedComponent checkBox(RenderedComponent component,
										String dataWidgetVar,
										CheckBox checkBox,
										String title,
										boolean required) {
		imports.add(CHECKBOX_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();

		// output.append("SkyveCheckBox(label: '").append(title).append("', isChecked: _bean[\"")
		// .append(BindUtil.sanitiseBinding(checkBox.getBinding())).append("\"], tristate: ")
		// .append(required + "),");
		// TODO: handle proper input
		output.append("const SkyveCheckBox(label: '").append(title).append("', isChecked: false, tristate: ").append(required + "),");

		return result;
	}

	@Override
	public RenderedComponent colourPicker(RenderedComponent component,
											String dataWidgetVar,
											ColourPicker colour,
											String title,
											boolean required) {
		imports.add(COLOURPICKER_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('ColourPicker ${_bean[\"").append(BindUtil.sanitiseBinding(colour.getBinding())).append("\"]}'),");
		output.append("SkyveColourPicker(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public RenderedComponent combo(RenderedComponent component,
									String dataWidgetVar,
									Combo combo,
									String title,
									boolean required) {
		imports.add(COMBO_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('Combo ${_bean[\"").append(BindUtil.sanitiseBinding(combo.getBinding())).append("\"]}'),");
		output.append("SkyveCombo(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public RenderedComponent contentImage(RenderedComponent component,
											String dataWidgetVar,
											ContentImage image,
											String title,
											boolean required) {
		imports.add(CONTENTIMAGE_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('ContentImage ${_bean[\"").append(BindUtil.sanitiseBinding(image.getBinding())).append("\"]}'),");
		output.append("SkyveContentImage(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public RenderedComponent contentLink(RenderedComponent component,
											String dataWidgetVar,
											ContentLink link,
											String title,
											boolean required) {
		imports.add(CONTENTLINK_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('ContentLink ${_bean[\"").append(BindUtil.sanitiseBinding(link.getBinding())).append("\"]}'),");
		output.append("SkyveContentLink(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public RenderedComponent contentSignature(RenderedComponent component,
												String dataWidgetVar,
												ContentSignature signature,
												String title,
												boolean required) {
		imports.add(CONTENTSIGNATURE_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('ContentSignature
		// ${_bean[\"").append(BindUtil.sanitiseBinding(signature.getBinding())).append("\"]}'),");
		output.append("SkyveContentSignature(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public RenderedComponent html(RenderedComponent component,
									String dataWidgetVar,
									HTML html,
									String title,
									boolean required) {
		imports.add(HTML_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('HTML ${_bean[\"").append(BindUtil.sanitiseBinding(html.getBinding())).append("\"]}'),");
		output.append("SkyveHTML(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public RenderedComponent lookupDescription(RenderedComponent component,
												String dataWidgetVar,
												LookupDescription lookup,
												String title,
												boolean required,
												String displayBinding,
												QueryDefinition query) {
		imports.add(LOOKUP_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('LookupDescription
		// ${_bean[\"").append(BindUtil.sanitiseBinding(lookup.getBinding())).append("\"]}'),");
		output.append("SkyveLookupDescription(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public RenderedComponent password(RenderedComponent component,
										String dataWidgetVar,
										Password password,
										String title,
										boolean required) {
		imports.add(PASSWORD_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("const Text('Password'),");
		output.append("SkyvePassword(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public RenderedComponent radio(RenderedComponent component,
									String dataWidgetVar,
									Radio radio,
									String title,
									boolean required) {
		imports.add(RADIO_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('Radio ${_bean[\"").append(BindUtil.sanitiseBinding(radio.getBinding())).append("\"]}'),");
		output.append("SkyveRadio(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public RenderedComponent richText(RenderedComponent component,
										String dataWidgetVar,
										RichText text,
										String title,
										boolean required) {
		imports.add(RICHTEXT_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('RichText ${_bean[\"").append(BindUtil.sanitiseBinding(text.getBinding())).append("\"]}'),");
		output.append("SkyveRichText(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public RenderedComponent spinner(RenderedComponent component,
										String dataWidgetVar,
										Spinner spinner,
										String title,
										boolean required) {
		imports.add(SPINNER_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('Spinner ${_bean[\"").append(BindUtil.sanitiseBinding(spinner.getBinding())).append("\"]}'),");
		output.append("SkyveSpinner(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public RenderedComponent text(RenderedComponent component,
									String dataWidgetVar,
									TextField text,
									String title,
									boolean required,
									Integer length,
									Converter<?> converter,
									Format<?> format) {
        imports.add(TEXTFIELD_IMPORT);
        RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);

        final String stfTemplate = "SkyveTextField(label: '%s', propertyKey: '%s'),";
        String stfCode = String.format(stfTemplate, title, BindUtil.sanitiseBinding(text.getBinding()));

        result.getOutput()
              .append(stfCode);

        return result;
    }

	@Override
	public RenderedComponent textArea(RenderedComponent component,
										String dataWidgetVar,
										TextArea text,
										String title,
										boolean required,
										Integer length) {
		imports.add(TEXTAREA_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveTextArea(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public RenderedComponent actionLink(RenderedComponent component,
											String dataWidgetBinding,
											String dataWidgetVar,
											Link link,
											Action action) {
		imports.add(ACTIONLINK_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveActionLink(label: 'ActionLink'),");
		return result;
	}

	@Override
	public RenderedComponent report(RenderedComponent component, Action action) {
		imports.add(REPORT_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveReport(label: 'Report'),");
		return result;
	}

	@Override
	public RenderedComponent download(RenderedComponent component,
										Action action,
										String moduleName,
										String documentName) {
		imports.add(DOWNLOAD_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveDownload(label: 'Download'),");
		return result;
	}

	@Override
	public RenderedComponent upload(RenderedComponent component, Action action) {
		imports.add(UPLOAD_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveUpload(label: 'Upload'),");
		return result;
	}

	@Override
	public RenderedComponent action(RenderedComponent component,
										String dataWidgetBinding,
										String dataWidgetVar,
										Action action,
										ImplicitActionName name,
										String title) {
        imports.add(BUTTON_IMPORT);
        RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);

        final String actionName;

        if (name == null) {
            // custom action
            // name == null
            // action.getName() has what we need
            actionName = action.getName();
        } else {
            // typical action
            // name != null
            actionName = name.name();
        }

        String actionType = deriveActionType(name);
        String btnText = createButton(actionType, actionName, title);
        result.getOutput()
              .append(btnText);
        return result;
    }

    private String createButton(String actionType, String actionName, String label) {
        return String.format(btnTemplate, actionType, actionName, label);
    }
}
