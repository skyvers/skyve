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
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.ContentUpload;
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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Flutter-specific widget/component renderer.
 */
@SuppressWarnings("java:S1192") // Repeated literals are deliberate fragments of generated Flutter component output.
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

	public FlutterComponentRenderer(@Nonnull Set<String> imports, @Nonnull String startingIndent) {
		this.imports = imports;
		this.startingIndent = startingIndent;
	}
	
	@Override
	public @Nonnull RenderedComponent view(@Nullable RenderedComponent component, @Nonnull String invisibleCondition) {
		imports.add(FlutterLayoutRenderer.VBOX_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),").setIndent(startingIndent);
		StringBuilder output = result.getOutput();
		output.append("Column(children: [");
		return result;
	}

	@Override
	public @Nonnull List<RenderedComponent> toolbars(@Nullable List<RenderedComponent> components, @Nullable String widgetId) {
		imports.add(TOOLBAR_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),");
		StringBuilder output = result.getOutput();
		output.append("SkyveToolbar(children: [");
		return Collections.singletonList(result);
	}

	@Override
	public @Nonnull RenderedComponent tabPane(@Nullable RenderedComponent component, @Nonnull TabPane tabPane) {
		imports.add(TAB_PANE_IMPORT);
		imports.add(TAB_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),");
		StringBuilder output = result.getOutput();
		output.append("SkyveTabPane(children: [");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent tab(@Nullable RenderedComponent component, @Nullable String title, @Nonnull Tab tab) {
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),");
		StringBuilder output = result.getOutput();
		output.append("SkyveTab(title: '").append(title).append("', children: [");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent border(@Nullable RenderedComponent component,
												@Nullable String title,
												@Nullable String invisibleCondition,
												@Nullable Integer pixelWidth) {
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
	public @Nonnull RenderedComponent label(@Nullable RenderedComponent component, @Nonnull String value) {
		imports.add(LABEL_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("const SkyveLabel('").append(value).append("'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent spacer(@Nullable RenderedComponent component, @Nonnull Spacer spacer) {
		imports.add(SPACER_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("const SkyveSpacer(),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent actionButton(@Nullable RenderedComponent component,
													@Nullable String dataWidgetBinding,
													@Nullable String dataWidgetVar,
													@Nonnull Button button,
													@Nonnull Action action) {
        imports.add(BUTTON_IMPORT);
        RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);

        String btnText = createButton(deriveActionType(action.getImplicitName()), action.getName(),
                action.getLocalisedDisplayName());
        result.getOutput()
              .append(btnText);

        return result;
    }
	
    private static @Nonnull String deriveActionType(@Nullable ImplicitActionName implicitName) {
        return Optional.ofNullable(implicitName)
                       .map(ImplicitActionName::name)
                       .orElse(" ");
    }

	@Override
	public @Nonnull RenderedComponent reportButton(@Nullable RenderedComponent component, @Nonnull Button button, @Nonnull Action action) {
		imports.add(BUTTON_IMPORT);
        RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
        
        String btnText = createButton("P", action.getName(), action.getLocalisedDisplayName());
        result.getOutput().append(btnText);

        return result;
    }

	@Override
	public @Nonnull RenderedComponent downloadButton(@Nullable RenderedComponent component,
														@Nonnull Button button,
														@Nonnull Action action,
														@Nonnull String moduleName,
														@Nonnull String documentName) {
        imports.add(BUTTON_IMPORT);
        RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);

        String btnText = createButton("L", action.getName(), action.getLocalisedDisplayName());
        result.getOutput()
              .append(btnText);

        return result;
    }

	@Override
	public @Nonnull RenderedComponent staticImage(@Nullable RenderedComponent component, @Nonnull String fileUrl, @Nonnull StaticImage image) {
		imports.add(STATICIMAGE_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveStaticImage(label: 'StaticImage'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent dynamicImage(@Nullable RenderedComponent component,
													@Nonnull DynamicImage image,
													@Nonnull String moduleName,
													@Nonnull String documentName) {
		imports.add(DYNAMICIMAGE_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveDynamicImage(label: 'DynamicImage'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent blurb(@Nullable RenderedComponent component,
											@Nullable String dataWidgetVar,
											@Nullable String value,
											@Nullable String binding,
											@Nonnull Blurb blurb) {
		imports.add(BLURB_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveBlurb(label: 'Blurb'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent label(@Nullable RenderedComponent component,
											@Nullable String dataWidgetVar,
											@Nullable String value,
											@Nullable String binding,
											@Nonnull Label label) {
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
	public @Nonnull RenderedComponent dataGrid(@Nullable RenderedComponent component,
												@Nullable String dataWidgetVar,
												boolean ordered,
												@Nullable String title,
												@Nonnull DataGrid grid) {
		imports.add(DATA_GRID_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("SkyveDataGrid(rows: _bean['").append(BindUtil.sanitiseBinding(grid.getBinding()));
		output.append("'], children: [");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent dataRepeater(@Nullable RenderedComponent component,
													@Nullable String dataWidgetVar,
													@Nullable String title,
													@Nonnull DataRepeater repeater) {
		imports.add(DATA_GRID_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("SkyveDataGrid(rows: _bean['").append(BindUtil.sanitiseBinding(repeater.getBinding()));
		output.append("'], children: [");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent addDataGridBoundColumn(@Nullable RenderedComponent component,
																@Nonnull RenderedComponent current,
																@Nonnull AbstractDataWidget widget,
																@Nonnull DataGridBoundColumn column,
																@Nullable String dataWidgetVar,
																@Nullable String columnTitle,
																@Nonnull String columnBinding,
																@Nonnull StringBuilder gridColumnExpression) {
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
	public @Nonnull RenderedComponent addedDataGridBoundColumn(@Nullable RenderedComponent component, @Nonnull RenderedComponent current) {
//		return current.getParent();
		return current;
	}

	@Override
	public @Nonnull RenderedComponent addDataGridContainerColumn(@Nullable RenderedComponent component,
																	@Nonnull RenderedComponent current,
																	@Nonnull AbstractDataWidget widget,
																	@Nullable String columnTitle,
																	@Nonnull DataGridContainerColumn column) {
		return current;
	}

	@Override
	public @Nonnull RenderedComponent addedDataGridContainerColumn(@Nullable RenderedComponent component, @Nonnull RenderedComponent current) {
		return current;
	}

	@Override
	public @Nonnull RenderedComponent addDataGridActionColumn(@Nullable RenderedComponent component,
																@Nonnull RenderedComponent current,
																@Nonnull DataGrid grid,
																@Nullable String dataWidgetVar,
																@Nonnull String gridColumnExpression,
																@Nullable String singluarDocumentAlias,
																boolean inline) {
		return current;
	}

	@Override
	public @Nonnull RenderedComponent listGrid(@Nullable RenderedComponent component,
												@Nonnull String modelDocumentName,
												@Nonnull String modelName,
												@Nonnull ListModel<Bean> model,
												@Nullable String title,
												@Nonnull ListGrid listGrid,
												boolean aggregateQuery) {
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("const Text('ListGrid'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent listRepeater(@Nullable RenderedComponent component,
													@Nonnull String modelDocumentName,
													@Nonnull String modelName,
													@Nonnull ListModel<Bean> model,
													@Nonnull List<FilterParameter> filterParameters,
													@Nonnull List<Parameter> parameters,
													@Nullable String title,
													boolean showColumnHeaders,
													boolean showGrid) {
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("const Text('ListRepeater'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent listMembership(@Nullable RenderedComponent component, @Nonnull ListMembership membership) {
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("const Text('ListMembership'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent checkBox(@Nullable RenderedComponent component,
												@Nullable String dataWidgetVar,
												@Nonnull CheckBox checkBox,
												@Nullable String title,
												@Nullable String requiredMessage) {
		imports.add(CHECKBOX_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();

		// output.append("SkyveCheckBox(label: '").append(title).append("', isChecked: _bean[\"")
		// .append(BindUtil.sanitiseBinding(checkBox.getBinding())).append("\"], tristate: ")
		// .append(required + "),");
		// TODO: handle proper input
		output.append("const SkyveCheckBox(label: '").append(title).append("', isChecked: false, tristate: ").append((requiredMessage == null) + "),");

		return result;
	}

	@Override
	public @Nonnull RenderedComponent colourPicker(@Nullable RenderedComponent component,
													@Nullable String dataWidgetVar,
													@Nonnull ColourPicker colour,
													@Nullable String title,
													@Nullable String requiredMessage) {
		imports.add(COLOURPICKER_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('ColourPicker ${_bean[\"").append(BindUtil.sanitiseBinding(colour.getBinding())).append("\"]}'),");
		output.append("SkyveColourPicker(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent combo(@Nullable RenderedComponent component,
											@Nullable String dataWidgetVar,
											@Nonnull Combo combo,
											@Nullable String title,
											@Nullable String requiredMessage) {
		imports.add(COMBO_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('Combo ${_bean[\"").append(BindUtil.sanitiseBinding(combo.getBinding())).append("\"]}'),");
		output.append("SkyveCombo(label: '").append(title).append("'),");
		return result;
	}

	/**
	 * Creates the temporary Flutter placeholder for content metadata.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param content source managed-content upload metadata
	 * @param title resolved input title
	 * @param requiredMessage optional required-message expression
	 * @return the placeholder rendered component
	 */
	@Override
	public @Nonnull RenderedComponent content(@Nullable RenderedComponent component,
												@Nullable String dataWidgetVar,
												@Nonnull ContentUpload content,
												@Nullable String title,
												@Nullable String requiredMessage) {
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("Text('Content'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent contentSignature(@Nullable RenderedComponent component,
														@Nullable String dataWidgetVar,
														@Nonnull ContentSignature signature,
														@Nullable String title,
														@Nullable String requiredMessage) {
		imports.add(CONTENTSIGNATURE_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('ContentSignature
		// ${_bean[\"").append(BindUtil.sanitiseBinding(signature.getBinding())).append("\"]}'),");
		output.append("SkyveContentSignature(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent html(@Nullable RenderedComponent component,
											@Nullable String dataWidgetVar,
											@Nonnull HTML html,
											@Nullable String title,
											@Nullable String requiredMessage) {
		imports.add(HTML_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('HTML ${_bean[\"").append(BindUtil.sanitiseBinding(html.getBinding())).append("\"]}'),");
		output.append("SkyveHTML(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent lookupDescription(@Nullable RenderedComponent component,
														@Nullable String dataWidgetVar,
														@Nonnull LookupDescription lookup,
														@Nullable String title,
														@Nullable String requiredMessage,
														@Nullable String displayBinding,
														@Nullable QueryDefinition query) {
		imports.add(LOOKUP_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('LookupDescription
		// ${_bean[\"").append(BindUtil.sanitiseBinding(lookup.getBinding())).append("\"]}'),");
		output.append("SkyveLookupDescription(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent password(@Nullable RenderedComponent component,
												@Nullable String dataWidgetVar,
												@Nonnull Password password,
												@Nullable String title,
												@Nullable String requiredMessage) {
		imports.add(PASSWORD_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("const Text('Password'),");
		output.append("SkyvePassword(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent radio(@Nullable RenderedComponent component,
											@Nullable String dataWidgetVar,
											@Nonnull Radio radio,
											@Nullable String title,
											@Nullable String requiredMessage) {
		imports.add(RADIO_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('Radio ${_bean[\"").append(BindUtil.sanitiseBinding(radio.getBinding())).append("\"]}'),");
		output.append("SkyveRadio(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent richText(@Nullable RenderedComponent component,
												@Nullable String dataWidgetVar,
												@Nonnull RichText text,
												@Nullable String title,
												@Nullable String requiredMessage) {
		imports.add(RICHTEXT_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('RichText ${_bean[\"").append(BindUtil.sanitiseBinding(text.getBinding())).append("\"]}'),");
		output.append("SkyveRichText(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent spinner(@Nullable RenderedComponent component,
												@Nullable String dataWidgetVar,
												@Nonnull Spinner spinner,
												@Nullable String title,
												@Nullable String requiredMessage) {
		imports.add(SPINNER_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		// output.append("Text('Spinner ${_bean[\"").append(BindUtil.sanitiseBinding(spinner.getBinding())).append("\"]}'),");
		output.append("SkyveSpinner(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent text(@Nullable RenderedComponent component,
											@Nullable String dataWidgetVar,
											@Nonnull TextField text,
											@Nullable String title,
											@Nullable String requiredMessage,
											@Nullable Integer length,
											@Nullable Converter<?> converter,
											@Nullable Format<?> format) {
        imports.add(TEXTFIELD_IMPORT);
        RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);

        final String stfTemplate = "SkyveTextField(label: '%s', propertyKey: '%s'),";
        String stfCode = String.format(stfTemplate, title, BindUtil.sanitiseBinding(text.getBinding()));

        result.getOutput()
              .append(stfCode);

        return result;
    }

	@Override
	public @Nonnull RenderedComponent textArea(@Nullable RenderedComponent component,
												@Nullable String dataWidgetVar,
												@Nonnull TextArea text,
												@Nullable String title,
												@Nullable String requiredMessage,
												@Nullable Integer length) {
		imports.add(TEXTAREA_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveTextArea(label: '").append(title).append("'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent actionLink(@Nullable RenderedComponent component,
													@Nullable String dataWidgetBinding,
													@Nullable String dataWidgetVar,
													@Nonnull Link link,
													@Nonnull Action action) {
		imports.add(ACTIONLINK_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveActionLink(label: 'ActionLink'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent report(@Nullable RenderedComponent component, @Nonnull Action action) {
		imports.add(REPORT_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveReport(label: 'Report'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent download(@Nullable RenderedComponent component,
												@Nonnull Action action,
												@Nonnull String moduleName,
												@Nonnull String documentName) {
		imports.add(DOWNLOAD_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveDownload(label: 'Download'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent upload(@Nullable RenderedComponent component, @Nonnull Action action) {
		imports.add(UPLOAD_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveUpload(label: 'Upload'),");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent action(@Nullable RenderedComponent component,
												@Nullable String dataWidgetBinding,
												@Nullable String dataWidgetVar,
												@Nonnull Action action,
												@Nullable ImplicitActionName name,
												@Nullable String title) {
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

    private @Nonnull String createButton(@Nonnull String actionType, @Nullable String actionName, @Nullable String label) {
        return String.format(btnTemplate, actionType, actionName, label);
    }
}
