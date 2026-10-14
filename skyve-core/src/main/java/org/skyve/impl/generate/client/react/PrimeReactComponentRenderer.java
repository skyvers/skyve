package org.skyve.impl.generate.client.react;

import java.util.Collections;
import java.util.List;
import java.util.Map;

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
 * PrimeReact-specific widget/component renderer.
 */
@SuppressWarnings("java:S1192") // Repeated literals are deliberate fragments of generated PrimeReact component output.
public class PrimeReactComponentRenderer extends ComponentRenderer {
	private Map<String, String> imports;
	private String startingIndent;

	public PrimeReactComponentRenderer(@Nonnull Map<String, String> imports, @Nonnull String startingIndent) {
		this.imports = imports;
		this.startingIndent = startingIndent;
	}
	
	@Override
	public @Nonnull RenderedComponent view(@Nullable RenderedComponent component, @Nonnull String invisibleCondition) {
		imports.put("{VBox}", PrimeReactViewRenderer.PRIME_REACT_VIEW_FILE);
		RenderedComponent result = new RenderedComponent().setAfter("</VBox>").setIndent(startingIndent);
		StringBuilder output = result.getOutput();
		output.append("<VBox>");
		return result;
	}

	@Override
	public @Nonnull List<RenderedComponent> toolbars(@Nullable List<RenderedComponent> components, @Nullable String widgetId) {
		imports.put("{Toolbar}", "primereact/toolbar");
		RenderedComponent result = new RenderedComponent().setAfter("</Toolbar>");
		StringBuilder output = result.getOutput();
		output.append("<Toolbar>");
		return Collections.singletonList(result);
	}

	@Override
	public @Nonnull RenderedComponent tabPane(@Nullable RenderedComponent component, @Nonnull TabPane tabPane) {
		imports.put("{TabView, TabPanel}", "primereact/tabview");
		RenderedComponent result = new RenderedComponent().setAfter("</TabView>");
		StringBuilder output = result.getOutput();
		output.append("<TabView>");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent tab(@Nullable RenderedComponent component, @Nullable String title, @Nonnull Tab tab) {
		RenderedComponent result = new RenderedComponent().setAfter("</TabPanel>");
		StringBuilder output = result.getOutput();
		output.append("<TabPanel header=\"").append(title).append("\">");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent border(@Nullable RenderedComponent component,
												@Nullable String title,
												@Nullable String invisibleCondition,
												@Nullable Integer pixelWidth) {
		imports.put("{Card}", "primereact/card");
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<Card");
		if (title != null) {
			output.append(" title=\"").append(title).append('"');
		}
		output.append('>');
		result.setAfter("</Card>");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent label(@Nullable RenderedComponent component, @Nonnull String value) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("label");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent spacer(@Nullable RenderedComponent component, @Nonnull Spacer spacer) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span />");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent actionButton(@Nullable RenderedComponent component,
													@Nullable String dataWidgetBinding,
													@Nullable String dataWidgetVar,
													@Nonnull Button button,
													@Nonnull Action action) {
		imports.put("{Button}", "primereact/button");
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<Button label=\"").append(action.getLocalisedDisplayName()).append("\" />");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent reportButton(@Nullable RenderedComponent component, @Nonnull Button button, @Nonnull Action action) {
		imports.put("{Button}", "primereact/button");
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<Button label=\"").append(action.getLocalisedDisplayName()).append("\" />");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent downloadButton(@Nullable RenderedComponent component,
														@Nonnull Button button,
														@Nonnull Action action,
														@Nonnull String moduleName,
														@Nonnull String documentName) {
		imports.put("{Button}", "primereact/button");
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<Button label=\"").append(action.getLocalisedDisplayName()).append("\" />");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent staticImage(@Nullable RenderedComponent component, @Nonnull String fileUrl, @Nonnull StaticImage image) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>StaticImage</span>");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent dynamicImage(@Nullable RenderedComponent component,
													@Nonnull DynamicImage image,
													@Nonnull String moduleName,
													@Nonnull String documentName) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>DynamicImage</span>");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent blurb(@Nullable RenderedComponent component,
											@Nullable String dataWidgetVar,
											@Nullable String value,
											@Nullable String binding,
											@Nonnull Blurb blurb) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>Blurb</span>");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent label(@Nullable RenderedComponent component,
											@Nullable String dataWidgetVar,
											@Nullable String value,
											@Nullable String binding,
											@Nonnull Label label) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>Label</span>");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent dataGrid(@Nullable RenderedComponent component,
												@Nullable String dataWidgetVar,
												boolean ordered,
												@Nullable String title,
												@Nonnull DataGrid grid) {
		imports.put("{DataTable}", "primereact/datatable");
		RenderedComponent result = new RenderedComponent().setAfter("</DataTable>").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("<DataTable value={this.state.").append(BindUtil.sanitiseBinding(grid.getBinding()));
		output.append("} selectionMode=\"single\" onSelectionChange={e => alert(e.data.bizModule + '.' + e.data.bizDocument + '.' + e.data.bizId)}>");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent dataRepeater(@Nullable RenderedComponent component,
													@Nullable String dataWidgetVar,
													@Nullable String title,
													@Nonnull DataRepeater repeater) {
		RenderedComponent result = new RenderedComponent().setAfter("</div>").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("<div>dataRepeater");
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
		imports.put("{Column}", "primereact/column");
		RenderedComponent result = new RenderedComponent().setIndent("");
		StringBuilder output = result.getOutput();
		output.append("<Column field=\"").append(BindUtil.sanitiseBinding(columnBinding));
		output.append("\" header=\"").append(columnTitle).append("\" />");
		current.addChild(result);
		return result;
	}

	@Override
	public @Nonnull RenderedComponent addedDataGridBoundColumn(@Nullable RenderedComponent component, @Nonnull RenderedComponent current) {
		return current.getParent();
	}

	@Override
	public @Nonnull RenderedComponent addDataGridContainerColumn(@Nullable RenderedComponent component,
																	@Nonnull RenderedComponent current,
																	@Nonnull AbstractDataWidget widget,
																	@Nullable String columnTitle,
																	@Nonnull DataGridContainerColumn column) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>addDataGridContainerColumn</span>");
		current.addChild(result);
		return result;
	}

	@Override
	public @Nonnull RenderedComponent addedDataGridContainerColumn(@Nullable RenderedComponent component, @Nonnull RenderedComponent current) {
		return current.getParent();
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
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>ListGrid</span>");
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
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>listRepeater</span>");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent listMembership(@Nullable RenderedComponent component, @Nonnull ListMembership membership) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("listMembership");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent checkBox(@Nullable RenderedComponent component,
												@Nullable String dataWidgetVar,
												@Nonnull CheckBox checkBox,
												@Nullable String title,
												@Nullable String requiredMessage) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("checkBox");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent colourPicker(@Nullable RenderedComponent component,
													@Nullable String dataWidgetVar,
													@Nonnull ColourPicker colour,
													@Nullable String title,
													@Nullable String requiredMessage) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("colourPicker");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent combo(@Nullable RenderedComponent component,
											@Nullable String dataWidgetVar,
											@Nonnull Combo combo,
											@Nullable String title,
											@Nullable String requiredMessage) {
		imports.put("{Dropdown}", "primereact/dropdown");
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<Dropdown />");
		return result;
	}

	/**
	 * Creates the temporary PrimeReact placeholder for content metadata.
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
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>Content</span>");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent contentSignature(@Nullable RenderedComponent component,
														@Nullable String dataWidgetVar,
														@Nonnull ContentSignature signature,
														@Nullable String title,
														@Nullable String requiredMessage) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("contentSignature");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent html(@Nullable RenderedComponent component,
											@Nullable String dataWidgetVar,
											@Nonnull HTML html,
											@Nullable String title,
											@Nullable String requiredMessage) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("html");
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
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("lookupDescription");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent password(@Nullable RenderedComponent component,
												@Nullable String dataWidgetVar,
												@Nonnull Password password,
												@Nullable String title,
												@Nullable String requiredMessage) {
		imports.put("{Password}", "primereact/password");
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<Password />");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent radio(@Nullable RenderedComponent component,
											@Nullable String dataWidgetVar,
											@Nonnull Radio radio,
											@Nullable String title,
											@Nullable String requiredMessage) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("radio");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent richText(@Nullable RenderedComponent component,
												@Nullable String dataWidgetVar,
												@Nonnull RichText text,
												@Nullable String title,
												@Nullable String requiredMessage) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("richText");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent spinner(@Nullable RenderedComponent component,
												@Nullable String dataWidgetVar,
												@Nonnull Spinner spinner,
												@Nullable String title,
												@Nullable String requiredMessage) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("spinner");
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
		imports.put("{InputText}", "primereact/inputtext");
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		String sanitisedBinding = BindUtil.sanitiseBinding(text.getBinding());
		output.append("<InputText value={this.state.").append(sanitisedBinding).append("} onChange={(e) => this.change('");
		output.append(sanitisedBinding).append("', e)} />");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent textArea(@Nullable RenderedComponent component,
												@Nullable String dataWidgetVar,
												@Nonnull TextArea text,
												@Nullable String title,
												@Nullable String requiredMessage,
												@Nullable Integer length) {
		imports.put("{InputTextarea}", "primereact/inputtextarea");
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		String sanitisedBinding = BindUtil.sanitiseBinding(text.getBinding());
		output.append("<InputTextarea value={this.state.").append(sanitisedBinding).append("} onChange={(e) => this.change('");
		output.append(sanitisedBinding).append("', e)} />");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent actionLink(@Nullable RenderedComponent component,
													@Nullable String dataWidgetBinding,
													@Nullable String dataWidgetVar,
													@Nonnull Link link,
													@Nonnull Action action) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>ActionLink</span>");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent report(@Nullable RenderedComponent component, @Nonnull Action action) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("report");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent download(@Nullable RenderedComponent component,
												@Nonnull Action action,
												@Nonnull String moduleName,
												@Nonnull String documentName) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("download");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent upload(@Nullable RenderedComponent component, @Nonnull Action action) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("upload");
		return result;
	}
	
	@Override
	public @Nonnull RenderedComponent action(@Nullable RenderedComponent component,
												@Nullable String dataWidgetBinding,
												@Nullable String dataWidgetVar,
												@Nonnull Action action,
												@Nullable ImplicitActionName name,
												@Nullable String title) {
		imports.put("{Button}", "primereact/button");
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		if (ImplicitActionName.Cancel.equals(name)) {
			output.append("<Button label=\"").append(title).append("\" onClick={(e) => this.props.history.goBack()} />");
		}
		else {
			output.append("action " + name);
		}
		return result;
	}
}
