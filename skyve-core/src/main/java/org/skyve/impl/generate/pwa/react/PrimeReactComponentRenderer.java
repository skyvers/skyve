package org.skyve.impl.generate.pwa.react;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.generate.pwa.ComponentRenderer;
import org.skyve.impl.generate.pwa.RenderedComponent;
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
import org.skyve.metadata.view.widget.bound.FilterParameter;

public class PrimeReactComponentRenderer extends ComponentRenderer {
	private Map<String, String> imports;
	private String startingIndent;

	public PrimeReactComponentRenderer(Map<String, String> imports, String startingIndent) {
		this.imports = imports;
		this.startingIndent = startingIndent;
	}
	
	@Override
	public RenderedComponent view(RenderedComponent component, String invisibleConditionName) {
		imports.put("{VBox}", PrimeReactViewRenderer.PRIME_REACT_VIEW_FILE);
		RenderedComponent result = new RenderedComponent().setAfter("</VBox>").setIndent(startingIndent);
		StringBuilder output = result.getOutput();
		output.append("<VBox>");
		return result;
	}

	@Override
	public List<RenderedComponent> toolbars(List<RenderedComponent> components, String widgetId) {
		imports.put("{Toolbar}", "primereact/toolbar");
		RenderedComponent result = new RenderedComponent().setAfter("</Toolbar>");
		StringBuilder output = result.getOutput();
		output.append("<Toolbar>");
		return Collections.singletonList(result);
	}

	@Override
	public RenderedComponent tabPane(RenderedComponent component, TabPane tabPane) {
		imports.put("{TabView, TabPanel}", "primereact/tabview");
		RenderedComponent result = new RenderedComponent().setAfter("</TabView>");
		StringBuilder output = result.getOutput();
		output.append("<TabView>");
		return result;
	}

	@Override
	public RenderedComponent tab(RenderedComponent component, String title, Tab tab) {
		RenderedComponent result = new RenderedComponent().setAfter("</TabPanel>");
		StringBuilder output = result.getOutput();
		output.append("<TabPanel header=\"").append(title).append("\">");
		return result;
	}

	@Override
	public RenderedComponent border(RenderedComponent component,
										String title,
										String invisibileConditionName,
										Integer pixelWidth) {
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
	public RenderedComponent label(RenderedComponent component, String value) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("label");
		return result;
	}

	@Override
	public RenderedComponent spacer(RenderedComponent component, Spacer spacer) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span />");
		return result;
	}

	@Override
	public RenderedComponent actionButton(RenderedComponent component,
											String dataWidgetBinding,
											String dataWidgetVar,
											Button button,
											Action action) {
		imports.put("{Button}", "primereact/button");
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<Button label=\"").append(action.getDisplayName()).append("\" />");
		return result;
	}

	@Override
	public RenderedComponent reportButton(RenderedComponent component, Button button, Action action) {
		imports.put("{Button}", "primereact/button");
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<Button label=\"").append(action.getDisplayName()).append("\" />");
		return result;
	}

	@Override
	public RenderedComponent downloadButton(RenderedComponent component,
												Button button,
												Action action,
												String moduleName,
												String documentName) {
		imports.put("{Button}", "primereact/button");
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<Button label=\"").append(action.getDisplayName()).append("\" />");
		return result;
	}

	@Override
	public RenderedComponent staticImage(RenderedComponent component, String fileUrl, StaticImage image) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>StaticImage</span>");
		return result;
	}

	@Override
	public RenderedComponent dynamicImage(RenderedComponent component,
											DynamicImage image,
											String moduleName,
											String documentName) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>DynamicImage</span>");
		return result;
	}

	@Override
	public RenderedComponent blurb(RenderedComponent component,
									String dataWidgetVar,
									String value,
									String binding,
									Blurb blurb) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>Blurb</span>");
		return result;
	}

	@Override
	public RenderedComponent label(RenderedComponent component,
									String dataWidgetVar,
									String value,
									String binding,
									Label label) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>Label</span>");
		return result;
	}

	@Override
	public RenderedComponent dataGrid(RenderedComponent component,
										String dataWidgetVar,
										boolean ordered,
										String title,
										DataGrid grid) {
		imports.put("{DataTable}", "primereact/datatable");
		RenderedComponent result = new RenderedComponent().setAfter("</DataTable>").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("<DataTable value={this.state.").append(BindUtil.sanitiseBinding(grid.getBinding()));
		output.append("} selectionMode=\"single\" onSelectionChange={e => alert(e.data.bizModule + '.' + e.data.bizDocument + '.' + e.data.bizId)}>");
		return result;
	}

	@Override
	public RenderedComponent dataRepeater(RenderedComponent component,
											String dataWidgetVar,
											String title,
											DataRepeater repeater) {
		RenderedComponent result = new RenderedComponent().setAfter("</div>").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("<div>dataRepeater");
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
		imports.put("{Column}", "primereact/column");
		RenderedComponent result = new RenderedComponent().setIndent("");
		StringBuilder output = result.getOutput();
		output.append("<Column field=\"").append(BindUtil.sanitiseBinding(columnBinding));
		output.append("\" header=\"").append(columnTitle).append("\" />");
		current.addChild(result);
		return result;
	}

	@Override
	public RenderedComponent addedDataGridBoundColumn(RenderedComponent component, RenderedComponent current) {
		return current.getParent();
	}

	@Override
	public RenderedComponent addDataGridContainerColumn(RenderedComponent component,
															RenderedComponent current,
															AbstractDataWidget widget,
															String columnTitle,
															DataGridContainerColumn column) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>addDataGridContainerColumn</span>");
		current.addChild(result);
		return result;
	}

	@Override
	public RenderedComponent addedDataGridContainerColumn(RenderedComponent component, RenderedComponent current) {
		return current.getParent();
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
										ListModel<? extends Bean> model,
										String title,
										ListGrid listGrid,
										boolean canCreateDocument,
										boolean aggregateQuery) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>ListGrid</span>");
		return result;
	}

	@Override
	public RenderedComponent listRepeater(RenderedComponent component,
											String modelDocumentName,
											String modelName,
											ListModel<? extends Bean> model,
											List<FilterParameter> filterParameters,
											String title,
											boolean showColumnHeaders,
											boolean showGrid) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>listRepeater</span>");
		return result;
	}

	@Override
	public RenderedComponent listMembership(RenderedComponent component, ListMembership membership) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("listMembership");
		return result;
	}

	@Override
	public RenderedComponent checkBox(RenderedComponent component,
										String dataWidgetVar,
										CheckBox checkBox,
										String title,
										boolean required) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("checkBox");
		return result;
	}

	@Override
	public RenderedComponent colourPicker(RenderedComponent component,
											String dataWidgetVar,
											ColourPicker colour,
											String title,
											boolean required) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("colourPicker");
		return result;
	}

	@Override
	public RenderedComponent combo(RenderedComponent component,
									String dataWidgetVar,
									Combo combo,
									String title,
									boolean required) {
		imports.put("{Dropdown}", "primereact/dropdown");
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<Dropdown />");
		return result;
	}

	@Override
	public RenderedComponent contentImage(RenderedComponent component,
											String dataWidgetVar,
											ContentImage image,
											String title,
											boolean required) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("contentImage");
		return result;
	}

	@Override
	public RenderedComponent contentLink(RenderedComponent component,
											String dataWidgetVar,
											ContentLink link,
											String title,
											boolean required) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>ContentLink</span>");
		return result;
	}

	@Override
	public RenderedComponent html(RenderedComponent component,
									String dataWidgetVar,
									HTML html,
									String title,
									boolean required) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("html");
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
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("lookupDescription");
		return result;
	}

	@Override
	public RenderedComponent password(RenderedComponent component,
										String dataWidgetVar,
										Password password,
										String title,
										boolean required) {
		imports.put("{Password}", "primereact/password");
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<Password />");
		return result;
	}

	@Override
	public RenderedComponent radio(RenderedComponent component,
									String dataWidgetVar,
									Radio radio,
									String title,
									boolean required) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("radio");
		return result;
	}

	@Override
	public RenderedComponent richText(RenderedComponent component,
										String dataWidgetVar,
										RichText text,
										String title,
										boolean required) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("richText");
		return result;
	}

	@Override
	public RenderedComponent spinner(RenderedComponent component,
										String dataWidgetVar,
										Spinner spinner,
										String title,
										boolean required) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("spinner");
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
		imports.put("{InputText}", "primereact/inputtext");
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		String sanitisedBinding = BindUtil.sanitiseBinding(text.getBinding());
		output.append("<InputText value={this.state.").append(sanitisedBinding).append("} onChange={(e) => this.change('");
		output.append(sanitisedBinding).append("', e)} />");
		return result;
	}

	@Override
	public RenderedComponent textArea(RenderedComponent component,
										String dataWidgetVar,
										TextArea text,
										String title,
										boolean required,
										Integer length) {
		imports.put("{InputTextarea}", "primereact/inputtextarea");
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		String sanitisedBinding = BindUtil.sanitiseBinding(text.getBinding());
		output.append("<InputTextarea value={this.state.").append(sanitisedBinding).append("} onChange={(e) => this.change('");
		output.append(sanitisedBinding).append("', e)} />");
		return result;
	}

	@Override
	public RenderedComponent actionLink(RenderedComponent component,
											String dataWidgetBinding,
											String dataWidgetVar,
											Link link,
											Action action) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<span>ActionLink</span>");
		return result;
	}

	@Override
	public RenderedComponent report(RenderedComponent component, Action action) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("report");
		return result;
	}

	@Override
	public RenderedComponent download(RenderedComponent component,
										Action action,
										String moduleName,
										String documentName) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("download");
		return result;
	}

	@Override
	public RenderedComponent upload(RenderedComponent component, Action action) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("upload");
		return result;
	}
	
	@Override
	public RenderedComponent action(RenderedComponent component,
										String dataWidgetBinding,
										String dataWidgetVar,
										Action action,
										ImplicitActionName name,
										String title) {
		imports.put("{Button}", "primereact/button");
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		if (ImplicitActionName.Cancel.equals(name)) {
			output.append("<Button label=\"Cancel\" onClick={(e) => this.props.history.goBack()} />");
		}
		else {
			output.append("action " + name);
		}
		return result;
	}
}
