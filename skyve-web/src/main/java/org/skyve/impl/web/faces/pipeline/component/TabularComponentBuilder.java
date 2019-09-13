package org.skyve.impl.web.faces.pipeline.component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import javax.el.MethodExpression;
import javax.el.ValueExpression;
import javax.faces.component.UICommand;
import javax.faces.component.UIComponent;
import javax.faces.component.UIComponentBase;
import javax.faces.component.UIInput;
import javax.faces.component.UIOutput;
import javax.faces.component.UIPanel;
import javax.faces.component.UISelectItems;
import javax.faces.component.html.HtmlInputHidden;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.component.html.HtmlPanelGrid;
import javax.faces.component.html.HtmlPanelGroup;
import javax.faces.component.html.HtmlSelectOneMenu;
import javax.faces.convert.Converter;

import org.primefaces.behavior.ajax.AjaxBehavior;
import org.primefaces.behavior.ajax.AjaxBehaviorListenerImpl;
import org.primefaces.behavior.confirm.ConfirmBehavior;
import org.primefaces.component.accordionpanel.AccordionPanel;
import org.primefaces.component.autocomplete.AutoComplete;
import org.primefaces.component.barchart.BarChart;
import org.primefaces.component.button.Button;
import org.primefaces.component.calendar.Calendar;
import org.primefaces.component.colorpicker.ColorPicker;
import org.primefaces.component.column.Column;
import org.primefaces.component.commandbutton.CommandButton;
import org.primefaces.component.commandlink.CommandLink;
import org.primefaces.component.datalist.DataList;
import org.primefaces.component.datatable.DataTable;
import org.primefaces.component.dialog.Dialog;
import org.primefaces.component.donutchart.DonutChart;
import org.primefaces.component.editor.Editor;
import org.primefaces.component.graphicimage.GraphicImage;
import org.primefaces.component.inputmask.InputMask;
import org.primefaces.component.inputtext.InputText;
import org.primefaces.component.inputtextarea.InputTextarea;
import org.primefaces.component.linechart.LineChart;
import org.primefaces.component.message.Message;
import org.primefaces.component.outputlabel.OutputLabel;
import org.primefaces.component.overlaypanel.OverlayPanel;
import org.primefaces.component.panel.Panel;
import org.primefaces.component.password.Password;
import org.primefaces.component.picklist.PickList;
import org.primefaces.component.piechart.PieChart;
import org.primefaces.component.polarareachart.PolarAreaChart;
import org.primefaces.component.radarchart.RadarChart;
import org.primefaces.component.remotecommand.RemoteCommand;
import org.primefaces.component.selectbooleancheckbox.SelectBooleanCheckbox;
import org.primefaces.component.selectonemenu.SelectOneMenu;
import org.primefaces.component.selectoneradio.SelectOneRadio;
import org.primefaces.component.spacer.Spacer;
import org.primefaces.component.spinner.Spinner;
import org.primefaces.component.tabview.Tab;
import org.primefaces.component.tabview.TabView;
import org.primefaces.component.toolbar.Toolbar;
import org.primefaces.component.tristatecheckbox.TriStateCheckbox;
import org.primefaces.model.DualListModel;
import org.primefaces.model.charts.ChartModel;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Format.TextCase;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.generate.SmartClientGenerateUtils;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.LoadingType;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryInputType;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.faces.converters.select.AssociationAutoCompleteConverter;
import org.skyve.impl.web.faces.converters.select.AssociationPickListConverter;
import org.skyve.impl.web.faces.converters.select.SelectItemsBeanConverter;
import org.skyve.impl.web.faces.converters.select.TriStateCheckboxBooleanConverter;
import org.skyve.impl.web.faces.models.BeanMapAdapter;
import org.skyve.impl.web.faces.models.SkyveLazyDataModel;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.report.ReportFormat;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.WebAction;

public class TabularComponentBuilder extends ComponentBuilder {

	public static final String EMPTY_DATA_TABLE_CAN_ADD_MESSAGE = "No Items to show. Click <span class=\"fa fa-plus-circle skyveEmptyListAddIcon\"></span> to add a new Item.";
	public static final String EMPTY_DATA_TABLE_MESSAGE = "No Items to show.";
	public static final String SINGLE_ACTION_COLUMN_WIDTH = "60";
	public static final Integer SINGLE_ACTION_COLUMN_WIDTH_INTEGER = Integer.valueOf(60);
	public static final String DOUBLE_ACTION_COLUMN_WIDTH = "95";
	
	@Override
	public UIComponent view(UIComponent component, String invisibleConditionName) {
		if (component != null) {
			return component;
		}
		
		return panelGroup(true, false, false, invisibleConditionName, null);
	}

	@Override
	public List<UIComponent> toolbars(List<UIComponent> components, String widgetId) {
		if (components != null) {
			return components;
		}

		Toolbar toolbar = (Toolbar) a.createComponent(Toolbar.COMPONENT_TYPE);
		setId(toolbar, widgetId);
		toolbar.setStyle("width:100%");
		
		List<UIComponent> result = new ArrayList<>(1);
		result.add(toolbar);
		return result;
	}

	@Override
	public UIComponent tabPane(UIComponent component,
								TabPane tabPane,
								String moduleName,
								String documentName,
								StringBuilder stickyTabScript) {
		if (component != null) {
			return component;
		}

		TabView result = (TabView) a.createComponent(TabView.COMPONENT_TYPE);
		setInvisible(result, tabPane.getInvisibleConditionName(), null);
		setId(result, tabPane.getWidgetId());
		String id = result.getId();
		String selectedTabIndexBinding = tabPane.getSelectedTabIndexBinding();
		if (selectedTabIndexBinding != null) {
			result.setValueExpression("activeIndex", createValueExpressionFromFragment(selectedTabIndexBinding, true, null, Number.class));
		}
		else {
			result.setWidgetVar(id);
			result.setOnTabChange(String.format("sessionStorage.tab_%s_%s_%s=index", moduleName, documentName, id));			

			stickyTabScript.append(String.format("var t=PF('%s');if(t){t.select(sessionStorage.tab_%s_%s_%s?sessionStorage.tab_%s_%s_%s:0);}else{$(document).ready(function(){PF('%s').select(sessionStorage.tab_%s_%s_%s?sessionStorage.tab_%s_%s_%s:0);});}",
													id,
													moduleName, documentName, id,
													moduleName, documentName, id,
													id,
													moduleName, documentName, id,
													moduleName, documentName, id));
		}
		return result;
	}
	
	@Override
	public UIComponent tab(UIComponent component, String title, org.skyve.impl.metadata.view.container.Tab tab) {
		if (component != null) {
			return component;
		}

		Tab result = (Tab) a.createComponent(Tab.COMPONENT_TYPE);
		setValueOrValueExpression(title, result::setTitle, "title", result);
		setDisabled(result, tab.getDisabledConditionName(), null);
		setInvisible(result, tab.getInvisibleConditionName(), null);
		setId(result, null);
		return result;
	}

	@Override
	public UIComponent border(UIComponent component, String borderTitle, String invisibleConditionName, Integer pixelWidth) {
		if (component != null) {
			return component;
		}

		return panel(borderTitle, invisibleConditionName, pixelWidth, null);
	}
	
	@Override
	public UIComponent label(UIComponent component, String value) {
		if (component != null) {
			return component;
		}

		OutputLabel result = (OutputLabel) a.createComponent(OutputLabel.COMPONENT_TYPE);
		setId(result, null);
		result.setValue(value);
		return result;
	}
	
	@Override
	public UIComponent actionButton(UIComponent component, 
										String dataWidgetBinding, 
										String dataWidgetVar,
										String label,
										String iconStyleClass,
										String toolTip,
										String confirmationText, 
										org.skyve.impl.metadata.view.widget.Button button, 
										String formDisabledConditionName,
										Action action) {
		if (component != null) {
			return component;
		}

		Map<String, String> properties = button.getProperties();
		return actionButton(label,
								iconStyleClass,
				                toolTip,
				                action.getImplicitName(),
				                action.getName(),
				                false,
				                dataWidgetBinding,
				                dataWidgetVar,
				                button.getPixelWidth(),
				                button.getPixelHeight(),
				                action.getClientValidation(),
				                confirmationText,
				                action.getDisabledConditionName(),
				                formDisabledConditionName,
				                action.getInvisibleConditionName(),
				                properties.get(PROCESS_KEY),
				                properties.get(UPDATE_KEY));
	}
	
	@Override
	public UIComponent reportButton(UIComponent component, 
										String label,
										String iconStyleClass,
										String toolTip,
										String confirmationText, 
										org.skyve.impl.metadata.view.widget.Button button, 
										String formDisabledConditionName,
										Action action) {
		if (component != null) {
			return component;
		}

		return reportButton(label, 
								iconStyleClass,
								toolTip, 
								action.getParameters(), 
								button.getPixelWidth(),
								button.getPixelHeight(),
								action.getClientValidation(),
								confirmationText,
								action.getDisabledConditionName(), 
								formDisabledConditionName,
								action.getInvisibleConditionName());
	}

	@Override
	public UIComponent downloadButton(UIComponent component, 
										String dataWidgetBinding,
										String dataWidgetVar,
										String label,
										String iconStyleClass,
										String toolTip,
										String confirmationText, 
										org.skyve.impl.metadata.view.widget.Button button, 
										String formDisabledConditionName,
										Action action) {
		if (component != null) {
			return component;
		}

		Map<String, String> properties = action.getProperties();
		
		return downloadButton(label,
								iconStyleClass,
								toolTip,
								dataWidgetBinding,
								dataWidgetVar,
								action.getName(),
								button.getPixelWidth(),
								button.getPixelHeight(),
								confirmationText,
								action.getDisabledConditionName(), 
								formDisabledConditionName,
								action.getInvisibleConditionName(),
								properties.get(PROCESS_KEY),
								properties.get(UPDATE_KEY));
	}
	
	@Override
	public UIComponent uploadButton(UIComponent component, 
										String label,
										String iconStyleClass,
										String toolTip,
										String confirmationText, 
										org.skyve.impl.metadata.view.widget.Button button, 
										String formDisabledConditionName,
										Action action) {
		if (component != null) {
			return component;
		}

		return uploadButton(label, 
								iconStyleClass,
								toolTip, 
								action.getName(), 
								button.getPixelWidth(),
								button.getPixelHeight(),
								action.getClientValidation(),
								confirmationText,
								action.getDisabledConditionName(), 
								formDisabledConditionName,
								action.getInvisibleConditionName());
	}

	@Override
	public UIComponent blurb(UIComponent component, 
								String dataWidgetVar,
								String value,
								String binding,
								Blurb blurb) {
		if (component != null) {
			return component;
		}

		return outputText(dataWidgetVar,
							value,
							binding,
							blurb.getTextAlignment(),
							blurb.getPixelWidth(),
							blurb.getPixelHeight(),
							blurb.getInvisibleConditionName());
	}
	
	@Override
	public UIComponent label(UIComponent component, 
								String dataWidgetVar,
								String value,
								String binding,
								Label label) {
		if (component != null) {
			return component;
		}

		return outputText(dataWidgetVar,
							value,
							binding,
							label.getTextAlignment(),
							label.getPixelWidth(),
							label.getPixelHeight(),
							label.getInvisibleConditionName());
	}

	private HtmlOutputText outputText(String dataWidgetVar, 
										String value, 
										String binding,
										HorizontalAlignment textAlignment,
										Integer pixelWidth,
										Integer pixelHeight,
										String invisibleConditionName) {
		HtmlOutputText result = (HtmlOutputText) a.createComponent(HtmlOutputText.COMPONENT_TYPE);
		setId(result, null);
		if (value != null) {
			result.setValue(value);
		} 
		else {
			// escape bindings with ' as \' as the binding could be for blurb expressions
			String sanitisedBinding = ((binding.indexOf('\'') >= 0) ? binding.replace("'", "\\'") : binding);
			if (dataWidgetVar != null) {
				result.setValueExpression("value", createValueExpressionFromFragment(dataWidgetVar, true, sanitisedBinding, true, null, Object.class));
			}
			else {
				result.setValueExpression("value", createValueExpressionFromFragment(sanitisedBinding, true, null, Object.class));
			}
		}
		result.setEscape(false);

		setTextAlign(result, textAlignment);
		setSize(result, null, pixelWidth, null, null, pixelHeight, null, null);
		setInvisible(result, invisibleConditionName, null);

		return result;
	}
	
	private int columnPriority;
	
	@Override
	public UIComponent dataGrid(UIComponent component, String dataWidgetVar, boolean ordered, String title, DataGrid grid) {
		if (component != null) {
			return component;
		}

		columnPriority = 1;
		
		String disabledConditionName = grid.getDisabledConditionName();
		String disableZoomConditionName = grid.getDisableZoomConditionName();
		String[] clickToZoomDisabledConditionNames = (disableZoomConditionName == null) ? 
														((disabledConditionName == null) ? 
															null : 
															new String[] {disabledConditionName}) :
														((disabledConditionName == null) ? 
															new String[] {disableZoomConditionName} : 
															new String[] {disableZoomConditionName, disabledConditionName});

		final DataTable dataTable = dataTable(grid.getBinding(),
												dataWidgetVar,
												title,
												grid.getInvisibleConditionName(),
												((! Boolean.TRUE.equals(grid.getInline())) &&
														(! Boolean.FALSE.equals(grid.getShowZoom())) &&
														(! Boolean.FALSE.equals(grid.getEditable()))),
												clickToZoomDisabledConditionNames,
												grid.getSelectedIdBinding(),
												grid.getSelectedActions(),
												ordered,
												grid.getWidgetId());

		UIOutput emptyMessage = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
		if ((! Boolean.FALSE.equals(grid.getEditable())) && (! Boolean.FALSE.equals(grid.getShowAdd()))) {
			emptyMessage.setValue(EMPTY_DATA_TABLE_CAN_ADD_MESSAGE);
		}
		else {
			emptyMessage.setValue(EMPTY_DATA_TABLE_MESSAGE);
		}
        dataTable.getFacets().put("emptyMessage", emptyMessage);

		return dataTable;
	}

	/*
	 * Data Repeater is just like a data grid - a data table but...
	 * The grid column headers can be turned off (uses prime.css)
	 * The grid (borders) can be turned off (uses prime.css)
	 * Any bound columns are editable inline.
	 */
	@Override
	public UIComponent dataRepeater(UIComponent component, String dataWidgetVar, String title, DataRepeater repeater) {
		if (component != null) {
			return component;
		}

		columnPriority = 1;

		DataTable result = dataTable(repeater.getBinding(),
										dataWidgetVar,
										title,
										repeater.getInvisibleConditionName(),
										false,
										null,
										null,
										null,
										false,
										repeater.getWidgetId());
		result.setEmptyMessage("");
		result.setStyleClass(repeaterStyleClass(Boolean.TRUE.equals(repeater.getShowColumnHeaders()),
													Boolean.TRUE.equals(repeater.getShowGrid())));
		result.setReflow(true);
		return result;
	}
	
	@Override
	public UIComponent addDataGridBoundColumn(UIComponent component,
												UIComponent current, 
												AbstractDataWidget widget,
												DataGridBoundColumn column,
												String dataWidgetVar,
												String columnTitle,
												String columnBinding,
												StringBuilder gridColumnExpression) {
		if (component != null) {
			return component;
		}

		Column result = column(dataWidgetVar,
								null,
								columnTitle,
	                            column.getAlignment(),
	                            false,
	                            column.getPixelWidth());
		result.setPriority(columnPriority);
		if (columnPriority < 6) {
			columnPriority++;
		}
		current.getChildren().add(result);

		// Output the value as boilerplate text in the table column if 
		// this is not an inline grid or the column is not editable
		boolean inline = (widget instanceof DataGrid) ? 
							Boolean.TRUE.equals(((DataGrid) widget).getInline()) :
							true;
		if ((! inline) || Boolean.FALSE.equals(column.getEditable())) {
	        gridColumnExpression.setLength(0);
	        gridColumnExpression.append('{').append(columnBinding).append('}');
	        result.getChildren().add(outputText(dataWidgetVar, gridColumnExpression.toString()));
		}
		
		return result;
	}
	
	@Override
	public UIComponent addedDataGridBoundColumn(UIComponent component, UIComponent current) {
		if (component != null) {
			return component;
		}

		// Insert <p:message> before the contents of the data grid column
		// and surround the lot with <div style="display:flex"></div>
		// The flex div ensures controls are laid out to availabel column width correctly (think combos)
		List<UIComponent> currentChildren = current.getChildren();
		if (! currentChildren.isEmpty()) {
			UIComponent contents = currentChildren.get(0);
			String forId = contents.getId();
			
			// If we have an input control in the column, surround it with the div
			HtmlPanelGroup div = null;
			if (contents instanceof UIInput) {
				div = panelGroup(true, true, true, null, null);
				div.setStyle("display:flex");
			}
			
			// The message to the left
			Message message = message(forId);
			message.setStyle("float:left");

			// If a div was not required (no input control), insert the message into the column
			if (div == null) {
				currentChildren.add(0, message);
			}
			else {
				// Add the message to the div
				List<UIComponent> divChildren = div.getChildren();
				divChildren.add(message);

				// Set the width of the input component to 100%
				UIComponent firstComponent = currentChildren.get(0);
				firstComponent.setValueExpression("style", ef.createValueExpression(elc, "width:100%", String.class));
	
				// add all the children column children to the div and add the div to the column
				divChildren.addAll(currentChildren);
				currentChildren.clear();
				currentChildren.add(div);
			}
		}

		return current.getParent(); // move from column to table
	}

	@Override
	public UIComponent addDataGridContainerColumn(UIComponent component,
													UIComponent current, 
													AbstractDataWidget widget,
													String title,
													DataGridContainerColumn column) {
		if (component != null) {
			return component;
		}

		Column col = column(widget.getBinding(),
								null,
								title,
				                column.getAlignment(),
				                false,
				                column.getPixelWidth());
		col.setPriority(columnPriority);
		if (columnPriority < 6) {
			columnPriority++;
		}
		current.getChildren().add(col);
		return col;
	}
	
	@Override
	public UIComponent addedDataGridContainerColumn(UIComponent component, UIComponent current) {
		if (component != null) {
			return component;
		}

		return current.getParent(); // move from column to table
	}
	
	@Override
	public UIComponent addDataGridActionColumn(UIComponent component,
												UIComponent current, 
												DataGrid grid, 
												String dataWidgetVar,
												String gridColumnExpression,
												String singularDocumentAlias,
												boolean inline) {
		if (component != null) {
			return component;
		}

		// only add a column if grid is editable
		if (! Boolean.FALSE.equals(grid.getEditable())) {
			String dataWidgetBinding = grid.getBinding();
			
			Column col = column(null,
									null,
									"",
					                HorizontalAlignment.centre,
					                true,
					                SINGLE_ACTION_COLUMN_WIDTH_INTEGER);
			col.setPriority(1);
			List<UIComponent> children = col.getChildren();

			String disabledConditionName = grid.getDisabledConditionName();

			// column header is a vertical flex with a little bit of space between the 2 buttons if needed
			final HtmlPanelGroup columnHeader = (HtmlPanelGroup) a.createComponent(HtmlPanelGroup.COMPONENT_TYPE);
			columnHeader.setLayout("block");
			columnHeader.setStyle("display:flex;flex-direction:column;height:70px;justify-content:space-evenly;align-items:center");
			col.getFacets().put("header", columnHeader);

			if (! Boolean.FALSE.equals(grid.getShowAdd())) {
				CommandButton button = createDataGridAddButton(grid, dataWidgetVar, singularDocumentAlias, inline, dataWidgetBinding, disabledConditionName);
				columnHeader.getChildren().add(button);
			}
			
			if (! Boolean.FALSE.equals(grid.getShowZoom())) {
				CommandButton button = createDataGridZoomButton(grid, dataWidgetVar, singularDocumentAlias, inline, dataWidgetBinding, disabledConditionName);
				children.add(button);
			}

			if (! Boolean.FALSE.equals(grid.getShowRemove())) {
				// Conditionally add some whitespace between buttons
				if (! col.getChildren().isEmpty()) {
					children.add(label(null, " "));
				}

				CommandButton button = createDataGridRemoveButton(grid, dataWidgetVar, singularDocumentAlias, dataWidgetBinding, disabledConditionName);
				children.add(button);
			}

			if (! children.isEmpty()) {
				if (children.size() > 1) {
					col.setWidth(DOUBLE_ACTION_COLUMN_WIDTH);
					col.setStyle("text-align:center !important");
				}
				current.getChildren().add(col);
			}
		}
		
		return current;
	}

	protected UIComponent createDataTableFilterToggle(String dataTableId) {
		final Button button = (Button) a.createComponent(Button.COMPONENT_TYPE);
		setId(button, null);
		button.setValue(null);
		button.setTitle("Toggle filters");
		button.setIcon("fa fa-filter");
		button.setOnclick(String.format("SKYVE.PF.toggleFilters('%s'); return false;", dataTableId));
		return button;
	}

	protected CommandButton createDataGridAddButton(DataGrid grid,
														String dataWidgetVar,
														String singularDocumentAlias,
														boolean inline,
														String dataWidgetBinding,
														String disabledConditionName) {
		CommandButton button = (CommandButton) a.createComponent(CommandButton.COMPONENT_TYPE);
		setId(button, null);
		button.setValue(null);
		button.setTitle("Add a new " + singularDocumentAlias);
		button.setIcon("fa fa-plus");
		action(button, ImplicitActionName.Add, null, dataWidgetBinding, dataWidgetVar, inline, null);
		// if we are in an inline data grid, update the grid on a new record
		if (inline) {
			button.setUpdate("@namingcontainer"); // update the data table - the closest naming container
		}
		String disableAddConditionName = grid.getDisableAddConditionName();
		String[] createDisabled = (disableAddConditionName == null) ?
									((disabledConditionName == null) ?
										null :
										new String[] {disabledConditionName}) :
									((disabledConditionName == null) ?
										new String[] {disableAddConditionName} :
										new String[] {disableAddConditionName, disabledConditionName});
		ValueExpression disabled = createOredValueExpressionFromConditions(createDisabled);
		if (disabled != null) {
			button.setValueExpression("disabled", disabled);
		}
		return button;
	}

	protected CommandButton createDataGridRemoveButton(DataGrid grid, String dataWidgetVar, String singularDocumentAlias, String dataWidgetBinding, String disabledConditionName) {
		CommandButton button = (CommandButton) a.createComponent(CommandButton.COMPONENT_TYPE);
		setId(button, null);
		button.setValue(null);
		button.setTitle("Remove this " + singularDocumentAlias);
		button.setIcon("fa fa-minus");
		// We cannot just update the data table ever when removing a row as
		// the grid may go invisible if the last row is removed.
		// There is no performance shortcut we can do as we dont know what is going on
		button.setUpdate(update); // update all forms (by default)

		action(button, ImplicitActionName.Remove, null, dataWidgetBinding, dataWidgetVar, true, grid.getRemovedActions());
		String disableRemoveConditionName = grid.getDisableRemoveConditionName();
		String[] removeDisabled = (disableRemoveConditionName == null) ?
									((disabledConditionName == null) ?
										null :
										new String[] {disabledConditionName}) :
									((disabledConditionName == null) ?
										new String[] {disableRemoveConditionName} :
										new String[] {disableRemoveConditionName, disabledConditionName});
		if (removeDisabled != null) {
			button.setValueExpression("disabled",
										createOredValueExpressionFromConditions(removeDisabled));
		}
		return button;
	}

	protected CommandButton createDataGridZoomButton(DataGrid grid, String dataWidgetVar, String singularDocumentAlias, boolean inline, String dataWidgetBinding, String disabledConditionName) {
		CommandButton button = (CommandButton) a.createComponent(CommandButton.COMPONENT_TYPE);
		setId(button, null);
		button.setValue(null);
		button.setTitle("Edit this " + singularDocumentAlias);
		button.setIcon("fa fa-chevron-right");
		action(button, ImplicitActionName.Navigate, null, dataWidgetBinding, dataWidgetVar, inline, null);
		String disableZoomConditionName = grid.getDisableZoomConditionName();
		String[] zoomDisabled = (disableZoomConditionName == null) ?
									((disabledConditionName == null) ?
										null :
										new String[] {disabledConditionName}) :
									((disabledConditionName == null) ?
										new String[] {disableZoomConditionName} :
										new String[] {disableZoomConditionName, disabledConditionName});
		if (zoomDisabled != null) {
			button.setValueExpression("disabled",
										createOredValueExpressionFromConditions(zoomDisabled));
		}
		return button;
	}

	@Override
	public UIComponent map(UIComponent component, 
							MapDisplay map,
							String moduleName,
							String queryName,
							String geometryBinding) {
		if (component != null) {
			return component;
		}
		return map(map, moduleName, queryName, geometryBinding, null);
	}

	@Override
	public UIComponent map(UIComponent component, MapDisplay map, String modelName) {
		if (component != null) {
			return component;
		}
		return map(map, null, null, null, modelName);
	}
	
	private UIComponent map(MapDisplay map, 
								String moduleName,
								String queryName,
								String geometryBinding,
								String modelName) {
		HtmlPanelGroup result = mapDiv(map);

		UIOutput script = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);

		StringBuilder value = new StringBuilder(128);
		value.append("#{").append(managedBeanName).append(".getMapScript('").append(result.getChildren().get(0).getClientId());
		if (modelName != null) {
			value.append("', null, null, null, '").append(modelName).append("'");
		}
		else {
			value.append("', '").append(moduleName);
			value.append("', '").append(queryName);
			value.append("', '").append(geometryBinding).append("', null");
		}
		LoadingType loading = map.getLoading();
		value.append(", '").append((loading == null) ? LoadingType.eager : loading);
		value.append("', ").append(map.getRefreshTimeInSeconds());
		value.append(", ").append(map.getShowRefreshControls());
		value.append(", null, false, true)}");
		script.setValueExpression("value", ef.createValueExpression(elc, value.toString(), String.class));
		result.getChildren().add(script);
		
		return result;
	}
	
	private HtmlPanelGroup mapDiv(RelativeSize widget) {
		HtmlPanelGroup result = (HtmlPanelGroup) a.createComponent(HtmlPanelGroup.COMPONENT_TYPE);
		result.setLayout("block");
		setId(result, null);
		
		Integer pixelHeight = widget.getPixelHeight();
		if (pixelHeight == null) {
			pixelHeight = Integer.valueOf(300);
		}
		setSize(result,
					null,
					widget.getPixelWidth(),
					widget.getResponsiveWidth(),
					widget.getPercentageWidth(),
					pixelHeight,
					widget.getPercentageHeight(),
					null);

		HtmlPanelGroup mapDiv = (HtmlPanelGroup) a.createComponent(HtmlPanelGroup.COMPONENT_TYPE);
		mapDiv.setLayout("block");
		mapDiv.setStyle("margin:0;padding:0;height:100%;width:100%");
		setId(mapDiv, null);
		
		UIOutput output = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
		output.setValue("Loading Map...");
		mapDiv.getChildren().add(output);
		
		result.getChildren().add(mapDiv);
		
		return result;		
	}
	
	@Override
	public EventSourceComponent geometry(EventSourceComponent component,
											String dataWidgetVar,
											Geometry geometry,
											String formDisabledConditionName,
											String title,
											boolean required) {
		if (component != null) {
			return component;
		}

		HtmlPanelGrid result = (HtmlPanelGrid) a.createComponent(HtmlPanelGrid.COMPONENT_TYPE);
		setId(result, null);
		String id = result.getId();
		setId(result, null); // new id for the panel but we'll use the old id for the text value and the map
		result.setColumns(3);
		result.setColumnClasses(",shrink,");
		Integer pixelWidth = geometry.getPixelWidth();
		if (pixelWidth != null) {
			result.setWidth(pixelWidth + "px");
		}
		else {
			result.setStyleClass("inputComponent");
		}
		List<UIComponent> toAddTo = result.getChildren();
		
		String binding = geometry.getBinding();
		InputText textField = textField(dataWidgetVar,
											geometry.getBinding(),
											title,
											required,
											false,
											geometry.getDisabledConditionName(),
											formDisabledConditionName,
											null,
											null,
											null,
											true);
		textField.setId(id + "_value");
		toAddTo.add(textField);
		editableGeometry(toAddTo,
							id,
							binding,
							geometry.getType(),
							geometry.getDisabledConditionName(),
							formDisabledConditionName);
		return new EventSourceComponent(result, textField);
	}

	/**
	 * Add the buttons and overlays etc
	 * 			<h:panelGrid> (from caller)
	 * 				...
	 *				<p:commandButton id="s03" icon="fa fa-globe" title="Map" type="button" />
	 *			    <p:overlayPanel id="s04" for="s03" hideEffect="fade" dynamic="false" showCloseIcon="true" modal="true" style="width:50%;height:300px" onShow="SKYVE.PF.gmap({elementId:'poo',geometryBinding:'boundry',disabled:false})">
	 *					<h:panelGroup layout="block" style="height:280px">
	 *						<h:panelGroup id="poo" layout="block" style="margin:0;padding:0;height:100%;width:100%">
	 *							Loading Map
	 *						</h:panelGroup>
	 *					</h:panelGroup>
	 *			    </p:overlayPanel>
	 *			</h:panelGrid>
	 */
	private void editableGeometry(List<UIComponent> toAddTo,
									String id,
									String binding,
									GeometryInputType type,
									String disabledConditionName,
									String formDisabledConditionName) {
		CommandButton mapButton = (CommandButton) a.createComponent(CommandButton.COMPONENT_TYPE);
		setId(mapButton, null);
		String mapButtonId = mapButton.getId();
		mapButton.setIcon("fa fa-globe");
		mapButton.setTitle("Map");
		mapButton.setValue(null);
		mapButton.setType("button");
		setDisabled(mapButton, disabledConditionName, formDisabledConditionName);
		// for admin theme
		setSize(mapButton, null, Integer.valueOf(30), null, null, Integer.valueOf(30), null, null);
		toAddTo.add(mapButton);

		OverlayPanel overlay = (OverlayPanel) a.createComponent(OverlayPanel.COMPONENT_TYPE);
		setId(overlay, null);
		overlay.setFor(mapButtonId);
		overlay.setHideEffect("fade");
		overlay.setDynamic(false);
		overlay.setShowCloseIcon(true);
		overlay.setModal(true);
		overlay.setStyle("width:50%;height:300px");
		
		MapDisplay display = new MapDisplay();
		display.setPixelHeight(Integer.valueOf(280));
		HtmlPanelGroup mapDivs = mapDiv(display);
		UIComponent mapDiv = mapDivs.getChildren().get(0);
		mapDiv.setId(id);
		overlay.getChildren().add(mapDivs);
		
		overlay.setValueExpression("onShow", generateMapScriptExpression(mapDiv.getClientId(),
																			binding,
																			type,
																			disabledConditionName,
																			formDisabledConditionName,
																			false));
		toAddTo.add(overlay);
	}
	
	@Override
	public EventSourceComponent geometryMap(EventSourceComponent component,
												GeometryMap geometry,
												String formDisabledConditionName,
												String title,
												boolean required) {
		if (component != null) {
			return component;
		}
		
		String binding = geometry.getBinding();
		
		HtmlPanelGroup result = mapDiv(geometry);
		UIComponent mapDiv = result.getChildren().get(0);
		
		// We use an input text here as there is no change event allowed on HtmlInputHidden
		HtmlInputText hidden = (HtmlInputText) input(HtmlInputText.COMPONENT_TYPE, null, binding, null, false, null, null);
		setId(hidden, mapDiv.getId() + "_value");
		hidden.setStyle("display:none");
		result.getChildren().add(hidden);
		
		UIOutput script = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
		script.setValueExpression("value", generateMapScriptExpression(mapDiv.getClientId(),
																		geometry.getBinding(),
																		geometry.getType(),
																		geometry.getDisabledConditionName(),
																		formDisabledConditionName,
																		true));
		result.getChildren().add(script);
		
		return new EventSourceComponent(result, hidden);
	}
	
	private ValueExpression generateMapScriptExpression(String mapDivClientId,
															String geometryBinding, 
															GeometryInputType type,
															String disabledConditionName,
															String formDisabledConditionName,
															boolean includeScriptTag) {
		StringBuilder value = new StringBuilder(128);
		value.append("#{").append(managedBeanName).append(".getMapScript('").append(mapDivClientId);
		value.append("', null, null, '").append(geometryBinding).append("', null, 'eager', null, null,");
		if (type == null) {
			value.append("null, ");
		}
		else {
			value.append("'").append(type).append("', ");
		}
		if (formDisabledConditionName == null) {
			if (disabledConditionName != null) {
				value.append(createOredValueExpressionFragmentFromConditions(new String[] {disabledConditionName}));
			}
			else {
				value.append("false");
			}
		}
		else {
			if (disabledConditionName == null) {
				value.append(createOredValueExpressionFragmentFromConditions(new String[] {formDisabledConditionName}));
			}
			else {
				value.append(createOredValueExpressionFragmentFromConditions(new String[] {disabledConditionName, formDisabledConditionName}));
			}
		}
		value.append(", ").append(includeScriptTag).append(")}");
		return ef.createValueExpression(elc, value.toString(), String.class);
	}
	
	@Override
	public UIComponent chart(UIComponent component, Chart chart) {
		if (component != null) {
			return component;
		}
		UIComponent result = null;
		ChartType type = chart.getType();
		switch (type) {
		case bar:
		case horizontalBar:
			result = a.createComponent(BarChart.COMPONENT_TYPE);
			break;
		case doughnut:
			result = a.createComponent(DonutChart.COMPONENT_TYPE);
			break;
		case line:
		case lineArea:
			result = a.createComponent(LineChart.COMPONENT_TYPE);
			break;
		case pie:
			result = a.createComponent(PieChart.COMPONENT_TYPE);
			break;
		case polarArea:
			result = a.createComponent(PolarAreaChart.COMPONENT_TYPE);
			break;
		case radar:
			result = a.createComponent(RadarChart.COMPONENT_TYPE);
			break;
		default:
			throw new IllegalArgumentException("Chart Type " + type + " is not supported.");
		}

		setId(result, null);
		
		Map<String, Object> attributes = result.getAttributes();
		attributes.put("skyveType", type);
		String modelName = chart.getModelName();
		if (modelName != null) {
			attributes.put("skyveModel", modelName);
		}
		else {
			attributes.put("skyveModel", chart.getModel());
		}
		StringBuilder value = new StringBuilder(64);
		value.append("#{").append(managedBeanName).append(".chartModel}");
		result.setValueExpression("model", ef.createValueExpression(elc, value.toString(), ChartModel.class));
		return result;
	}
	
	/*
		<p:dataTable id="list"
						var="row"
						value="#{skyve.getBeans(skyve.bizModuleParameter, skyve.bizDocumentParameter, skyve.queryNameParameter, skyve.modelName)}">
			<f:facet name="header">
				<p:outputPanel>
					Contacts
					<p:button href="./?a=#{WebAction.e.toString()}&amp;m=#{skyve.bizModuleParameter}&amp;d=#{skyve.bizDocumentParameter}" value="New" />
				</p:outputPanel>
			</f:facet>
			<p:column headerText="Name">
				<h:outputText value="#{row['bizKey']}" />
			</p:column>
			<p:column headerText="Actions" style="width:75px">
				<h:outputLink value="./">
					<h:outputText value="Edit" />
					<f:param name="a" value="#{WebAction.e.toString()}" />
					<f:param name="f" value="t" />
					<f:param name="m" value="#{row['bizModule']}" />
					<f:param name="d" value="#{row['bizDocument']}" />
					<f:param name="i" value="#{row['bizId']}" />
				</h:outputLink>
			</p:column>
		</p:dataTable>
	*/
	@Override
	public UIComponent listGrid(UIComponent component,
									String moduleName,
									String modelDocumentName,
									String modelName,
									ListModel<? extends Bean> model,
									String title,
									ListGrid grid,
									boolean canCreateDocument,
									boolean aggregateQuery) {
		if (component != null) {
			return component;
		}

		Document drivingDocument =  model.getDrivingDocument();
		String owningModuleName = drivingDocument.getOwningModuleName();
		String drivingDocumentName = drivingDocument.getName();

		boolean createRendered = (! aggregateQuery) && (! Boolean.FALSE.equals(grid.getShowAdd()));
		String disableAddConditionName = grid.getDisableAddConditionName();
		String disabledConditionName = grid.getDisabledConditionName();
		String[] createDisabled = (disableAddConditionName == null) ?
									((disabledConditionName == null) ?
											null :
											new String[] {disabledConditionName}) :
									((disabledConditionName == null) ?
											new String[] {disableAddConditionName} :
											new String[] {disableAddConditionName, disabledConditionName});
		boolean zoomRendered = (! aggregateQuery) && (! Boolean.FALSE.equals(grid.getShowZoom()));

		DataTable result = (DataTable) a.createComponent(DataTable.COMPONENT_TYPE);
        result.setVar("row");

        result.setLazy(true);
    	result.setRows(50);
    	
		UIOutput emptyMessage = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
        emptyMessage.setValue((canCreateDocument && createRendered) ? EMPTY_DATA_TABLE_CAN_ADD_MESSAGE : EMPTY_DATA_TABLE_MESSAGE);
        result.getFacets().put("emptyMessage", emptyMessage);

		result.setSortMode("multiple");

        setId(result, null);
    	result.setWidgetVar(result.getId());
    	
    	if (grid.getSelectedIdBinding() != null) {
    		addDataTableSelection(result, grid.getSelectedIdBinding(), grid.getSelectedActions(), modelName);
    	}
    	else if (zoomRendered) {
    		if (grid.getDisableZoomConditionName() == null) {
	    		result.setSelectionMode("single"); 
    		}
    		else {
	    		result.setValueExpression("selectionMode", 
	    									ef.createValueExpression(elc, String.format("#{(%s) ? '' : 'single'}", 
	    																					createOredValueExpressionFragmentFromConditions(new String[] {grid.getDisableZoomConditionName()})),
	    																					String.class));
    		}
	        result.setValueExpression("rowKey", ef.createValueExpression(elc, "&i=#{row['bizId']}&d=#{row['bizDocument']}&m=#{row['bizModule']}", String.class));
        
	        AjaxBehavior ajax = (AjaxBehavior) a.createBehavior(AjaxBehavior.BEHAVIOR_ID);
	        StringBuilder start = new StringBuilder(64);
	        start.append("var s=PF('").append(result.getId()).append("').selection[0];window.location='");
			start.append("?a=").append(WebAction.e.toString()).append("'+s;return false;");
			ajax.setOnstart(start.toString());
	        result.addClientBehavior("rowSelect", ajax);
    	}
    	
        // Write out getLazyDataModel call as the value
        StringBuilder modelExpression = new StringBuilder(128);
		modelExpression.append("#{").append(managedBeanName).append(".getLazyDataModel('").append(moduleName).append("','");
		if (model instanceof DocumentQueryListModel) {
			modelExpression.append(drivingDocumentName).append("','").append(modelName).append("',null,");
		}
		else {
			modelExpression.append(modelDocumentName).append("',null,'").append(modelName).append("',");
		}

		// Add filter parameters to getLazyDataModel call
		StringBuilder createUrlParams = null;
		List<FilterParameter> filterParameters = grid.getFilterParameters();
		List<Parameter> parameters = grid.getParameters();
		if (((filterParameters != null) && (! filterParameters.isEmpty())) ||
				((parameters != null) && (! parameters.isEmpty()))) {
			createUrlParams = new StringBuilder(64);
			modelExpression.append('[');
			if (filterParameters != null) {
				for (FilterParameter param : filterParameters) {
					String name = param.getName();
					String binding = param.getBinding();
					String value = param.getValue();
					
					createUrlParams.append('&').append(name).append("=#{").append(managedBeanName).append(".currentBean['");
					modelExpression.append("['").append(name).append("','");
					modelExpression.append(param.getOperator()).append("','");
					if (binding != null) {
						createUrlParams.append('{').append(binding).append("}']}");
						modelExpression.append('{').append(binding).append("}'],");
					}
					else {
						createUrlParams.append(binding).append("']}");
						modelExpression.append(value).append("'],");
					}
				}
			}
			if (parameters != null) {
				for (Parameter param : parameters) {
					String name = param.getName();
					String binding = param.getBinding();
					String value = param.getValue();
					
					createUrlParams.append('&').append(name).append("=#{").append(managedBeanName).append(".currentBean['");
					modelExpression.append("['").append(name).append("','");
					if (binding != null) {
						createUrlParams.append('{').append(binding).append("}']}");
						modelExpression.append('{').append(binding).append("}'],");
					}
					else {
						createUrlParams.append(binding).append("']}");
						modelExpression.append(value).append("'],");
					}
				}
			}
			modelExpression.setLength(modelExpression.length() - 1); // remove last comma
			modelExpression.append("])}");
		}
		else {
			modelExpression.append("null)}");
		}
		
		result.setValueExpression("value", ef.createValueExpression(elc, modelExpression.toString(), SkyveLazyDataModel.class));

		if (title != null) {
			addListGridHeader(title, result);
		}

		boolean showFilter = (! Boolean.FALSE.equals(grid.getShowFilter()));
		if (showFilter) {
			result.setFilterDelay(500);
		}
		
		List<UIComponent> children = result.getChildren();
        addListGridDataColumns(model, children, showFilter, result.getWidgetVar());
        if ((canCreateDocument && createRendered) || zoomRendered) {
        	final UIComponent actionColumn = createListGridActionColumn(owningModuleName,
									        								drivingDocumentName, 
									        								canCreateDocument,
									        								createRendered, 
									        								createDisabled,
									        								(createUrlParams == null) ? null : createUrlParams.toString(),
									        								zoomRendered,
									        								grid.getDisableZoomConditionName(),
																			result.getId(),
																			grid.getProperties());
			children.add(actionColumn);
        }
    	
    	return result;
	}
	
	protected void addDataTableSelection(DataTable table,
										String selectedIdBinding, 
										List<EventAction> selectedActions,
										String source) {
		table.setSelectionMode("single"); 
		table.setValueExpression("rowKey", ef.createValueExpression(elc, String.format("#{%s['bizId']}", table.getVar()), String.class));
		Map<String, Object> attributes = table.getAttributes();
		attributes.put("selectedIdBinding", selectedIdBinding);

        AjaxBehavior ajax = (AjaxBehavior) a.createBehavior(AjaxBehavior.BEHAVIOR_ID);
		if (selectedActions != null) {
			ActionFacesAttributes actionAttributes = determineActionFacesAttributes(selectedActions);
			if (actionAttributes.actionName == null) { // when no selected action defined (collection is empty) 
				ajax.setProcess((actionAttributes.process == null) ? "@this" : actionAttributes.process);
				ajax.setUpdate((actionAttributes.update == null) ? "@none" : actionAttributes.update);
			}
			else {
				attributes.put("actionName", actionAttributes.actionName);
				if (Boolean.TRUE.toString().equals(actionAttributes.actionName) || 
						Boolean.FALSE.toString().equals(actionAttributes.actionName)) {
					attributes.put("source", source);
				}
				ajax.setProcess((actionAttributes.process == null) ? process : actionAttributes.process);
				ajax.setUpdate((actionAttributes.update == null) ? update : actionAttributes.update);
			}
        }
        else {
			ajax.setProcess("@this");
        	ajax.setUpdate("@none");
        }

		String expression = String.format("#{%s.selectGridRow}", managedBeanName);
		MethodExpression me = ef.createMethodExpression(elc, expression, null, new Class[0]);
		ajax.addAjaxBehaviorListener(new AjaxBehaviorListenerImpl(me, me));
        table.addClientBehavior("rowSelect", ajax);
	}

	protected void addListGridHeader(String title,
									UIComponent componentToAddTo) {
		UIOutput heading = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
        heading.setValue(title);
		componentToAddTo.getFacets().put("header", heading);
	}

	protected void addListGridDataColumns(ListModel<? extends Bean> model,
											List<UIComponent> componentChildrenToAddTo,
											boolean showFilter,
											String tableVar) {
		Customer customer = CORE.getUser().getCustomer();
		Document document = model.getDrivingDocument();
		Module module = customer.getModule(document.getOwningModuleName());
		
		columnPriority = 1;

		for (MetaDataQueryColumn queryColumn : model.getColumns()) {
			MetaDataQueryProjectedColumn projectedQueryColumn = (queryColumn instanceof MetaDataQueryProjectedColumn) ?
																	(MetaDataQueryProjectedColumn) queryColumn :
																	null;
			if (queryColumn.isHidden() || 
					((projectedQueryColumn != null) && (! projectedQueryColumn.isProjected()))) {
				continue;
			}
			
			String name = queryColumn.getName();
			String binding = queryColumn.getBinding();

			// Sort out a display name and filter facet
			String displayName = queryColumn.getDisplayName();
			UIComponent specialFilterComponent = null;
			AttributeType attributeType = null;
			if (binding != null) {
				TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
				Document bindingDocument = target.getDocument();
				Attribute bindingAttribute = target.getAttribute();
				if (binding.endsWith(Bean.BIZ_KEY)) {
					if (displayName == null) {
						if (bindingDocument != null) {
							displayName = bindingDocument.getSingularAlias();
						}
						else {
							displayName = DocumentImpl.getBizKeyAttribute().getDisplayName();
						}
					}
				}
				else if (binding.endsWith(Bean.ORDINAL_NAME)) {
					if (displayName == null) {
						displayName = DocumentImpl.getBizOrdinalAttribute().getDisplayName();
					}
				}
				else if (bindingAttribute != null) {
					attributeType = bindingAttribute.getAttributeType();
					if (displayName == null) {
						displayName = bindingAttribute.getDisplayName();
					}
					if (showFilter && 
							(projectedQueryColumn != null) &&
							projectedQueryColumn.isFilterable()) {
						specialFilterComponent = createSpecialColumnFilterFacetComponent(document,
																							binding,
																							bindingAttribute,
																							tableVar);
					}
				}
			}
			
			// Create the column
			Column column = (Column) a.createComponent(Column.COMPONENT_TYPE);
			setId(column, null);
			column.setHeaderText(displayName);
			column.setPriority(columnPriority);
			column.setStyleClass("hiddenFilter");
			if (columnPriority < 6) {
				columnPriority++;
			}
			column.setField((name != null) ? name : binding);

			// Unbound columns or content columns or unsortable columns should be set unsortable
			if ((binding == null) || (projectedQueryColumn == null) || (! projectedQueryColumn.isSortable())) {
				column.setSortable(false);
			}
			else {
				column.setValueExpression("sortBy", 
											createValueExpressionFromFragment("row", true, binding, true, null, Object.class));
			}

			// Unbound columns, content columns or unfilterable columns should be set unfilterable
			if ((binding != null) && 
					showFilter && 
					(projectedQueryColumn != null) && 
					projectedQueryColumn.isFilterable()) {
				column.setValueExpression("filterBy", 
											createValueExpressionFromFragment("row", true, binding, true, null, Object.class));
				if (specialFilterComponent != null) {
					column.getFacets().put("filter", specialFilterComponent);
				}
			}
			else {
				column.setFilterable(false);
			}
			
			// Column styling
			StringBuilder style = new StringBuilder(64);
			Integer pixelWidth = queryColumn.getPixelWidth();
			if (pixelWidth == null) {
				pixelWidth = SmartClientGenerateUtils.determineDefaultColumnWidth(attributeType);
			}

			String value = null;
			if (projectedQueryColumn != null) { // projected column
				value = String.format("#{row['{%s}']}", (name != null) ? name : binding);
			}
			else { // content column
				MetaDataQueryContentColumn contentColumn = (MetaDataQueryContentColumn) queryColumn;
				DisplayType display = contentColumn.getDisplay();
				String emptyThumbnailRelativeFile = contentColumn.getEmptyThumbnailRelativeFile();
				Integer pixelHeight = contentColumn.getPixelHeight();
				String href = String.format("'content?_n='.concat(row['%s']).concat('&_doc=').concat(row['%s']).concat('.').concat(row['%s']).concat('&_b=%s')",
												binding, 
												Bean.MODULE_KEY,
												Bean.DOCUMENT_KEY,
												binding);

				if (DisplayType.thumbnail.equals(display)) {
					String width = (pixelWidth == null) ? 
			    						((pixelHeight == null) ? "64" : pixelHeight.toString()) :
										pixelWidth.toString();
					String height = (pixelHeight == null) ? 
										((pixelWidth == null) ? "64" : pixelWidth.toString()) : 
										pixelHeight.toString();

					// Adjust the table column
					// set content columns to have a padding of 5px on the left and right as the image served takes up
					// all of the available <td/>'s allotted space.
					// Also, increase the columns pixel width by 10px
					style.append("padding-left:5px;padding-right:5px;");
					pixelWidth = Integer.valueOf(Integer.parseInt(width) + 10);

					String empty = "''";
					if (emptyThumbnailRelativeFile != null) {
						empty = String.format("'<img src=\"resources?_n=%s'.concat('&_doc=').concat(row['%s']).concat('.').concat(row['%s']).concat('&_w=%s&_h=%s\"/>')",
												emptyThumbnailRelativeFile,
												Bean.MODULE_KEY,
												Bean.DOCUMENT_KEY,
												width,
												height);
					}
					value = String.format("#{(empty row['%s']) ? %s : '<a href=\"'.concat(%s).concat('\" target=\"_blank\"><img src=\"').concat(%s).concat('&_w=%s&_h=%s\"/></a>')}",
											binding,
											empty,
											href,
											href,
					                		width,
					                		height);
				}
				else if (DisplayType.link.equals(display)) {
					value = String.format("#{(empty row['%s']) ? '' : '<a href=\"'.concat(%s).concat('\" target=\"_blank\">Content</a>')}",
											binding,
											href);
					
				}
			}
			
			// Finish the column styling
			if (pixelWidth != null) {
				style.append("width:").append(pixelWidth).append("px;");
			}
			HorizontalAlignment alignment = queryColumn.getAlignment();
			if (alignment == null) {
				alignment = SmartClientGenerateUtils.determineDefaultColumnAlignment(attributeType);
			}
			if ((alignment != null) && (! HorizontalAlignment.left.equals(alignment))) {
				style.append("text-align:").append(HorizontalAlignment.centre.equals(alignment) ? "center" : "right").append(" !important;");
			}
			if (style.length() > 0) {
				column.setStyle(style.toString());
			}

			UIOutput outputText = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
			outputText.setValueExpression("value", ef.createValueExpression(elc, value, Object.class));
			column.getChildren().add(outputText);
			componentChildrenToAddTo.add(column);
		}
	}

	protected UIComponent createSpecialColumnFilterFacetComponent(Document modelDrivingDocument, 
																	String columnBinding,
																	Attribute columnAttribute,
																	String tableVar) {
		// To keep the appropriate length of the filter input components inside the data table columns,
		// A <div style="display:flex" /> should be used, but nesting the control in a div breaks
		// the filtering processing of faces...so instead, the <div class=".ui-column-customfilter" />
		// already rendered by PF data table is hijacked into a flexbox in prime.css.
		UIComponent result = null;

		if (DomainType.constant.equals(columnAttribute.getDomainType())) {
			HtmlSelectOneMenu s = selectOneMenu(null, null, null, false, null, null, null);
			s.setStyle("width:100%");
			s.setOnchange(String.format("PF('%s').filter()", tableVar));
			UISelectItems i = selectItems(modelDrivingDocument.getOwningModuleName(), 
											modelDrivingDocument.getName(),
											null,
											columnBinding,
											true);
			s.getChildren().add(i);
			result = s;
		}
		else {
			AttributeType type = columnAttribute.getAttributeType();
			if (AttributeType.bool.equals(type)) {
				TriStateCheckbox cb = (TriStateCheckbox) checkbox(null, null, null, false, null, null, true);
				cb.setOnchange(String.format("PF('%s').filter()", tableVar));
				result = cb;
			}
		}
		
		return result;
	}
	
	protected UIComponent createListGridActionColumn(String moduleName,
													   String documentName,
													   boolean canCreateDocument,
													   boolean createRendered,
													   String[] createDisabledConditionNames,
													   String createUrlParams,
													   boolean zoomRendered,
													   String zoomDisabledConditionName,
													   String parentId,
													   Map<String, String> properties) {
		Column column = (Column) a.createComponent(Column.COMPONENT_TYPE);
		column.setPriority(1);
		column.setWidth(SINGLE_ACTION_COLUMN_WIDTH);
		column.setStyle("text-align:center !important");

		// column header is a vertical flex with a little bit of space between the 2 buttons if needed
		final HtmlPanelGroup columnHeader = (HtmlPanelGroup) a.createComponent(HtmlPanelGroup.COMPONENT_TYPE);
		columnHeader.setLayout("block");
		columnHeader.setStyle("display:flex;flex-direction:column;height:70px;justify-content:space-evenly;align-items:center");
		column.getFacets().put("header", columnHeader);
		List<UIComponent> columnHeaderChildren = columnHeader.getChildren();

		final UIComponent filterToggle = createDataTableFilterToggle(parentId);
		columnHeaderChildren.add(filterToggle);

		if (canCreateDocument && createRendered) {
			Button button = (Button) a.createComponent(Button.COMPONENT_TYPE);
			setId(button, null);
			button.setValue(null);
			button.setTitle("New record");
			button.setIcon("fa fa-plus");
			ValueExpression disabled = createOredValueExpressionFromConditions(createDisabledConditionNames);
			if (disabled != null) {
				button.setValueExpression("disabled", disabled);
			}
			StringBuilder value = new StringBuilder(128);
			value.append("./?a=").append(WebAction.e.toString()).append("&m=").append(moduleName);
			value.append("&d=").append(documentName);
			if (createUrlParams != null) {
				value.append(createUrlParams);
				button.setValueExpression("href", ef.createValueExpression(elc, value.toString(), String.class));
			}
			else {
				button.setHref(value.toString());
			}
			columnHeaderChildren.add(button);
		}
		else {
			column.setHeaderText("");
		}
		if (zoomRendered) {
			final UIComponent button = createListGridZoomButton(zoomDisabledConditionName, properties);
			column.getChildren().add(button);
		}
		return column;
	}

	protected UIComponent createListGridZoomButton(String zoomDisabledConditionName,
													@SuppressWarnings("unused") Map<String, String> properties) {
		final Button button = (Button) a.createComponent(Button.COMPONENT_TYPE);
		setId(button, null);
		button.setValue(null);
		button.setTitle("View Detail");
		button.setIcon("fa fa-chevron-right");
		if (zoomDisabledConditionName != null) {
			button.setValueExpression("disabled",
					createValueExpressionFromCondition(zoomDisabledConditionName, null));
		}
		StringBuilder value = new StringBuilder(128);
		value.append("./?a=").append(WebAction.e.toString());
		value.append("&m=#{row['bizModule']}&d=#{row['bizDocument']}&i=#{row['bizId']}");
		button.setValueExpression("href", ef.createValueExpression(elc, value.toString(), String.class));

		return button;
	}

	/*
	 * List Repeater is just like a list grid - a data table but...
	 * The grid column headers can be turned off (uses prime.css)
	 * The grid (borders) can be turned off (uses prime.css)
	 * It implements infinite scrolling instead of the page controls.
	 * Any bound columns are editable inline.
	 * There is no action column.
	 * No CRUD.
	 */
	@Override
	public UIComponent listRepeater(UIComponent component,
										String modelDocumentName,
										String modelName,
										ListModel<? extends Bean> model,
										List<FilterParameter> filterParameters,
										List<Parameter> parameters,
										String title,
										boolean showColumnHeaders,
										boolean showGrid) {
		if (component != null) {
			return component;
		}

		Document drivingDocument =  model.getDrivingDocument();
		String moduleName = drivingDocument.getOwningModuleName();
		String drivingDocumentName = drivingDocument.getName();

		DataTable result = (DataTable) a.createComponent(DataTable.COMPONENT_TYPE);
        result.setVar("row");
        result.setPaginator(false);
        result.setLazy(true);
        result.setEmptyMessage("");
        
        setId(result, null);
    	result.setWidgetVar(result.getId());
    	    	
        // Write out getLazyDataModel call as the value
        StringBuilder value = new StringBuilder(64);
		value.append("#{").append(managedBeanName).append(".getLazyDataModel('").append(moduleName).append("','");
		if (model instanceof DocumentQueryListModel) {
			value.append(drivingDocumentName).append("','").append(modelName).append("',null,");
		}
		else {
			value.append(modelDocumentName).append("',null,'").append(modelName).append("',");
		}

		// Add filter parameters to getLazyDataModel call
		if (((filterParameters != null) && (! filterParameters.isEmpty())) ||
				((parameters != null) && (! parameters.isEmpty()))) {
			value.append('[');
			if (filterParameters != null) {
				for (FilterParameter param : filterParameters) {
					value.append("['").append(param.getName()).append("','");
					value.append(param.getOperator()).append("','");
					String binding = param.getBinding();
					if (binding != null) {
						value.append('{').append(binding).append("}'],");
					}
					else {
						value.append(param.getValue()).append("'],");
					}
				}
			}
			if (parameters != null) {
				for (Parameter param : parameters) {
					value.append("['").append(param.getName()).append("','");
					String binding = param.getBinding();
					if (binding != null) {
						value.append('{').append(binding).append("}'],");
					}
					else {
						value.append(param.getValue()).append("'],");
					}
				}
			}
			value.setLength(value.length() - 1); // remove last comma
			value.append("])}");
		}
		else {
			value.append("null)}");
		}
		
		result.setValueExpression("value", ef.createValueExpression(elc, value.toString(), SkyveLazyDataModel.class));

		if (title != null) {
			addListGridHeader(title, result);
		}
        List<UIComponent> children = result.getChildren();
        addListGridDataColumns(model, children, false, result.getWidgetVar());

        result.setStyleClass(repeaterStyleClass(showColumnHeaders, showGrid));
        result.setEmptyMessage("");
        result.setReflow(true);
        
        result.setScrollable(true);
        result.setScrollRows(50);
		result.setLiveScroll(true);
		//result.setScrollHeight(200);
		
        return result;
	}

	private static String repeaterStyleClass(boolean showColumnHeaders, boolean showGrid) {
        StringBuilder result = new StringBuilder(64);
        result.append("repeater");
        if (! showColumnHeaders) {
        	result.append(" repeater-no-headers");
        }
        if (! showGrid) {
        	result.append(" repeater-no-border");
        }
		return result.toString();
	}
	
	@Override
	public EventSourceComponent listMembership(EventSourceComponent component, ListMembership membership) {
		if (component != null) {
			return component;
		}

		PickList result = (PickList) a.createComponent(PickList.COMPONENT_TYPE);
		result.setVar("item");
		result.setShowSourceControls(false);
		result.setShowTargetControls(false);
		result.setShowSourceFilter(false);
		result.setShowTargetFilter(false);
		result.setResponsive(true);
		
        StringBuilder value = new StringBuilder(128);
        value.append("#{").append(managedBeanName).append(".dualListModels['");
        value.append(membership.getBinding()).append("']}");
        result.setValueExpression("value", ef.createValueExpression(elc, value.toString(), DualListModel.class));
		result.setConverter(new AssociationPickListConverter());

        result.setVar("item");
        result.setValueExpression("itemValue", ef.createValueExpression(elc, "#{item}", DomainValue.class));
        result.setValueExpression("itemLabel", ef.createValueExpression(elc, "#{item.description}", String.class));
        
        Map<String, UIComponent> facets = result.getFacets();
		String heading = membership.getCandidatesHeading();
		UIOutput text = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
		text.setValue((heading == null) ? "Candidates" : heading);
		setId(text, null);
		facets.put("sourceCaption", text);
		heading = membership.getMembersHeading();
		text = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
		text.setValue((heading == null) ? "Members" : heading);
		setId(text, null);
		facets.put("targetCaption", text);
		return new EventSourceComponent(result, result);
	}

	@Override
	public EventSourceComponent checkBox(EventSourceComponent component,
											String dataWidgetVar,
											CheckBox checkBox,
											String formDisabledConditionName,
											String title,
											boolean required) {
		if (component != null) {
			return component;
		}

		UIInput result = checkbox(dataWidgetVar,
									checkBox.getBinding(), 
									title,
									required,
									checkBox.getDisabledConditionName(),
									formDisabledConditionName,
									! Boolean.FALSE.equals(checkBox.getTriState()));
		return new EventSourceComponent(result, result);
	}
	
	@Override
	public EventSourceComponent colourPicker(EventSourceComponent component,
												String dataWidgetVar,
												ColourPicker colour,
												String formDisabledConditionName,
												String title,
												boolean required) {
		if (component != null) {
			return component;
		}

		ColorPicker result = colourPicker(dataWidgetVar, 
											colour.getBinding(), 
											title, 
											required, 
											colour.getDisabledConditionName(),
											formDisabledConditionName,
											colour.getPixelWidth(),
											true);
		return new EventSourceComponent(result, result);
	}
	
	@Override
	public EventSourceComponent combo(EventSourceComponent component,
										String dataWidgetVar,
										Combo combo,
										String formDisabledConditionName,
										String title,
										boolean required) {
		if (component != null) {
			return component;
		}

		String binding = combo.getBinding();
		HtmlSelectOneMenu result = selectOneMenu(dataWidgetVar,
													binding,
									                title,
									                required,
									                combo.getDisabledConditionName(),
									                formDisabledConditionName,
									                null);
		UISelectItems i = selectItems(null, null, dataWidgetVar, binding, true);
		result.getChildren().add(i);
		
		return new EventSourceComponent(result, result);
	}

	@Override
	public UIComponent contentImage(UIComponent component, 
										String dataWidgetVar,
										ContentImage image,
										String formDisabledConditionName,
										String title,
										boolean required) {
		if (component != null) {
			return component;
		}

		HtmlPanelGrid result = (HtmlPanelGrid) a.createComponent(HtmlPanelGrid.COMPONENT_TYPE);
		setId(result, null);
		result.setColumns(5);
		String id = result.getId();
		List<UIComponent> toAddTo = result.getChildren();
		
		String binding = image.getBinding();
		String sanitisedBinding = BindUtil.sanitiseBinding(binding);
		Integer pixelWidth = image.getPixelWidth();
		Integer pixelHeight = image.getPixelHeight();
		HtmlPanelGroup contentImage = contentGraphicImage((pixelWidth == null) ? ONE_HUNDRED : pixelWidth, 
															null,
															null, 
															(pixelHeight == null) ? ONE_HUNDRED : pixelHeight, 
															null, 
															binding);
		// Set the id of the inner image element
		contentImage.getChildren().get(0).setId(String.format("%s_%s_image", id, sanitisedBinding));
		toAddTo.add(contentImage);
		if (! Boolean.FALSE.equals(image.getEditable())) {
			editableContent(toAddTo,
								id,
								binding,
								sanitisedBinding,
								image.getDisabledConditionName(),
								formDisabledConditionName,
								true);
		}
		
		return result;
	}
	
	/**
	 * Content link in faces looks like...
	 * 				<h:outputLink href="SHITE">shiter</p:link>
	 * 				... then the buttons etc ...
	 */
	@Override
	public UIComponent contentLink(UIComponent component, 
									String dataWidgetVar,
									ContentLink link,
									String formDisabledConditionName,
									String title,
									boolean required) {
		if (component != null) {
			return component;
		}

		HtmlPanelGrid result = (HtmlPanelGrid) a.createComponent(HtmlPanelGrid.COMPONENT_TYPE);
		setId(result, null);
		result.setColumns(5);
		String id = result.getId();
		List<UIComponent> toAddTo = result.getChildren();
		
		String binding = link.getBinding();
		String sanitisedBinding = BindUtil.sanitiseBinding(binding);
		HtmlOutputLink contentLink = contentLink(link.getPixelWidth(), binding);
		contentLink.setId(String.format("%s_%s_link", id, sanitisedBinding));
		toAddTo.add(contentLink);
		if (! Boolean.FALSE.equals(link.getEditable())) {
			editableContent(toAddTo,
								id,
								binding,
								sanitisedBinding,
								link.getDisabledConditionName(),
								formDisabledConditionName,
								false);
		}
		
		return result;
	}
	
	/**
	 * Add the buttons and overlays etc
	 * 			<h:panelGrid> (from caller)
	 * 				...
	 *				<h:inputHidden id="s01_hidden" value="#{skyve.poo}" />
	 *			    <p:commandButton id="s03" icon="fa fa-upload" title="Upload Content" type="button" onclick="$(PrimeFaces.escapeClientId('s06')).attr('src', '/skyve/{content/image}Upload.xhtml')" />
	 *			    <p:overlayPanel id="s04" for="s03" hideEffect="fade" dynamic="true" showCloseIcon="true" modal="true" style="width:50%;height:300px">
	 *					<iframe id="s01_iframe" src="/skyve/{content/image}Upload.xhtml" style="width:100%;height:280px;border:none"></iframe>
	 *			    </p:overlayPanel>
	 *				<p:commandButton id="s05" icon="fa fa-trash" title="Clear Content" type="button" onclick="$(PrimeFaces.escapeClientId('s01_hidden')).val('')" />
	 *				...
	 *			</h:panelGrid>
	 * 
	 * @param toAddTo
	 * @param binding
	 */
	private void editableContent(List<UIComponent> toAddTo,
									String id,
									String binding,
									String sanitisedBinding,
									String disabledConditionName,
									String formDisabledConditionName,
									boolean image) {
		HtmlInputHidden hidden = (HtmlInputHidden) input(HtmlInputHidden.COMPONENT_TYPE, null, binding, null, false, null, null);
		setId(hidden, String.format("%s_%s", id, sanitisedBinding));
		toAddTo.add(hidden);
		
		CommandButton uploadButton = (CommandButton) a.createComponent(CommandButton.COMPONENT_TYPE);
		setId(uploadButton, null);
		String uploadButtonId = uploadButton.getId();
		uploadButton.setIcon("fa fa-upload");
		uploadButton.setTitle("Upload Content");
		uploadButton.setValue(null);
		uploadButton.setType("button");
		setDisabled(uploadButton, disabledConditionName, formDisabledConditionName);
		// for admin theme
		setSize(uploadButton, null, Integer.valueOf(30), null, null, Integer.valueOf(30), null, null);
		toAddTo.add(uploadButton);

		String var = sanitisedBinding + "Overlay";
		if (image) {
			uploadButton.setOnclick("PF('" + var + "').show();PF('" + var + "').toggleMaximize()");
		}

		UIPanel panel = null;
		if (image) {
			Dialog dialog = (Dialog) a.createComponent(Dialog.COMPONENT_TYPE);
			setId(dialog, null);
			dialog.setWidgetVar(var);
			dialog.setModal(true);
			dialog.setResponsive(true);
			dialog.setFitViewport(true);
			// clear the iframe src on hide so there is no flash next open
			dialog.setOnHide("SKYVE.PF.contentOverlayOnHide('" + id + "');PF('" + var + "').toggleMaximize()");
			panel = dialog;
		}
		else {
			OverlayPanel overlay = (OverlayPanel) a.createComponent(OverlayPanel.COMPONENT_TYPE);
			setId(overlay, null);
			overlay.setWidgetVar(sanitisedBinding + "Overlay");
			overlay.setFor(uploadButtonId);
			overlay.setHideEffect("fade");
			overlay.setDynamic(false);
			overlay.setShowCloseIcon(true);
			overlay.setModal(true);
			overlay.setStyle("width:50%;height:300px");
			// clear the iframe src on hide so there is no flash next open
			overlay.setOnHide(String.format("SKYVE.PF.contentOverlayOnHide('%s')", id));
			panel = overlay;
		}
		
		// $(PrimeFaces.escapeClientId('<id>')).attr('src', '<url>')
		StringBuilder value = new StringBuilder(64);
		value.append("#{'SKYVE.PF.contentOverlayOnShow(\\'").append(id).append("\\',\\''.concat(");
		value.append(managedBeanName).append(".getContentUploadUrl('").append(sanitisedBinding).append("',");
		value.append(image).append(")).concat('\\')')}");
		panel.setValueExpression("onShow", ef.createValueExpression(elc, value.toString(), String.class));
		toAddTo.add(panel);

		// <iframe id="s06" src="" style="width:100%;height:280px;border:none"></iframe>
		HtmlOutputText iframe = (HtmlOutputText) a.createComponent(HtmlOutputText.COMPONENT_TYPE);
		iframe.setEscape(false);
		iframe.setValue(String.format("<iframe id=\"%s_iframe\" src=\"\" style=\"width:100%%;height:100%%;border:none\"></iframe>", id));
		setId(iframe, null);
		panel.getChildren().add(iframe);
		
		CommandButton clearButton = (CommandButton) a.createComponent(CommandButton.COMPONENT_TYPE);
		setId(clearButton, null);
		clearButton.setIcon("fa fa-trash");
		clearButton.setTitle("Clear Content");
		clearButton.setValue(null);
		clearButton.setType("button");
		if (image) {
			clearButton.setOnclick(String.format("SKYVE.PF.clearContentImage('%s')", sanitisedBinding));
		}
		else {
			clearButton.setOnclick(String.format("SKYVE.PF.clearContentLink('%s')", sanitisedBinding));
		}
		setDisabled(clearButton, disabledConditionName, formDisabledConditionName);
		// for admin theme
		setSize(clearButton, null, Integer.valueOf(30), null, null, Integer.valueOf(30), null, null);
		toAddTo.add(clearButton);
	}
	
	@Override
	public UIComponent html(UIComponent component,
								String dataWidgetVar,
								HTML html,
								String formDisabledConditionName,
								String title,
								boolean required) {
		if (component != null) {
			return component;
		}

		return editor(dataWidgetVar,
						html.getBinding(),
						title,
						required,
						html.getDisabledConditionName(),
						formDisabledConditionName);
	}
	
	@Override
	public EventSourceComponent lookupDescription(EventSourceComponent component,
													String dataWidgetVar, 
													LookupDescription lookup, 
													String formDisabledConditionName,
													String title, 
													boolean required,
													String displayBinding,
													QueryDefinition query) {
		if (component != null) {
			return component;
		}

		AutoComplete result = autoComplete(dataWidgetVar,
											lookup.getBinding(),
											title,
											required,
											lookup.getDisabledConditionName(),
											formDisabledConditionName,
											displayBinding,
											query,
											lookup.getFilterParameters(),
											lookup.getParameters(),
											lookup.getPixelWidth(),
											false);
		return new EventSourceComponent(result, result);
	}

	@Override
	public EventSourceComponent password(EventSourceComponent component,
											String dataWidgetVar, 
											org.skyve.impl.metadata.view.widget.bound.input.Password password,
											String formDisabledConditionName,
											String title, 
											boolean required) {
		if (component != null) {
			return component;
		}

		Password result = password(dataWidgetVar,
									password.getBinding(), 
					                title,
					                required,
					                password.getDisabledConditionName(),
					                formDisabledConditionName,
					                password.getPixelWidth(),
					                true);
		return new EventSourceComponent(result, result);
	}

	@Override
	public EventSourceComponent radio(EventSourceComponent component,
										String dataWidgetVar,
										Radio radio,
										String formDisabledConditionName,
										String title,
										boolean required) {
		if (component != null) {
			return component;
		}

		String binding = radio.getBinding();
        SelectOneRadio result = selectOneRadio(dataWidgetVar,
												binding,
				                                title,
				                                required,
				                                radio.getDisabledConditionName(),
				                                formDisabledConditionName);
        result.getAttributes().put("binding", radio.getBinding());
        UISelectItems i = selectItems(null, null, dataWidgetVar, binding, false);
		result.getChildren().add(i);
		return new EventSourceComponent(result, result);
	}
	
	@Override
	public EventSourceComponent richText(EventSourceComponent component, 
											String dataWidgetVar, 
											RichText text, 
											String formDisabledConditionName,
											String title,
											boolean required) {
		if (component != null) {
			return component;
		}

		Editor result = editor(dataWidgetVar,
								text.getBinding(),
								title,
								required,
								text.getDisabledConditionName(),
								formDisabledConditionName);
		return new EventSourceComponent(result, result);
	}

	@Override
	public EventSourceComponent spinner(EventSourceComponent component,
											String dataWidgetVar, 
											org.skyve.impl.metadata.view.widget.bound.input.Spinner spinner,
											String formDisabledConditionName,
											String title, 
											boolean required) {
		if (component != null) {
			return component;
		}

		Spinner result = spinner(dataWidgetVar, 
									spinner.getBinding(), 
									title, 
									required, 
									spinner.getDisabledConditionName(),
									formDisabledConditionName,
									spinner.getPixelWidth());
		return new EventSourceComponent(result, result);
	}
	
	@Override
	public EventSourceComponent textArea(EventSourceComponent component,
											String dataWidgetVar, 
											TextArea text, 
											String formDisabledConditionName,
											String title, 
											boolean required,
											Integer length) {
		if (component != null) {
			return component;
		}

		InputTextarea result = textArea(dataWidgetVar,
											text.getBinding(),
											title,
											required,
											Boolean.FALSE.equals(text.getEditable()),
											text.getDisabledConditionName(),
											formDisabledConditionName,
											length,
											text.getPixelWidth(),
											text.getPixelHeight(),
											true);
		return new EventSourceComponent(result, result);
	}
	
	@Override
	public EventSourceComponent text(EventSourceComponent component,
										String dataWidgetVar, 
										TextField text, 
										String formDisabledConditionName,
										String title, 
										boolean required,
										Integer length,
										org.skyve.domain.types.converters.Converter<?> converter,
										Format<?> format,
										Converter facesConverter) {
		if (component != null) {
			return component;
		}

		boolean useCalendar = false;
		Format<?> mutableFormat = format;
		if (converter != null) {
			AttributeType converterAttributeType = converter.getAttributeType();
			// Date type and editable field - use readonly mask if not editable.
			useCalendar = (! Boolean.FALSE.equals(text.getEditable())) &&
	        				(AttributeType.date.equals(converterAttributeType) || 
        						AttributeType.dateTime.equals(converterAttributeType) ||
        						AttributeType.timestamp.equals(converterAttributeType));
	        if (mutableFormat == null) {
		        mutableFormat = converter.getFormat();
	        }
		}
		
        UIComponentBase result = null;
        if (useCalendar) {
            result = calendar(dataWidgetVar,
	            				text.getBinding(),
	                            title,
	                            required,
	                            false,
	                            text.getDisabledConditionName(),
	                            formDisabledConditionName,
	                            facesConverter);
        }
        else if (mutableFormat != null) {
            result = maskField(dataWidgetVar,
								text.getBinding(),
								title,
								required,
								Boolean.FALSE.equals(text.getEditable()),
								text.getDisabledConditionName(),
								formDisabledConditionName,
								length,
								mutableFormat,
								facesConverter,
								text.getPixelWidth(),
								true);
        }
        else if (Boolean.TRUE.equals(text.getPreviousValues())) {
        	result = previousValues(dataWidgetVar,
			        					text.getBinding(),
			        					title,
			        					required,
			        					text.getDisabledConditionName(),
			        					length,
			        					formDisabledConditionName,
			        					text.getPixelWidth());
        }
        else {
        	result = textField(dataWidgetVar,
								text.getBinding(),
								title,
								required,
								Boolean.FALSE.equals(text.getEditable()),
								text.getDisabledConditionName(),
								formDisabledConditionName,
								length,
								facesConverter,
								text.getPixelWidth(),
								true);
        }
        
        return new EventSourceComponent(result, result);
	}

	@Override
	public UIComponent actionLink(UIComponent component,
								  String dataWidgetBinding,
								  String dataWidgetVar,
								  String value,
								  Link link,
								  Action action) {
		if (component != null) {
			return component;
		}

		Map<String, String> properties = link.getProperties();

		ImplicitActionName name = action.getImplicitName();
		if (ImplicitActionName.Download.equals(name)) {
			return downloadLink(value,
									action.getToolTip(),
									action.getName(),
									dataWidgetBinding,
									dataWidgetVar, 
									link.getPixelWidth(), 
									action.getConfirmationText(),
									action.getDisabledConditionName(), 
									null,
									action.getInvisibleConditionName(),
									properties.get(PROCESS_KEY),
									properties.get(UPDATE_KEY));
		}

		return actionLink(value,
							action.getToolTip(),
							null,
							action.getName(),
							false,
							dataWidgetBinding,
							dataWidgetVar,
							link.getPixelWidth(),
							null,
							action.getClientValidation(),
							action.getConfirmationText(),
							action.getDisabledConditionName(),
							null,
							action.getInvisibleConditionName(),
							properties.get(PROCESS_KEY),
							properties.get(UPDATE_KEY));
	}
	
	@Override
	public UIComponent report(UIComponent component, Action action) {
		if (component != null) {
			return component;
		}

		return reportButton(action.getDisplayName(), 
								action.getIconStyleClass(),
								action.getToolTip(), 
								action.getParameters(), 
								null,
								null,
								action.getClientValidation(),
								action.getConfirmationText(),
								action.getDisabledConditionName(), 
								null,
								action.getInvisibleConditionName());
	}
	
	@Override
	public UIComponent download(UIComponent component,
									String dataWidgetBinding,
									String dataWidgetVar,
									Action action) {
		if (component != null) {
			return component;
		}
	
		Map<String, String> properties = action.getProperties();

		return downloadButton(action.getDisplayName(), 
								action.getIconStyleClass(),
								action.getToolTip(), 
								dataWidgetBinding,
								dataWidgetVar,
								action.getName(), 
								null,
								null,
								action.getConfirmationText(),
								action.getDisabledConditionName(), 
								null,
								action.getInvisibleConditionName(),
								properties.get(PROCESS_KEY),
								properties.get(UPDATE_KEY));
	}

	@Override
	public UIComponent upload(UIComponent component, Action action) {
		if (component != null) {
			return component;
		}

		return uploadButton(action.getDisplayName(), 
								action.getIconStyleClass(),
								action.getToolTip(), 
								action.getName(), 
								null,
								null,
								action.getClientValidation(),
								action.getConfirmationText(),
								action.getDisabledConditionName(), 
								null,
								action.getInvisibleConditionName());
	}

	@Override
	public UIComponent action(UIComponent component,
								String dataWidgetBinding,
								String dataWidgetVar,
								Action action, 
								ImplicitActionName name,
								String title) {
		if (component != null) {
			return component;
		}

		Map<String, String> properties = action.getProperties();
		return actionButton(title,
								action.getIconStyleClass(),
								action.getToolTip(),
								name,
								action.getName(),
								false,
								dataWidgetBinding,
								dataWidgetVar,
								null,
								null,
								action.getClientValidation(),
								action.getConfirmationText(),
								action.getDisabledConditionName(),
								null,
								action.getInvisibleConditionName(),
								properties.get(PROCESS_KEY),
								properties.get(UPDATE_KEY));
	}

	protected Panel panel(String title, String invisible, Integer pixelWidth, String widgetId) {
		Panel result = (Panel) a.createComponent(Panel.COMPONENT_TYPE);
		setValueOrValueExpression(title, result::setHeader, "header", result);
		setInvisible(result, invisible, null);
		setSize(result, null, pixelWidth, null, null, null, null, NINETY_EIGHT);
		setId(result, widgetId);
		return result;
	}

	protected Password password(String dataWidgetVar, 
									String binding, 
									String title, 
									boolean required, 
									String disabled,
									String formDisabled,
									Integer pixelWidth, 
									boolean applyDefaultWidth) {
		Password result = (Password) input(Password.COMPONENT_TYPE, dataWidgetVar, binding, title, required, disabled, formDisabled);
		result.setId(result.getId() + "password"); // ensures that the password field value is not logged in the request parameters on the server
		setSize(result, null, pixelWidth, null, null, null, null, applyDefaultWidth ? ONE_HUNDRED : null);
		return result;
	}

	protected InputText textField(String dataWidgetVar, 
									String binding, 
									String title, 
									boolean required, 
									boolean readonly,
									String disabled,
									String formDisabled,
									Integer maxLength, 
									Converter converter, 
									Integer pixelWidth, 
									boolean applyDefaultWidth) {
		InputText result = (InputText) input(InputText.COMPONENT_TYPE, 
												dataWidgetVar, 
												binding, 
												title, 
												required,
												disabled,
												formDisabled);
		if (readonly) {
			result.setReadonly(true);
		}
		if (maxLength != null) {
			result.setMaxlength(maxLength.intValue());
		}
		if (converter != null) {
			result.setConverter(converter);
		}
		setSize(result, null, pixelWidth, null, null, null, null, applyDefaultWidth ? ONE_HUNDRED : null);
		return result;
	}

	private InputMask maskField(String dataWidgetVar,
									String binding, 
									String title, 
									boolean required, 
									boolean readonly,
									String disabled,
									String formDisabled,
									Integer maxLength, 
									Format<?> format, 
									Converter converter, 
									Integer pixelWidth, 
									boolean applyDefaultWidth) {
		InputMask result = (InputMask) input(InputMask.COMPONENT_TYPE, 
												dataWidgetVar, 
												binding, 
												title, 
												required,
												disabled,
												formDisabled);
		if (maxLength != null) {
			result.setMaxlength(maxLength.intValue());
		}
		if (readonly) {
			result.setReadonly(true);
		}
		result.setMask(determineMask(format));
		String existingStyle = null;
		TextCase textCase = format.getTextCase();
		if (textCase != null) {
			switch (textCase) {
			case upper:
				existingStyle = "text-transform:uppercase;";
				break;
			case capital:
				existingStyle = "text-transform:capitalize;";
				break;
			case lower:
				existingStyle = "text-transform:lowercase;";
				break;
			default:
				throw new IllegalStateException(textCase + " is not supported");
			}
		}
		if (converter != null) {
			result.setConverter(converter);
		}
		setSize(result, existingStyle, pixelWidth, null, null, null, null, applyDefaultWidth ? ONE_HUNDRED : null);
		return result;
	}

	/**
	 * My spec is A - alphanumeric # - digit L - letter
	 * 
	 * PF spec is 
	 * Character Description 
	 * 9 Digit (0 through 9) 
	 * a Letter (A through Z) 
	 * * Letter (A through Z) or number (0 through 9) 
	 * ? Allow optional matching of the rest of the expression
	 * 
	 * This method escapes anything that should be literal and then converts the
	 * expression taking into consideration the case setting.
	 * 
	 * @param format
	 * @return
	 */
	private static String determineMask(Format<?> format) {
		String result = null;

		if (format != null) {
			result = format.getMask();
			if (result != null) {
				// first escape characters with meaning
				result = result.replace("9", "\\9");
				result = result.replace("a", "\\a");
				result = result.replace("*", "\\*");
				result = result.replace("?", "\\?");

				// transpose my spec to the PF spec
				result = result.replace("A", "*");
				result = result.replace("#", "9");
				result = result.replace("L", "a");
			}
		}

		return result;
	}

	private Spinner spinner(String dataWidgetVar, 
								String binding, 
								String title, 
								boolean required, 
								String disabled,
								String formDisabled,
								Integer pixelWidth) {
		Spinner result = (Spinner) input(Spinner.COMPONENT_TYPE, dataWidgetVar, binding, title, required, disabled, formDisabled);
		setSize(result, null, pixelWidth, null, null, null, null, null);
		return result;
	}

	private Calendar calendar(String dataWidgetVar,
								String binding, 
								String title, 
								boolean required, 
								boolean mobile,
								String disabled,
								String formDisabled,
								Converter converter) {
		Calendar result = (Calendar) input(Calendar.COMPONENT_TYPE, dataWidgetVar, binding, title, required, disabled, formDisabled);
		if (! mobile) {
			result.setMode("popup");
			result.setShowOn("button");
			result.setNavigator(true);
			result.setShowButtonPanel(true);
		}
		result.setYearRange("c-100:c+10");

		String converterName = converter.getClass().getSimpleName();
		if ("DD_MM_YYYY".equals(converterName)) {
			result.setPattern("dd/MM/yyyy");
			result.setMask("99/99/9999");
		} 
		else if ("DD_MMM_YYYY".equals(converterName)) {
			result.setPattern("dd-MMM-yyyy");
			result.setMask("99-aaa-9999");
		} 
		else if ("DD_MM_YYYY_HH_MI".equals(converterName)) {
			result.setPattern("dd/MM/yyyy hh:mm");
			result.setMask("99/99/9999 99:99");
		} 
		else if ("DD_MM_YYYY_HH24_MI".equals(converterName)) {
			result.setPattern("dd/MM/yyyy HH:mm");
			result.setMask("99/99/9999 99:99");
		} 
		else if ("DD_MMM_YYYY_HH_MI".equals(converterName)) {
			result.setPattern("dd-MMM-yyyy hh:mm");
			result.setMask("99-aaa-9999 99:99");
		} 
		else if ("DD_MMM_YYYY_HH24_MI".equals(converterName)) {
			result.setPattern("dd-MMM-yyyy HH:mm");
			result.setMask("99-aaa-9999 99:99");
		} 
		else if ("DD_MM_YYYY_HH_MI_SS".equals(converterName)) {
			result.setPattern("dd/MM/yyyy hh:mm:ss");
			result.setMask("99/99/9999 99:99:99");
		} 
		else if ("DD_MM_YYYY_HH24_MI_SS".equals(converterName)) {
			result.setPattern("dd/MM/yyyy HH:mm:ss");
			result.setMask("99/99/9999 99:99:99");
		} 
		else if ("DD_MMM_YYYY_HH_MI_SS".equals(converterName)) {
			result.setPattern("dd-MMM-yyyy hh:mm:ss");
			result.setMask("99-aaa-9999 99:99:99");
		} 
		else if ("DD_MMM_YYYY_HH24_MI_SS".equals(converterName)) {
			result.setPattern("dd-MMM-yyyy HH:mm:ss");
			result.setMask("99-aaa-9999 99:99:99");
		}
		result.setConverter(converter);
		return result;
	}

	protected InputTextarea textArea(String dataWidgetVar, 
										String binding, 
										String title, 
										boolean required, 
										boolean readonly,
										String disabled,
										String formDisabled,
										Integer maxLength, 
										Integer pixelWidth, 
										Integer pixelHeight, 
										boolean applyDefaultWidth) {
		InputTextarea result = (InputTextarea) input(InputTextarea.COMPONENT_TYPE, 
														dataWidgetVar, 
														binding, 
														title,
														required, 
														disabled,
														formDisabled);
		if (readonly) {
			result.setReadonly(true);
		}
		if (maxLength != null) {
			result.setMaxlength(maxLength.intValue());
		}
		setSize(result, null, pixelWidth, null, null, pixelHeight, null, applyDefaultWidth ? ONE_HUNDRED : null);
		return result;
	}

	protected CommandButton actionButton(String title, 
											String iconStyleClass,
											String tooltip, 
											ImplicitActionName implicitActionName,
											String actionName,
											boolean inline, 
											String dataWidgetBinding, 
											String dataWidgetVar,
											Integer pixelWidth, 
											Integer pixelHeight,
											Boolean clientValidation, 
											String confirmationText,
											String disabled, 
											String formDisabled,
											String invisible,
											String processOverride,
											String updateOverride) {
		CommandButton result = (CommandButton) a.createComponent(CommandButton.COMPONENT_TYPE);

		result.setValue(title);
		result.setIcon(iconStyleClass);
		result.setTitle(tooltip);

		action(result, implicitActionName, actionName, dataWidgetBinding, dataWidgetVar, inline, null);
		setSize(result, null, pixelWidth, null, null, pixelHeight, null, null);
		setDisabled(result, disabled, formDisabled);
		setConfirmation(result, confirmationText);
		setId(result, null);

		// set a default icon if not already set and client Validation (immediate)
		if (implicitActionName != null) {
			switch (implicitActionName) {
				case OK:
					if (iconStyleClass == null) { 
						result.setIcon("fa fa-check");
					}
					result.setImmediate(Boolean.FALSE.equals(clientValidation)); // switch validation
					break;
				case Save:
					if (iconStyleClass == null) { 
						result.setIcon("fa fa-save");
					}
					result.setImmediate(Boolean.FALSE.equals(clientValidation)); // switch validation
					break;
				case Delete:
					if (iconStyleClass == null) { 
						result.setIcon("fa fa-trash-o");
					}
					result.setImmediate(Boolean.FALSE.equals(clientValidation)); // switch validation
					// Add the standard confirmation text if none exists
					if (confirmationText == null) {
						setConfirmation(result, "Do you want to delete this data?");
					}
					break;
				case Add:
				case New:
					if (iconStyleClass == null) { 
						result.setIcon("fa fa-plus");
					}
					break;
				case ZoomOut:
					if (iconStyleClass == null) { 
						result.setIcon("fa fa-reply");
					}
					result.setImmediate(Boolean.FALSE.equals(clientValidation)); // switch validation
					break;
				case Cancel:
					if (iconStyleClass == null) { 
						result.setIcon("fa fa-chevron-left");
					}
					result.setImmediate(true); // no validation
					result.setAjax(false); // normal request - which is slightly faster
					break;
				case Remove:
					if (iconStyleClass == null) { 
						result.setIcon("fa fa-minus");
					}
					result.setImmediate(true);
					// Add the standard confirmation text if none exists
					if (confirmationText == null) {
						setConfirmation(result, "Do you want to remove this data?");
					}
					break;
				case Edit:
					if (iconStyleClass == null) { 
						result.setIcon("fa fa-pencil");
					}
					break;
				case Report:
					if (iconStyleClass == null) { 
						result.setIcon("fa fa-newspaper");
					}
					break;
				case BizImport:
					if (iconStyleClass == null) { 
						result.setIcon("fa fa-cloud-download");
					}
					result.setImmediate(Boolean.FALSE.equals(clientValidation)); // switch validation
					break;
				case BizExport:
					if (iconStyleClass == null) { 
						result.setIcon("fa fa-cloud-upload");
					}
					result.setImmediate(Boolean.FALSE.equals(clientValidation)); // switch validation
					break;
				case Download:
					if (iconStyleClass == null) { 
						result.setIcon("fa fa-download");
					}
					result.setImmediate(Boolean.FALSE.equals(clientValidation)); // switch validation
					break;
				case Upload:
					if (iconStyleClass == null) { 
						result.setIcon("fa fa-upload");
					}
					result.setImmediate(Boolean.FALSE.equals(clientValidation)); // switch validation
					break;
				default:
					break;
			}
		}

		// show/hide the implicit buttons - TODO base this also on security privileges.
		if (ImplicitActionName.OK.equals(implicitActionName) || 
				ImplicitActionName.Save.equals(implicitActionName) || 
				ImplicitActionName.Cancel.equals(implicitActionName) || 
				ImplicitActionName.Delete.equals(implicitActionName)) {
			StringBuilder expression = new StringBuilder(128);
			expression.append("empty ").append(managedBeanName).append(".viewBinding");
			
			// Add check for history before showing OK, Cancel or Delete buttons
			if (! ImplicitActionName.Save.equals(implicitActionName)) {
				expression.insert(0, "((");
				expression.append(") and ").append(managedBeanName).append(".hasHistory)");
			}
			
			// Add invisible condition to the mix
			if (invisible == null) {
				result.setValueExpression("rendered",
											createValueExpressionFromFragment(null, 
																				false,
																				expression.toString(), 
																				false, 
																				null, 
																				Boolean.class));
			} 
			else {
				setInvisible(result, invisible, expression.toString());
			}
		} 
		else if (ImplicitActionName.ZoomOut.equals(implicitActionName) || 
					ImplicitActionName.Remove.equals(implicitActionName)) {
			if (! inline) { // inline grids don't need invisible expression on remove button or link
				StringBuilder expression = new StringBuilder(128);
				expression.append("not empty ").append(managedBeanName).append(".viewBinding");
				if (invisible == null) {
					result.setValueExpression("rendered",
												createValueExpressionFromFragment(null, 
																					false,
																					expression.toString(), 
																					false, 
																					null, 
																					Boolean.class));
				}
				else {
					setInvisible(result, invisible, expression.toString());
				}
			}
		}
		else {
			setInvisible(result, invisible, null);
		}

		if (! ImplicitActionName.Cancel.equals(implicitActionName)) { // cancel is not ajax
			result.setProcess((processOverride == null) ? process : processOverride); // process the current form (by default)
			result.setUpdate((updateOverride == null) ? update : updateOverride); // update all forms (by default)
		}

		return result;
	}

	/**
	 * Create a button with a href URL that looks like...
	 * http://localhost:8080/skyve/report/Bum.html?_f=html&_c=<webId>&_id=<id>&wee=poo&_n=Bum&_mod=<module>&_doc=<document>
	 */
	private Button reportButton(String title,
									String iconStyleClass,
									String tooltip,
									List<Parameter> parameters,
									Integer pixelWidth, 
									Integer pixelHeight,
									@SuppressWarnings("unused") Boolean clientValidation, // TODO not implemented
									// TODO LinkButton is not a Confirmable. ConfirmBehavior can only be attached to components that implement org.primefaces.component.api.Confirmable interface
									@SuppressWarnings("unused") String confirmationText,
									String disabled,
									String formDisabled,
									String invisible) {
		StringBuilder href = new StringBuilder(128);
		String reportName = null;
		ReportFormat reportFormat = null;
		for (Parameter param : parameters) {
			String paramName = param.getName();
			String paramValue = param.getValue();
			String paramBinding = param.getBinding();
			if (AbstractWebContext.REPORT_NAME.equals(paramName)) {
				reportName = paramValue;
			}
			else if (AbstractWebContext.REPORT_FORMAT.equals(paramName)) {
				reportFormat = ReportFormat.valueOf(paramValue);
			}
			
			if (paramValue != null) {
				href.append(paramName).append('=').append(paramValue).append('&'); 
			}
			else if (paramBinding != null) {
				href.append(paramName).append("=#{").append(managedBeanName).append(".currentBean['{");
				href.append(paramBinding).append("}']}&"); 
			}
		}

		// add Web Id and Current Bean Id
		href.append(AbstractWebContext.CONTEXT_NAME).append("=#{").append(managedBeanName).append(".webContext.webId}&");
		href.append(AbstractWebContext.ID_NAME).append("=#{").append(managedBeanName).append(".currentBean['{");
		href.append(Bean.DOCUMENT_ID).append("}']}");
		
		// if no report format parameter set, add it
		if (reportFormat == null) {
			reportFormat = ReportFormat.pdf;
			href.append('&').append(AbstractWebContext.REPORT_FORMAT).append('=').append(reportFormat);
		}

		
		// NB yes this is backwards coz its inserted
		href.insert(0, '?').insert(0, reportFormat).insert(0, '.').insert(0, reportName).insert(0, "report/");

		return linkButton((iconStyleClass == null) ? "fa fa-newspaper" : iconStyleClass, 
							null,
							null,
							title,
							tooltip,
							href.toString(),
							pixelWidth,
							pixelHeight,
							disabled,
							formDisabled,
							invisible,
							(ReportFormat.html.equals(reportFormat) ||
								ReportFormat.xhtml.equals(reportFormat)) ? 
									"_blank" : 
									null);
	}

	/**
	 * Create a command button that redirects to a URL that looks like...
	 * http://localhost:8080/skyve/download?_n=<downloadAction>&_doc=<module>.<document>&_c=<webId>&_b=<form binding>&_ctim=<currentTimeInMillis>
	 */
	private CommandButton downloadButton(String title, 
											String iconStyleClass,
											String tooltip,
											String dataWidgetBinding,
											String dataWidgetVar,
											String downloadActionName,
											Integer pixelWidth, 
											Integer pixelHeight,
											String confirmationText,
											String disabled,
											String formDisabled,
											String invisible,
											String processOverride,
											String updateOverride) {
		CommandButton result = (CommandButton) a.createComponent(CommandButton.COMPONENT_TYPE);

		result.setValue(title);
		result.setIcon(iconStyleClass);
		result.setTitle(tooltip);

		setSize(result, null, pixelWidth, null, null, pixelHeight, null, null);
		setInvisible(result, invisible, null);
		setDisabled(result, disabled, formDisabled);
		setConfirmation(result, confirmationText);
		setId(result, null);

		downloadActionExpression(downloadActionName, dataWidgetBinding, dataWidgetVar, result);
		
		result.setProcess((processOverride == null) ? process : processOverride); // process the current form (by default)
		result.setUpdate((updateOverride == null) ? update : updateOverride); // update all forms (by default)

		return result;
	}
	
	/**
	 * Add the buttons and overlay
	 *			    <p:commandButton id="s03" icon="fa fa-upload" title="Upload Content" type="button" onclick="$(PrimeFaces.escapeClientId('s06')).attr('src', '/skyve/contentUpload.xhtml')" />
	 *			    <p:overlayPanel id="s04" for="s03" hideEffect="fade" dynamic="true" showCloseIcon="true" modal="true" style="width:50%;height:300px">
	 *					<iframe id="s01_iframe" src="/skyve/contentUpload.xhtml" style="width:100%;height:280px;border:none"></iframe>
	 *			    </p:overlayPanel>
	 */
	private UIComponent uploadButton(String title,
										String iconStyleClass,
										String tooltip,
										String actionName,
										Integer pixelWidth, 
										Integer pixelHeight,
										@SuppressWarnings("unused") Boolean clientValidation, // TODO not implemented
										String confirmationText,
										String disabled,
										String formDisabled,
										String invisible) {
		// A span as the top item so it can floiw correctly in the action panel
		HtmlPanelGroup result = panelGroup(false, false, false, invisible, null);
		List<UIComponent> children = result.getChildren();
		
		// Refresh remote command that calls rerender when the overlay panel is closed
		RemoteCommand refresh = (RemoteCommand) a.createComponent(RemoteCommand.COMPONENT_TYPE);
		setId(refresh, null);
		String refreshId = refresh.getId();
		refresh.setName(refreshId);
		refresh.setActionExpression(methodExpressionForRerender(actionName, false));
		refresh.setProcess("@none");
		refresh.setUpdate(update);
		children.add(refresh);
		
		// Upload button that the overlay panel is attached to
		CommandButton uploadButton = (CommandButton) a.createComponent(CommandButton.COMPONENT_TYPE);
		setId(uploadButton, null);
		String uploadButtonId = uploadButton.getId();
		uploadButton.setValue(title);
		uploadButton.setIcon((iconStyleClass == null) ? "fa fa-upload" : iconStyleClass);
		uploadButton.setTitle(tooltip);
		uploadButton.setType("button");
		setSize(uploadButton, null, pixelWidth, null, null, pixelHeight, null, null);
		setDisabled(uploadButton, disabled, formDisabled);
		setConfirmation(uploadButton, confirmationText);
		children.add(uploadButton);
		
		// Overlay panel attached to the upload button that houses the iframe
		OverlayPanel overlay = (OverlayPanel) a.createComponent(OverlayPanel.COMPONENT_TYPE);
		setId(overlay, null);
		String overlayId = overlay.getId();
		overlay.setWidgetVar(overlayId + "Overlay");
		overlay.setFor(uploadButtonId);
		overlay.setHideEffect("fade");
		overlay.setDynamic(false);
		overlay.setShowCloseIcon(true);
		overlay.setModal(true);
		overlay.setStyle("width:50%;height:300px");
		// clear the iframe src on hide so there is no flash next open, and call the refresh remote command
		overlay.setOnHide(String.format("SKYVE.PF.contentOverlayOnHide('%s');%s()", overlayId, refreshId));

		// show the overlay, reset the fileUpload.xhtml iframe
		StringBuilder value = new StringBuilder(64);
		value.append("#{'SKYVE.PF.contentOverlayOnShow(\\'").append(overlayId).append("\\',\\''.concat(");
		value.append(managedBeanName).append(".getFileUploadUrl('").append(actionName).append("')).concat('\\')')}");
		overlay.setValueExpression("onShow", ef.createValueExpression(elc, value.toString(), String.class));

		children.add(overlay);
		
		// <iframe id="s01_iframe" src="" style="width:100%;height:280px;border:none"></iframe>
		HtmlOutputText iframe = (HtmlOutputText) a.createComponent(HtmlOutputText.COMPONENT_TYPE);
		iframe.setEscape(false);
		iframe.setValue(String.format("<iframe id=\"%s_iframe\" src=\"\" style=\"width:100%%;height:280px;border:none\"></iframe>", overlayId));
		setId(iframe, null);
		overlay.getChildren().add(iframe);

		return result;
	}
	
	protected CommandLink downloadLink(String title,
										String tooltip,
										String actionName,
										String dataWidgetBinding,
										String dataWidgetVar, 
										Integer pixelWidth, 
										String confirmationText,
										String disabled, 
										String formDisabled,
										String invisible,
										String processOverride,
										String updateOverride) {
		CommandLink result = (CommandLink) a.createComponent(CommandLink.COMPONENT_TYPE);

		result.setValue(title);
		result.setTitle(tooltip);

		setSize(result, null, pixelWidth, null, null, null, null, null);
		setDisabled(result, disabled, formDisabled);
		setInvisible(result, dataWidgetVar, invisible, null);
		setConfirmation(result, confirmationText);
		setId(result, null);

		downloadActionExpression(actionName, dataWidgetBinding, dataWidgetVar, result);
		
		result.setProcess((processOverride == null) ? process : processOverride); // process the current form (by default)
		result.setUpdate((updateOverride == null) ? update : updateOverride); // update all forms (by default)

		return result;
	}
	
	protected CommandLink actionLink(String title, 
										String tooltip, 
										ImplicitActionName implicitActionName,
										String actionName, 
										boolean inline, 
										String dataWidgetBinding,
										String dataWidgetVar, 
										Integer pixelWidth, 
										Integer pixelHeight,
										@SuppressWarnings("unused") Boolean clientValidation, 
										String confirmationText,
										String disabled, 
										String formDisabled,
										String invisible,
										String processOverride,
										String updateOverride) {
		CommandLink result = (CommandLink) a.createComponent(CommandLink.COMPONENT_TYPE);

		result.setValue(title);
		result.setTitle(tooltip);

		action(result, implicitActionName, actionName, dataWidgetBinding, dataWidgetVar, inline, null);

		setSize(result, null, pixelWidth, null, null, pixelHeight, null, null);
		setDisabled(result, disabled, formDisabled);
		setInvisible(result, dataWidgetVar, invisible, null);
		setConfirmation(result, confirmationText);
		setId(result, null);

		if (ImplicitActionName.Cancel.equals(implicitActionName) || ImplicitActionName.OK.equals(implicitActionName)) {
			result.setAjax(false);
		} 
		else {
			result.setProcess((processOverride == null) ? process : processOverride); // process the current form (by default)
			result.setUpdate((updateOverride == null) ? update : updateOverride); // update all forms (by default)
		}

		return result;
	}

	private void action(UICommand command, 
							ImplicitActionName implicitActionName, 
							String actionName,
							String collectionBinding,
							String dataWidgetVar, 
							boolean inline,
							List<EventAction> eventHandlerActions) {
		// Marshall the event actions into strings for the remove EL
		// NB rerender action represented as true/false for client validation true/false
		List<String> eventHandlerActionNames = null;
		if ((eventHandlerActions != null) && (! eventHandlerActions.isEmpty())) { 
			eventHandlerActionNames = new ArrayList<>(eventHandlerActions.size());
			for (EventAction eventAction : eventHandlerActions) {
				if (eventAction instanceof ServerSideActionEventAction) {
					eventHandlerActionNames.add(((ServerSideActionEventAction) eventAction).getActionName());
				}
				else if (eventAction instanceof RerenderEventAction) {
					if (Boolean.FALSE.equals(((RerenderEventAction) eventAction).getClientValidation())) {
						eventHandlerActionNames.add(Boolean.FALSE.toString());
					}
					else {
						eventHandlerActionNames.add(Boolean.TRUE.toString());
					}
				}
			}
		}
		command.setActionExpression(methodExpressionForAction(implicitActionName, actionName, collectionBinding, dataWidgetVar, inline, eventHandlerActionNames));
	}

	private Button linkButton(String icon, 
								String styleClass, 
								String style, 
								String value, 
								String title, 
								String href,
								Integer pixelWidth,
								Integer pixelHeight,
								String disabled, 
								String formDisabled,
								String invisible, 
								String target) {
		Button result = button(icon, styleClass, style);
		result.setValue(value);
		result.setTitle(title);
		result.setValueExpression("href", ef.createValueExpression(elc, href, String.class));
		result.setTarget(target);

		setId(result, null);
		setSize(result, null, pixelWidth, null, null, pixelHeight, null, null);
		setDisabled(result, disabled, formDisabled);
		setInvisible(result, invisible, null);

		return result;
	}

	private UIOutput outputText(String dataWidgetVar, String binding) {
		// escape bindings with ' as \' as the binding could be for blurb expressions
		String sanitisedBinding = ((binding.indexOf('\'') >= 0) ? binding.replace("'", "\\'") : binding);
		ValueExpression ve = createValueExpressionFromFragment(dataWidgetVar, true, sanitisedBinding, true, null, String.class);
		UIOutput result = new UIOutput();
		result.setValueExpression("value", ve);
		setId(result, null);
		return result;
	}

	@Override
	public UIComponent spacer(UIComponent component, org.skyve.impl.metadata.view.widget.Spacer spacer) {
		if (component != null) {
			return component;
		}

		Spacer result = (Spacer) a.createComponent(Spacer.COMPONENT_TYPE);
		setSize(result, null, spacer.getPixelWidth(), null, null, spacer.getPixelHeight(), null, null);
		setInvisible(result, spacer.getInvisibleConditionName(), null);
		setId(result, null);

		return result;
	}

	@Override
	public UIComponent staticImage(UIComponent component, String fileUrl, StaticImage image) {
		if (component != null) {
			return component;
		}

		GraphicImage result = (GraphicImage) a.createComponent(GraphicImage.COMPONENT_TYPE);
		result.setUrl(fileUrl);
		setSize(result, 
					null, 
					image.getPixelWidth(), 
					image.getResponsiveWidth(), 
					image.getPercentageWidth(), 
					image.getPixelHeight(), 
					image.getPercentageHeight(), 
					null);
		setInvisible(result, image.getInvisibleConditionName(), null);
		setId(result, null);
		return result;
	}

	@Override
	public UIComponent dynamicImage(UIComponent component, 
										DynamicImage image,
										String moduleName,
										String documentName) {
		if (component != null) {
			return component;
		}

		GraphicImage result = (GraphicImage) a.createComponent(GraphicImage.COMPONENT_TYPE);

		String name = image.getName();
		Integer pixelWidth = image.getPixelHeight();
		Integer pixelHeight = image.getPixelHeight();
		Integer initialPixelWidth = image.getImageInitialPixelWidth();
		Integer initialPixelHeight = image.getImageInitialPixelHeight();
		
		String expression = String.format("#{%s.getDynamicImageUrl('%s','%s','%s',%s,%s,%s,%s)}", 
											managedBeanName,
											name,
											moduleName,
											documentName,
											(pixelWidth == null) ? "null" : pixelWidth.toString(),
											(pixelHeight == null) ? "null" : pixelHeight.toString(),
											(initialPixelWidth == null) ? "null" : initialPixelWidth.toString(),
											(initialPixelHeight == null) ? "null" : initialPixelHeight.toString());
		result.setValueExpression("value", ef.createValueExpression(elc, expression.toString(), String.class));

		setSize(result, 
					"border:1px solid gray;", 
					pixelWidth, 
					image.getResponsiveWidth(), 
					image.getPercentageWidth(), 
					pixelHeight, 
					image.getPercentageHeight(),
					null);
		setInvisible(result, image.getInvisibleConditionName(), null);
		setId(result, null);
		return result;
	}
	
	// To enable a scaled image to keep its aspect ratio, construct something like
	// <div><img/></div> & set the size of the div to what is required
	// and set the image as width:100%;height:100%;object-fit:contain;
	private HtmlPanelGroup contentGraphicImage(Integer pixelWidth, 
												Integer responsiveWidth,
												Integer percentageWidth, 
												Integer pixelHeight,
												Integer percentageHeight, 
												String binding) {
		HtmlPanelGroup result = panelGroup(true, true, true, null, null);
		setId(result, null);
		setSize(result, "border:1px solid gray;", pixelWidth, responsiveWidth, percentageWidth, pixelHeight, percentageHeight, null);

		GraphicImage image = (GraphicImage) a.createComponent(GraphicImage.COMPONENT_TYPE);
		setId(image, null);
		String expression = String.format("#{%s.getContentUrl('%s', true)}", managedBeanName, binding);
		image.setValueExpression("value", ef.createValueExpression(elc, expression, String.class));
		image.setStyle("width:100%;height:100%;object-fit:contain;");
		result.getChildren().add(image);
		
		return result;
	}

	private HtmlOutputLink contentLink(Integer pixelWidth, String binding) {
		HtmlOutputLink result = (HtmlOutputLink) a.createComponent(HtmlOutputLink.COMPONENT_TYPE);
		
		String expression = String.format("#{%s.getContentUrl('%s', false)}", managedBeanName, binding);
		result.setValueExpression("value", ef.createValueExpression(elc, expression, String.class));

		expression = String.format("#{%s.getContentFileName('%s')}", managedBeanName, binding);
		UIOutput outputText = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
		outputText.setValueExpression("value", ef.createValueExpression(elc, expression, String.class));
		result.getChildren().add(outputText);
			
		result.setTarget("_blank");
		setSize(result, null, pixelWidth, null, null, null, null, null);
		setId(result, null);

		return result;
	}

	// TODO do the grids

	protected UIInput checkbox(String dataWidgetVar, 
								String binding, 
								String title, 
								boolean required,
								String disabled,
								String formDisabled,
								boolean triState) {
		if (triState) {
			TriStateCheckbox result = (TriStateCheckbox) input(TriStateCheckbox.COMPONENT_TYPE,
																dataWidgetVar, 
																binding, 
																title, 
																required, 
																disabled,
																formDisabled);
			result.setConverter(new TriStateCheckboxBooleanConverter());
			return result;
		}
		
		return input(SelectBooleanCheckbox.COMPONENT_TYPE, dataWidgetVar, binding, title, required, disabled, formDisabled);
	}

	protected ColorPicker colourPicker(String dataWidgetVar, 
										String binding, 
										String title, 
										boolean required,
										String disabled,
										String formDisabled,
										Integer pixelWidth, 
										boolean applyDefaultWidth) {
		ColorPicker result = (ColorPicker) input(ColorPicker.COMPONENT_TYPE, 
													dataWidgetVar, 
													binding, 
													title, 
													required,
													disabled,
													formDisabled);
		setSize(result, null, pixelWidth, null, null, null, null, applyDefaultWidth ? ONE_HUNDRED : null);
		return result;
	}

	private SelectOneMenu selectOneMenu(String dataWidgetVar, 
											String binding, 
											String title, 
											boolean required,
											String disabled, 
											String formDisabled,
											Integer pixelWidth) {
		SelectOneMenu result = (SelectOneMenu) input(SelectOneMenu.COMPONENT_TYPE, 
														dataWidgetVar, 
														binding, 
														title,
														required, 
														disabled,
														formDisabled);
		// Do not default pixel width to 100% as it causes renderering issues on the drop button on the end.
		// The control sets its width by default based on the font metrics of the drop-down values.
		setSize(result, null, pixelWidth, null, null, null, null, null);
		result.setConverter(new SelectItemsBeanConverter());
		return result;
	}

	private SelectOneRadio selectOneRadio(String dataWidgetVar, 
											String binding, 
											String title, 
											boolean required,
											String disabled,
											String formDisabled) {
		SelectOneRadio result = (SelectOneRadio) input(SelectOneRadio.COMPONENT_TYPE, 
														dataWidgetVar, 
														binding, 
														title,
														required, 
														disabled,
														formDisabled);
		result.setConverter(new SelectItemsBeanConverter());
		return result;
	}

	protected AutoComplete autoComplete(String dataWidgetVar, 
											String binding, 
											String title, 
											boolean required,
											String disabled,
											String formDisabled,
											String displayBinding, 
											QueryDefinition query, 
											List<FilterParameter> filterParameters,
											List<Parameter> parameters,
											Integer pixelWidth,
											boolean dontDisplay) {
		AutoComplete result = (AutoComplete) input(AutoComplete.COMPONENT_TYPE, 
													dataWidgetVar, 
													binding, 
													title, 
													required,
													disabled,
													formDisabled);
		result.setForceSelection(true);
		result.setDropdown(true);
		String var = BindUtil.sanitiseBinding(binding) + "Row";
		result.setVar(var);
		StringBuilder expression = new StringBuilder(32);
		result.setValueExpression("itemLabel",
									createValueExpressionFromFragment(var, false, displayBinding, true, null, String.class));
		result.setValueExpression("itemValue",
									createValueExpressionFromFragment(null, false, var, false, null, BeanMapAdapter.class));
		result.setConverter(new AssociationAutoCompleteConverter());
		result.setScrollHeight(200);

		expression.setLength(0);
		expression.append("#{").append(managedBeanName).append(".complete}");
		result.setCompleteMethod(ef.createMethodExpression(elc, 
															expression.toString(), 
															List.class, 
															new Class[] {String.class}));

		Map<String, Object> attributes = result.getAttributes();
		attributes.put("module", query.getOwningModule().getName());
		attributes.put("query", query.getName());
		attributes.put("display", displayBinding);
		attributes.put("filterParameters", filterParameters);
		attributes.put("parameters", parameters);

		setSize(result, 
					dontDisplay ? "display:none" : null, 
					pixelWidth, 
					null,
					null, 
					null, 
					null,
					// width cannot be set correctly on this component when laid out in a table
					null); // applyDefaultWidth ? ONE_HUNDRED : null); 

		return result;
	}

	protected AutoComplete previousValues(String dataWidgetVar, 
											String binding, 
											String title, 
											boolean required,
											String disabled,
											Integer length,
											String formDisabled,
											Integer pixelWidth) {
		AutoComplete result = (AutoComplete) input(AutoComplete.COMPONENT_TYPE, 
													dataWidgetVar, 
													binding, 
													title, 
													required,
													disabled,
													formDisabled);
		result.setForceSelection(false);
		result.setDropdown(false);
		if (length != null) {
			result.setMaxlength(length.intValue());
		}
		result.setScrollHeight(200);

		StringBuilder expression = new StringBuilder(32);
		expression.append("#{").append(managedBeanName).append(".previousValues}");
		result.setCompleteMethod(ef.createMethodExpression(elc, 
															expression.toString(), 
															List.class, 
															new Class[] {String.class}));

		Map<String, Object> attributes = result.getAttributes();
		attributes.put("binding", binding);

		// NB width cannot be set correctly on this component when laid out in a table
		setSize(result, null, pixelWidth, null, null, null, null, null);

		return result;
	}

	protected Button button(String icon, String styleClass, String style) {
		Button result = (Button) a.createComponent(Button.COMPONENT_TYPE);
		if (icon != null) {
			result.setIcon(icon);
		}
		if (styleClass != null) {
			result.setStyleClass(styleClass);
		}
		if (style != null) {
			result.setStyle(style);
		}
		setId(result, null);

		return result;
	}

	// this has a customisable toolbar for rich and html skyve editors.
	private Editor editor(String dataWidgetVar,
							String binding,
							String title,
							boolean required,
							String disabled,
							String formDisabled) {
		return (Editor) input(Editor.COMPONENT_TYPE, dataWidgetVar, binding, title, required, disabled, formDisabled);
	}

	private DataTable dataTable(String binding, 
									String dataWidgetVar,
									String title, 
									String invisible, 
									boolean clickToZoom,
									String[] clickToZoomDisabledConditionNames,
									String selectedIdBinding,
									List<EventAction> selectedActions,
									boolean ordered,
									String widgetId) {
		DataTable result = (DataTable) a.createComponent(DataTable.COMPONENT_TYPE);
		setId(result, widgetId);
		setInvisible(result, invisible, null);
		addGridHeader(title, result);

		result.setVar(dataWidgetVar);
		result.setValueExpression("value", createValueExpressionFromFragment(binding, true, null, List.class));

		if (selectedIdBinding != null) {
			addDataTableSelection(result, selectedIdBinding, selectedActions, binding);
		}
		else if (clickToZoom) {
			String id = result.getId();
			result.setWidgetVar(id);
			result.setSelectionMode("single");
			result.setValueExpression("rowKey",
										createValueExpressionFromFragment(dataWidgetVar,
																			false,
																			Bean.DOCUMENT_ID, 
																			true, 
																			null, 
																			String.class));

			AjaxBehavior ajax = (AjaxBehavior) a.createBehavior(AjaxBehavior.BEHAVIOR_ID);
			StringBuilder expression = new StringBuilder(64);
			expression.append("#{").append(managedBeanName).append('.');
			expression.append(ImplicitActionName.Navigate.name().toLowerCase()).append('}');
			MethodExpression me = ef.createMethodExpression(elc, expression.toString(), null, new Class[0]);
			ajax.addAjaxBehaviorListener(new AjaxBehaviorListenerImpl(me, me));

			ValueExpression disabled = createOredValueExpressionFromConditions(clickToZoomDisabledConditionNames);
			if (disabled != null) {
				ajax.setValueExpression("disabled", disabled);
			}
			result.addClientBehavior("rowSelect", ajax);
		}
		if (ordered) {
            result.setDraggableRows(true);
            result.getAttributes().put(COLLECTION_BINDING_ATTRIBUTE_KEY, binding);

            final AjaxBehavior ajax = (AjaxBehavior) a.createBehavior(AjaxBehavior.BEHAVIOR_ID);
            final MethodExpression me = ef.createMethodExpression(elc, String.format("#{%s.onRowReorder}", managedBeanName), null, new Class[0]);
            ajax.addAjaxBehaviorListener(new AjaxBehaviorListenerImpl(me, me));
            result.addClientBehavior("rowReorder", ajax);

            final Column dragHandleColumn = (Column) a.createComponent(Column.COMPONENT_TYPE);
            setId(dragHandleColumn, null);
            dragHandleColumn.setWidth("10");
            dragHandleColumn.setPriority(1);

            final HtmlPanelGroup dragHandle = (HtmlPanelGroup) a.createComponent(HtmlPanelGroup.COMPONENT_TYPE);
			dragHandle.setStyleClass("fa fa-sort");
            dragHandleColumn.getChildren().add(dragHandle);

            result.getChildren().add(dragHandleColumn);
		}

		return result;
	}

	protected DataList dataList(String binding, String dataWidgetVar, String title, String invisible, String widgetId) {
		DataList result = (DataList) a.createComponent(DataList.COMPONENT_TYPE);
		setId(result, widgetId);
		setInvisible(result, invisible, null);
		addGridHeader(title, result);

		result.setVar(dataWidgetVar);
		result.setValueExpression("value", createValueExpressionFromFragment(binding, true, null, List.class));

		return result;
	}
	
	private void addGridHeader(String title, 
								UIComponent dataTableOrList) {
		if (title != null) {
			UIOutput text = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
			text.setValue(title);
			setId(text, null);
			dataTableOrList.getFacets().put("header", text);
		}
	}

	protected AccordionPanel accordionPanel(String invisible, String widgetId) {
		AccordionPanel result = (AccordionPanel) a.createComponent(AccordionPanel.COMPONENT_TYPE);
		setId(result, widgetId);
		setInvisible(result, invisible, null);
		return result;
	}

	private Column column(String dataWidgetVar, 
							String sortBinding, 
							String title, 
							HorizontalAlignment alignment,
							boolean noWrap, 
							Integer pixelWidth) {
		Column result = (Column) a.createComponent(Column.COMPONENT_TYPE);
		setId(result, null);

		result.setHeaderText(title);
		if (sortBinding != null) {
			result.setValueExpression("sortBy",
										createValueExpressionFromFragment(dataWidgetVar, true, sortBinding, true, null, Object.class));
		}

		StringBuilder style = new StringBuilder(64);
		if (pixelWidth != null) {
			style.append("width:").append(pixelWidth).append("px;");
		}
		if (noWrap) {
			style.append("white-space:nowrap;");
		}
		if ((alignment != null) && (! HorizontalAlignment.left.equals(alignment))) {
			style.append("text-align:").append(HorizontalAlignment.centre.equals(alignment) ? "center" : "right").append(" !important;");
		}
		if (style.length() > 0) {
			result.setStyle(style.toString());
		}

		return result;
	}

	private UISelectItems selectItems(String moduleName,
										String documentName,
										String dataWidgetVar,
										String binding,
										boolean includeEmptyItems) {
		UISelectItems result = (UISelectItems) a.createComponent(UISelectItems.COMPONENT_TYPE);
		setId(result, null);
		String expression = null;
		ValueExpression valueExpression = null;
		if ((moduleName != null) && (documentName != null)) { // module and document, use FacesView.getSelectItems()
			expression = String.format("getSelectItems('%s','%s','%s',%s)", 
										moduleName,
										documentName,
										binding,
										String.valueOf(includeEmptyItems));
			valueExpression = createValueExpressionFromFragment(managedBeanName, false, expression, false, null, List.class);
		}
		else { // use the FacesView.currentBean.getSelectItems()
			expression = String.format("getSelectItems('%s',%s)", binding, String.valueOf(includeEmptyItems));
			if (dataWidgetVar != null) {
				valueExpression = createValueExpressionFromFragment(dataWidgetVar, true, expression, false, null, List.class);
			}
			else {
				valueExpression = createValueExpressionFromFragment(expression, false, null, List.class);
			}
		}
		result.setValueExpression("value", valueExpression);

		return result;
	}

	private UIInput input(String componentType, 
							String dataWidgetVar, 
							String binding,
							String title, 
							boolean required,
							String disabled,
							String formDisabled) {
		UIInput result = (UIInput) a.createComponent(componentType);
		setId(result, null);
		if (binding != null) { // data table filter components don't set a binding
			if (dataWidgetVar != null) {
				result.setValueExpression("value",
											createValueExpressionFromFragment(dataWidgetVar, true, binding, true, null, Object.class));
			}
			else {
				result.setValueExpression("value", createValueExpressionFromFragment(binding, true, null, Object.class));
			}
		}
		if (title != null) {
			result.setValueExpression("title",
										ef.createValueExpression(elc, required ? title + " *" : title, String.class));
		}
		
		// Cannot utilise the faces required attributes as some requests need to ignore required-ness.
		// eg - triggered actions on widget events.
		// Setting required attribute to an expression worked server-side but the client-side message integration didn't.
		// result.setValueExpression("required", ef.createValueExpression(required ? "true" : "false", Boolean.class));
		// So we use the requiredMessage to perform the check ourselves based on clientValidation attribute
		if (required) {
			result.setRequiredMessage(title + " is required");
		}
		setDisabled(result, disabled, formDisabled);
		return result;
	}

	private void setConfirmation(UIComponentBase component, String confirmationText) {
		if (confirmationText != null) {
			ConfirmBehavior confirm = (ConfirmBehavior) a.createBehavior(ConfirmBehavior.BEHAVIOR_ID);
			confirm.setMessage(confirmationText);
			confirm.setEscape(false);
			component.addClientBehavior("click", confirm);
		}
	}

	protected void setValueOrValueExpression(String value, Consumer<String> valueSetter, String valueExpressionName, UIComponent component) {
		if (value != null && value.indexOf('{') > -1) {
			final String sanitisedBinding = ((value.indexOf('\'') >= 0) ? value.replace("'", "\\'") : value);
			final ValueExpression ve = createValueExpressionFromFragment(sanitisedBinding, true, null, String.class);
			component.setValueExpression(valueExpressionName, ve);
		} else if (value != null) {
			valueSetter.accept(value);
		}
	}
	
/*
	private HtmlForm form() {
		HtmlForm result = (HtmlForm) a.createComponent(HtmlForm.COMPONENT_TYPE);
		setId(result);

		return result;
	}

	private Fieldset fieldset(String legend, String invisible) {
		Fieldset result = (Fieldset) a.createComponent(Fieldset.COMPONENT_TYPE);
		if (legend != null) {
			result.setLegend(legend);
		}
		setInvisible(result, invisible, null);
		setId(result);
		return result;
	}
	
	private UIParameter parameter(String name, Object value) {
		UIParameter result = (UIParameter) a.createComponent(UIParameter.COMPONENT_TYPE);
		result.setName(name);
		result.setValue(value);
		setId(result);
		return result;
	}
	
	private ProgressBar progressBar() {
		ProgressBar result = (ProgressBar) a.createComponent(ProgressBar.COMPONENT_TYPE);
		setId(result);
		return result;
	}
	
	private TriStateCheckbox triStateCheckbox(String bindingPrefix, 
												String binding, 
												String title, 
												boolean required,
												String disabled) {
		return (TriStateCheckbox) input(TriStateCheckbox.COMPONENT_TYPE, 
											bindingPrefix, 
											binding, 
											title, 
											required,
											disabled);
	}

	private SelectManyCheckbox manyCheckbox(String bindingPrefix, 
												String binding, 
												String title, 
												boolean required,
												String disabled) {
		return (SelectManyCheckbox) input(SelectManyCheckbox.COMPONENT_TYPE, 
											bindingPrefix, 
											binding, 
											title, 
											required,
											disabled);
	}
	
	private FileUpload fileUpload(String bindingPrefix, 
									String binding, 
									String title, 
									boolean required,
									String disabled) {
		return (FileUpload) input(FileUpload.COMPONENT_TYPE, bindingPrefix, binding, title, required, disabled);
	}
*/
	/**
	 * <h:link outcome="reviewBatch" value="Restart" rendered=#{batch.renderRestart}">
	 *     <f:param name="c" value=#{batch.row.batchHeader.identifier.clientId}" />
	 *     <f:param name="b" value="#{batch.row.batchHeader.identifier.batchNumber}" />
	 * </h:link>
	 */
/*
	private HtmlOutputLink outputLink(String value, String outcome, String disabled, String invisible) {
		HtmlOutputLink result = (HtmlOutputLink) a.createComponent(HtmlOutputLink.COMPONENT_TYPE);
		result.setValue(value);
		setDisabled(result, disabled);
		setInvisible(result, invisible, null);
		setId(result);
		return result;
	}
*/	
}
