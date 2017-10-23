package org.skyve.impl.web.faces.pipeline.component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.el.MethodExpression;
import javax.el.ValueExpression;
import javax.faces.component.UICommand;
import javax.faces.component.UIComponent;
import javax.faces.component.UIComponentBase;
import javax.faces.component.UIInput;
import javax.faces.component.UIOutput;
import javax.faces.component.UISelectItems;
import javax.faces.component.html.HtmlOutputLabel;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.component.html.HtmlPanelGroup;
import javax.faces.component.html.HtmlSelectOneMenu;
import javax.faces.convert.Converter;

import org.primefaces.behavior.ajax.AjaxBehavior;
import org.primefaces.behavior.ajax.AjaxBehaviorListenerImpl;
import org.primefaces.behavior.confirm.ConfirmBehavior;
import org.primefaces.component.accordionpanel.AccordionPanel;
import org.primefaces.component.autocomplete.AutoComplete;
import org.primefaces.component.button.Button;
import org.primefaces.component.calendar.Calendar;
import org.primefaces.component.colorpicker.ColorPicker;
import org.primefaces.component.column.Column;
import org.primefaces.component.commandbutton.CommandButton;
import org.primefaces.component.commandlink.CommandLink;
import org.primefaces.component.datalist.DataList;
import org.primefaces.component.datatable.DataTable;
import org.primefaces.component.editor.Editor;
import org.primefaces.component.graphicimage.GraphicImage;
import org.primefaces.component.inputmask.InputMask;
import org.primefaces.component.inputtext.InputText;
import org.primefaces.component.inputtextarea.InputTextarea;
import org.primefaces.component.outputlabel.OutputLabel;
import org.primefaces.component.panel.Panel;
import org.primefaces.component.password.Password;
import org.primefaces.component.picklist.PickList;
import org.primefaces.component.selectbooleancheckbox.SelectBooleanCheckbox;
import org.primefaces.component.selectonemenu.SelectOneMenu;
import org.primefaces.component.selectoneradio.SelectOneRadio;
import org.primefaces.component.spacer.Spacer;
import org.primefaces.component.spinner.Spinner;
import org.primefaces.component.tabview.Tab;
import org.primefaces.component.tabview.TabView;
import org.primefaces.component.toolbar.Toolbar;
import org.primefaces.model.DualListModel;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Format.TextCase;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
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
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridColumn;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.faces.converters.select.AssociationAutoCompleteConverter;
import org.skyve.impl.web.faces.converters.select.SelectItemsBeanConverter;
import org.skyve.impl.web.faces.models.BeanMapAdapter;
import org.skyve.impl.web.faces.models.SkyveLazyDataModel;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.QueryColumn;
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

	@Override
	public HtmlPanelGroup view(String invisibleConditionName) {
		return panelGroup(true, false, false, invisibleConditionName, null);
	}

	@Override
	public List<UIComponent> toolbars(String widgetId) {
		Toolbar toolbar = (Toolbar) a.createComponent(Toolbar.COMPONENT_TYPE);
		setId(toolbar, widgetId);
		toolbar.setStyle("width:100%");
		
		List<UIComponent> result = new ArrayList<>(1);
		result.add(toolbar);
		return result;
	}

	@Override
	public UIComponent tabPane(TabPane tabPane) {
		return tabView(tabPane.getInvisibleConditionName(), 
						tabPane.getSelectedTabIndexBinding(),
						tabPane.getWidgetId());
	}
	
	@Override
	public UIComponent tab(org.skyve.impl.metadata.view.container.Tab tab) {
		Tab result = (Tab) a.createComponent(Tab.COMPONENT_TYPE);
		result.setTitle(tab.getTitle());
		setDisabled(result, tab.getDisabledConditionName());
		setInvisible(result, tab.getInvisibleConditionName(), null);
		setId(result, null);
		return result;
	}

	@Override
	public UIComponent border(String borderTitle, String invisibleConditionName, Integer pixelWidth) {
		return panel(borderTitle, invisibleConditionName, pixelWidth, null);
	}
	
	@Override
	public UIComponent label(String value) {
		OutputLabel result = (OutputLabel) a.createComponent(OutputLabel.COMPONENT_TYPE);
		setId(result, null);
		result.setValue(value);
		return result;
	}
	
	@Override
	public UIComponent actionButton(String listBinding, 
										String listVar,
										org.skyve.impl.metadata.view.widget.Button button, 
										Action action) {
		return actionButton(action.getDisplayName(),
								action.getIconStyleClass(),
				                action.getToolTip(),
				                action.getImplicitName(),
				                action.getName(),
				                false,
				                listBinding,
				                listVar,
				                button.getPixelWidth(),
				                button.getPixelHeight(),
				                action.getClientValidation(),
				                action.getConfirmationText(),
				                action.getDisabledConditionName(),
				                action.getInvisibleConditionName());
	}
	
	@Override
	public UIComponent reportButton(org.skyve.impl.metadata.view.widget.Button button, 
										Action action) {
		return reportButton(action.getDisplayName(), 
								action.getIconStyleClass(),
								action.getToolTip(), 
								action.getParameters(), 
								button.getPixelWidth(),
								button.getPixelHeight(),
								action.getClientValidation(),
								action.getConfirmationText(),
								action.getDisabledConditionName(), 
								action.getInvisibleConditionName());
	}

	@Override
	public UIComponent downloadButton(org.skyve.impl.metadata.view.widget.Button button, 
										Action action,
										String moduleName, 
										String documentName) {
		return downloadButton(action.getDisplayName(),
								action.getIconStyleClass(),
								action.getToolTip(),
								action.getName(),
								moduleName, 
								documentName, 
								button.getPixelWidth(),
								button.getPixelHeight(),
								action.getClientValidation(),
								action.getConfirmationText(),
								action.getDisabledConditionName(), 
								action.getInvisibleConditionName());
	}
	
	@Override
	public UIComponent blurb(String listVar, String value, String binding, Blurb blurb) {
		HtmlOutputText result = (HtmlOutputText) a.createComponent(HtmlOutputText.COMPONENT_TYPE);
		setId(result, null);
		if (value != null) {
			result.setValue(value);
		} 
		else {
			// escape bindings with ' as \' as the binding could be for blurb expressions
			String sanitisedBinding = ((binding.indexOf('\'') >= 0) ? binding.replace("'", "\\'") : binding);
			if (listVar != null) {
				result.setValueExpression("value", createValueExpressionFromFragment(listVar, true, sanitisedBinding, true, null, Object.class));
			}
			else {
				result.setValueExpression("value", createValueExpressionFromFragment(sanitisedBinding, true, null, Object.class));
			}
		}
		result.setEscape(false);

		HorizontalAlignment textAlignment = blurb.getTextAlignment();
		String style = null;
		if (HorizontalAlignment.left.equals(textAlignment)) {
			style = "text-align:left;";
		} 
		else if (HorizontalAlignment.centre.equals(textAlignment)) {
			style = "text-align:center;";
		} 
		else if (HorizontalAlignment.right.equals(textAlignment)) {
			style = "text-align:right;";
		}

		setSize(result, style, blurb.getPixelWidth(), null, null, blurb.getPixelHeight(), null, null);
		setInvisible(result, blurb.getInvisibleConditionName(), null);

		return result;
	}
	
	@Override
	public UIComponent label(String listVar, String value, String binding, Label label) {
		HtmlOutputLabel result = (HtmlOutputLabel) a.createComponent(HtmlOutputLabel.COMPONENT_TYPE);
		setId(result, null);
		if (value != null) {
			result.setValue(value);
		} 
		else {
			// escape bindings with ' as \' as the binding could be for blurb expressions
			String sanitisedBinding = ((binding.indexOf('\'') >= 0) ? binding.replace("'", "\\'") : binding);
			result.setValueExpression("value", createValueExpressionFromFragment(listVar, true, sanitisedBinding, true, null, Object.class));
		}

		return result;
	}

	private int columnPriority;
	
	@Override
	public UIComponent dataGrid(String listVar, DataGrid grid) {
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

		return dataTable(grid.getBinding(),
							listVar,
							grid.getTitle(),
							grid.getInvisibleConditionName(),
							((! Boolean.TRUE.equals(grid.getInline())) && 
								(! Boolean.FALSE.equals(grid.getShowZoom())) &&
								(! Boolean.FALSE.equals(grid.getEditable()))),
							clickToZoomDisabledConditionNames,
							grid.getSelectedIdBinding(),
							grid.getSelectedActions(),
							grid.getWidgetId());
	}
	
	@Override
	public UIComponent addDataGridBoundColumn(UIComponent current, 
												DataGrid grid,
												DataGridColumn column,
												String listVar,
												String columnTitle,
												String columnBinding,
												StringBuilder gridColumnExpression) {
		Column result = column(listVar,
								(columnBinding == null) ? Bean.BIZ_KEY : columnBinding,
								columnTitle,
	                            column.getAlignment(),
	                            false,
	                            column.getPixelWidth());
		result.setPriority(columnPriority);
		if (columnPriority < 6) {
			columnPriority++;
		}
		current.getChildren().add(result);

		if (! Boolean.TRUE.equals(grid.getInline())) {
	        gridColumnExpression.setLength(0);
	        gridColumnExpression.append('{').append(columnBinding).append('}');
	        result.getChildren().add(outputText(listVar, gridColumnExpression.toString()));
		}
		
		return result;
	}
	
	@Override
	public UIComponent addedDataGridBoundColumn(UIComponent current) {
		return current.getParent(); // move from column to table
	}

	@Override
	public UIComponent addDataGridContainerColumn(UIComponent current, DataGrid grid, DataGridColumn column) {
		Column col = column(grid.getBinding(),
								null,
								column.getTitle(),
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
	public UIComponent addedDataGridContainerColumn(UIComponent current) {
		return current.getParent(); // move from column to table
	}
	
	@Override
	public UIComponent addDataGridActionColumn(UIComponent current, 
												DataGrid grid, 
												String listVar,
												String gridColumnExpression,
												String singularDocumentAlias,
												boolean inline) {
		// only add a column if grid is editable
		if (! Boolean.FALSE.equals(grid.getEditable())) {
			String listBinding = grid.getBinding();
			
			Column col = column(null,
									null,
									"",
					                HorizontalAlignment.centre,
					                true,
					                Integer.valueOf(45));
			col.setPriority(1);
			List<UIComponent> children = col.getChildren();

			String disabledConditionName = grid.getDisabledConditionName();

			if (! Boolean.FALSE.equals(grid.getShowAdd())) {
				CommandButton button = (CommandButton) a.createComponent(CommandButton.COMPONENT_TYPE);
		    	button.setValue(null);
	        	button.setTitle("Add a new " + singularDocumentAlias);
		    	button.setIcon("fa fa-plus");
				action(button, ImplicitActionName.Add, null, listBinding, listVar, inline, null);
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
				col.getFacets().put("header", button);
			}
			
			if (! Boolean.FALSE.equals(grid.getShowZoom())) {
				CommandButton button = (CommandButton) a.createComponent(CommandButton.COMPONENT_TYPE);
		    	button.setValue(null);
	        	button.setTitle("Edit this " + singularDocumentAlias);
		    	button.setIcon("fa fa-chevron-right");
				action(button, ImplicitActionName.Navigate, null, listBinding, listVar, inline, null);
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
				children.add(button);
			}

			if (! Boolean.FALSE.equals(grid.getShowRemove())) {
				// Conditionally add some whitespace between buttons
				if (! col.getChildren().isEmpty()) {
					children.add(label(" "));
				}
	
				CommandButton button = (CommandButton) a.createComponent(CommandButton.COMPONENT_TYPE);
		    	button.setValue(null);
	        	button.setTitle("Remove this " + singularDocumentAlias);
		    	button.setIcon("fa fa-minus");

		    	// If the remove button is vanilla, no events, just update the table as we know there'll
		    	// be no side effects, but if there is an action on it, update the forms to render
		    	// any side effects
		    	List<EventAction> removedActions = grid.getRemovedActions();
		    	if ((removedActions != null) && (! removedActions.isEmpty())) {
					button.setUpdate(update); // update all forms (by default)
		    	}
		    	else {
		    		button.setUpdate("@namingcontainer"); // update the data table - the closest naming container
		    	}

		    	action(button, ImplicitActionName.Remove, null, listBinding, listVar, true, removedActions);
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
				children.add(button);
			}

			if (! children.isEmpty()) {
				if (children.size() > 1) {
					col.setStyle("width:90px;text-align:center !important");
				}
				current.getChildren().add(col);
			}
		}
		
		return current;
	}

	/*
		<p:dataTable id="list"
						var="row"
						value="#{skyve.getBeans(skyve.bizModuleParameter, skyve.queryNameParameter)}">
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
	public UIComponent listGrid(String modelDocumentName,
									String modelName,
									ListModel<? extends Bean> model,
									List<FilterParameter> filterParameters,
									boolean canCreateDocument,
									boolean createRendered,
									String[] createDisabledConditionNames,
									boolean zoomRendered,
									String zoomDisabledConditionName,
									String selectedIdBinding,
									List<EventAction> selectedActions,
									boolean showPaginator,
									boolean stickyHeader) {
		Document drivingDocument =  model.getDrivingDocument();
		String moduleName = drivingDocument.getOwningModuleName();
		String drivingDocumentName = drivingDocument.getName();

		DataTable result = (DataTable) a.createComponent(DataTable.COMPONENT_TYPE);
        result.setVar("row");
        result.setPaginator(showPaginator);
        if (showPaginator) {
	        result.setRowsPerPageTemplate("25,50,75,100");
	        result.setRows(50);
	        result.setPaginatorAlwaysVisible(false);
        }
        result.setLazy(true);
        result.setEmptyMessage("No Items to show");
        result.setStickyHeader(stickyHeader);
        
        setId(result, null);
    	result.setWidgetVar(result.getId());
    	
    	if (selectedIdBinding != null) {
    		addDataTableSelection(result, selectedIdBinding, selectedActions, modelName);
    	}
    	else if (zoomRendered) {
    		if (zoomDisabledConditionName == null) {
	    		result.setSelectionMode("single"); 
    		}
    		else {
	    		result.setValueExpression("selectionMode", 
	    									ef.createValueExpression(elc, String.format("#{(%s) ? '' : 'single'}", 
	    																					createOredValueExpressionFragmentFromConditions(new String[] {zoomDisabledConditionName})),
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
    	
        // Write out getModel call as the value
        StringBuilder value = new StringBuilder(64);
		value.append("#{").append(managedBeanName).append(".getModel('").append(moduleName).append("','");
		if (model instanceof DocumentQueryListModel) {
			value.append(drivingDocumentName).append("','").append(modelName).append("',null,");
		}
		else {
			value.append(modelDocumentName).append("',null,'").append(modelName).append("',");
		}

		// Add filter parameters to getModel call
		if ((filterParameters != null) && (! filterParameters.isEmpty())) {
			value.append('[');
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
			value.setLength(value.length() - 1); // remove last comma
			value.append("])}");
		}
		else {
			value.append("null)}");
		}
		
		result.setValueExpression("value", ef.createValueExpression(elc, value.toString(), SkyveLazyDataModel.class));

        addListGridHeader(model, result);
        List<UIComponent> children = result.getChildren();
        addListGridDataColumns(model, children);
        if ((canCreateDocument && createRendered) || zoomRendered) {
        	addListGridActionColumn(moduleName, 
        								drivingDocumentName, 
        								canCreateDocument,
        								createRendered, 
        								createDisabledConditionNames, 
        								zoomRendered,
        								zoomDisabledConditionName,
        								children);
        }
    	
    	return result;
	}
	
	private void addDataTableSelection(DataTable table, 
										String selectedIdBinding, 
										List<EventAction> selectedActions,
										String source) {
		table.setSelectionMode("single"); 
		table.setValueExpression("rowKey", ef.createValueExpression(elc, String.format("#{%s['bizId']}", table.getVar()), String.class));
		Map<String, Object> attributes = table.getAttributes();
		attributes.put("selectedIdBinding", selectedIdBinding);

        AjaxBehavior ajax = (AjaxBehavior) a.createBehavior(AjaxBehavior.BEHAVIOR_ID);
		if (selectedActions != null) {
			String actionName = determineActionName(selectedActions);
			if (actionName == null) { // when no selected action defined (collection is empty) 
				ajax.setProcess("@this");
	        	ajax.setUpdate("@none");
			}
			else {
				attributes.put("actionName", actionName);
				if (Boolean.TRUE.toString().equals(actionName) || Boolean.FALSE.toString().equals(actionName)) {
					attributes.put("source", source);
				}
				ajax.setProcess(process);
				ajax.setUpdate(update);
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
	
	private void addListGridHeader(ListModel<? extends Bean> model,
									UIComponent componentToAddTo) {
		UIOutput heading = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
        heading.setValue(model.getDescription());
		componentToAddTo.getFacets().put("header", heading);
	}
	
	private void addListGridDataColumns(ListModel<? extends Bean> model,
											List<UIComponent> componentChildrenToAddTo) {
		Customer customer = CORE.getUser().getCustomer();
		Document document = model.getDrivingDocument();
		Module module = customer.getModule(document.getOwningModuleName());
		
		columnPriority = 1;

		for (QueryColumn queryColumn : model.getColumns()) {
			if (queryColumn.isHidden() || (! queryColumn.isProjected())) {
				continue;
			}
			
			String name = queryColumn.getName();
			String binding = queryColumn.getBinding();

			// Sort out a display name
			String displayName = queryColumn.getDisplayName();
			if (displayName == null) {
				if (binding != null) {
					TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
					Document bindingDocument = target.getDocument();
					Attribute bindingAttribute = target.getAttribute();
					if (binding.endsWith(Bean.BIZ_KEY)) {
						if (bindingDocument != null) {
							displayName = bindingDocument.getSingularAlias();
						}
						else {
							displayName = DocumentImpl.getBizKeyAttribute().getDisplayName();
						}
					}
					else if (binding.endsWith(Bean.ORDINAL_NAME)) {
						displayName = DocumentImpl.getBizOrdinalAttribute().getDisplayName();
					}
					else if (bindingAttribute != null) {
						displayName = bindingAttribute.getDisplayName();
					}
				}
			}
			
			// Create the column
			Column column = (Column) a.createComponent(Column.COMPONENT_TYPE);
			column.setHeaderText(displayName);
			column.setPriority(columnPriority);
			if (columnPriority < 6) {
				columnPriority++;
			}
				
/* TODO complete this
			column.setSortBy(queryColumn.getBinding());
			column.setFilterBy(queryColumn.getBinding());
*/
			// Add the EL expression
			String value = String.format("#{row['{%s}']}", (name != null) ? name : binding);
			UIOutput outputText = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
			outputText.setValueExpression("value", ef.createValueExpression(elc, value, Object.class));
			column.getChildren().add(outputText);
			componentChildrenToAddTo.add(column);
		}
	}

	private void addListGridActionColumn(String moduleName,
											String documentName,
											boolean canCreateDocument,
											boolean createRendered,
											String[] createDisabledConditionNames,
											boolean zoomRendered,
											String zoomDisabledConditionName,
											List<UIComponent> componentChildrenToAddTo) {
		Column column = (Column) a.createComponent(Column.COMPONENT_TYPE);
		column.setPriority(1);
		column.setWidth("40");
		column.setStyle("text-align:center !important");
        if (canCreateDocument && createRendered) {
	    	Button button = (Button) a.createComponent(Button.COMPONENT_TYPE);
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
        	button.setHref(value.toString());

	        column.getFacets().put("header", button);
        }
        else {
    		column.setHeaderText("");
        }
        if (zoomRendered) {
	    	Button button = (Button) a.createComponent(Button.COMPONENT_TYPE);
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
			column.getChildren().add(button);
        }
		componentChildrenToAddTo.add(column);
	}
	
	@Override
	public UIComponent listMembership(ListMembership membership) {
		PickList result = (PickList) a.createComponent(PickList.COMPONENT_TYPE);
		result.setVar("item");
		result.setShowSourceControls(false);
		result.setShowTargetControls(false);
		result.setShowSourceFilter(false);
		result.setShowTargetFilter(false);
		
        StringBuilder value = new StringBuilder(128);
        /*
        value.append("#{").append(managedBeanName).append(".getListMembershipModel('");
        value.append(membership.getBinding()).append("')}");
        result.setValueExpression("value", ef.createValueExpression(elc, value.toString(), DomainValueDualListModel.class));
		*/
        value.append("#{").append(managedBeanName).append(".listMembershipModel");
        value.append("}");
        result.setValueExpression("value", ef.createValueExpression(elc, value.toString(), DualListModel.class));

        result.setVar("item");
        result.setValueExpression("itemValue", ef.createValueExpression(elc, "#{item.code}", String.class));
        result.setValueExpression("itemLabel", ef.createValueExpression(elc, "#{item.description}", String.class));
        
        // TODO add a converter for domain values - can I use select converter shit?
        
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
		return result;
	}

	@Override
	public UIComponent checkBox(String listVar, CheckBox checkBox, String title, boolean required) {
/* TODO Why don't tri state checkboxes work???
		UIComponentBase c = Boolean.FALSE.equals(checkBox.getTriState()) ?
								b.checkbox(listVar,
										checkBox.getBinding(), 
										def.getTitle(),
										def.isRequired(),
										checkBox.getDisabledConditionName()) :
								b.triStateCheckbox(listVar,
													checkBox.getBinding(), 
													def.getTitle(),
													def.isRequired(),
													checkBox.getDisabledConditionName());
*/
		return checkbox(listVar,
							checkBox.getBinding(), 
							title,
							required,
							checkBox.getDisabledConditionName());
	}
	
	@Override
	public UIComponent colourPicker(String listVar, ColourPicker colour, String title, boolean required) {
		return colourPicker(listVar, 
								colour.getBinding(), 
								title, 
								required, 
								colour.getPixelWidth(),
								true);
	}
	
	@Override
	public UIComponent combo(String listVar, Combo combo, String title, boolean required) {
		String binding = combo.getBinding();
		HtmlSelectOneMenu s = selectOneMenu(listVar,
												binding,
								                title,
								                required,
								                combo.getDisabledConditionName(),
								                null);
		UISelectItems i = selectItems(listVar, binding, true);
		s.getChildren().add(i);
		
		return s;
	}

	@Override
	public UIComponent contentImage(String listVar, ContentImage image, String title, boolean required) {
		UIComponent result = panelGroup(true, true, false, null, null);
		result.getChildren().add(contentGraphicImage(image.getPixelWidth(), 
														null,
														null, 
														image.getPixelHeight(), 
														null, 
														image.getBinding(), 
														null));
		if (! Boolean.FALSE.equals(image.getEditable())) {
			result.getChildren().add(label("Upload"));
		}
		
		return result;
	}
	
	@Override
	public UIComponent contentLink(String listVar, ContentLink link, String title, boolean required) {
		String text = link.getValue();
		if (text == null) {
			text = "Content";
		}

		UIComponent result = panelGroup(true, true, false, null, null);
		result.getChildren().add(contentLink(link.getPixelWidth(), text, link.getBinding()));
		if (! Boolean.FALSE.equals(link.getEditable())) {
			result.getChildren().add(label("Upload"));
		}
		
		return result;
	}
	
	@Override
	public UIComponent html(String listVar, HTML html, String title, boolean required) {
		return editor(listVar, html.getBinding(), title, required, html.getDisabledConditionName());
	}
	
	@Override
	public UIComponent lookupDescription(String listVar, 
											LookupDescription lookup, 
											String title, 
											boolean required,
											String displayBinding,
											QueryDefinition query) {
		return autoComplete(listVar,
							lookup.getBinding(),
							title,
							required,
							lookup.getDisabledConditionName(),
							displayBinding,
							query,
							lookup.getPixelWidth(),
							false);
	}

	@Override
	public UIComponent password(String listVar, 
									org.skyve.impl.metadata.view.widget.bound.input.Password password,
									String title, 
									boolean required) {
		return password(listVar,
							password.getBinding(), 
			                title,
			                required,
			                password.getDisabledConditionName(),
			                password.getPixelWidth(),
			                true);
	}

	@Override
	public UIComponent radio(String listVar, Radio radio, String title, boolean required) {
		String binding = radio.getBinding();
        UIComponent result = selectOneRadio(listVar,
												binding,
				                                title,
				                                required,
				                                radio.getDisabledConditionName());
        result.getAttributes().put("binding", radio.getBinding());
        UISelectItems i = selectItems(listVar, binding, false);
		result.getChildren().add(i);
		return result;
	}
	
	@Override
	public UIComponent richText(String listVar, RichText text, String title, boolean required) {
        return editor(listVar, text.getBinding(), title, required, text.getDisabledConditionName());
	}

	@Override
	public UIComponent spinner(String listVar, 
								org.skyve.impl.metadata.view.widget.bound.input.Spinner spinner,
								String title, 
								boolean required) {
		return spinner(listVar, 
						spinner.getBinding(), 
						title, 
						required, 
						spinner.getDisabledConditionName(),
						spinner.getPixelWidth());
	}
	
	@Override
	public UIComponent textArea(String listVar, 
									TextArea text, 
									String title, 
									boolean required,
									Integer length) {
        return textArea(listVar,
							text.getBinding(),
							title,
							required,
							text.getDisabledConditionName(),
							length,
							text.getPixelWidth(),
							text.getPixelHeight(),
							true);
	}
	
	@Override
	public UIComponent text(String listVar, 
								TextField text, 
								String title, 
								boolean required,
								Integer length,
								org.skyve.domain.types.converters.Converter<?> converter,
								Format<?> format,
								Converter facesConverter) {
		boolean useCalendar = false;
		Format<?> mutableFormat = format;
		if (converter != null) {
			AttributeType converterAttributeType = converter.getAttributeType();
	        useCalendar = (AttributeType.date.equals(converterAttributeType) || 
			        		AttributeType.dateTime.equals(converterAttributeType) ||
			        		AttributeType.timestamp.equals(converterAttributeType));
	        if (mutableFormat == null) {
		        mutableFormat = converter.getFormat();
	        }
		}
		
        UIComponent result = null;
        if (useCalendar) {
            result = calendar(listVar,
	            				text.getBinding(),
	                            title,
	                            required,
	                            false,
	                            text.getDisabledConditionName(),
	                            facesConverter);
        }
        else if (mutableFormat != null) {
            result = maskField(listVar,
								text.getBinding(),
								title,
								required,
								text.getDisabledConditionName(),
								length,
								mutableFormat,
								facesConverter,
								text.getPixelWidth(),
								true);
        }
        else {
        	result = textField(listVar,
								text.getBinding(),
								title,
								required,
								text.getDisabledConditionName(),
								length,
								facesConverter,
								text.getPixelWidth(),
								true);
        }
        
        return result;
	}

	@Override
	public UIComponent actionLink(String listBinding, String listVar, Link link, String actionName) {
		// TODO do the tooltip and client validation, disabled, invisible thing,
		// Need the action, not just it's name
		return actionLink(link.getValue(),
							null,
							null,
							actionName,
							false,
							listBinding,
							listVar,
							link.getPixelWidth(),
							null,
							Boolean.FALSE,
							null,
							null,
							link.getInvisibleConditionName());
	}
	
	@Override
	public UIComponent report(Action action) {
		return reportButton(action.getDisplayName(), 
								action.getIconStyleClass(),
								action.getToolTip(), 
								action.getParameters(), 
								null,
								null,
								action.getClientValidation(),
								action.getConfirmationText(),
								action.getDisabledConditionName(), 
								action.getInvisibleConditionName());
	}
	
	@Override
	public UIComponent download(Action action,
									String moduleName,
									String documentName) {
		return downloadButton(action.getDisplayName(), 
								action.getIconStyleClass(),
								action.getToolTip(), 
								action.getName(), 
								moduleName,
								documentName,
								null,
								null,
								action.getClientValidation(),
								action.getConfirmationText(),
								action.getDisabledConditionName(), 
								action.getInvisibleConditionName());
	}

	@Override
	public UIComponent action(String listBinding,
								String listVar,
								Action action, 
								ImplicitActionName name,
								String title) {
		return actionButton(title,
								action.getIconStyleClass(),
								action.getToolTip(),
								name,
								action.getName(),
								false,
								listBinding,
								listVar,
								null,
								null,
								action.getClientValidation(),
								action.getConfirmationText(),
								action.getDisabledConditionName(),
								action.getInvisibleConditionName());
	}
	
	private Panel panel(String title, String invisible, Integer pixelWidth, String widgetId) {
		Panel result = (Panel) a.createComponent(Panel.COMPONENT_TYPE);
		if (title != null) {
			result.setHeader(title);
		}

		setInvisible(result, invisible, null);
		setSize(result, null, pixelWidth, null, null, null, null, NINETY_EIGHT);
		setId(result, widgetId);
		return result;
	}

	protected Password password(String listVar, 
									String binding, 
									String title, 
									boolean required, 
									String disabled,
									Integer pixelWidth, 
									boolean applyDefaultWidth) {
		Password result = (Password) input(Password.COMPONENT_TYPE, listVar, binding, title, required, disabled);
		result.setId(result.getId() + "password"); // ensures that the password field value is not logged in the request parameters on the server
		setSize(result, null, pixelWidth, null, null, null, null, applyDefaultWidth ? ONE_HUNDRED : null);
		return result;
	}

	protected InputText textField(String listVar, 
									String binding, 
									String title, 
									boolean required, 
									String disabled,
									Integer maxLength, 
									Converter converter, 
									Integer pixelWidth, 
									boolean applyDefaultWidth) {
		InputText result = (InputText) input(InputText.COMPONENT_TYPE, 
												listVar, 
												binding, 
												title, 
												required,
												disabled);
		if (maxLength != null) {
			result.setMaxlength(maxLength.intValue());
		}
		if (converter != null) {
			result.setConverter(converter);
		}
		setSize(result, null, pixelWidth, null, null, null, null, applyDefaultWidth ? ONE_HUNDRED : null);
		return result;
	}

	private InputMask maskField(String listVar,
									String binding, 
									String title, 
									boolean required, 
									String disabled,
									Integer maxLength, 
									Format<?> format, 
									Converter converter, 
									Integer pixelWidth, 
									boolean applyDefaultWidth) {
		InputMask result = (InputMask) input(InputMask.COMPONENT_TYPE, 
												listVar, 
												binding, 
												title, 
												required,
												disabled);
		if (maxLength != null) {
			result.setMaxlength(maxLength.intValue());
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
	 * @param text
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

	private Spinner spinner(String listVar, 
								String binding, 
								String title, 
								boolean required, 
								String disabled,
								Integer pixelWidth) {
		Spinner result = (Spinner) input(Spinner.COMPONENT_TYPE, listVar, binding, title, required, disabled);
		setSize(result, null, pixelWidth, null, null, null, null, null);
		return result;
	}

	private Calendar calendar(String listVar,
								String binding, 
								String title, 
								boolean required, 
								boolean mobile,
								String disabled, 
								Converter converter) {
		Calendar result = (Calendar) input(Calendar.COMPONENT_TYPE, listVar, binding, title, required, disabled);
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
		else if ("DD_MM_YYYY_HH_MI".equals(converterName)) {
			result.setPattern("dd-MMM-yyyy hh:mm");
			result.setMask("99-aaa-9999 99:99");
		} 
		else if ("DD_MM_YYYY_HH24_MI".equals(converterName)) {
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
		else if ("DD_MM_YYYY_HH_MI_SS".equals(converterName)) {
			result.setPattern("dd-MMM-yyyy hh:mm:ss");
			result.setMask("99-aaa-9999 99:99:99");
		} 
		else if ("DD_MM_YYYY_HH24_MI_SS".equals(converterName)) {
			result.setPattern("dd-MMM-yyyy HH:mm:ss");
			result.setMask("99-aaa-9999 99:99:99");
		}
		result.setConverter(converter);
		return result;
	}

	protected InputTextarea textArea(String listVar, 
										String binding, 
										String title, 
										boolean required, 
										String disabled,
										Integer maxLength, 
										Integer pixelWidth, 
										Integer pixelHeight, 
										boolean applyDefaultWidth) {
		InputTextarea result = (InputTextarea) input(InputTextarea.COMPONENT_TYPE, 
														listVar, 
														binding, 
														title,
														required, 
														disabled);
		if (maxLength != null) {
			result.setMaxlength(maxLength.intValue());
		}
		setSize(result, null, pixelWidth, null, null, pixelHeight, null, applyDefaultWidth ? ONE_HUNDRED : null);
		return result;
	}

	private TabView tabView(String invisible, String activeIndexBinding, String widgetId) {
		TabView result = (TabView) a.createComponent(TabView.COMPONENT_TYPE);
		setInvisible(result, invisible, null);
		setId(result, widgetId);
		if (activeIndexBinding != null) {
			result.setValueExpression("activeIndex", createValueExpressionFromFragment(activeIndexBinding, true, null, Number.class));
		}
		return result;
	}

	protected CommandButton actionButton(String title, 
											String iconStyleClass,
											String tooltip, 
											ImplicitActionName implicitActionName,
											String actionName,
											boolean inline, 
											String listBinding, 
											String listVar,
											Integer pixelWidth, 
											Integer pixelHeight,
											Boolean clientValidation, 
											String confirmationText,
											String disabled, 
											String invisible) {
		CommandButton result = (CommandButton) a.createComponent(CommandButton.COMPONENT_TYPE);

		result.setValue(title);
		result.setIcon(iconStyleClass);
		result.setTitle(tooltip);

		action(result, implicitActionName, actionName, listBinding, listVar, inline, null);
		setSize(result, null, pixelWidth, null, null, pixelHeight, null, null);
		setDisabled(result, disabled);
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
						result.setIcon("fa fa-mail-forward");
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
			result.setProcess(process); // process the current form (by default)
			result.setUpdate(update); // update all forms (by default)
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
							invisible,
							(ReportFormat.html.equals(reportFormat) ||
								ReportFormat.xhtml.equals(reportFormat)) ? 
									"_blank" : 
									null);
	}

	/**
	 * Create a button with a href URL that looks like...
	 * http://localhost:8080/skyve/download?_n=<downloadAction>&_doc=<module>.<document>&_c=<webId>&_ctim=<currentTimeInMillis>
	 */
	private Button downloadButton(String title, 
									String iconStyleClass,
									String tooltip,
									String downloadActionName,
									String moduleName,
									String documentName,
									Integer pixelWidth, 
									Integer pixelHeight,
									@SuppressWarnings("unused") Boolean clientValidation, // TODO not implemmented
									// TODO LinkButton is not a Confirmable. ConfirmBehavior can only be attached to components that implement org.primefaces.component.api.Confirmable interface
									@SuppressWarnings("unused") String confirmationText,
									String disabled,
									String invisible) {
		String href = String.format("download?%s=%s&%s=%s.%s&%s=#{%s.webContext.webId}&%s=%d", 
										AbstractWebContext.RESOURCE_FILE_NAME,
										downloadActionName,
										AbstractWebContext.DOCUMENT_NAME,
										moduleName,
										documentName,
										AbstractWebContext.CONTEXT_NAME,
										managedBeanName,
										AbstractWebContext.CURRENT_TIME_IN_MILLIS,
										Long.valueOf(System.currentTimeMillis()));
		return linkButton((iconStyleClass == null) ? "fa fa-download" : iconStyleClass, 
							null, 
							null,
							title,
							tooltip,
							href.toString(),
							pixelWidth,
							pixelHeight,
							disabled,
							invisible,
							null);
	}
	
	protected CommandLink actionLink(String title, 
										String tooltip, 
										ImplicitActionName implicitActionName,
										String actionName, 
										boolean inline, 
										String collectionBinding,
										String listVar, 
										Integer pixelWidth, 
										Integer pixelHeight,
										Boolean clientValidation, 
										String confirmationText,
										String disabled, 
										String invisible) {
		CommandLink result = (CommandLink) a.createComponent(CommandLink.COMPONENT_TYPE);

		result.setValue(title);
		result.setTitle(tooltip);

		action(result, implicitActionName, actionName, collectionBinding, listVar, inline, null);

		setSize(result, null, pixelWidth, null, null, pixelHeight, null, null);
		setDisabled(result, disabled);
		setInvisible(result, invisible, null);
		setConfirmation(result, confirmationText);
		setId(result, null);

		if (ImplicitActionName.Cancel.equals(implicitActionName) || ImplicitActionName.OK.equals(implicitActionName)) {
			result.setAjax(false);
		} 
		else {
			result.setProcess(process);
			result.setUpdate(update);
		}

		return result;
	}

	private void action(UICommand command, 
							ImplicitActionName implicitActionName, 
							String actionName,
							String collectionBinding,
							String listVar, 
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
		command.setActionExpression(methodExpressionForAction(implicitActionName, actionName, collectionBinding, listVar, inline, eventHandlerActionNames));
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
								String invisible, 
								String target) {
		Button result = button(icon, styleClass, style);
		result.setValue(value);
		result.setTitle(title);
		result.setValueExpression("href", ef.createValueExpression(elc, href, String.class));
		result.setTarget(target);

		setId(result, null);
		setSize(result, null, pixelWidth, null, null, pixelHeight, null, null);
		setDisabled(result, disabled);
		setInvisible(result, invisible, null);

		return result;
	}

	private UIOutput outputText(String listVar, String binding) {
		// escape bindings with ' as \' as the binding could be for blurb expressions
		String sanitisedBinding = ((binding.indexOf('\'') >= 0) ? binding.replace("'", "\\'") : binding);
		ValueExpression ve = createValueExpressionFromFragment(listVar, true, sanitisedBinding, true, null, String.class);
		UIOutput result = new UIOutput();
		result.setValueExpression("value", ve);
		setId(result, null);
		return result;
	}

	@Override
	public Spacer spacer(org.skyve.impl.metadata.view.widget.Spacer spacer) {
		Spacer result = (Spacer) a.createComponent(Spacer.COMPONENT_TYPE);
		setSize(result, null, spacer.getPixelWidth(), null, null, spacer.getPixelHeight(), null, null);
		setInvisible(result, spacer.getInvisibleConditionName(), null);
		setId(result, null);

		return result;
	}

	@Override
	public GraphicImage staticImage(StaticImage image) {
		GraphicImage result = (GraphicImage) a.createComponent(GraphicImage.COMPONENT_TYPE);
		result.setUrl("images/" + image.getRelativeFile());
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
	public UIComponent dynamicImage(DynamicImage image, String moduleName, String documentName) {
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
	
	private GraphicImage contentGraphicImage(Integer pixelWidth, 
												Integer responsiveWidth,
												Integer percentageWidth, 
												Integer pixelHeight,
												Integer percentageHeight, 
												String binding, 
												String invisible) {
		GraphicImage result = (GraphicImage) a.createComponent(GraphicImage.COMPONENT_TYPE);

		StringBuilder expression = new StringBuilder(64);
		expression.append("#{").append(managedBeanName).append(".getContentUrl('");
		expression.append(binding).append("')}");

		result.setValueExpression("value", ef.createValueExpression(elc, expression.toString(), String.class));
		setSize(result, "border:1px solid gray;", pixelWidth, responsiveWidth, percentageWidth, pixelHeight, percentageHeight, null);
		setInvisible(result, invisible, null);
		setId(result, null);
		return result;
	}

	private HtmlOutputLink contentLink(Integer pixelWidth, String text, String binding) {
		HtmlOutputLink result = (HtmlOutputLink) a.createComponent(HtmlOutputLink.COMPONENT_TYPE);

		StringBuilder expression = new StringBuilder(64);
		expression.append("#{").append(managedBeanName).append(".getContentUrl('");
		expression.append(binding).append("')}");
		result.setValueExpression("value", ef.createValueExpression(elc, expression.toString(), String.class));

		if (text != null) {
			UIOutput outputText = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
			outputText.setValue(text);
			result.getChildren().add(outputText);
		}

		result.setTarget("_blank");
		setSize(result, null, pixelWidth, null, null, null, null, null);
		setId(result, null);

		return result;
	}

	// TODO do the grids

	protected SelectBooleanCheckbox checkbox(String listVar, 
												String binding, 
												String title, 
												boolean required,
												String disabled) {
		return (SelectBooleanCheckbox) input(SelectBooleanCheckbox.COMPONENT_TYPE,
												listVar, 
												binding, 
												title, 
												required, 
												disabled);
	}

	protected ColorPicker colourPicker(String listVar, 
										String binding, 
										String title, 
										boolean required,
										Integer pixelWidth, 
										boolean applyDefaultWidth) {
		ColorPicker result = (ColorPicker) input(ColorPicker.COMPONENT_TYPE, 
													listVar, 
													binding, 
													title, 
													required,
													null);
		setSize(result, null, pixelWidth, null, null, null, null, applyDefaultWidth ? ONE_HUNDRED : null);
		return result;
	}

	private SelectOneMenu selectOneMenu(String listVar, 
											String binding, 
											String title, 
											boolean required,
											String disabled, 
											Integer pixelWidth) {
		SelectOneMenu result = (SelectOneMenu) input(SelectOneMenu.COMPONENT_TYPE, 
														listVar, 
														binding, 
														title,
														required, 
														disabled);
		// Do not default pixel width to 100% as it causes renderering issues on the drop button on the end.
		// The control sets its width by default based on the font metrics of the drop-down values.
		setSize(result, null, pixelWidth, null, null, null, null, null);
		result.setConverter(new SelectItemsBeanConverter());
		return result;
	}

	private SelectOneRadio selectOneRadio(String listVar, 
											String binding, 
											String title, 
											boolean required,
											String disabled) {
		SelectOneRadio result = (SelectOneRadio) input(SelectOneRadio.COMPONENT_TYPE, 
														listVar, 
														binding, 
														title,
														required, 
														disabled);
		result.setConverter(new SelectItemsBeanConverter());
		return result;
	}

	protected AutoComplete autoComplete(String listVar, 
											String binding, 
											String title, 
											boolean required,
											String disabled, 
											String displayBinding, 
											QueryDefinition query, 
											Integer pixelWidth,
											boolean dontDisplay) {
		AutoComplete result = (AutoComplete) input(AutoComplete.COMPONENT_TYPE, 
													listVar, 
													binding, 
													title, 
													required,
													disabled);
		result.setForceSelection(true);
		result.setDropdown(true);
		String var = binding.replace('.', '_') + "Row";
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
	private Editor editor(String listVar, String binding, String title, boolean required, String disabled) {
		return (Editor) input(Editor.COMPONENT_TYPE, listVar, binding, title, required, disabled);
	}

	private DataTable dataTable(String binding, 
									String listVar,
									String title, 
									String invisible, 
									boolean clickToZoom,
									String[] clickToZoomDisabledConditionNames,
									String selectedIdBinding,
									List<EventAction> selectedActions,
									String widgetId) {
		DataTable result = (DataTable) a.createComponent(DataTable.COMPONENT_TYPE);
		setId(result, widgetId);
		setInvisible(result, invisible, null);
		addGridHeader(title, result);

		result.setVar(listVar);
		result.setValueExpression("value", createValueExpressionFromFragment(binding, true, null, List.class));

		if (selectedIdBinding != null) {
			addDataTableSelection(result, selectedIdBinding, selectedActions, binding);
		}
		else if (clickToZoom) {
			String id = result.getId();
			result.setWidgetVar(id);
			result.setSelectionMode("single");
			result.setValueExpression("rowKey",
										createValueExpressionFromFragment(listVar,
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

		return result;
	}

	protected DataList dataList(String binding, String listVar, String title, String invisible, String widgetId) {
		DataList result = (DataList) a.createComponent(DataList.COMPONENT_TYPE);
		setId(result, widgetId);
		setInvisible(result, invisible, null);
		addGridHeader(title, result);

		result.setVar(listVar);
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

	private Column column(String listVar, 
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
										createValueExpressionFromFragment(listVar, true, sortBinding, true, null, Object.class));
		}

		StringBuilder style = new StringBuilder(64);
		if (pixelWidth != null) {
			style.append("width:").append(pixelWidth).append("px;");
		}
		if (noWrap) {
			style.append("white-space:nowrap;");
		}
		if ((alignment != null) && (!HorizontalAlignment.left.equals(alignment))) {
			style.append("text-align:").append(HorizontalAlignment.centre.equals(alignment) ? "center" : "right").append(" !important;");
		}
		if (style.length() > 0) {
			result.setStyle(style.toString());
		}

		return result;
	}

	private UISelectItems selectItems(String listVar, String binding, boolean includeEmptyItems) {
		UISelectItems result = (UISelectItems) a.createComponent(UISelectItems.COMPONENT_TYPE);
		setId(result, null);
		StringBuilder expression = new StringBuilder(32);
		expression.append("getSelectItems('").append(binding).append("',").append(includeEmptyItems).append(')');
		ValueExpression valueExpression = null;
		if (listVar != null) {
			valueExpression = createValueExpressionFromFragment(listVar, true, expression.toString(), false, null, List.class);
		}
		else {
			valueExpression = createValueExpressionFromFragment(expression.toString(), false, null, List.class);
		}
		result.setValueExpression("value", valueExpression);

		return result;
	}

	private UIInput input(String componentType, 
							String listVar, 
							String binding,
							String title, 
							boolean required,
							String disabled) {
		UIInput result = (UIInput) a.createComponent(componentType);
		setId(result, null);
		if (listVar != null) {
			result.setValueExpression("value",
										createValueExpressionFromFragment(listVar, true, binding, true, null, Object.class));
		}
		else {
			result.setValueExpression("value", createValueExpressionFromFragment(binding, true, null, Object.class));
		}
		result.setValueExpression("title",
									ef.createValueExpression(elc, required ? title + " *" : title, String.class));

		// Cannot utilise the faces required attributes as some requests need to ignore required-ness.
		// eg - triggered actions on widget events.
		// Setting required attribute to an expression worked server-side but the client-side message integration didn't.
		// result.setValueExpression("required", ef.createValueExpression(required ? "true" : "false", Boolean.class));
		// So we use the requiredMessage to perform the check ourselves based on clientValidation attribute
		if (required) {
			result.setRequiredMessage(title + " is required");
		}
		setDisabled(result, disabled);
		return result;
	}

	private void setConfirmation(UIComponentBase component, String confirmationText) {
		if (confirmationText != null) {
			ConfirmBehavior confirm = (ConfirmBehavior) a.createBehavior(ConfirmBehavior.BEHAVIOR_ID);
			confirm.setMessage(confirmationText);
			component.addClientBehavior("click", confirm);
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
