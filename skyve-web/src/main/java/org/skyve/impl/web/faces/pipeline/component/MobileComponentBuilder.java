package org.skyve.impl.web.faces.pipeline.component;

import java.util.List;

import javax.el.ValueExpression;
import javax.faces.component.UIComponent;
import javax.faces.component.UIOutput;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.convert.Converter;

import org.primefaces.component.button.Button;
import org.primefaces.component.column.Column;
import org.primefaces.component.commandbutton.CommandButton;
import org.primefaces.component.commandlink.CommandLink;
import org.primefaces.component.datalist.DataList;
import org.primefaces.component.inputtext.InputText;
import org.primefaces.component.outputpanel.OutputPanel;
import org.primefaces.component.selectbooleancheckbox.SelectBooleanCheckbox;
import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Format;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.web.WebAction;

public class MobileComponentBuilder extends TabularComponentBuilder {

	@Override
	public List<UIComponent> toolbars(List<UIComponent> components, String widgetId) {
		if (components != null) {
			return components;
		}

		return null; // no toolbars for mobile
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

		return accordionPanel(tabPane.getInvisibleConditionName(), tabPane.getWidgetId());
	}
	
	@Override
	public UIComponent spacer(UIComponent component, org.skyve.impl.metadata.view.widget.Spacer spacer) {
		if (component != null) {
			return component;
		}

		// Don't add spacers to the mobile UI as they just leave a space and a line which sux
		return null;
	}

	@Override
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
											String invisible,
											String processOverride,
											String updateOverride) {
		CommandButton result = super.actionButton(title,
													iconStyleClass,
													tooltip, 
													implicitActionName, 
													actionName, 
													inline, 
													listBinding, 
													listVar,
													pixelWidth, 
													pixelHeight,
													clientValidation, 
													null, // confirmation dialogs don't work in mobile
													disabled, 
													invisible,
													processOverride,
													updateOverride);
		result.setIcon(null); // fa is not compatible with PF mobile
		return result;
	}
	
	@Override
	public CommandLink actionLink(String title, 
									String tooltip, 
									ImplicitActionName implicitActionName,
									String actionName, 
									boolean inline, 
									String collectionName, 
									String listVar,
									Integer pixelWidth, 
									Integer pixelHeight,
									Boolean clientValidation, 
									String confirmationText, 
									String disabled, 
									String invisible,
									String processOverride,
									String updateOverride) {
		return super.actionLink(title, 
									tooltip, 
									implicitActionName, 
									actionName, 
									inline, 
									collectionName, 
									listVar,
									pixelWidth, 
									pixelHeight,
									clientValidation, 
									null, // confirmation dialogs don't work in mobile
									disabled, 
									invisible,
									processOverride,
									updateOverride);
	}
	
	@Override
	public UIComponent dataGrid(UIComponent component, String listVar, DataGrid grid) {
		if (component != null) {
			return component;
		}

		DataList result = dataList(grid.getBinding(), 
									listVar,
		                			grid.getTitle(),
		                			grid.getInvisibleConditionName(),
		                			grid.getWidgetId());
		result.getPassThroughAttributes().put("data-inset", createValueExpressionFromCondition("true", null));
		return result;
	}
	
	@Override
	public UIComponent addDataGridBoundColumn(UIComponent component,
												UIComponent current, 
												AbstractDataWidget widget,
												DataGridBoundColumn column,
												String listVar,
												String columnTitle,
												String columnBinding,
												StringBuilder gridColumnExpression) {
		if (component != null) {
			return component;
		}

		UIComponent result = current;
		String gridBinding = widget.getBinding();

    	boolean first = false;
    	if (gridColumnExpression.length() == 0) { // no columns processed yet
    		first = true;
    		Column col = column(null, false, false, null, null, null, null, null);
			current.getChildren().add(col);
	        result = col;
    	}

    	gridColumnExpression.append(first ? "<h2>" : "<p>");
    	gridColumnExpression.append("#{").append(gridBinding).append("['{");
    	gridColumnExpression.append(columnBinding).append("}']}");
		gridColumnExpression.append(first ? "</h2>" : "</p>");
		
		return result;
	}
	
	@Override
	public UIComponent addedDataGridBoundColumn(UIComponent component, UIComponent current) {
		if (component != null) {
			return component;
		}

		return current;
	}

	@Override
	public UIComponent addDataGridContainerColumn(UIComponent component,
													UIComponent current, 
													AbstractDataWidget widget, 
													DataGridContainerColumn column) {
		if (component != null) {
			return component;
		}

		return current;
	}
	
	@Override
	public UIComponent addedDataGridContainerColumn(UIComponent component, UIComponent current) {
		if (component != null) {
			return component;
		}

		return current.getParent();
	}
	
	@Override
	public UIComponent addDataGridActionColumn(UIComponent component,
												UIComponent current, 
												DataGrid grid, 
												String listVar,
												String gridColumnExpression,
												String singularDocumentAlias,
												boolean inline) {
		if (component != null) {
			return component;
		}

		UIComponent result = current;
		String listBinding = grid.getBinding();
		
		UIOutput outputText = outputText(gridColumnExpression);
		// If the grid is editable, add the ability to zoom
		if (! Boolean.FALSE.equals(grid.getEditable())) {
			CommandLink link = actionLink(null,
												"Edit the record",
												ImplicitActionName.Navigate,
												null,
												false,
												listBinding,
												listVar,
												null,
												null,
												Boolean.TRUE,
												null,
												null,
												null,
												null,
												null);
			link.getChildren().add(outputText);
			current.getChildren().add(link);
		}
		else {
			current.getChildren().add(outputText);
		}

		result = current.getParent(); // finished with the single dataList column
		
		return result;
	}
	
	private UIOutput outputText(String expression) {
		ValueExpression ve = ef.createValueExpression(elc, expression, String.class);
		UIOutput result = new UIOutput();
		result.setValueExpression("value", ve);
		setId(result, null);
		return result;
	}


	@Override
	public UIComponent checkBox(UIComponent component,
									String listVar,
									CheckBox checkBox,
									String title,
									boolean required) {
		if (component != null) {
			return component;
		}

		SelectBooleanCheckbox result = (SelectBooleanCheckbox) checkbox(listVar,
																			checkBox.getBinding(), 
																			title,
																			required,
																			checkBox.getDisabledConditionName(),
																			false);
		result.setItemLabel(title);
		return result;
	}
	
	@Override
	public UIComponent colourPicker(UIComponent component,
										String listVar,
										ColourPicker colour,
										String title,
										boolean required) {
		if (component != null) {
			return component;
		}

		return colourPicker(listVar, 
								colour.getBinding(), 
								title, 
								required, 
								colour.getPixelWidth(),
								false);
	}
	
	@Override
	public UIComponent lookupDescription(UIComponent component,
											String listVar, 
											LookupDescription lookup, 
											String title, 
											boolean required,
											String displayBinding,
											QueryDefinition query) {
		if (component != null) {
			return component;
		}

		UIComponent c = autoComplete(listVar,
										lookup.getBinding(),
										title,
										required,
										lookup.getDisabledConditionName(),
										displayBinding,
										query,
										lookup.getParameters(),
										lookup.getPixelWidth(),
										true);

		UIComponent result = panelGroup(false, false, false, null, null);
		List<UIComponent> children = result.getChildren();
		children.add(c);
		InputText text = textField(listVar, 
									String.format("%s.%s", lookup.getBinding(), displayBinding), 
									title,
									required, 
									false,
									"true", 
									null, 
									null, 
									null, 
									false);
		children.add(text);

		Button button = button("ui-icon-search", 
								"ui-btn-right",
								(title == null) ? "top:1em !important" : "top:2.3em !important");
		children.add(button);
		button.setOnclick("return SKYVE.switchToAutoComplete(this)");
         
        return result;
	}

	@Override
	public UIComponent password(UIComponent component,
									String listVar, 
									org.skyve.impl.metadata.view.widget.bound.input.Password password,
									String title, 
									boolean required) {
		if (component != null) {
			return component;
		}

		return password(listVar,
							password.getBinding(), 
			                title,
			                required,
			                password.getDisabledConditionName(),
			                password.getPixelWidth(),
			                false);
	}

	@Override
	public UIComponent textArea(UIComponent component,
									String listVar, 
									TextArea text, 
									String title, 
									boolean required,
									Integer length) {
		if (component != null) {
			return component;
		}

		return textArea(listVar,
							text.getBinding(),
							title,
							required,
							Boolean.FALSE.equals(text.getEditable()),
							text.getDisabledConditionName(),
							length,
							text.getPixelWidth(),
							text.getPixelHeight(),
							false);
	}
	
	@Override
	public UIComponent text(UIComponent component,
								String listVar, 
								TextField text, 
								String title, 
								boolean required,
								Integer length,
								org.skyve.domain.types.converters.Converter<?> converter,
								Format<?> format,
								Converter facesConverter) {
		if (component != null) {
			return component;
		}

		return textField(listVar,
							text.getBinding(),
							title,
							required,
							Boolean.FALSE.equals(text.getEditable()),
							text.getDisabledConditionName(),
							length,
							facesConverter,
							text.getPixelWidth(),
							false);
	}
	
	/*
		<p:dataList id="list" 
						var="row"
						paginator="true" 
						rows="10" 
						value="#{skyve.getBeans(skyve.bizModuleParameter, skyve.queryNameParameter)}">
			<f:facet name="header">
				<p:button href="./?a=#{WebAction.e.toString()}&amp;m=#{skyve.bizModuleParameter}&amp;d=#{skyve.bizDocumentParameter}" value="New" />
			</f:facet>
			<f:attribute name="paginatorText" value="More..." />
			<f:attribute name="filter" value="true" />
			<h:outputLink value="/?a=e&amp;m=#{row['bizModule']}&amp;d=#{row['bizDocument']}&amp;i=#{row['bizId']}">
				<h2><h:outputText value="#{row['bizKey']}" /></h2>
				<p><h:outputText value="#{row['bizKey']}" /></p>
			</h:outputLink>
		</p:dataList>
	 */
	@Override
	public UIComponent listGrid(UIComponent component,
									String modelDocumentName,
									String modelName,
									ListModel<? extends Bean> model,
									ListGrid grid,
									boolean canCreateDocument) {
		if (component != null) {
			return component;
		}

		boolean createRendered = (! Boolean.FALSE.equals(grid.getShowAdd()));
		String disableAddConditionName = grid.getDisableAddConditionName();
		String disabledConditionName = grid.getDisabledConditionName();
		String[] createDisabledConditionNames = (disableAddConditionName == null) ?
				((disabledConditionName == null) ?
						null :
						new String[] {disabledConditionName}) :
				((disabledConditionName == null) ?
						new String[] {disableAddConditionName} :
						new String[] {disableAddConditionName, disabledConditionName});
		boolean zoomRendered = (! Boolean.FALSE.equals(grid.getShowZoom()));

		DataList result = (DataList) a.createComponent(DataList.COMPONENT_TYPE);
		setId(result, null);
		result.setVar("row");
        result.setLazy(true);
		result.setEmptyMessage((canCreateDocument && createRendered) ? EMPTY_DATA_TABLE_CAN_ADD_MESSAGE : EMPTY_DATA_TABLE_MESSAGE);

		Document drivingDocument = model.getDrivingDocument();
		String moduleName = drivingDocument.getOwningModuleName();
		String drivingDocumentName = drivingDocument.getName();
// Lazy data models don't seem to work on mobile data lists 		
/*
		String value = (model instanceof DocumentQueryListModel) ? 
        					String.format("#{%s.getLazyDataModel('%s','%s','%s',null)}",
											managedBeanName, 
											moduleName, 
											drivingDocumentName, 
											modelName) :
    						String.format("#{%s.getLazyDataModel('%s','%s',null,'%s')}", 
										managedBeanName, 
										moduleName, 
										modelDocumentName, 
										modelName);
        result.setValueExpression("value", ef.createValueExpression(elc, value, SkyveLazyDataModel.class));
*/
        String value = String.format("#{%s.getBeans('%s','%s',null)}", 
        								managedBeanName, 
        								moduleName,
        								modelName);
        result.setValueExpression("value", ef.createValueExpression(elc, value, List.class));

		if (canCreateDocument && createRendered) {
        	addListGridHeader(result, moduleName, drivingDocumentName, createDisabledConditionNames);
        }
		addListGridBoundColumns(model, result.getChildren(), zoomRendered, grid.getDisableZoomConditionName());
		
		return result;
	}
	
	private void addListGridHeader(UIComponent componentToAddTo,
							String moduleName,
							String documentName,
							String[] createDisabledConditionNames) {
		Button button = (Button) a.createComponent(Button.COMPONENT_TYPE);
    	button.setValue("New");
    	button.setTitle("New record");
		ValueExpression disabled = createOredValueExpressionFromConditions(createDisabledConditionNames);
		if (disabled != null) {
			button.setValueExpression("disabled", disabled);
		}
    	StringBuilder value = new StringBuilder(128);
    	value.append("./?a=").append(WebAction.e.toString()).append("&m=").append(moduleName);
    	value.append("&d=").append(documentName);
    	button.setHref(value.toString());

        OutputPanel headingPanel = (OutputPanel) a.createComponent(OutputPanel.COMPONENT_TYPE);
        headingPanel.getChildren().add(button);
        componentToAddTo.getFacets().put("header", headingPanel);
	}
	
	private void addListGridBoundColumns(ListModel<? extends Bean> model,
											List<UIComponent> componentChildrenToAddTo,
											boolean zoomRendered,
											String zoomDisabledConditionName) {
		StringBuilder value = new StringBuilder(128);
		
		for (MetaDataQueryColumn column : model.getColumns()) {
			if (column.isHidden() || 
					((column instanceof MetaDataQueryProjectedColumn) && 
							(! ((MetaDataQueryProjectedColumn) column).isProjected()))) {
				continue;
			}
			
			boolean first = (value.length() == 0);
			value.append(first ? "<h2>" : "<p>");
			value.append("#{row['{").append(column.getBinding()).append("}']}");
			value.append(first ? "</h2>" : "</p>");
		}
		
		UIOutput outputText = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
		outputText.setValueExpression("value", ef.createValueExpression(elc, value.toString(), Object.class));

		if (zoomRendered) { // require a zoom link
			HtmlOutputLink link = (HtmlOutputLink) a.createComponent(HtmlOutputLink.COMPONENT_TYPE);
			value.setLength(0);
			value.append("./?a=").append(WebAction.e.toString());
			value.append("&m=#{row['bizModule']}&d=#{row['bizDocument']}&i=#{row['bizId']}");
			link.setValueExpression("value", ef.createValueExpression(elc, value.toString(), String.class));
			link.getChildren().add(outputText);
			if (zoomDisabledConditionName != null) {
				link.setValueExpression("disabled",
											createValueExpressionFromCondition(zoomDisabledConditionName, null));
			}
			componentChildrenToAddTo.add(link);
		}
		else { // no zoom link
			componentChildrenToAddTo.add(outputText);
		}
	}
}
