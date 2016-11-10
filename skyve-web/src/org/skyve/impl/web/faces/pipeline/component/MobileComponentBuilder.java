package org.skyve.impl.web.faces.pipeline.component;

import java.util.List;

import javax.el.ValueExpression;
import javax.faces.component.UIComponent;
import javax.faces.component.UIOutput;
import javax.faces.convert.Converter;

import org.primefaces.component.button.Button;
import org.primefaces.component.column.Column;
import org.primefaces.component.commandbutton.CommandButton;
import org.primefaces.component.commandlink.CommandLink;
import org.primefaces.component.datalist.DataList;
import org.primefaces.component.inputtext.InputText;
import org.primefaces.component.selectbooleancheckbox.SelectBooleanCheckbox;
import org.primefaces.component.spacer.Spacer;
import org.skyve.domain.types.converters.Format;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridColumn;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.module.query.QueryDefinition;

public class MobileComponentBuilder extends TabularComponentBuilder {

	@Override
	public UIComponent toolbar() {
		return null; // no toolbar for mobile
	}

	@Override
	public UIComponent tabPane(TabPane tabPane) {
		return accordionPanel(tabPane.getInvisibleConditionName());
	}
	
	@Override
	public Spacer spacer(Integer pixelWidth, Integer pixelHeight) {
		// Don't add spacers to the mobile UI as they just leave a space and a line which sux
		return null;
	}

	@Override
	public CommandButton actionButton(String title, 
										String tooltip, 
										ImplicitActionName implicitActionName,
										String actionName, 
										boolean inline, 
										String listBinding, 
										Integer pixelWidth, 
										Integer pixelHeight,
										Boolean clientValidation, 
										String confirmationText, 
										String disabled, 
										String invisible) {
		return super.actionButton(title, 
									tooltip, 
									implicitActionName, 
									actionName, 
									inline, 
									listBinding, 
									pixelWidth, 
									pixelHeight,
									clientValidation, 
									null, // confirmation dialogs don't work in mobile
									disabled, 
									invisible);
	}
	
	@Override
	public CommandLink actionLink(String title, 
									String tooltip, 
									ImplicitActionName implicitActionName,
									String actionName, 
									boolean inline, 
									String collectionName, 
									Integer pixelWidth, 
									Integer pixelHeight,
									Boolean clientValidation, 
									String confirmationText, 
									String disabled, 
									String invisible) {
		return super.actionLink(title, 
									tooltip, 
									implicitActionName, 
									actionName, 
									inline, 
									collectionName, 
									pixelWidth, 
									pixelHeight,
									clientValidation, 
									null, // confirmation dialogs don't work in mobile
									disabled, 
									invisible);
	}
	
	@Override
	public UIComponent dataGrid(DataGrid grid) {
		DataList result = dataList(grid.getBinding(), 
		                			grid.getTitle(),
		                			grid.getInvisibleConditionName());
		result.getPassThroughAttributes().put("data-inset", createValueExpressionFromCondition("true", null));
		return result;
	}
	
	@Override
	public UIComponent addDataGridBoundColumn(UIComponent current, 
												DataGrid grid,
												DataGridColumn column,
												String columnTitle,
												String columnBinding,
												StringBuilder gridColumnExpression) {
		UIComponent result = current;
		String gridBinding = grid.getBinding();

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
	public UIComponent addedDataGridBoundColumn(UIComponent current) {
		return current;
	}

	@Override
	public UIComponent addDataGridContainerColumn(UIComponent current, DataGrid grid, DataGridColumn column) {
		return current;
	}
	
	@Override
	public UIComponent addedDataGridContainerColumn(UIComponent current) {
		return current.getParent();
	}
	
	@Override
	public UIComponent addDataGridActionColumn(UIComponent current, 
												DataGrid grid, 
												String gridColumnExpression,
												String singularDocumentAlias,
												boolean inline) {
		UIComponent result = current;
		String gridBinding = grid.getBinding();
		
		UIOutput outputText = outputText(gridColumnExpression);
		// If the grid is editable, add the ability to zoom
		if (! Boolean.FALSE.equals(grid.getEditable())) {
			CommandLink link = actionLink(null,
												"Edit the record",
												ImplicitActionName.Navigate,
												null,
												false,
												gridBinding,
												null,
												null,
												Boolean.TRUE,
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
		setId(result);
		return result;
	}


	@Override
	public UIComponent checkBox(String listBinding, CheckBox checkBox, String title, boolean required) {
		SelectBooleanCheckbox result = checkbox(listBinding,
												checkBox.getBinding(), 
												title,
												required,
												checkBox.getDisabledConditionName());
		result.setItemLabel(title);
		return result;
	}
	
	@Override
	public UIComponent colourPicker(String listBinding, ColourPicker colour, String title, boolean required) {
		return colourPicker(listBinding, 
								colour.getBinding(), 
								title, 
								required, 
								colour.getPixelWidth(),
								false);
	}
	
	@Override
	public UIComponent lookupDescription(String listBinding, 
											LookupDescription lookup, 
											String title, 
											boolean required,
											String displayBinding,
											QueryDefinition query) {
		UIComponent c = autoComplete(listBinding,
										lookup.getBinding(),
										title,
										required,
										lookup.getDisabledConditionName(),
										displayBinding,
										query,
										lookup.getPixelWidth(),
										true);

		UIComponent result = panelGroup(false, false, false, null);
		List<UIComponent> children = c.getChildren();
		children.add(c);
		InputText text = textField(listBinding, 
									String.format("%s.%s", lookup.getBinding(), displayBinding), 
									title,
									required, 
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
	public UIComponent password(String listBinding, 
									org.skyve.impl.metadata.view.widget.bound.input.Password password,
									String title, 
									boolean required) {
		return password(listBinding,
							password.getBinding(), 
			                title,
			                required,
			                password.getDisabledConditionName(),
			                password.getPixelWidth(),
			                false);
	}

	@Override
	public UIComponent textArea(String listBinding, 
									TextArea text, 
									String title, 
									boolean required,
									Integer length) {
        return textArea(listBinding,
							text.getBinding(),
							title,
							required,
							text.getDisabledConditionName(),
							length,
							text.getPixelWidth(),
							text.getPixelHeight(),
							false);
	}
	
	@Override
	public UIComponent text(String listBinding, 
								TextField text, 
								String title, 
								boolean required,
								Integer length,
								org.skyve.domain.types.converters.Converter<?> converter,
								Format<?> format,
								Converter facesConverter) {
        return textField(listBinding,
							text.getBinding(),
							title,
							required,
							text.getDisabledConditionName(),
							length,
							facesConverter,
							text.getPixelWidth(),
							false);
	}
}
