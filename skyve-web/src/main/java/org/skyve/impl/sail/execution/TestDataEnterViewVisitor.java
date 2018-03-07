package org.skyve.impl.sail.execution;

import java.util.ArrayList;
import java.util.List;

import javax.faces.model.SelectItem;

import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.NoOpViewVisitor;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.web.faces.actions.GetSelectItemsAction;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomOut;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew;
import org.skyve.metadata.view.widget.bound.Bound;

public class TestDataEnterViewVisitor extends NoOpViewVisitor {
	private Bean bean;
	private List<Step> scalarSteps = new ArrayList<>();
	private List<Step> dataGridSteps = new ArrayList<>();
	private boolean inDataWidget = false;
	
	protected TestDataEnterViewVisitor(CustomerImpl customer, 
										ModuleImpl module,
										DocumentImpl document,
										ViewImpl view,
										Bean bean) {
		super(customer, module, document, view);
		this.bean = bean;
	}

	@Override
	public void visitTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		TabSelect select = new TabSelect();
		select.setTabPath(tab.getTitle());
		scalarSteps.add(select);
		dataGridSteps.add(select);
	}
	
	@Override
	public void visitCheckBox(CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(checkBox);
	}

	@Override
	public void visitCheckMembership(CheckMembership membership, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}
	
	@Override
	public void visitColourPicker(ColourPicker colour, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(colour);
	}

	@Override
	public void visitCombo(Combo combo, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(combo);
	}
	
	@Override
	public void visitContentImage(ContentImage image, boolean parentVisible, boolean parentEnabled) {
		// TODO implement in selenese
//		addDataEnter(image);
	}
	
	@Override
	public void visitContentLink(ContentLink link, boolean parentVisible, boolean parentEnabled) {
		// TODO implement in selenese
//		addDataEnter(link);
	}

	@Override
	public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		DataGridNew nu = new DataGridNew();
		nu.setBinding(grid.getBinding());
		dataGridSteps.add(nu);
		
		dataGridSteps.add(new TestDataEnter());
		dataGridSteps.add(new ZoomOut());

		inDataWidget = true;
	}

	@Override
	public void visitedDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		inDataWidget = false;
	}

	@Override
	public void visitDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		inDataWidget = true;
	}
	
	@Override
	public void visitedDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		inDataWidget = false;
	}
	
	@Override
	public void visitGeometry(Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		// TODO implement in selenese
//		addDataEnter(geometry);
	}
	
	@Override
	public void visitLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// TODO implement in selenese - should this even be done?
//		addDataEnter(lookup);
	}

	@Override
	public void visitPassword(Password password, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(password);
	}
	
	@Override
	public void visitRadio(Radio radio, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(radio);
	}
	
	@Override
	public void visitRichText(RichText richText, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(richText);
	}
	
	@Override
	public void visitSlider(Slider slider, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(slider);
	}
	
	@Override
	public void visitSpinner(Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(spinner);
	}

	@Override
	public void visitTextArea(TextArea text, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(text);
	}
	
	@Override
	public void visitTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(text);
	}
	
	private void addDataEnter(Bound bound) {
		if (! inDataWidget) {
			String binding = bound.getBinding();
			String value = null;
			// Checkbox needs to test for true or false,  not Yes/No
			if (bound instanceof CheckBox) {
				Boolean bool = (Boolean) BindUtil.get(bean, binding);
				if (bool != null) {
					value = bool.toString();
				}
			}
			else {
				value = BindUtil.getDisplay(customer, bean, binding);
			}

			// Need to change the value in the combo into an index
			if (bound instanceof Combo) {
				if (value == null) {
					value = "0";
				}
				else {
					GetSelectItemsAction get = new GetSelectItemsAction(bean, binding, true);
					boolean found = false;
					int index = 0;
					for (SelectItem item : get.execute()) {
						if (value.equals(item.getLabel())) {
							found = true;
							break;
						}
						index++;
					}
					if (found) {
						value = String.valueOf(index);
					}
					else {
						throw new MetaDataException(String.format("Value %s does not exist in combo [%s]. Check the test data factory!", value, binding));
					}
				}
			}
			
			if (value != null) {
				DataEnter enter = new DataEnter();
				enter.setBinding(binding);
				enter.setValue(value);
				scalarSteps.add(enter);
			}
		}
	}
	
	List<Step> getScalarSteps() {
		return scalarSteps;
	}
	
	List<Step> getDataGridSteps() {
		return dataGridSteps;
	}
}
