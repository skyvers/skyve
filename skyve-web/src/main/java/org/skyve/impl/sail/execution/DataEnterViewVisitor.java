package org.skyve.impl.sail.execution;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.NoOpViewVisitor;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.metadata.view.widget.bound.Bound;

public class DataEnterViewVisitor extends NoOpViewVisitor {
	private Stack<Tab> tabs = new Stack<>();
	private String bindingToFind;
	private String bindingPrefix = null;
	private List<List<Tab>> result = new ArrayList<>();
	
	protected DataEnterViewVisitor(CustomerImpl customer, 
									ModuleImpl module,
									DocumentImpl document,
									ViewImpl view,
									String uxui,
									String bindingToFind) {
		super(customer, module, document, view, uxui);
// TODO account for indexed & mapped bindings and ElementById() on the way in
		this.bindingToFind = bindingToFind;
	}

	@Override
	public void visitTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		tabs.push(tab);
	}
	
	@Override
	public void visitedTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		tabs.pop();
	}

	@Override
	public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		bindingPrefix = grid.getBinding();
	}

	@Override
	public void visitedDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		bindingPrefix = null;
	}

	private void match(Bound bound) {
		String binding = bound.getBinding();
		if (bindingPrefix == null) {
			binding = String.format("%s.%s", bindingPrefix, binding);
		}

		if (bindingToFind.equals(binding)) {
			result.add(new ArrayList<>(tabs));
		}
	}
	
	@Override
	public void visitCheckBox(CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		match(checkBox);
	}
	
	@Override
	public void visitColourPicker(ColourPicker colour, boolean parentVisible, boolean parentEnabled) {
		match(colour);
	}
	
	@Override
	public void visitCombo(Combo combo, boolean parentVisible, boolean parentEnabled) {
		match(combo);
	}
	
	@Override
	public void visitContentImage(ContentImage image, boolean parentVisible, boolean parentEnabled) {
		match(image);
	}
	
	@Override
	public void visitContentLink(ContentLink link, boolean parentVisible, boolean parentEnabled) {
		match(link);
	}
	
	@Override
	public void visitGeometry(Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		match(geometry);
	}
	
	@Override
	public void visitHTML(HTML html, boolean parentVisible, boolean parentEnabled) {
		match(html);
	}
	
	@Override
	public void visitLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		match(lookup);
	}
	
	@Override
	public void visitPassword(Password password, boolean parentVisible, boolean parentEnabled) {
		match(password);
	}
	
	@Override
	public void visitRadio(Radio radio, boolean parentVisible, boolean parentEnabled) {
		match(radio);
	}
	
	@Override
	public void visitRichText(RichText richText, boolean parentVisible, boolean parentEnabled) {
		match(richText);
	}
	
	@Override
	public void visitSlider(Slider slider, boolean parentVisible, boolean parentEnabled) {
		match(slider);
	}
	
	@Override
	public void visitSpinner(Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		match(spinner);
	}
	
	@Override
	public void visitTextArea(TextArea text, boolean parentVisible, boolean parentEnabled) {
		match(text);
	}
	
	@Override
	public void visitTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
		match(text);
	}
}
