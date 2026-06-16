package org.skyve.impl.sail.execution;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.NoOpViewVisitor;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
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

/**
 * Traverses an edit view and records tab paths that lead to a specific bound input.
 */
public class DataEnterViewVisitor extends NoOpViewVisitor {
	/**
	 * Stack of currently visited tabs from outer-most to inner-most traversal depth.
	 */
	private Deque<Tab> tabs = new ArrayDeque<>(4); // non-null elements
	/**
	 * Fully qualified binding path to locate.
	 */
	private String bindingToFind;
	/**
	 * Current prefix contributed by the active tabular binding scope.
	 */
	private String bindingPrefix = null;
	/**
	 * Matched tab-path results in encounter order.
	 */
	private List<List<Tab>> result = new ArrayList<>();
	
	/**
	 * Creates a visitor that collects tab paths leading to a specific binding in an edit view.
	 *
	 * @param customer the owning customer metadata
	 * @param module the owning module metadata
	 * @param document the owning document metadata
	 * @param view the view to traverse
	 * @param uxui the selected UX/UI profile
	 * @param bindingToFind the fully qualified binding to match
	 */
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

	/**
	 * Pushes the current tab onto the traversal stack before visiting tab contents.
	 *
	 * @param tab the tab metadata being entered
	 * @param parentVisible whether ancestor containers are visible
	 * @param parentEnabled whether ancestor containers are enabled
	 */
	@Override
	public void visitTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		tabs.push(tab);
	}
	
	/**
	 * Pops the current tab from the traversal stack after visiting tab contents.
	 *
	 * @param tab the tab metadata being exited
	 * @param parentVisible whether ancestor containers are visible
	 * @param parentEnabled whether ancestor containers are enabled
	 */
	@Override
	public void visitedTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		tabs.pop();
	}

	/**
	 * Sets the current binding prefix while traversing a data-grid scope.
	 *
	 * @param grid the data-grid metadata being entered
	 * @param parentVisible whether ancestor containers are visible
	 * @param parentEnabled whether ancestor containers are enabled
	 */
	@Override
	public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		bindingPrefix = grid.getBinding();
	}

	/**
	 * Clears the data-grid binding prefix after leaving a data-grid scope.
	 *
	 * @param grid the data-grid metadata being exited
	 * @param parentVisible whether ancestor containers are visible
	 * @param parentEnabled whether ancestor containers are enabled
	 */
	@Override
	public void visitedDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		bindingPrefix = null;
	}

	/**
	 * Evaluates a bound widget for a binding-path match and stores the active tab path on success.
	 *
	 * @param bound the bound widget metadata to evaluate
	 */
	private void match(Bound bound) {
		String binding = bound.getBinding();
		if (bindingPrefix != null) {
			binding = bindingPrefix = '.' +  binding;
		}

		if (bindingToFind.equals(binding)) {
			ArrayList<Tab> l = new ArrayList<>(tabs.size());
			tabs.descendingIterator().forEachRemaining(l::add);
			result.add(l);
		}
	}
	
	/**
	 * Evaluates a check-box binding for a target match.
	 *
	 * @param checkBox the check-box metadata being visited
	 * @param parentVisible whether ancestor containers are visible
	 * @param parentEnabled whether ancestor containers are enabled
	 */
	@Override
	public void visitCheckBox(CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		match(checkBox);
	}
	
	/**
	 * Evaluates a colour-picker binding for a target match.
	 *
	 * @param colour the colour-picker metadata being visited
	 * @param parentVisible whether ancestor containers are visible
	 * @param parentEnabled whether ancestor containers are enabled
	 */
	@Override
	public void visitColourPicker(ColourPicker colour, boolean parentVisible, boolean parentEnabled) {
		match(colour);
	}
	
	/**
	 * Evaluates a combo binding for a target match.
	 *
	 * @param combo the combo metadata being visited
	 * @param parentVisible whether ancestor containers are visible
	 * @param parentEnabled whether ancestor containers are enabled
	 */
	@Override
	public void visitCombo(Combo combo, boolean parentVisible, boolean parentEnabled) {
		match(combo);
	}
	
	/**
	 * Evaluates a geometry binding for a target match.
	 *
	 * @param geometry the geometry metadata being visited
	 * @param parentVisible whether ancestor containers are visible
	 * @param parentEnabled whether ancestor containers are enabled
	 */
	@Override
	public void visitGeometry(Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		match(geometry);
	}
	
	/**
	 * Evaluates an HTML binding for a target match.
	 *
	 * @param html the HTML metadata being visited
	 * @param parentVisible whether ancestor containers are visible
	 * @param parentEnabled whether ancestor containers are enabled
	 */
	@Override
	public void visitHTML(HTML html, boolean parentVisible, boolean parentEnabled) {
		match(html);
	}
	
	/**
	 * Evaluates a lookup-description binding for a target match.
	 *
	 * @param lookup the lookup-description metadata being visited
	 * @param parentVisible whether ancestor containers are visible
	 * @param parentEnabled whether ancestor containers are enabled
	 */
	@Override
	public void visitLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		match(lookup);
	}
	
	/**
	 * Evaluates a password binding for a target match.
	 *
	 * @param password the password metadata being visited
	 * @param parentVisible whether ancestor containers are visible
	 * @param parentEnabled whether ancestor containers are enabled
	 */
	@Override
	public void visitPassword(Password password, boolean parentVisible, boolean parentEnabled) {
		match(password);
	}
	
	/**
	 * Evaluates a radio binding for a target match.
	 *
	 * @param radio the radio metadata being visited
	 * @param parentVisible whether ancestor containers are visible
	 * @param parentEnabled whether ancestor containers are enabled
	 */
	@Override
	public void visitRadio(Radio radio, boolean parentVisible, boolean parentEnabled) {
		match(radio);
	}
	
	/**
	 * Evaluates a rich-text binding for a target match.
	 *
	 * @param richText the rich-text metadata being visited
	 * @param parentVisible whether ancestor containers are visible
	 * @param parentEnabled whether ancestor containers are enabled
	 */
	@Override
	public void visitRichText(RichText richText, boolean parentVisible, boolean parentEnabled) {
		match(richText);
	}
	
	/**
	 * Evaluates a slider binding for a target match.
	 *
	 * @param slider the slider metadata being visited
	 * @param parentVisible whether ancestor containers are visible
	 * @param parentEnabled whether ancestor containers are enabled
	 */
	@Override
	public void visitSlider(Slider slider, boolean parentVisible, boolean parentEnabled) {
		match(slider);
	}
	
	/**
	 * Evaluates a spinner binding for a target match.
	 *
	 * @param spinner the spinner metadata being visited
	 * @param parentVisible whether ancestor containers are visible
	 * @param parentEnabled whether ancestor containers are enabled
	 */
	@Override
	public void visitSpinner(Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		match(spinner);
	}
	
	/**
	 * Evaluates a text-area binding for a target match.
	 *
	 * @param text the text-area metadata being visited
	 * @param parentVisible whether ancestor containers are visible
	 * @param parentEnabled whether ancestor containers are enabled
	 */
	@Override
	public void visitTextArea(TextArea text, boolean parentVisible, boolean parentEnabled) {
		match(text);
	}
	
	/**
	 * Evaluates a text-field binding for a target match.
	 *
	 * @param text the text-field metadata being visited
	 * @param parentVisible whether ancestor containers are visible
	 * @param parentEnabled whether ancestor containers are enabled
	 */
	@Override
	public void visitTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
		match(text);
	}
}
