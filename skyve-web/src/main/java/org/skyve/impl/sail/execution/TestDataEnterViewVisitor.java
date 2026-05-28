package org.skyve.impl.sail.execution;

import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.NoOpViewVisitor;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.form.FormItem;
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
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.impl.web.faces.actions.GetSelectItemsAction;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.util.Binder.TargetMetaData;

import jakarta.faces.model.SelectItem;

/**
 * A specialized {@link NoOpViewVisitor} that traverses a view and generates a series of
 * {@link Step}'s to simulate data entry for automated testing.
 * 
 * @author mike
 */
public class TestDataEnterViewVisitor extends NoOpViewVisitor {
	private Bean bean;
	private List<Step> scalarSteps = new ArrayList<>();
	private boolean inDataWidget = false;
	private Boolean requiredWidget = null;
	
	/**
	 * Creates a visitor that generates test data-entry steps for visible and enabled bound widgets.
	 *
	 * @param customer the current customer metadata context
	 * @param module the current module metadata context
	 * @param document the current document metadata context
	 * @param view the view metadata being traversed
	 * @param uxui the active UX/UI name
	 * @param bean the bean supplying source values and condition evaluation
	 */
	public TestDataEnterViewVisitor(
			CustomerImpl customer,
			ModuleImpl module,
			DocumentImpl document,
			ViewImpl view,
			String uxui,
			Bean bean) {
		super(customer, module, document, view, uxui);

		this.bean = bean;
	}

	/**
	 * Adds a tab-selection step when the tab is visible and enabled in the current traversal context.
	 *
	 * @param tab the tab metadata being visited
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 */
	@Override
	public void visitTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		if (parentVisible && parentEnabled && visible(tab) && enabled(tab)) {
			TabSelect select = new TabSelect();
			select.setTabPath(tab.getLocalisedTitle());

			scalarSteps.add(select);
		}
	}
	
	/**
	 * Captures required-state overrides declared by the current form item.
	 *
	 * @param item the form item metadata being entered
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 */
	@Override
	public void visitFormItem(FormItem item, boolean parentVisible, boolean parentEnabled) {
		requiredWidget = item.getRequired();
	}
	
	/**
	 * Clears required-state overrides after leaving the current form item.
	 *
	 * @param item the form item metadata being exited
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 */
	@Override
	public void visitedFormItem(FormItem item, boolean parentVisible, boolean parentEnabled) {
		requiredWidget = null;
	}
	
	/**
	 * Generates data-entry for a check-box when visible, enabled, and outside tabular widgets.
	 *
	 * @param checkBox the check-box metadata being visited
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 */
	@Override
	public void visitCheckBox(CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(checkBox, parentVisible, parentEnabled, visible(checkBox), enabled(checkBox));
	}

	/**
	 * No-op for check-membership widgets in current test data generation strategy.
	 *
	 * @param membership the check-membership widget metadata
	 * @param parentVisible whether parent containers are visible
	 * @param parentEnabled whether parent containers are enabled
	 */
	@Override
	public void visitCheckMembership(CheckMembership membership, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}
	
	/**
	 * Generates data-entry for a colour-picker when visible, enabled, and outside tabular widgets.
	 *
	 * @param colour the colour-picker metadata being visited
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 */
	@Override
	public void visitColourPicker(ColourPicker colour, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(colour, parentVisible, parentEnabled, visible(colour), enabled(colour));
	}

	/**
	 * Generates data-entry for a combo when visible, enabled, and outside tabular widgets.
	 *
	 * @param combo the combo metadata being visited
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 */
	@Override
	public void visitCombo(Combo combo, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(combo, parentVisible, parentEnabled, visible(combo), enabled(combo));
	}
	
	/**
	 * No-op for content-image widgets in current test data generation strategy.
	 *
	 * @param image the content-image widget metadata
	 * @param parentVisible whether parent containers are visible
	 * @param parentEnabled whether parent containers are enabled
	 */
	@Override
	public void visitContentImage(ContentImage image, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}
	
	/**
	 * No-op for content-link widgets in current test data generation strategy.
	 *
	 * @param link the content-link widget metadata
	 * @param parentVisible whether parent containers are visible
	 * @param parentEnabled whether parent containers are enabled
	 */
	@Override
	public void visitContentLink(ContentLink link, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	/**
	 * Marks entry into a data-grid scope so scalar generation is deferred.
	 *
	 * @param grid the data-grid metadata being entered
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 */
	@Override
	public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		inDataWidget = true;
	}

	/**
	 * Marks exit from a data-grid scope so scalar generation resumes.
	 *
	 * @param grid the data-grid metadata being exited
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 */
	@Override
	public void visitedDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		inDataWidget = false;
	}

	/**
	 * Marks entry into a data-repeater scope so scalar generation is deferred.
	 *
	 * @param repeater the data-repeater metadata being entered
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 */
	@Override
	public void visitDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		inDataWidget = true;
	}
	
	/**
	 * Marks exit from a data-repeater scope so scalar generation resumes.
	 *
	 * @param repeater the data-repeater metadata being exited
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 */
	@Override
	public void visitedDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		inDataWidget = false;
	}
	
	/**
	 * No-op for geometry widgets in current test data generation strategy.
	 *
	 * @param geometry the geometry widget metadata
	 * @param parentVisible whether parent containers are visible
	 * @param parentEnabled whether parent containers are enabled
	 */
	@Override
	public void visitGeometry(Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}
	
	/**
	 * No-op for lookup-description widgets in current test data generation strategy.
	 *
	 * @param lookup the lookup-description widget metadata
	 * @param parentVisible whether parent containers are visible
	 * @param parentEnabled whether parent containers are enabled
	 */
	@Override
	public void visitLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	/**
	 * Generates data-entry for a password input when visible, enabled, and outside tabular widgets.
	 *
	 * @param password the password metadata being visited
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 */
	@Override
	public void visitPassword(Password password, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(password, parentVisible, parentEnabled, visible(password), enabled(password));
	}
	
	/**
	 * Generates data-entry for a radio input when visible, enabled, and outside tabular widgets.
	 *
	 * @param radio the radio metadata being visited
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 */
	@Override
	public void visitRadio(Radio radio, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(radio, parentVisible, parentEnabled, visible(radio), enabled(radio));
	}
	
	/**
	 * Generates data-entry for rich-text input when visible, enabled, and outside tabular widgets.
	 *
	 * @param richText the rich-text metadata being visited
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 */
	@Override
	public void visitRichText(RichText richText, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(richText, parentVisible, parentEnabled, visible(richText), enabled(richText));
	}
	
	/**
	 * Generates data-entry for slider input when visible, enabled, and outside tabular widgets.
	 *
	 * @param slider the slider metadata being visited
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 */
	@Override
	public void visitSlider(Slider slider, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(slider, parentVisible, parentEnabled, visible(slider), enabled(slider));
	}
	
	/**
	 * Generates data-entry for spinner input when visible, enabled, and outside tabular widgets.
	 *
	 * @param spinner the spinner metadata being visited
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 */
	@Override
	public void visitSpinner(Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(spinner, parentVisible, parentEnabled, visible(spinner), enabled(spinner));
	}

	/**
	 * Generates data-entry for text-area input when visible, enabled, and outside tabular widgets.
	 *
	 * @param text the text-area metadata being visited
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 */
	@Override
	public void visitTextArea(TextArea text, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(text, parentVisible, parentEnabled, visible(text), enabled(text));
	}
	
	/**
	 * Generates data-entry for text-field input when visible, enabled, and outside tabular widgets.
	 *
	 * @param text the text-field metadata being visited
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 */
	@Override
	public void visitTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(text, parentVisible, parentEnabled, visible(text), enabled(text));
	}
	
	/**
	 * Converts a bound widget value into a {@link DataEnter} step and appends it to the scalar-step sequence.
	 *
	 * @param bound the bound metadata item providing the source binding
	 * @param parentVisible whether ancestor containers are currently visible
	 * @param parentEnabled whether ancestor containers are currently enabled
	 * @param visible the effective visibility for the current widget
	 * @param enabled the effective enabled-state for the current widget
	 */
	private void addDataEnter(Bound bound, boolean parentVisible, boolean parentEnabled, boolean visible, boolean enabled) {
		if (parentVisible && parentEnabled && visible && enabled && !inDataWidget) {
			String binding = bound.getBinding();

			String value = null;

			// Checkbox needs to test for true or false, not yes/no
			if (bound instanceof CheckBox) {
				Boolean bool = (Boolean) BindUtil.get(bean, binding);
				if (bool != null) {
					value = bool.toString();
				}
			} else {
				try {
					value = BindUtil.getDisplay(customer, bean, binding);
				} catch (@SuppressWarnings("unused") Exception e) {
					// Nothing to see here
				}
			}

			// Need to convert the value of a combo or radio into an index
			if (bound instanceof Combo || bound instanceof Radio) {
				if (value == null) {
					value = "0";
				} else {
					boolean includeEmptyItem = false;

					// Include the empty item for a combo if it is not required
					if (bound instanceof Combo) {
						if (requiredWidget == null) {
							TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
							Attribute attribute = target.getAttribute();
							if (attribute != null) {
								includeEmptyItem = !attribute.isRequired();
							}
						} else if (Boolean.FALSE.equals(requiredWidget)) {
							includeEmptyItem = true;
						}
					}
					
					GetSelectItemsAction get = new GetSelectItemsAction(bean, new MockWebContext(), binding, includeEmptyItem);
					boolean found = false;
					int index = 0;

					try {
						for (SelectItem item : get.execute()) {
							if (value.equals(item.getLabel())) {
								found = true;

								break;
							}

							index++;
						}
					} catch (@SuppressWarnings("unused") Exception e) {
						String message = String.format(
								"WARNING: Can't set value for combo/radio [%s] in document %s.%s as there were no domain values.",
								binding,
								bean.getBizModule(),
								bean.getBizDocument());
						LOGGER.warn(message);
						Comment comment = new Comment();
						comment.setComment(message);

						scalarSteps.add(comment);

						return;
					}

					if (found) {
						value = String.valueOf(index);
					} else {
						String message = String.format(
								"WARNING: Can't set value '%s' for combo/radio [%s] in document %s.%s as it is not a valid value.",
								value,
								binding,
								bean.getBizModule(),
								bean.getBizDocument());
						LOGGER.warn(message);
						Comment comment = new Comment();
						comment.setComment(message);

						scalarSteps.add(comment);

						value = null;
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
	
	/**
	 * Evaluates visibility by negating the configured invisible condition result.
	 *
	 * @param invisible metadata providing the invisible-condition name
	 * @return {@code true} when the widget is effectively visible
	 */
	@Override
	protected boolean visible(Invisible invisible) {
		return evaluateConditionInOppositeSense(invisible.getInvisibleConditionName());
	}

	/**
	 * Evaluates enabled-state by negating the configured disabled condition result.
	 *
	 * @param disableable metadata providing the disabled-condition name
	 * @return {@code true} when the widget is effectively enabled
	 */
	@Override
	protected boolean enabled(Disableable disableable) {
		return evaluateConditionInOppositeSense(disableable.getDisabledConditionName());
	}
	
	/**
	 * Evaluates a bean condition and returns the logical opposite of its value.
	 *
	 * @param conditionName the bean condition name to evaluate; may be {@code null}
	 * @return {@code true} when condition is absent or evaluates to {@code false}
	 */
	private boolean evaluateConditionInOppositeSense(String conditionName) {
		boolean result = true;

		if (conditionName != null) {
			result = !bean.evaluateCondition(conditionName);
		}
		
		return result;
	}
	
	/**
	 * Returns generated scalar data-entry steps in visit order.
	 *
	 * @return mutable list of generated scalar SAIL steps
	 */
	public List<Step> getScalarSteps() {
		return scalarSteps;
	}
}
