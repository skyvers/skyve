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

	@Override
	public void visitTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		if (parentVisible && parentEnabled && visible(tab) && enabled(tab)) {
			TabSelect select = new TabSelect();
			select.setTabPath(tab.getLocalisedTitle());

			scalarSteps.add(select);
		}
	}
	
	@Override
	public void visitFormItem(FormItem item, boolean parentVisible, boolean parentEnabled) {
		requiredWidget = item.getRequired();
	}
	
	@Override
	public void visitedFormItem(FormItem item, boolean parentVisible, boolean parentEnabled) {
		requiredWidget = null;
	}
	
	@Override
	public void visitCheckBox(CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(checkBox, parentVisible, parentEnabled, visible(checkBox), enabled(checkBox));
	}

	@Override
	public void visitCheckMembership(CheckMembership membership, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}
	
	@Override
	public void visitColourPicker(ColourPicker colour, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(colour, parentVisible, parentEnabled, visible(colour), enabled(colour));
	}

	@Override
	public void visitCombo(Combo combo, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(combo, parentVisible, parentEnabled, visible(combo), enabled(combo));
	}
	
	@Override
	public void visitContentImage(ContentImage image, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}
	
	@Override
	public void visitContentLink(ContentLink link, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
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
		// TODO Auto-generated method stub
	}
	
	@Override
	public void visitLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// TODO Auto-generated method stub
	}

	@Override
	public void visitPassword(Password password, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(password, parentVisible, parentEnabled, visible(password), enabled(password));
	}
	
	@Override
	public void visitRadio(Radio radio, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(radio, parentVisible, parentEnabled, visible(radio), enabled(radio));
	}
	
	@Override
	public void visitRichText(RichText richText, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(richText, parentVisible, parentEnabled, visible(richText), enabled(richText));
	}
	
	@Override
	public void visitSlider(Slider slider, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(slider, parentVisible, parentEnabled, visible(slider), enabled(slider));
	}
	
	@Override
	public void visitSpinner(Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(spinner, parentVisible, parentEnabled, visible(spinner), enabled(spinner));
	}

	@Override
	public void visitTextArea(TextArea text, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(text, parentVisible, parentEnabled, visible(text), enabled(text));
	}
	
	@Override
	public void visitTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
		addDataEnter(text, parentVisible, parentEnabled, visible(text), enabled(text));
	}
	
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
	
	@Override
	protected boolean visible(Invisible invisible) {
		return evaluateConditionInOppositeSense(invisible.getInvisibleConditionName());
	}

	@Override
	protected boolean enabled(Disableable disableable) {
		return evaluateConditionInOppositeSense(disableable.getDisabledConditionName());
	}
	
	private boolean evaluateConditionInOppositeSense(String conditionName) {
		boolean result = true;

		if (conditionName != null) {
			result = !bean.evaluateCondition(conditionName);
		}
		
		return result;
	}
	
	public List<Step> getScalarSteps() {
		return scalarSteps;
	}
}
