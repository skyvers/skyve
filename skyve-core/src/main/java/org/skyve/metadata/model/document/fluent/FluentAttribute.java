package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.AbstractAttribute;
import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.view.fluent.FluentCheckBox;
import org.skyve.metadata.view.fluent.FluentColourPicker;
import org.skyve.metadata.view.fluent.FluentCombo;
import org.skyve.metadata.view.fluent.FluentContentImage;
import org.skyve.metadata.view.fluent.FluentContentLink;
import org.skyve.metadata.view.fluent.FluentGeometry;
import org.skyve.metadata.view.fluent.FluentHTML;
import org.skyve.metadata.view.fluent.FluentInputWidget;
import org.skyve.metadata.view.fluent.FluentLookupDescription;
import org.skyve.metadata.view.fluent.FluentPassword;
import org.skyve.metadata.view.fluent.FluentRadio;
import org.skyve.metadata.view.fluent.FluentRichText;
import org.skyve.metadata.view.fluent.FluentSlider;
import org.skyve.metadata.view.fluent.FluentSpinner;
import org.skyve.metadata.view.fluent.FluentTextArea;
import org.skyve.metadata.view.fluent.FluentTextField;

abstract class FluentAttribute<T extends FluentAttribute<T>> {
	protected FluentAttribute() {
		// nothing to see
	}

	@SuppressWarnings("unchecked")
	protected T from(Attribute attribute) {
		audited(attribute.isAudited());
		deprecated(attribute.isDeprecated());
		description(attribute.getDescription());
		displayName(attribute.getDisplayName());
		documentation(attribute.getDocumentation());
		name(attribute.getName());
		trackChanges(attribute.isTrackChanges());
		transientAttribute(attribute.isTransient());
		usage(attribute.getUsage());
		sensitivity(attribute.getSensitivity());
		
		if ((attribute instanceof AbstractAttribute abstractAttribute) && 
				(abstractAttribute.getDefaultWidgetReference() != null)) {
			InputWidget input = abstractAttribute.getDefaultInputWidget();
			if (input instanceof ContentImage image) {
				defaultWidget(new FluentContentImage().from(image));
			}
			else if (input instanceof ContentLink link) {
				defaultWidget(new FluentContentLink().from(link));
			}
			else if (input instanceof CheckBox check) {
				defaultWidget(new FluentCheckBox().from(check));
			}
			else if (input instanceof ColourPicker colour) {
				defaultWidget(new FluentColourPicker().from(colour));
			}
			else if (input instanceof Combo combo) {
				defaultWidget(new FluentCombo().from(combo));
			}
			else if (input instanceof Geometry geometry) {
				defaultWidget(new FluentGeometry().from(geometry));
			}
			else if (input instanceof HTML html) {
				defaultWidget(new FluentHTML().from(html));
			}
			else if (input instanceof LookupDescription lookup) {
				defaultWidget(new FluentLookupDescription().from(lookup));
			}
			else if (input instanceof Password password) {
				defaultWidget(new FluentPassword().from(password));
			}
			else if (input instanceof Radio radio) {
				defaultWidget(new FluentRadio().from(radio));
			}
			else if (input instanceof RichText text) {
				defaultWidget(new FluentRichText().from(text));
			}
			else if (input instanceof Slider slider) {
				defaultWidget(new FluentSlider().from(slider));
			}
			else if (input instanceof Spinner spinner) {
				defaultWidget(new FluentSpinner().from(spinner));
			}
			else if (input instanceof TextField text) {
				defaultWidget(new FluentTextField().from(text));
			}
			else if (input instanceof TextArea text) {
				defaultWidget(new FluentTextArea().from(text));
			}
			else {
				throw new IllegalStateException(input + " is not catered for");
			}
		}

		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T audited(boolean audited) {
		get().setAudited(audited);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T deprecated(boolean deprecated) {
		get().setDeprecated(deprecated);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T description(String description) {
		get().setDescription(description);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T displayName(String displayName) {
		get().setDisplayName(displayName);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T documentation(String documentation) {
		get().setDocumentation(documentation);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T name(String name) {
		get().setName(name);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T trackChanges(boolean trackChanges) {
		get().setTrackChanges(trackChanges);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T transientAttribute(boolean transientAttribute) {
		get().setTransient(transientAttribute);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T usage(UsageType usage) {
		get().setUsage(usage);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T sensitivity(Sensitivity sensitivity) {
		get().setSensitivity(sensitivity);
		return (T) this;
	}

	public T defaultWidget(FluentContentImage image) {
		return defaultWidgetReference(image);
	}

	public T defaultWidget(FluentContentLink link) {
		return defaultWidgetReference(link);
	}

	public T defaultWidget(FluentCheckBox check) {
		return defaultWidgetReference(check);
	}

	public T defaultWidget(FluentColourPicker colour) {
		return defaultWidgetReference(colour);
	}

	public T defaultWidget(FluentCombo combo) {
		return defaultWidgetReference(combo);
	}

	public T defaultWidget(FluentGeometry geometry) {
		return defaultWidgetReference(geometry);
	}

	public T defaultWidget(FluentHTML html) {
		return defaultWidgetReference(html);
	}

	public T defaultWidget(FluentLookupDescription lookup) {
		return defaultWidgetReference(lookup);
	}

	public T defaultWidget(FluentPassword password) {
		return defaultWidgetReference(password);
	}

	public T defaultWidget(FluentRadio radio) {
		return defaultWidgetReference(radio);
	}

	public T defaultWidget(FluentRichText richText) {
		return defaultWidgetReference(richText);
	}

	public T defaultWidget(FluentSlider slider) {
		return defaultWidgetReference(slider);
	}

	public T defaultWidget(FluentSpinner spinner) {
		return defaultWidgetReference(spinner);
	}

	public T defaultWidget(FluentTextArea textArea) {
		return defaultWidgetReference(textArea);
	}

	public T defaultWidget(FluentTextField textField) {
		return defaultWidgetReference(textField);
	}

	@SuppressWarnings("unchecked")
	private T defaultWidgetReference(FluentInputWidget<?> widget) {
		WidgetReference reference = null;
		if (widget != null) {
			reference = new WidgetReference();
			reference.setWidget(widget.get());
		}
		get().setDefaultWidgetReference(reference);
		return (T) this;
	}

	public abstract AbstractAttribute get();
}
