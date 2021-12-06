package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.metadata.view.TextOutput.Sanitisation;

public class FluentLabel extends FluentBoundWidget<FluentLabel> implements FluentAbsoluteSize<FluentLabel> {
	private Label label = null;
	
	public FluentLabel() {
		label = new Label();
	}

	public FluentLabel(Label label) {
		this.label = label;
	}
	
	public FluentLabel from(@SuppressWarnings("hiding") Label label) {

		value(label.getValue());
		forBinding(label.getFor());

		absoluteSize(label, this);

		invisibleConditionName(label.getInvisibleConditionName());
		formatted(label.getFormatted());
		textAlignment(label.getTextAlignment());
		escape(label.getEscape());
		sanitise(label.getSanitise());

		super.from(label);
		return this;
	}

	public FluentLabel value(String value) {
		label.setValue(value);
		return this;
	}

	public FluentLabel forBinding(String forBinding) {
		label.setFor(forBinding);
		return this;
	}

	public FluentLabel invisibleConditionName(String invisibleConditionName) {
		label.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	public FluentLabel formatted(Boolean formatted) {
		label.setFormatted(formatted);
		return this;
	}

	public FluentLabel textAlignment(HorizontalAlignment textAlignment) {
		label.setTextAlignment(textAlignment);
		return this;
	}

	public FluentLabel escape(Boolean escape) {
		label.setEscape(escape);
		return this;
	}

	public FluentLabel sanitise(Sanitisation sanitise) {
		label.setSanitise(sanitise);
		return this;
	}
	@Override
	public FluentLabel pixelWidth(int width) {
		label.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public FluentLabel pixelHeight(int height) {
		label.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	@Override
	public Label get() {
		return label;
	}
}
