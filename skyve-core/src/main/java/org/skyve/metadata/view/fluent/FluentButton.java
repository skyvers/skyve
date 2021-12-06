package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.metadata.view.Action.ActionShow;

public class FluentButton extends FluentWidget implements FluentAbsoluteSize<FluentButton>,
															FluentConstrainableHeight<FluentButton> {
	private Button button = null;
	
	public FluentButton() {
		button = new Button();
	}
	
	public FluentButton(Button button) {
		this.button = button;
	}
	
	public FluentButton from(@SuppressWarnings("hiding") Button button) {

		actionName(button.getActionName());

		absoluteSize(button, this);
		constrainableHeight(button, this);

		show(button.getShow());
		return this;
	}

	public FluentButton actionName(String actionName) {
		button.setActionName(actionName);
		return this;
	}

	@Override
	public FluentButton pixelWidth(int width) {
		button.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public FluentButton pixelHeight(int height) {
		button.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	@Override
	public FluentButton minPixelHeight(int minPixelHeight) {
		button.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentButton maxPixelHeight(int maxPixelHeight) {
		button.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}
	
	public FluentButton show(ActionShow show) {
		button.setShow(show);
		return this;
	}

	@Override
	public Button get() {
		return button;
	}
}
