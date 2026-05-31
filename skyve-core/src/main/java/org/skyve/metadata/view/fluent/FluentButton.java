package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.metadata.view.Action.ActionShow;

/**
 * Builds {@link Button} widget metadata using a fluent API.
 */
public class FluentButton extends FluentWidget implements FluentAbsoluteSize<FluentButton>,
															FluentConstrainableHeight<FluentButton> {
	private Button button = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link Button} metadata instance.
	 */
	public FluentButton() {
		button = new Button();
	}
	
	/**
	 * Creates a fluent builder backed by the supplied button metadata instance.
	 *
	 * @param button the metadata instance to mutate
	 */
	public FluentButton(Button button) {
		this.button = button;
	}
	
	/**
	 * Copies button metadata into this fluent builder.
	 */
	public FluentButton from(@SuppressWarnings("hiding") Button button) {

		actionName(button.getActionName());

		absoluteSize(button, this);
		constrainableHeight(button, this);

		show(button.getShow());
		return this;
	}

	/**
	 * Sets the action name invoked when the button is triggered.
	 *
	 * @param actionName the action identifier
	 * @return this builder
	 */
	public FluentButton actionName(String actionName) {
		button.setActionName(actionName);
		return this;
	}

	/**
	 * Sets the absolute pixel width for this button widget.
	 *
	 * @param width the width in pixels
	 * @return this builder
	 */
	@Override
	public FluentButton pixelWidth(int width) {
		button.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the absolute pixel height for this button widget.
	 *
	 * @param height the height in pixels
	 * @return this builder
	 */
	@Override
	public FluentButton pixelHeight(int height) {
		button.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Sets the minimum allowed pixel height for this button widget.
	 *
	 * @param minPixelHeight the minimum height in pixels
	 * @return this builder
	 */
	@Override
	public FluentButton minPixelHeight(int minPixelHeight) {
		button.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	/**
	 * Sets the maximum allowed pixel height for this button widget.
	 *
	 * @param maxPixelHeight the maximum height in pixels
	 * @return this builder
	 */
	@Override
	public FluentButton maxPixelHeight(int maxPixelHeight) {
		button.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}
	
	/**
	 * Sets the action visibility policy used by this button.
	 *
	 * @param show the action show mode
	 * @return this builder
	 */
	public FluentButton show(ActionShow show) {
		button.setShow(show);
		return this;
	}

	/**
	 * Returns the wrapped button metadata instance.
	 *
	 * @return the mutable button metadata being configured
	 */
	@Override
	public Button get() {
		return button;
	}
}
