package org.skyve.wildcat.web;

import java.io.Serializable;

import org.skyve.metadata.SortDirection;

public class SortParameter implements Serializable, org.skyve.web.SortParameter {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 4250974309552241616L;

	private String binding;
	private SortDirection direction;

	@Override
	public String getBinding() {
		return binding;
	}

	@Override
	public void setBinding(String binding) {
		this.binding = binding;
	}

	@Override
	public SortDirection getDirection() {
		return direction;
	}

	@Override
	public void setDirection(SortDirection direction) {
		this.direction = direction;
	}

	public void populateFromString(String string) {
		int spaceIndex = string.indexOf(' ');
		if (spaceIndex < 0) {
			throw new IllegalStateException("No space in the string value");
		}
		setBinding(string.substring(0, spaceIndex));
		setDirection(SortDirection.valueOf(string.substring(spaceIndex + 1)));
	}

	@Override
	public String toString() {
		return new StringBuilder(32).append(binding).append(' ').append(direction.toString()).toString();
	}
}
