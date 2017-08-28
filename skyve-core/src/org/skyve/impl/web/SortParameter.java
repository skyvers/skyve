package org.skyve.impl.web;

import java.io.Serializable;

import org.skyve.metadata.SortDirection;

public class SortParameter implements Serializable, org.skyve.web.SortParameter {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 4250974309552241616L;

	private String by;
	private SortDirection direction;

	@Override
	public String getBy() {
		return by;
	}

	@Override
	public void setBy(String by) {
		this.by = by;
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
		setBy(string.substring(0, spaceIndex));
		setDirection(SortDirection.valueOf(string.substring(spaceIndex + 1)));
	}

	@Override
	public String toString() {
		return String.format("%s %s", by, direction.toString());
	}
}
