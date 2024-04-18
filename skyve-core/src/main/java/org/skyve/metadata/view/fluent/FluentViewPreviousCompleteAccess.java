package org.skyve.metadata.view.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.view.access.ViewPreviousCompleteUserAccessMetaData;

/**
 * A fluent helper builder class to construct and manipulate the {@link ViewPreviousCompleteUserAccessMetaData} metadata.
 * 
 * @author brandon-klar
 */
public class FluentViewPreviousCompleteAccess extends FluentViewUserAccess<FluentViewPreviousCompleteAccess, ViewPreviousCompleteUserAccessMetaData> {
	/**
	 * Creates a new FluentViewPreviousCompleteAccess
	 */
	public FluentViewPreviousCompleteAccess() {
		access = new ViewPreviousCompleteUserAccessMetaData();
	}

	/**
	 * Creates a new FluentViewPreviousCompleteAccess from the specified ViewPreviousCompleteUserAccessMetaData.
	 */
	public FluentViewPreviousCompleteAccess(ViewPreviousCompleteUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentViewPreviousCompleteAccess from a runtime metadata.
	 */
	protected FluentViewPreviousCompleteAccess from(String binding, Set<String> uxuis) {
		binding(binding);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the binding for this FluentViewPreviousCompleteAccess.
	 */
	public FluentViewPreviousCompleteAccess binding(final String binding) {
		access.setBinding(binding);
		return this;
	}

	/**
	 * Returns the underlying view user access metadata from this builder.
	 */
	@Override
	public ViewPreviousCompleteUserAccessMetaData get() {
		return access;
	}
}
