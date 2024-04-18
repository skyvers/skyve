package org.skyve.metadata.view.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.view.access.ViewContentUserAccessMetaData;

/**
 * A fluent helper builder class to construct and manipulate the {@link ViewContentUserAccessMetaData} metadata.
 * 
 * @author mike
 */
public class FluentViewContentAccess extends FluentViewUserAccess<FluentViewContentAccess, ViewContentUserAccessMetaData> {
	/**
	 * Creates a new FluentViewContentAccess
	 */
	public FluentViewContentAccess() {
		access = new ViewContentUserAccessMetaData();
	}

	/**
	 * Creates a new FluentViewContentAccess from the specified ViewContentUserAccessMetaData.
	 */
	public FluentViewContentAccess(ViewContentUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentViewContentAccess from a runtime metadata.
	 */
	protected FluentViewContentAccess from(String binding, Set<String> uxuis) {
		binding(binding);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the binding for this FluentViewContentAccess.
	 */
	public FluentViewContentAccess binding(final String binding) {
		access.setBinding(binding);
		return this;
	}

	/**
	 * Returns the underlying view user access metadata from this builder.
	 */
	@Override
	public ViewContentUserAccessMetaData get() {
		return access;
	}
}
