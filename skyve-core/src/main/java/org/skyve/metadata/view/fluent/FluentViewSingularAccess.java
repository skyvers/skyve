package org.skyve.metadata.view.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.view.access.ViewSingularUserAccessMetaData;

import jakarta.annotation.Nonnull;

/**
 * A fluent helper builder class to construct and manipulate the {@link ViewSingularUserAccessMetaData} metadata.
 * 
 * @author brandon-klar
 */
public class FluentViewSingularAccess extends FluentViewUserAccess<FluentViewSingularAccess, ViewSingularUserAccessMetaData> {
	/**
	 * Creates a new FluentViewSingularAccess
	 */
	public FluentViewSingularAccess() {
		access = new ViewSingularUserAccessMetaData();
	}

	/**
	 * Creates a new FluentViewSingularAccess from the specified ViewSingularUserAccessMetaData.
	 */
	public FluentViewSingularAccess(@Nonnull ViewSingularUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentViewSingularAccess from a runtime metadata.
	 */
	protected @Nonnull FluentViewSingularAccess from(@Nonnull String documentName, @Nonnull Set<String> uxuis) {
		documentName(documentName);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the document name for this FluentViewSingularAccess
	 */
	public @Nonnull FluentViewSingularAccess documentName(final @Nonnull String documentName) {
		access.setDocumentName(documentName);
		return this;
	}

	/**
	 * Returns the underlying view user access metadata from this builder.
	 */
	@Override
	public @Nonnull ViewSingularUserAccessMetaData get() {
		return access;
	}
}
