package org.skyve.metadata.view.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.view.access.ViewSingularUserAccessMetaData;

/**
 * A fluent helper builder class to construct and manipulate the {@link ViewSingularUserAccessMetaData} metadata.
 * 
 * @author brandon-klar
 */
public class FluentViewSingularAccess extends FluentViewUserAccess<FluentViewSingularAccess, ViewSingularUserAccessMetaData> {

	/**
	 * Creates a new FluentViewDocumentAggregateAccess
	 */
	public FluentViewSingularAccess() {
		access = new ViewSingularUserAccessMetaData();
	}

	/**
	 * Creates a new FluentModuleRoleDocumentAggregateAccess from the specified ViewSingularUserAccessMetaData.
	 */
	public FluentViewSingularAccess(ViewSingularUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentViewDocumentAggregateAccess from a runtime metadata.
	 */
	protected FluentViewSingularAccess from(Set<String> uxuis) {
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the document name for this FluentViewSingularAccess
	 */
	public FluentViewSingularAccess documentName(final String documentName) {
		access.setDocumentName(documentName);
		return this;
	}

	/**
	 * Returns the underlying view user access metadata from this builder.
	 */
	@Override
	public ViewSingularUserAccessMetaData get() {
		return access;
	}

}
