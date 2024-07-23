package org.skyve.metadata.view.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.view.access.ViewDocumentAggregateUserAccessMetaData;

/**
 * A fluent helper builder class to construct and manipulate the {@link ViewDocumentAggregateUserAccessMetaData} metadata.
 * 
 * @author brandon-klar
 */
public class FluentViewDocumentAggregateAccess extends FluentViewUserAccess<FluentViewDocumentAggregateAccess, ViewDocumentAggregateUserAccessMetaData> {
	/**
	 * Creates a new FluentViewDocumentAggregateAccess
	 */
	public FluentViewDocumentAggregateAccess() {
		access = new ViewDocumentAggregateUserAccessMetaData();
	}

	/**
	 * Creates a new FluentModuleRoleDocumentAggregateAccess from the specified ViewDocumentAggregateUserAccessMetaData.
	 */
	public FluentViewDocumentAggregateAccess(ViewDocumentAggregateUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentViewDocumentAggregateAccess from a runtime metadata.
	 */
	protected FluentViewDocumentAggregateAccess from(String documentName, Set<String> uxuis) {
		documentName(documentName);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the document name for this FluentViewDocumentAggregateAccess.
	 */
	public FluentViewDocumentAggregateAccess documentName(final String documentName) {
		access.setDocumentName(documentName);
		return this;
	}

	/**
	 * Returns the underlying document aggregate user access metadata from this builder.
	 */
	@Override
	public ViewDocumentAggregateUserAccessMetaData get() {
		return access;
	}
}
