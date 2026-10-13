package org.skyve.impl.metadata.model.document;

import java.util.List;

import org.skyve.metadata.Ordering;

import jakarta.annotation.Nonnull;

/**
 * Implementation interface to deal with any Document Attribute that could be ordered.
 * Currently implemented by Collection and InverseMany.
 */
public interface OrderedAttribute {
	@Nonnull List<Ordering> getOrdering();
	boolean isComplexOrdering();
}
