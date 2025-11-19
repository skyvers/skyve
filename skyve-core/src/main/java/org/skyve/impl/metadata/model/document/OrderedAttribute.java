package org.skyve.impl.metadata.model.document;

import java.util.List;

import org.skyve.metadata.Ordering;

/**
 * Implementation interface to deal with any Document Attribute that could be ordered.
 * Currently implemented by Collection and InverseMany.
 */
public interface OrderedAttribute {
	List<Ordering> getOrdering();
	boolean isComplexOrdering();
}
