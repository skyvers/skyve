package org.skyve.impl.metadata.repository.view.access;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated UX/UI scope name for a user-access constraint on a view.
 *
 * <p>Carries a single UX/UI identifier (e.g. {@code desktop}, {@code phone}) to
 * restrict the enclosing access constraint to a specific rendering context.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ViewUserAccessMetaData
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class ViewUserAccessUxUiMetadata extends NamedMetaData {
	private static final long serialVersionUID = -3523148277675838881L;
}
