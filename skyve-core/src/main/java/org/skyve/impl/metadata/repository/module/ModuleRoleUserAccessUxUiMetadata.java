package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated UX/UI scope name for a user-access grant within a module role.
 *
 * <p>Carries a single UX/UI identifier (e.g. {@code desktop}, {@code phone})
 * to restrict the enclosing access grant to that rendering context.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ModuleRoleUserAccessMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public class ModuleRoleUserAccessUxUiMetadata extends NamedMetaData {
	private static final long serialVersionUID = 6147710006677424683L;
}
