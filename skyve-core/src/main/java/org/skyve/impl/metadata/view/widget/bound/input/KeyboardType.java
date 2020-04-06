package org.skyve.impl.metadata.view.widget.bound.input;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

/**
 * Represents different keyboard modes on mobile devices.
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public enum KeyboardType {
	numeric,
	tel,
	decimal,
	email,
	url,
	search
}
