package org.skyve.impl.metadata.view.widget.bound.input;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

/**
 * Represents auto-complete mechanisms in Skyve
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public enum CompleteType {
	previous, // a distinct set of previous values entered in this text field (from the data store)
	suggest, // a set of values from Bizlet.complete() that represent suggestions but new values may be entered
	constrain // a set of values from Bizlet.complete() where the typed value must be completed and constrained
}
