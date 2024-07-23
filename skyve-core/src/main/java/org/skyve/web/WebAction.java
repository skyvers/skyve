package org.skyve.web;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
public enum WebAction {
	l, // list
	c, // calendar
	t, // tree
	m, // map
	e // edit
}
