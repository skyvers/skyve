package org.skyve.web;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
public enum WebAction {
	l, // list
	c, // calendar
	t, // tree
	m, // map
	e // edit
}
