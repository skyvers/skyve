package org.skyve.web;

import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.COMMON_NAMESPACE)
public enum WebAction {
	g, // grid
	c, // calendar
	t, // tree
	m, // map
	e // edit
}
