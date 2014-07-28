package org.skyve.wildcat.metadata.view.widget.bound.input;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
public class CheckMembership extends ChangeableInputWidget implements MembershipWidget {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 4432121900038592599L;
}
