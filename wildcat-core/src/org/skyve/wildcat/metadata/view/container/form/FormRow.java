package org.skyve.wildcat.metadata.view.container.form;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.MetaData;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
public class FormRow implements MetaData {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 6724393610453650734L;

	private List<FormItem> items = new ArrayList<>();
	
	@XmlElementRef(type = FormItem.class)
	public List<FormItem> getItems() {
		return items;
	}
}
