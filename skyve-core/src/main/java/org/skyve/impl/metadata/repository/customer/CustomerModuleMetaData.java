package org.skyve.impl.metadata.repository.customer;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "module")
public class CustomerModuleMetaData extends NamedMetaData {
	private static final long serialVersionUID = -4251806306391755042L;

	private FormLabelLayout formLabelLayout;

	public FormLabelLayout getFormLabelLayout() {
		return formLabelLayout;
	}

	@XmlAttribute
	public void setFormLabelLayout(FormLabelLayout formLabelLayout) {
		this.formLabelLayout = formLabelLayout;
	}
}
