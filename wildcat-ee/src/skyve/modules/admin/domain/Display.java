package modules.admin.domain;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.wildcat.domain.AbstractTransientBean;

/**
 * Display
 * 
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
public class Display extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "Display";



	@Override
	@XmlTransient
	public String getBizModule() {
		return Display.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Display.DOCUMENT_NAME;
	}

	public static Display newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Display) && 
					this.getBizId().equals(((Display) o).getBizId()));
	}
}
