package customers.test.modules.test.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import modules.test.domain.OverrideSingleSub;

/**
 * Override Single Sub
 * <br/>
 * The test customer's override of OverrideSingleSub, adding customerText.
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class OverrideSingleSubOverride extends OverrideSingleSub {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String customerTextPropertyName = "customerText";

	/**
	 * Customer Text
	 **/
	private String customerText;

	/**
	 * {@link #customerText} accessor.
	 * @return	The value.
	 **/
	public String getCustomerText() {
		return customerText;
	}

	/**
	 * {@link #customerText} mutator.
	 * @param customerText	The new value.
	 **/
	@XmlElement
	public void setCustomerText(String customerText) {
		preset(customerTextPropertyName, customerText);
		this.customerText = customerText;
	}
}
