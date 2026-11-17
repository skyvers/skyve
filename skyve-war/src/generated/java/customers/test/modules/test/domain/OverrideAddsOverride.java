package customers.test.modules.test.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import modules.test.domain.OverrideAdds;
import org.skyve.impl.domain.ChangeTrackingArrayList;

/**
 * Override Adds
 * <br/>
 * The test customer's override of OverrideAdds, adding customerText, customerOnlyItem, customerOnlyItems and the customerTextSet condition.
 * 
 * @navhas n customerOnlyItem 0..1 OverrideCustomerOnly
 * @navhas n customerOnlyItems 0..n OverrideCustomerOnly
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class OverrideAddsOverride extends OverrideAdds {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String customerTextPropertyName = "customerText";

	/** @hidden */
	public static final String customerOnlyItemPropertyName = "customerOnlyItem";

	/** @hidden */
	public static final String customerOnlyItemsPropertyName = "customerOnlyItems";

	/**
	 * Customer Text
	 **/
	private String customerText;

	/**
	 * Customer Only Item
	 **/
	private OverrideCustomerOnly customerOnlyItem = null;

	/**
	 * Customer Only Items
	 **/
	private List<OverrideCustomerOnly> customerOnlyItems = new ChangeTrackingArrayList<>("customerOnlyItems", this);

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

	/**
	 * {@link #customerOnlyItem} accessor.
	 * @return	The value.
	 **/
	public OverrideCustomerOnly getCustomerOnlyItem() {
		return customerOnlyItem;
	}

	/**
	 * {@link #customerOnlyItem} mutator.
	 * @param customerOnlyItem	The new value.
	 **/
	@XmlElement
	public void setCustomerOnlyItem(OverrideCustomerOnly customerOnlyItem) {
		if (this.customerOnlyItem != customerOnlyItem) {
			preset(customerOnlyItemPropertyName, customerOnlyItem);
			this.customerOnlyItem = customerOnlyItem;
		}
	}

	/**
	 * {@link #customerOnlyItems} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<OverrideCustomerOnly> getCustomerOnlyItems() {
		return customerOnlyItems;
	}

	/**
	 * {@link #customerOnlyItems} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public OverrideCustomerOnly getCustomerOnlyItemsElementById(String bizId) {
		return getElementById(customerOnlyItems, bizId);
	}

	/**
	 * {@link #customerOnlyItems} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setCustomerOnlyItemsElementById(String bizId, OverrideCustomerOnly element) {
		setElementById(customerOnlyItems, element);
	}

	/**
	 * {@link #customerOnlyItems} add.
	 * @param element	The element to add.
	 **/
	public boolean addCustomerOnlyItemsElement(OverrideCustomerOnly element) {
		return customerOnlyItems.add(element);
	}

	/**
	 * {@link #customerOnlyItems} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addCustomerOnlyItemsElement(int index, OverrideCustomerOnly element) {
		customerOnlyItems.add(index, element);
	}

	/**
	 * {@link #customerOnlyItems} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeCustomerOnlyItemsElement(OverrideCustomerOnly element) {
		return customerOnlyItems.remove(element);
	}

	/**
	 * {@link #customerOnlyItems} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public OverrideCustomerOnly removeCustomerOnlyItemsElement(int index) {
		return customerOnlyItems.remove(index);
	}

	/**
	 * customerTextSet
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isCustomerTextSet() {
		return (customerText != null);
	}

	/**
	 * {@link #isCustomerTextSet} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotCustomerTextSet() {
		return (! isCustomerTextSet());
	}
}
