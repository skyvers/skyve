package modules.test.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Override Narrowed Linked
 * <br/>
 * Customer override fixture: the test customer's override leaves out dropped, and it references OverrideLinkTarget, which the test2 customer overrides.
 * 
 * @navhas n target 0..1 OverrideLinkTarget
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class OverrideNarrowedLinkedVanilla extends OverrideNarrowedLinked {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String droppedPropertyName = "dropped";

	/**
	 * Dropped
	 **/
	private String dropped;

	/**
	 * {@link #dropped} accessor.
	 * @return	The value.
	 **/
	public String getDropped() {
		return dropped;
	}

	/**
	 * {@link #dropped} mutator.
	 * @param dropped	The new value.
	 **/
	@XmlElement
	public void setDropped(String dropped) {
		preset(droppedPropertyName, dropped);
		this.dropped = dropped;
	}
}
