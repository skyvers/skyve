package modules.test.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Override Narrowed Extended
 * <br/>
 * Customer override fixture: has a hand-written extension, and the test customer's override leaves out dropped.
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class OverrideNarrowedExtendedVanilla extends OverrideNarrowedExtended {
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
