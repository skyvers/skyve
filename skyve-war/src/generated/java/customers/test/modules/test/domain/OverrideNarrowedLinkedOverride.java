package customers.test.modules.test.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import modules.test.domain.OverrideNarrowedLinked;

/**
 * Override Narrowed Linked
 * <br/>
 * The test customer's override of OverrideNarrowedLinked, leaving out dropped.
 * 
 * @navhas n target 0..1 OverrideLinkTarget
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class OverrideNarrowedLinkedOverride extends OverrideNarrowedLinked {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

}
