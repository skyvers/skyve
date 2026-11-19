package customers.test.modules.test.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import modules.test.domain.OverrideEnumJoinedSub;

/**
 * Override Enum Joined Sub
 * <br/>
 * The test customer's override of OverrideEnumJoinedSub, leaving out status.
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class OverrideEnumJoinedSubOverride extends OverrideEnumJoinedSub {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

}
