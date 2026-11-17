package customers.test2.modules.test.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import modules.test.OverrideEnumExtended.OverrideEnumExtendedExtension;

/**
 * Override Enum Extended
 * <br/>
 * The test2 customer's unchanged override of OverrideEnumExtended, keeping status.
 * 
 * @depend - - - Status
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class OverrideEnumExtendedOverride extends OverrideEnumExtendedExtension {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

}
