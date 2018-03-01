package org.skyve.metadata.sail.language;

import javax.xml.bind.annotation.XmlTransient;

/**
 * Abstract of all implicit "OPCODES" in SAIL language that can participate in a Procedure.
 * @author mike
 */
@XmlTransient
public interface Step extends Executable {
	// Nothing to see here
}
