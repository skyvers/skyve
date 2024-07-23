package org.skyve.metadata.sail.language;

import org.skyve.impl.sail.execution.AutomationContext;

import jakarta.xml.bind.annotation.XmlTransient;

/**
 * Abstract of all implicit "OPCODES" in SAIL language that can participate in a Procedure.
 * @author mike
 */
@XmlTransient
public interface Step extends Executable {
	public String getIdentifier(AutomationContext context);
}
