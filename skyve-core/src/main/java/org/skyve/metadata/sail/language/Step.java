package org.skyve.metadata.sail.language;

import org.skyve.impl.sail.execution.AutomationContext;

import jakarta.xml.bind.annotation.XmlTransient;

/**
 * Marker interface for all atomic steps in the SAIL language.
 *
 * <p>A {@code Step} is a single operation within a {@link Procedure} or an {@link Interaction}
 * method body — the "opcode" of the SAIL instruction set. Steps represent concrete
 * UI interactions (navigate, click, enter data, assert), context management (push/pop),
 * control flow (execute, pause), and test assertions (testValue, testSuccess, testFailure).
 *
 * <p>Each step is also {@link Executable}: it calls back to the matching
 * {@code execute*()} method on the {@link org.skyve.metadata.sail.execution.Executor}
 * visitor, dispatching to the correct handler without requiring instanceof checks.
 *
 * <p>The {@link #getIdentifier} method returns a human-readable string used by the
 * executor to label the step in logs, reports, and generated test code.
 *
 * @see Interaction
 * @see Procedure
 * @see org.skyve.metadata.sail.execution.Executor
 */
@XmlTransient
public interface Step extends Executable {
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	public String getIdentifier(AutomationContext context);
}
